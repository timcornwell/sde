C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)ipcscr.f	1.1    6/21/93
C
      SUBROUTINE SDEMAIN
C     
CD    Program to centroid IPCS data
C
C    Audit trail:
C             Cloned from ipcsdp
C                                         R.G. Marson     Dec 21 1992
C    
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C Parameter Definitions
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IPCSDP')
C
C Function definitions
C
      INTEGER              DATFGETI
C
C Local Variables
C
      CHARACTER*(SYSMXNAM) INFILE, OUTFILE, SORT
      CHARACTER ATYPE
      REAL    ZERO, VALUE
      INTEGER NPHOTONS, FRINDX, FRNPH, OUTINDX, NOTIFY
      INTEGER FRSTART, FREND, FR, NDUMMY, FRSIZEX, FRSIZEY
      INTEGER NAX, NAXIS(SYSMXDIM)
      INTEGER PADD, OXADD, OYADD, OFADD, ODADD
      INTEGER XADD, YADD, FADD, DADD
C     
C==================================================================
C     
      CALL MSGWELCO ('I centroid IPCS data')
      CALL USRCTL
C     
      CALL USRGETL ('Debug', SYSDEBUG, 1, NDUMMY)
      CALL USRGETI('Notify', NOTIFY, 1, NDUMMY)
C     
C     Get IPCS file
C     
      CALL USRGETC ('Infile', INFILE, 1, NDUMMY)
      CALL USRGETC ('Outfile', OUTFILE, 1, NDUMMY)
C
      CALL IPCSGET('IPCS', INFILE, '*', ' ')
      IF (ERROR) GOTO 999
C
C Sort the data
C
      CALL IPCSSORT('IPCS', 'IF')
C
C Get the start and stopping points
C
      FRSTART = DATFGETI('IPCS', 'FR_START')
      FREND   = DATFGETI('IPCS', 'FR_END')
      NPHOTONS = DATFGETI('IPCS', 'GCOUNT')
      FRSIZEX = DATFGETI('IPCS', 'WINDOW_W')
      FRSIZEY = DATFGETI('IPCS', 'WINDOW_H')
C
C Create the primary array
C
      NAXIS(1) = FRSIZEX
      NAXIS(2) = FRSIZEY
      CALL DATMAKAR('PRIMARY', 2, NAXIS, 'I', PADD)
C
C Create the output array (its size cannot be bigger than the input array)
C
      CALL DATCREAT('OUTPUT')
      NAXIS(1) = NPHOTONS
      CALL DATMAKAR('OUTPUT/XPIXEL', 1, NAXIS, 'R', OXADD)
      CALL DATMAKAR('OUTPUT/YPIXEL', 1, NAXIS, 'R', OYADD)
      CALL DATMAKAR('OUTPUT/FRAME', 1, NAXIS, 'R', OFADD)
      NAXIS(1) = 1
      NAXIS(2) = NPHOTONS
      CALL DATMAKAR('OUTPUT', 2, NAXIS, 'R', ODADD)
C
C Get the addresses of the input arrays
C
      CALL DATGETAR('IPCS', NAX, NAXIS, ATYPE, DADD)
      CALL DATGETAR('IPCS/XPIXEL', NAX, NAXIS, ATYPE, XADD)
      CALL DATGETAR('IPCS/YPIXEL', NAX, NAXIS, ATYPE, YADD)
      CALL DATGETAR('IPCS/FRAME', NAX, NAXIS, ATYPE, FADD)
C
C Check that all X, Y values are in range (by clipping them)
C 
      CALL PIXRCLIP(MEMR(XADD), 0.0, FLOAT(FRSIZEX - 1), 
     $     MEMR(XADD), NPHOTONS)
      CALL PIXRCLIP(MEMR(YADD), 0.0, FLOAT(FRSIZEY - 1), 
     $     MEMR(YADD), NPHOTONS)
C
C Initialise the primary array(s) to numbers much smaller than 
C  the first data frame
C
      ZERO = 0.0
      VALUE = FLOAT(FRSTART-1000)
      CALL ARRSETCO('PRIMARY', ZERO, VALUE)
      OUTINDX = 0
C
C Loop around once once for each frame
C
      DO FR = FRSTART, FREND
         IF (MOD(FR-FRSTART+1,NOTIFY).EQ.0) THEN
            WRITE(MESSAGE, '(A, I6, A, I6)') 
     $           'Doing Frame ', FR-FRSTART+1, 
     $           ' out of ', FREND-FRSTART + 1
            CALL MSGPUT(MESSAGE, 'I')
         END IF
C
C Find the bounds of the current frame
C
         CALL IPCWIN(MEMR(FADD), NPHOTONS, FR, FR+1, FRINDX, 
     $        FRNPH)
C
C Now centroid this Frame
C
         FRINDX = FRINDX - 1
         CALL PIXRCR(MEMI(PADD), FRSIZEX - 1, FRSIZEY - 1, 
     $        MEMR(DADD+FRINDX), MEMR(XADD+FRINDX), MEMR(YADD+FRINDX), 
     $        MEMR(FADD+FRINDX), FRNPH,
     $        MEMR(ODADD+OUTINDX), MEMR(OXADD+OUTINDX), 
     $        MEMR(OYADD+OUTINDX), MEMR(OFADD+OUTINDX))
         OUTINDX = OUTINDX + FRNPH
      END DO
C
C End of Frame loop
C
 980  CONTINUE
C
C We now know how big the final data set is so fiddle the axis to reflect this
C
      NAXIS(1) = OUTINDX
      CALL DATPUTI ('OUTPUT/XPIXEL', 'ARRAY/NAXIS', NAXIS, 1)
      CALL DATPUTI ('OUTPUT/YPIXEL', 'ARRAY/NAXIS', NAXIS, 1)
      CALL DATPUTI ('OUTPUT/FRAME', 'ARRAY/NAXIS', NAXIS, 1)
      NAXIS(2) = NAXIS(1)
      NAXIS(1) = 1
      CALL DATPUTI ('OUTPUT', 'ARRAY/NAXIS', NAXIS, 2)
C
C Sort out the header 
C
      CALL HEDCOPY('IPCS', 'OUTPUT')
C
C Sort out the history (note hisinput now done in ipcsput)
C
      CALL HISCOPY('IPCS', 'OUTPUT')
      MESSAGE = '----------------Centroiding Statistics---------------'
      CALL HISPUT('OUTPUT', MESSAGE)
      WRITE(MESSAGE, 777) NPHOTONS-OUTINDX, NPHOTONS,
     $     FLOAT(NPHOTONS-OUTINDX)/FLOAT(NPHOTONS)*100.
 777  FORMAT('Removed ', I10, ' Photons out of ', I10, ': ', F6.2,'%')
      CALL MSGPUT(MESSAGE, 'I')
      CALL HISPUT('OUTPUT', MESSAGE)
      MESSAGE = '-----------------------------------------------------'
      CALL HISPUT('OUTPUT', MESSAGE)
C
C Now put in the new header info
C 
      CALL DATPUTI('OUTPUT', 'GCOUNT', OUTINDX, 1)
C
C And write out the file
C
      CALL IPCSPUT ('OUTPUT', OUTFILE)
C
 999  CONTINUE
      END












