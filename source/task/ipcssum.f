C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)ipcssum.f	1.5    2/1/93
C
      SUBROUTINE SDEMAIN
C
CD    Program to sum IPCS data in a variety of ways (see .inf file)
C     
C    Arguments: CALL sdemain
C    Audit trail:
C             Cloned from ipcstv
C                                         R.G. Marson     Feb 1 1990
C             Rewritten to increase its speed
C                                         R.G. Marson     June 20, 1991
C    
C-----------------------------------------------------------------------
#include	"stdinc.h"
C     
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IPCSSUM')
C     
C Function Definitions
C
      CHARACTER*(SYSMXNAM) STRM2
      INTEGER DATFGETI, DATADD, STRLEN
C
C Local Variables
C
      CHARACTER*(SYSMXNAM) INFILE, OUTFILE, SORT
      CHARACTER*(SYSMXNAM) SAXIS, AAXIS, BAXIS
      CHARACTER*(SYSMXNAM) AONAME, BONAME, SONAME
      CHARACTER*(SYSMXNAM) SNAME, ANAME, BNAME
      REAL EXPAND
      INTEGER ASIZE, BSIZE, SCSIZE, MAXOSIZE, FRSIZE, ASTART, BSTART
      INTEGER NAXIS(SYSMXDIM)
      INTEGER TRC(SYSMXDIM), BLC(SYSMXDIM), WSTEP(SYSMXDIM)
      INTEGER NDUMMY, DTSTART, DTSTOP, I, WSUM(SYSMXDIM), INDEX, NUMPH
      INTEGER START, STOP, STEP, WIDTH, SUM, OUTINDEX, OUTSUM
      INTEGER CURSTART, NPHOTONS, NOTIFY
      INTEGER SADD, SCADD, AADD, BADD, DADD, AOADD, BOADD
      INTEGER DOADD, SOADD
C     
C=======================================================================
C     
      CALL MSGWELCO ('I sum IPCS data')
      CALL USRCTL
C     
      CALL USRGETL ('Debug', SYSDEBUG, 1, NDUMMY)
      CALL USRGETR ('Expand', expand, 1, NDUMMY)
C     
C     Get IPCS file
C     
      CALL USRGETC ('Infile', INFILE, 1, NDUMMY)
      CALL USRGETC ('Outfile', OUTFILE, 1, NDUMMY)
C
      CALL IPCSGET('IPCS', INFILE, '*', ' ')
      IF (ERROR) GOTO 999
C
C Here DTSTART (Data Start) coresponds to the first column of data
C      DTSTOP  (Data Stop) corresponds to the last column of data
C      {AB}SIZE is the size of the array in the other dimensions
C
      CALL USRGETC ('Axis', SAXIS, 1, NDUMMY)
      CALL STRUC(SAXIS, AAXIS)
      SAXIS = AAXIS
      IF (SAXIS(1:1).EQ.'X') THEN
         DTSTART = 0
         DTSTOP = DATFGETI('IPCS', 'WINDOW_W') - 1
         SORT = 'IX'
         AAXIS = 'FRAME'
         ASTART = DATFGETI('IPCS', 'FR_START')
         ASIZE = DATFGETI('IPCS', 'FR_END') - ASTART + 1
         BAXIS = 'YPIXEL'
         BSTART = 0
         BSIZE = DATFGETI('IPCS', 'WINDOW_H')
      ELSE IF (SAXIS(1:1).EQ.'Y') THEN
         DTSTART = 0
         DTSTOP = DATFGETI('IPCS', 'WINDOW_H') - 1
         SORT = 'IY'
         AAXIS = 'FRAME'
         ASTART = DATFGETI('IPCS', 'FR_START')
         ASIZE = DATFGETI('IPCS', 'FR_END') - ASTART + 1
         BAXIS = 'XPIXEL'
         BSTART = 0
         BSIZE = DATFGETI('IPCS', 'WINDOW_W')
      ELSE IF (SAXIS(1:1).EQ.'F') THEN
         DTSTART = DATFGETI('IPCS', 'FR_START')
         DTSTOP = DATFGETI('IPCS', 'FR_END')
         SORT = 'IF'
         AAXIS = 'XPIXEL'
         ASTART = 0
         ASIZE = DATFGETI('IPCS', 'WINDOW_W')
         BAXIS = 'YPIXEL'
         BSTART = 0
         BSIZE = DATFGETI('IPCS', 'WINDOW_H')
      ELSE
         CALL ERRREPOR(ERRNTFND, ROUTINE, 
     $        'Axis must be either XPIXEL, YPIXEL or FRAME')
         GOTO 999
      END IF
      SNAME = STRM2('IPCS', SAXIS)
      SADD = DATADD(SNAME)
      ANAME = STRM2('IPCS', AAXIS)
      AADD = DATADD(ANAME)
      BNAME = STRM2('IPCS', BAXIS)
      BADD = DATADD(BNAME)
      DADD = DATADD('IPCS')
      NPHOTONS = DATFGETI('IPCS', 'GCOUNT')
C
C ADjust the A and B Axis so that there minimum is 1 (for arrscat)
C
C
      IF (ASTART.NE.1) CALL PIXRSCAL(MEMR(AADD), 1.0, 
     $     FLOAT(1-ASTART), MEMR(AADD), NPHOTONS)
      IF (BSTART.NE.1) CALL PIXRSCAL(MEMR(BADD), 1.0, 
     $     FLOAT(1-BSTART), MEMR(BADD), NPHOTONS)
C
C Get the usr summing parameters
C
      CALL USRGETI ('Start', START, 1, NDUMMY)
      CALL USRGETI ('Stop', STOP, 1, NDUMMY)
      CALL USRGETI ('Step', STEP, 1, NDUMMY)
      CALL USRGETI ('Sum', SUM, 1, NDUMMY)
C
C Start and Stop bracket the the data exclusively. ie Start=10,Stop=10
C    Takes one column only. Start=0,Stop=4 takes 5 columns
C
      START = MAX(MIN(START, DTSTOP), DTSTART)
      IF (STOP.LT.0) STOP = DTSTOP
      STOP = MAX(MIN(STOP, DTSTOP), START)
      WIDTH = STOP - START + 1
      IF ((STEP.LE.0).OR.(STEP.GT.WIDTH)) STEP = WIDTH
      IF ((SUM.LE.0).OR.(SUM.GT.WIDTH)) SUM = WIDTH
C
      WRITE (MESSAGE, 1000) SAXIS, START, SAXIS, STOP
 1000 FORMAT('Summing from ', A6, ' = ', I7,
     $     ' to ', A6, ' = ', I7)
      CALL MSGPUT(MESSAGE, 'I')
      WRITE (MESSAGE, '(A, I6, 1X, A6, A, I6,1X,  A6, A)') 
     $     'Stepping every ', STEP, 
     $     SAXIS, '(s) and summing over ', SUM, SAXIS, '(s)'
      CALL MSGPUT(MESSAGE, 'I')
C
C Check that the data is sorted the way we want it
C
      CALL IPCSSORT('IPCS', SORT)
C
C Make a set tempory arrays to hold the output data
C As we do not know how big these are yet we make a guess based on the
C input file size and the summing parameters. To be on the safe side
C this is multiplyed by the expansion factor. If the out array is to small
C then just truncate the result
C
      AONAME = STRM2('OUTPUT', AAXIS)
      BONAME = STRM2('OUTPUT', BAXIS)
      SONAME = STRM2('OUTPUT', SAXIS)
      IF (EXPAND.LE.0.0) EXPAND = 1.5
C This expression is converted to floating point early to prevent problems
C  with integer overflow
      MAXOSIZE = NINT(
     $     FLOAT(NPHOTONS) * FLOAT(SUM) * FLOAT(STOP - START + 1) /
     $     (FLOAT(STEP) * FLOAT(DTSTOP-DTSTART + 1)) * EXPAND)
      NAXIS(1) = MAXOSIZE
      CALL DATMAKAR(AONAME, 1, NAXIS, 'R',  AOADD)
      CALL DATMAKAR(BONAME, 1, NAXIS, 'R',  BOADD)
      CALL DATMAKAR(SONAME, 1, NAXIS, 'R',  SOADD)
      CALL PIXRSETC(MEMR(AOADD), 0.0, NAXIS(1))
      CALL PIXRSETC(MEMR(BOADD), 0.0, NAXIS(1))
      CALL PIXRSETC(MEMR(SOADD), 0.0, NAXIS(1))
      NAXIS(2) = NAXIS(1)
      NAXIS(1) = 1
      CALL DATMAKAR('OUTPUT', 2, NAXIS, 'R',  DOADD)
      CALL PIXRSETC(MEMR(DOADD), 0.0, NAXIS(2))
C
C Make tempory array to store the results of arrscat
C
      IF (SUM.GT.1) THEN
         NAXIS(1) = ASIZE
         NAXIS(2) = BSIZE
         CALL DATMAKAR('SCATTER', 2, NAXIS, 'R', SCADD)
         SCSIZE = ASIZE*BSIZE
      END IF
C
C An array to hold the window parameters
C
      DO I = 1, SYSMXDIM
         BLC(I) = 1
         TRC(I) = 1
         WSTEP(I) = 1
         WSUM(I) = 1
      END DO
C
C Go around the loop once for each output frame
C
      CALL USRGETI('Notify', NOTIFY, 1, NDUMMY)
      OUTINDEX = 0
      OUTSUM = 0
      DO CURSTART = START, STOP, STEP
         IF (MOD(CURSTART-START+1,NOTIFY).LT.STEP) THEN
            WRITE(MESSAGE, '(A, A, I6, A, I6)') 
     $           'Doing ', SAXIS(1:STRLEN(SAXIS)), CURSTART-START+1, 
     $           ' out of ', STOP-START + 1
            CALL MSGPUT(MESSAGE, 'I')
         END IF
C
C Search for the start and end points of the array
C
         CALL IPCWIN(MEMR(SADD), NPHOTONS, CURSTART, CURSTART + SUM, 
     $        INDEX, NUMPH)
         BLC(1) = INDEX
         INDEX = INDEX - 1
         TRC(1) = INDEX + NUMPH
C
C Form a scatter plot (to sum the data)
C
         IF (SUM.GT.1) THEN
            CALL PIXRSETC(MEMR(SCADD), 0.0, SCSIZE)
            CALL PIXRSCAT(MEMR(AADD+INDEX), MEMR(BADD+INDEX), 
     $           MEMR(DADD+INDEX), NUMPH,
     $           MEMR(SCADD), ASIZE, BSIZE)
C
C Recover only the nonxero pixels
C
            FRSIZE = MAXOSIZE-OUTSUM
            CALL PIXRDSCAT(MEMR(SCADD), ASIZE, BSIZE, 
     $           MEMR(AOADD+OUTSUM), MEMR(BOADD+OUTSUM), 
     $           MEMR(DOADD+OUTSUM), FRSIZE)
            CALL PIXRSETC(MEMR(SOADD+OUTSUM), FLOAT(OUTINDEX), FRSIZE)
         ELSE
C
C If no summing of data is required then just copy the appropriate bits
C
            FRSIZE = NUMPH
            CALL PIXRSUBS (MEMR(AADD), MEMR(AOADD+OUTSUM), 
     $           NPHOTONS,        1,1,1,1,1,1,
     $           NPHOTONS-OUTSUM, 1,1,1,1,1,1,BLC,TRC,WSTEP,WSUM)
            CALL PIXRSUBS (MEMR(BADD), MEMR(BOADD+OUTSUM), 
     $           NPHOTONS,       1,1,1,1,1,1,
     $           NPHOTONS-OUTSUM,1,1,1,1,1,1,BLC,TRC,WSTEP, WSUM)
            CALL PIXRSETC(MEMR(SOADD+OUTSUM), FLOAT(OUTINDEX), FRSIZE)
            CALL PIXRSUBS (MEMR(DADD), MEMR(DOADD+OUTSUM), 
     $           NPHOTONS,       1,1,1,1,1,1,
     $           NPHOTONS-OUTSUM,1,1,1,1,1,1,BLC,TRC,WSTEP, WSUM)
         END IF
         OUTINDEX = OUTINDEX + 1
         OUTSUM = OUTSUM + FRSIZE
      END DO
C
C We now know how big the final data set is so fiddle the axis to reflect this
C
      IF (OUTSUM.LT.MAXOSIZE) THEN
         NAXIS(1) = 1
         NAXIS(2) = OUTSUM
         CALL DATPUTI('OUTPUT/ARRAY', 'NAXIS', NAXIS, 2)
         NAXIS(1) = NAXIS(2)
         CALL DATPUTI(STRM2(AONAME,'ARRAY'), 'NAXIS', NAXIS, 1)
         CALL DATPUTI(STRM2(BONAME,'ARRAY'), 'NAXIS', NAXIS, 1)
         CALL DATPUTI(STRM2(SONAME,'ARRAY'), 'NAXIS', NAXIS, 1)
      ELSE
         MESSAGE = 'Output file truncated'
         CALL MSGPUT(MESSAGE, 'W')
      ENDIF
C
C ADjust the A and B Axis so that the minimum is what it was
C
      IF (ASTART.NE.1) CALL PIXRSCAL(MEMR(AOADD), 1.0, 
     $     FLOAT(ASTART-1), MEMR(AOADD), OUTSUM)
      IF (BSTART.NE.1) CALL PIXRSCAL(MEMR(BOADD), 1.0, 
     $     FLOAT(BSTART-1), MEMR(BOADD), OUTSUM)

C
C Sort out the header 
C
      CALL HEDCOPY('IPCS', 'OUTPUT')
      CALL DATPUTI('OUTPUT', 'GCOUNT', OUTSUM, 1)
      IF (SAXIS(1:1).EQ.'F') THEN
         CALL DATPUTI('OUTPUT', 'FR_START', 0, 1)
         CALL DATPUTI('OUTPUT', 'FR_END', OUTINDEX-1, 1)
      ELSE IF(SAXIS(1:1).EQ.'X') THEN
         CALL DATPUTI('OUTPUT', 'WINDOW_W', OUTINDEX, 1)
      ELSE
         CALL DATPUTI('OUTPUT', 'WINDOW_H', OUTINDEX, 1)
      END IF
      CALL DATPUTC('OUTPUT', 'SORT', SORT, 1)
C
C Sort out the history (note hisinput now done in ipcsput)
C
      CALL HISCOPY('IPCS', 'OUTPUT')
C
C And write out the file
C
      CALL IPCSPUT ('OUTPUT', OUTFILE)
C
 999  CONTINUE
      END
