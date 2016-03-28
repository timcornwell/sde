C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)ipcsfilt.f	1.1    6/21/93
C
      SUBROUTINE SDEMAIN
C     
CD    Program to filter IPCS data for bright pixels and out of range photons
C
C    Audit trail:
C             Cloned from ipcsdp
C                                         R.G. Marson     Dec 30 1992
C    
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C Parameter Definitions
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IPCSFILT')
      INTEGER           MAXNBAD
      PARAMETER         (MAXNBAD = 100)
C
C Function definitions
C
      INTEGER              DATFGETI, STRLEN
C
C Local Variables
C
      CHARACTER*(SYSMXNAM) INFILE, OUTFILE, BADFILE, LINE
      CHARACTER ATYPE
      INTEGER NBAD, BADX(MAXNBAD), BADY(MAXNBAD), BADN(MAXNBAD)
      INTEGER NPHOTONS, OUTINDX, NOTIFY, I, NCHAR, PH,  GAP
      INTEGER NDUMMY, FRSIZEX, FRSIZEY, XLOC, YLOC, FR, LASTFR
      INTEGER BADFR, NBADFR
      INTEGER NAX, NAXIS(SYSMXDIM)
      INTEGER OXADD, OYADD, OFADD, ODADD
      INTEGER XADD, YADD, FADD, DADD
      LOGICAL EOF, BAD
C     
C==================================================================
C     
      CALL MSGWELCO ('I filter IPCS data')
      CALL USRCTL
C     
      CALL USRGETL ('Debug', SYSDEBUG, 1, NDUMMY)
      CALL USRGETI('Notify', NOTIFY, 1, NDUMMY)
      CALL USRGETI('Gap', GAP, 1, NDUMMY)
C     
C     Get IPCS file
C     
      CALL USRGETC ('Infile', INFILE, 1, NDUMMY)
      CALL USRGETC ('Badfile', BADFILE, 1, NDUMMY)
      CALL USRGETC ('Outfile', OUTFILE, 1, NDUMMY)
C
C Get the Bad Pixels file
C
      IF (BADFILE.NE.' ') THEN
         NBAD = 0
         CALL TXTOPEN(ROUTINE, BADFILE, 'READ')
         DO I = 1, MAXNBAD
            CALL TXTREAD (ROUTINE, LINE, NCHAR, EOF)
            IF (ERROR) GOTO 990
            IF ((NCHAR.EQ.0).OR.(EOF)) GOTO 100
            NBAD = NBAD + 1
            READ (LINE, *, ERR = 200) BADX(NBAD), BADY(NBAD)
            BADX(I) = BADX(I) - 1
            BADY(I) = BADY(I) - 1
         END DO
 100     CONTINUE
         CALL TXTCLOSE(ROUTINE)
         WRITE(MESSAGE, '(A, I2, A, A)') 'Read ', NBAD, 
     $        ' Bright Pixels from ', BADFILE(1:STRLEN(BADFILE))
         CALL MSGPUT(MESSAGE, 'I')
      END IF
C
C Get the IPCS file
C
      CALL IPCSGET('IPCS', INFILE, '*', ' ')
      IF (ERROR) GOTO 999
C
C Get the start and stopping points
C
      NPHOTONS = DATFGETI('IPCS', 'GCOUNT')
      FRSIZEX = DATFGETI('IPCS', 'WINDOW_W')
      FRSIZEY = DATFGETI('IPCS', 'WINDOW_H')
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
C Sort out the history (note hisinput now done in ipcsput)
C
      CALL HISCOPY('IPCS', 'OUTPUT')
      MESSAGE = '--------------------Filtering Info-------------------'
      CALL HISPUT('OUTPUT', MESSAGE)
C
C Get the addresses of the input arrays
C
      CALL DATGETAR('IPCS', NAX, NAXIS, ATYPE, DADD)
      CALL DATGETAR('IPCS/XPIXEL', NAX, NAXIS, ATYPE, XADD)
      CALL DATGETAR('IPCS/YPIXEL', NAX, NAXIS, ATYPE, YADD)
      CALL DATGETAR('IPCS/FRAME', NAX, NAXIS, ATYPE, FADD)
C
C Pre main loop definitions
C
      OUTINDX = 0
      DO I = 1, NBAD
         BADN(I) = 0
      END DO
      LASTFR = DATFGETI('IPCS', 'FR_START')
      NBADFR = 0
      BADFR = LASTFR - 1
C
C Loop around once once for each photon
C
      DO PH = 0, NPHOTONS - 1
         IF (MOD(PH+1,NOTIFY).EQ.0) THEN
            WRITE(MESSAGE, '(A, I8, A, I8)') 
     $           'Doing Photon ', PH+1, 
     $           ' out of ', NPHOTONS
            CALL MSGPUT(MESSAGE, 'I')
         END IF
C
C Check if this photon is out of range
C
         XLOC = NINT(MEMR(XADD+PH))
         YLOC = NINT(MEMR(YADD+PH))
         FR = NINT(MEMR(FADD+PH))
         IF ( (XLOC.GE.FRSIZEX).OR.(XLOC.LT.0)
     $        .OR.(YLOC.GE.FRSIZEY).OR.(YLOC.LT.0)) THEN
            WRITE(MESSAGE, '(A, I3, A, I3, A, I6, A)') 
     $           'Photon at X:', XLOC, ' Y:', YLOC, 
     $           ' Frame:', FR,
     $           ' is outside detector (Discarded)'
            CALL MSGPUT(MESSAGE, 'I')
            CALL HISPUT('OUTPUT', MESSAGE)
C
C Check if this photon has a bad frame number (ie. out of sequence)
C
         ELSE IF ((FR.LT.LASTFR).OR.(FR.GT.(LASTFR+GAP))) THEN
            IF (FR.NE.BADFR) THEN
               WRITE(MESSAGE, '(A, I8, A, I8, A)') 
     $              'Bad Frame:', FR,  
     $              ' Last Valid Frame:', LASTFR, 
     $              ' (Discarded) '
               CALL MSGPUT(MESSAGE, 'I')
               CALL HISPUT('OUTPUT', MESSAGE)
               BADFR = FR
               NBADFR = 1
            ELSE
               NBADFR = NBADFR + 1
            END IF
         ELSE 
            IF (NBADFR.GT.0) THEN
               WRITE(MESSAGE, '(A, I8, A)') 
     $              'Discarded ', NBADFR,  
     $              ' Photons from the last Bad Frame'
               CALL MSGPUT(MESSAGE, 'I')
               CALL HISPUT('OUTPUT', MESSAGE)
               NBADFR = 0
            END IF
C
C Check if this photon is on a bad pixel
C
            BAD = .FALSE.
            DO I = 1, NBAD
               IF ((XLOC.EQ.BADX(I)).AND.(YLOC.EQ.BADY(I))) THEN
                  BAD = .TRUE.
                  BADN(I) = BADN(I) + 1
               END IF
            END DO
            IF (.NOT.BAD) THEN
C
C Good photon so save it
C
               OUTINDX = OUTINDX + 1
               MEMR(ODADD + OUTINDX) = MEMR(DADD + PH)
               MEMR(OXADD + OUTINDX) = MEMR(XADD + PH)
               MEMR(OYADD + OUTINDX) = MEMR(YADD + PH)
               MEMR(OFADD + OUTINDX) = MEMR(FADD + PH)
               LASTFR = FR
            END IF
         END IF
      END DO
C
C End of Photon loop
C
      DO I = 1, NBAD
         IF (BADN(I).GT.0) THEN
            WRITE(MESSAGE, '(A, I4, A, I4, A, I4)') 'Removed ', BADN(I), 
     $      ' Photons from bright pixel at ', 
     $           BADX(I) + 1, ':', BADY(I) + 1
            CALL MSGPUT(MESSAGE, 'I')
            CALL HISPUT('OUTPUT', MESSAGE)
         END IF
      END DO
      WRITE (MESSAGE, '(A, I8, A, I8, A, F6.2, A)')
     $     'Removed ', NPHOTONS - OUTINDX, ' Photons out of ', NPHOTONS, 
     $     ': ', FLOAT(NPHOTONS-OUTINDX)/FLOAT(NPHOTONS)*100., '%'
      CALL MSGPUT(MESSAGE, 'I')
      CALL HISPUT('OUTPUT', MESSAGE)
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
      MESSAGE = '-----------------------------------------------------'
      CALL HISPUT('OUTPUT', MESSAGE)
      CALL HEDCOPY('IPCS', 'OUTPUT')
C
C Now put in the new header info
C 
      CALL DATPUTI('OUTPUT', 'GCOUNT', OUTINDX, 1)
C#      CALL DATPUTC('OUTPUT', 'PTYPE1', 'WAVE',    1)
C#      CALL DATPUTR('OUTPUT', 'PSCAL1',  1.36E-11, 1)
C#      CALL DATPUTR('OUTPUT', 'PZERO1',  5.63E-7, 1)
C#      CALL DATPUTC('OUTPUT', 'PTYPE2', 'DEC-', 1)
C#      CALL DATPUTR('OUTPUT', 'PSCAL2',  2.06E-6, 1)
C#      CALL DATPUTR('OUTPUT', 'PZERO2', -2.47E-4, 1)
C
C And write out the file
C
      CALL IPCSPUT ('OUTPUT', OUTFILE)
      GOTO 999
C
C Can jump to here if an error found
C
 200  CALL ERRREPOR (ERRINPUT, ROUTINE, 'Error in line '//LINE)
      GOTO 999
 990  IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
 999  CONTINUE
      END
