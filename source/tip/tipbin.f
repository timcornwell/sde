C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)tipbin.f	1.1    3/26/93
C
      SUBROUTINE TIPBIN (DB, BINSUB, STIME, CALIB, BINS, NB, WHAT, T)
C
CD Bin the tipper data
C
C	DB	CH*(*)	inp	Tipper DataBase
C	BINSUB	INT	inp	Name prefix of tipper subdirectory
C	STIME	INT(2)	inp	Time to select on
C	CALIB	INT	inp	Do calibrators or just SKY?
C	BINS	R(*)	inp	Define the bin range
C	NB	INT	inp	How many bins
C	WHAT	CH*(*)	inp	What to Bin on
C	T	REAL	inp	Characteristic Time to bin on
C
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	4 Jan 1992
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	DB, WHAT
      REAL		T, BINS(*)
      INTEGER		STIME(2), NB, BINSUB, CALIB
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'TIPBIN')
C
      INTEGER		NBINS

      INTEGER		I, NAX, NAXIS(SYSMXDIM), IT, JT, IL
      INTEGER		I1, I2, MAXPTS, IBIN, INAME(30)
      INTEGER		BADD, DADD, LADD, LADD2, T1ADD, T2ADD
      INTEGER		NDUMMY, NHERE, NTOT, NREJECT
      REAL		TEST, CASD, CSSF, FRACTION, SCALE
      CHARACTER*1	TYPE
      CHARACTER*(SYSMXNAM)	ANAME, BNAME(30), TIMENOW, WHAT2
      LOGICAL		ISACALIB
C
      CHARACTER*(SYSMXNAM)	STRM2, STRM3
      INTEGER			DATADD
      CHARACTER*15		STRINT
      LOGICAL			DATEXIST
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      NBINS = 0
      DO 10 I = NB, 1, -1
         IF (BINS(I) .NE. 0.0) THEN
            NBINS = I
            GOTO 20
         ENDIF
 10   CONTINUE
 20   CONTINUE
      IF (NBINS .EQ. 0) THEN
         CALL ERRREPOR( ERRBDARG, ROUTINE, 'No Bins Found')
         GOTO 999
      ENDIF
C
      MAXPTS = I2-I1+1
      IF (MAXPTS .LE. 0) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'No points to plot')
         GOTO 999
      ENDIF
C
C Create BIN Subdirectories, CASD, and CSSF
C
      DO 30 IBIN=1, NBINS
         INAME(IBIN) = 100000 * BINSUB + 1000 * BINS(IBIN)
         BNAME(IBIN) = STRINT(INAME(IBIN))
C
         IF (DATEXIST (STRM2(DB, BNAME(IBIN)))) THEN
            CALL DATDELET (STRM2(DB, BNAME(IBIN)))
         ENDIF
         CALL DATCREAT (STRM2(DB, BNAME(IBIN)))
         CALL DATCREAT (STRM3(DB, BNAME(IBIN), 'CAV'))
         CALL DATCREAT (STRM3(DB, BNAME(IBIN), 'CSF'))
         CALL DATMAKAR (STRM3(DB, BNAME(IBIN), 'CAV/ASD'), 1, 9, 
     $      'R', BADD)
         CALL ARRSETCO (STRM3(DB, BNAME(IBIN), 'CAV/ASD'), 0.0, 0.0)
         CALL DATMAKAR (STRM3(DB, BNAME(IBIN), 'CSF/SSF'), 1, 9, 
     $      'R', BADD)
         CALL ARRSETCO (STRM3(DB, BNAME(IBIN), 'CSF/SSF'), 0.0, 0.0)
         CALL DATPUTI  (STRM2(DB, BNAME(IBIN)), 'NHERE', 0, 1)
         IF (CALIB .EQ. 1) THEN
            CALL DATPUTL  (STRM2(DB, BNAME(IBIN)), 'Calibrator',
     $         .TRUE.,1)
         ELSE
            CALL DATPUTL  (STRM2(DB, BNAME(IBIN)), 'Calibrator',
     $         .FALSE.,1)
         ENDIF
         CALL DATPUTC  (STRM2(DB, BNAME(IBIN)), 'OriginalFileName',
     $      'BINNED DATA', 1)
         CALL DATPUTC  (STRM2(DB, BNAME(IBIN)), 'NewFileName',
     $      'BINNED DATA', 1)
         CALL DATMAKAR (STRM3(DB, BNAME(IBIN), 'CSF/TIME'), 1, 9, 'R',
     $      T1ADD)
         CALL DATMAKAR (STRM3(DB, BNAME(IBIN), 'CAV/TIME'), 1, 9, 'R',
     $      T2ADD)
         DO 28 I = 0, 8
            MEMR(T1ADD + I) =  3.5 * 2.0**(FLOAT(I))
            MEMR(T2ADD + I) =  3.5 * 2.0**(FLOAT(I))
 28      CONTINUE
  30   CONTINUE
C
C Add New Names to the LIST
C
      CALL DATGETAR (STRM2(DB, 'LIST'), NAX, NAXIS, TYPE, LADD)
      CALL DATMAKAR (STRM2(DB, 'LIST2'), 1, (NAXIS(1)+NBINS), 'I',
     $   LADD2)
      CALL PIXICOPY (MEMI(LADD), 1, MEMI(LADD2), 1, NAXIS(1))
      DO 40 IBIN = 1, NBINS
         MEMI(LADD2 + NAXIS(1) + IBIN - 1) = INAME(IBIN)
 40   CONTINUE
      CALL DATDELET (STRM2(DB, 'LIST'))
      CALL ARRCOPY (STRM2(DB, 'LIST2'), STRM2(DB, 'LIST')) 
      CALL UTLISORT(NAXIS(1)+NBINS, MEMI(DATADD(STRM2(DB, 'LIST2'))),
     $   MEMI(DATADD(STRM2(DB, 'LIST'))) )
      CALL DATDELET (STRM2(DB, 'LIST2'))
      CALL TIPRANGE (DB, STIME, I1, I2)
      CALL DATGETAR (STRM2(DB, 'LIST'), NAX, NAXIS, TYPE, LADD)
C
      IF (T .LT. 3.5) T = 3.5
      IT = NINT( ALOG10 (T/3.5) / ALOG10(2.0) )
      IF (IT .GT. 8) IT = 8
      JT = 3.5 * 2.0**FLOAT(IT)
      WRITE (MESSAGE, 1969) WHAT(1:5), JT
 1969 FORMAT ('Binning ',A5,'  at ',F6.1,' s using bins:')
      CALL MSGPUT (MESSAGE, 'B')
      WRITE (MESSAGE, 1979) (BINS(IBIN), IBIN=1, NBINS)
 1979 FORMAT (<NBINS>F10.2)
      CALL MSGPUT (MESSAGE, 'B')
      IF (WHAT .EQ. 'CASD') THEN
         WHAT2 = 'CAV/ASD'
      ELSE IF (WHAT .EQ. 'CSSF') THEN
         WHAT2 = 'CSF/SSF'
      ELSE
         CALL ERRREPOR (ERRLOGIC, ROUTINE,
     $      'Code doesn''t exist for WHAT='//WHAT)
         GOTO 990
      ENDIF
C
      NTOT = 0
      DO 500 IL = I1, I2
         TIMENOW = STRINT(MEMI(LADD+IL))
         ANAME = STRM2(DB, TIMENOW)
         CALL DATGETL (ANAME, 'Calibrator', ISACALIB, 1, NDUMMY)
         IF (ERROR) GOTO 990
         IF (( ISACALIB .AND. (CALIB .EQ. 1 .OR. CALIB .EQ. 2)).OR.
     $       (.NOT.ISACALIB .AND. (CALIB .EQ. 0 .OR. CALIB .EQ. 2)))
     $      THEN
            CALL DATGETAR ( STRM2(ANAME, WHAT2), NAX, NAXIS, TYPE, 
     $         DADD)
            NTOT = NTOT + 1
            TEST = MEMR(DADD + IT)
            DO 310 IBIN = 1, NBINS
               IF (TEST .LE. BINS(IBIN)) GOTO 320
 310        CONTINUE
            GOTO 500
 320        CONTINUE
            CALL DATGETI (STRM2(DB, BNAME(IBIN)), 'NHERE', NHERE, 1,
     $         NDUMMY)
            NHERE = NHERE + 1
            CALL DATPUTI (STRM2(DB, BNAME(IBIN)), 'NHERE', NHERE, 1)
            CALL ARRADD (STRM3(DB, BNAME(IBIN), 'CAV/ASD'), 
     $         STRM2( ANAME, 'CAV/ASD'),
     $         STRM3( DB, BNAME(IBIN), 'CAV/ASD') )
            CALL ARRADD (STRM3(DB, BNAME(IBIN), 'CSF/SSF'), 
     $         STRM2( ANAME, 'CSF/SSF'),
     $         STRM3(DB, BNAME(IBIN), 'CSF/SSF') )
         ENDIF
 500  CONTINUE
C
      IF (NTOT .EQ. 0) THEN
         CALL ERRREPOR (ERRLOGIC, ROUTINE,
     $      'Apparently no valid data')
         GOTO 999
      ENDIF
      NREJECT = NTOT
      WRITE (MESSAGE, 1945) WHAT(1:6), T
 1945 FORMAT ('Binned Data selected on ',A6, ' at ',F5.0,' s')
      CALL TIPCOMME (STRM2(DB, BNAME(1)), ' ')
      CALL TIPCOMME (STRM2(DB, BNAME(1)), MESSAGE)
      WRITE (MESSAGE, 1946) STIME(1), STIME(2)
 1946 FORMAT ('Binned Data from ',I8,' to ', I8)
      CALL TIPCOMME (STRM2(DB, BNAME(1)), MESSAGE)
      CALL MSGPUT ('Binning Results:', 'B')
      CALL MSGPUT ('Bin         N   frac   Mean CASD   Mean CSSF ', 'B')
      CALL TIPCOMME (STRM2(DB, BNAME(1)),
     $   '     N   frac   Mean CASD   Mean CSSF ')
      DO 600 IBIN = 1, NBINS
         CALL DATGETI (STRM2(DB, BNAME(IBIN)), 'NHERE', NHERE, 1,
     $      NDUMMY)
         NREJECT = NREJECT - NHERE
         FRACTION = FLOAT(NHERE) / FLOAT(NTOT)
         IF (NHERE .NE. 0.0) THEN
            SCALE = 1./FLOAT(NHERE)
            CALL ARRSCALE (STRM3(DB, BNAME(IBIN), 'CAV/ASD'), SCALE, 0.,
     $         STRM3(DB, BNAME(IBIN), 'CAV/ASD'))
            CALL ARRSCALE (STRM3(DB, BNAME(IBIN), 'CSF/SSF'), SCALE, 0.,
     $         STRM3(DB, BNAME(IBIN), 'CSF/SSF'))
         ENDIF            
         BADD = DATADD (STRM3(DB, BNAME(IBIN), 'CAV/ASD'))
         CASD = MEMR (BADD + IT)
         BADD = DATADD (STRM3(DB, BNAME(IBIN), 'CSF/SSF'))
         CSSF = MEMR (BADD + IT)
C
         ANAME = BNAME(IBIN)
         WRITE (MESSAGE, 1993) ANAME(1:8), NHERE, FRACTION, CASD, CSSF
 1993    FORMAT (A8, I5, F6.2, 2F12.4)
         CALL MSGPUT (MESSAGE, 'B')
         WRITE (MESSAGE, 1994) NHERE, FRACTION, CASD, CSSF
 1994    FORMAT (I5, F6.2, 2F12.4)
         CALL TIPCOMME (STRM2(DB, BNAME(IBIN)), MESSAGE)
C
 600  CONTINUE
      FRACTION = FLOAT(NREJECT) / FLOAT(NTOT)
      WRITE (MESSAGE, 1995) NREJECT, NTOT, FRACTION
 1995 FORMAT ('Rejected: ',I5, ' out of ', I5, ' fraction of ',F5.4)
      CALL MSGPUT (MESSAGE, 'B')
      CALL TIPCOMME(STRM2(DB, BNAME(NBINS)), MESSAGE)
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
