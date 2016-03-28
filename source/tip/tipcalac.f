C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)tipcalac.f	1.1    3/26/93
C
      SUBROUTINE TIPCALAC (DB, BINSUB, STIME, BINS, NB, WHAT, T)
C
CD Bin the Calibrator data.
C  Unlike TIPBIN, here we bin from 0 to BINS(1), from 0 to BINS(2)...
C  So a very good scan will be in all of the bins
C
C	DB	CH*(*)	inp	Tipper DataBase
C	BINSUB	INT	inp	Name prefix of tipper subdirectory
C	STIME	INT(2)	inp	Time to select on
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
      INTEGER		STIME(2), NB, BINSUB
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'TIPCALAC')
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
         CALL DATCREAT (STRM3(DB, BNAME(IBIN), 'AV'))
         CALL DATMAKAR (STRM3(DB, BNAME(IBIN), 'AV/AV'), 1, 9, 
     $      'R', BADD)
         CALL ARRSETCO (STRM3(DB, BNAME(IBIN), 'AV/AV'), 0.0, 0.0)
         CALL DATMAKAR (STRM3(DB, BNAME(IBIN), 'AV/TIME'), 1, 9, 'R',
     $      T2ADD)
C
         CALL DATCREAT (STRM3(DB, BNAME(IBIN), 'SF'))
         CALL DATMAKAR (STRM3(DB, BNAME(IBIN), 'SF/SF'), 1, 9, 
     $      'R', BADD)
         CALL ARRSETCO (STRM3(DB, BNAME(IBIN), 'SF/SF'), 0.0, 0.0)
         CALL DATMAKAR (STRM3(DB, BNAME(IBIN), 'SF/TIME'), 1, 9, 'R',
     $      T1ADD)
C
         CALL DATPUTI  (STRM2(DB, BNAME(IBIN)), 'NHERE', 0, 1)
         CALL DATPUTL  (STRM2(DB, BNAME(IBIN)), 'Calibrator',
     $      .TRUE.,1)
         CALL DATPUTC  (STRM2(DB, BNAME(IBIN)), 'OriginalFileName',
     $      'ACC CAL DATA', 1)
         CALL DATPUTC  (STRM2(DB, BNAME(IBIN)), 'NewFileName',
     $      'ACC CAL DATA!', 1)
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
      CALL MSGPUT ('Each bin contains ALL cal runs with PLOTWHAT', 'B')
      CALL MSGPUT ('at time T better than BINLIST(I)', 'B')
C
C What do we bin by?
C
      IF (WHAT .EQ. 'ASD') THEN
         WHAT2 = 'AV/ASD'
      ELSE IF (WHAT .EQ. 'SSF') THEN
         WHAT2 = 'SF/SSF'
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
         IF ( ISACALIB )  THEN
            CALL DATGETAR ( STRM2(ANAME, WHAT2), NAX, NAXIS, TYPE, 
     $         DADD)
            NTOT = NTOT + 1
            TEST = MEMR(DADD + IT)
            DO 310 IBIN = 1, NBINS
               IF (TEST .LE. BINS(IBIN)) THEN
                  CALL DATGETI (STRM2(DB, BNAME(IBIN)), 'NHERE', NHERE,
     $               1, NDUMMY)
                  NHERE = NHERE + 1
                  CALL DATPUTI (STRM2(DB, BNAME(IBIN)), 'NHERE',NHERE,1)
                  CALL ARRADD (STRM3(DB, BNAME(IBIN), 'AV/AV'), 
     $               STRM2( ANAME, 'AV/AV'),
     $               STRM3( DB, BNAME(IBIN), 'AV/AV') )
                  CALL ARRADD (STRM3(DB, BNAME(IBIN), 'SF/SF'), 
     $               STRM2( ANAME, 'SF/SF'),
     $               STRM3(DB, BNAME(IBIN), 'SF/SF') )
               ENDIF
 310        CONTINUE
         ENDIF
 500  CONTINUE
C
      IF (NTOT .EQ. 0) THEN
         CALL ERRREPOR (ERRLOGIC, ROUTINE,
     $      'Apparently no valid data')
         GOTO 999
      ENDIF
      NREJECT = NTOT
      CALL MSGPUT ('Binning Results:', 'B')
      CALL MSGPUT ('Bin      N   frac   Mean ASD   Mean SSF ', 'B')
      WRITE (MESSAGE, 1945) WHAT(1:6), T
 1945 FORMAT ('Cal Collection Selected on ', A6, ' at ',F4.0, ' s')
      CALL TIPCOMME (STRM2(DB, BNAME(1)), MESSAGE)
      WRITE (MESSAGE, 1946) STIME(1), STIME(2)
 1946 FORMAT ('Binned Data from ',I8,' to ', I8)
      CALL TIPCOMME (STRM2(DB, BNAME(1)), MESSAGE)
      CALL TIPCOMME (STRM2(DB, BNAME(1)),
     $   '    N   frac   Mean ASD   Mean SSF ')
      DO 600 IBIN = 1, NBINS
         CALL DATGETI (STRM2(DB, BNAME(IBIN)), 'NHERE', NHERE, 1,
     $      NDUMMY)
         NREJECT = MIN (NREJECT, (NTOT - NHERE))
         FRACTION = FLOAT(NHERE) / FLOAT(NTOT)
         IF (NHERE .NE. 0.0) THEN
            SCALE = 1./FLOAT(NHERE)
            CALL ARRSCALE (STRM3(DB, BNAME(IBIN), 'AV/AV'), SCALE, 0.,
     $         STRM3(DB, BNAME(IBIN), 'AV/AV'))
            CALL ARRSCALE (STRM3(DB, BNAME(IBIN), 'SF/SF'), SCALE, 0.,
     $         STRM3(DB, BNAME(IBIN), 'SF/SF'))
            CALL ARRPOWER (STRM3(DB, BNAME(IBIN), 'AV/AV'), 0.5, -99.0,
     $         STRM3(DB, BNAME(IBIN), 'AV/ASD'))
            CALL ARRPOWER (STRM3(DB, BNAME(IBIN), 'SF/SF'), 0.5, -99.0,
     $         STRM3(DB, BNAME(IBIN), 'SF/SSF'))
            BADD = DATADD (STRM3(DB, BNAME(IBIN), 'AV/ASD'))
            CASD = MEMR (BADD + IT)
            BADD = DATADD (STRM3(DB, BNAME(IBIN), 'SF/SSF'))
            CSSF = MEMR (BADD + IT)
         ELSE
            CASD = 0.0
            CSSF = 0.0
         ENDIF
C
         ANAME = BNAME(IBIN)
         WRITE (MESSAGE, 1993) ANAME(1:8), NHERE, FRACTION, CASD, CSSF
 1993    FORMAT (A8, I5, F6.2, 2F12.4)
         CALL MSGPUT (MESSAGE, 'B')
         WRITE (MESSAGE, 1994) NHERE, FRACTION, CASD, CSSF
 1994    FORMAT (I5, F6.2, 2F12.4)
         CALL TIPCOMME (STRM2(DB, BNAME(IBIN)), MESSAGE)
 600  CONTINUE
      FRACTION = FLOAT(NREJECT) / FLOAT(NTOT)
      WRITE (MESSAGE, 1995) NREJECT, NTOT, FRACTION
 1995 FORMAT ('Rejected: ',I5, ' out of ', I5, ' fraction of ',F5.4)
      CALL MSGPUT (MESSAGE, 'B')
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
