C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrscopy.f	1.3    6/7/93
C
      SUBROUTINE ARRSCOPY (INPUT, WT, OUTPUT)
C
CD Array Select-Copy (Input to output, based on a weight)
C
C	Adds the input array to the output, copying only the
C	entries with a weight greater than zero.  Output is a one
C	dimensional array.  If you want a copy, with an array
C	that pre-exists, zero it beforehand.
C
C	INPUT	CH*(*)	input	Name of input array
C	WT	CH*(*)	input	Name of REAL weight array
C	OUTPUT	CH*(*)	input	Name of output array
C	WT/DELTA	INT(2)	input	Offset between IN & WT
C
C	On axis I, WT(K) is the same pixel as IN(K+DELTA(I))
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				D.S. Briggs	August 30 1991
C	Check real number of axes, instead of that reported by DATGETAR
C				D.S. Briggs	August 15 1992
C	Allow for multi-dimension -> one.  Added DELTA option.  Actually
C	count positive pixels if needed for output creation.
C				D.S. Briggs	Nov 25 1992
C-----------------------------------------------------------------------
#include		"stdinc.h"
C
      CHARACTER*(*)	INPUT, WT, OUTPUT
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'ARRSCOPY')
C
      CHARACTER*(*)	TEMPNAME
      PARAMETER		(TEMPNAME=ROUTINE//'-TEMP')
C
      CHARACTER*1	TI, TW, TO
      INTEGER		NAXI, NAXW, NAXO, RNAXI, RNAXW, RNAXO,
     $			NAXISI(SYSMXDIM), NAXISW(SYSMXDIM),
     $   		NAXISO(SYSMXDIM), DELTA(SYSMXDIM),
     $   		I, ADDIN, ADDWT, ADDOUT, NWT, NOUT, NCOP, NDUMMY
      LOGICAL		DODELTA
C
      CHARACTER*(SYSMXNAM)	STRINT, STRM2
      INTEGER		CRDRNAX, DATFGETI
      LOGICAL		DATEXIST
C=====================================================================
      IF (ERROR) GO TO 999
C
C Get the input arrays
C
      CALL DATGETAR (INPUT, NAXI, NAXISI, TI, ADDIN)
      CALL DATGETAR (WT, NAXW, NAXISW, TW, ADDWT)
      RNAXI = CRDRNAX(NAXI,NAXISI)
      RNAXW = CRDRNAX(NAXW,NAXISW)
      IF (ERROR) GO TO 990
      IF (DATEXIST(STRM2(WT,'DELTA'))) THEN
         CALL DATGETI (WT, 'DELTA', DELTA, SYSMXDIM, NDUMMY)
         DO 10 I = RNAXW+1, SYSMXDIM
            DELTA(I) = 0
 10      CONTINUE
         DODELTA = .TRUE.
      ELSE
         DO 20 I = 1, SYSMXDIM
            DELTA(I) = 0
 20      CONTINUE
         DODELTA = .FALSE.
      END IF
      IF (RNAXI.NE.RNAXW) THEN
         CALL ERRREPOR(ERRBDARG, ROUTINE,
     $      'Input arrays have different number of dimensions')
         GO TO 999
      END IF
      IF (TW.NE.'R') THEN
         CALL ERRREPOR(ERRBDARG, ROUTINE,
     $      'Weight array is not type REAL')
         GO TO 999
      END IF
      NWT = 1
      DO 30 I = 1, RNAXI
         IF (DODELTA) THEN
            IF (DELTA(I).LT.0) THEN
               CALL ERRREPOR(ERRBDARG, ROUTINE,
     $            'DELTAs can not be negative')
               GO TO 999
            END IF
            IF (NAXISW(I)+DELTA(I).GT.NAXISI(I)) THEN
               CALL ERRREPOR(ERRBDARG, ROUTINE,
     $            'WT array exceeds bounds of INPUT')
               GO TO 999
            END IF
         ELSE
            IF (NAXISI(I).NE.NAXISW(I)) THEN
               CALL ERRREPOR(ERRBDARG, ROUTINE,
     $            'Different sizes on axis ' // STRINT(I))
               GO TO 999
            END IF
         END IF
         NWT = NWT * NAXISW(I)
 30   CONTINUE
      DO 40 I = RNAXI+1, SYSMXDIM
         NAXISI(I) = 1
         NAXISW(I) = 1
 40   CONTINUE
C
C Make output array if it does not exist
C
      IF (.NOT.DATEXIST(OUTPUT)) THEN
         CALL ARRCOPY (WT, TEMPNAME)
         CALL ARRCLIP(TEMPNAME, 0.0, 1.E10, TEMPNAME)
         CALL ARRSTAT (TEMPNAME, ' ')
         NOUT = DATFGETI(TEMPNAME, 'ARRNLOC')
         CALL DATDELET (TEMPNAME)
         TO = TI
         CALL DATMAKAR (OUTPUT, 1, NOUT, TO, ADDOUT)
         CALL ARRSETCO (OUTPUT, 0.0, 0.0)
      ELSE
         CALL DATGETAR (OUTPUT, NAXO, NAXISO, TO, ADDOUT)
         RNAXO = CRDRNAX(NAXO,NAXISO)
         NOUT = NAXISO(1)
         IF (RNAXO.GT.1) THEN
            CALL ERRREPOR(ERRBDARG, ROUTINE,
     $         'Output array is not 1D')
            GO TO 999
         END IF
         IF (TI.NE.TO) THEN
            WRITE (MESSAGE, 1100) TI, TO
 1100       FORMAT (
     1         'Array types for input and output image disagree : ',
     2         A1,1X,A1)
            CALL ERRREPOR (ERRBDARG, ROUTINE, MESSAGE)
         END IF
      END IF
C
C Call appropriate routine
C
      IF (TI.EQ.'R') THEN
         CALL PIXRSCPY (MEMR(ADDIN), MEMR(ADDWT), MEMR(ADDOUT),
     $      NAXISI(1),NAXISI(2),NAXISI(3),NAXISI(4),NAXISI(5),
     $      NAXISI(6),NAXISI(7),NAXISW(1),NAXISW(2),NAXISW(3),
     $	    NAXISW(4),NAXISW(5),NAXISW(6),NAXISW(7),
     $      NWT, DELTA, NOUT, NCOP)
      ELSE IF (TI.EQ.'X') THEN
         CALL PIXXSCPY (MEMX(ADDIN), MEMR(ADDWT), MEMX(ADDOUT),
     $      NWT, NOUT)
      ELSE IF (TI.EQ.'I') THEN
         CALL PIXISCPY (MEMI(ADDIN), MEMR(ADDWT), MEMI(ADDOUT),
     $      NWT, NOUT)
      ELSE IF (TI.EQ.'D') THEN
         CALL PIXDSCPY (MEMD(ADDIN), MEMR(ADDWT), MEMD(ADDOUT),
     $      NWT, NOUT)
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Array type '//TI//
     1      'Not supported')
      END IF
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END

