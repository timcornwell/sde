C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arruscpy.f	1.1    6/7/93
C
      SUBROUTINE ARRUSCPY (INPUT, WT, OUTPUT)
C
CD Array UnSelect-Copy (Input to output, based on a weight)
C
C	The is the inverse operation to select-copy.  The output is
C	the same size as the weight array, while there should be at
C	least as many input items as there are positive weights.
C	If the weight is positive, the next input item is added into
C	the position of the weight in the output array.  Note that
C	the input is added to the output.  If you want a straight
C	copy on an existing output, zero it first.
C
C	INPUT	CH*(*)	input	Name of input array
C	WT	CH*(*)	input	Name of REAL weight array
C	OUTPUT	CH*(*)	input	Name of output array
C	WT/DELTA	INT(2)	input	Offset between OUTPUT & WT
C
C	On axis I, WT(K) is the same pixel as OUTPUT(K+DELTA(I))
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				D.S. Briggs	Nov 13 1992
C-----------------------------------------------------------------------
#include		"stdinc.h"
C
      CHARACTER*(*)	INPUT, WT, OUTPUT
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'ARRUSCPY')
C
      CHARACTER*1	TI, TW, TO
      INTEGER		NAXI, NAXW, NAXO, RNAXI, RNAXW, RNAXO,
     $   		NAXISI(SYSMXDIM), NAXISW(SYSMXDIM),
     $   		NAXISO(SYSMXDIM), DELTA(SYSMXDIM),
     $   		ADDIN, ADDWT, ADDOUT, I, NWT, NIN, NCOP, NDUMMY
      LOGICAL		DODELTA
C
      CHARACTER*(SYSMXNAM)	STRINT, STRM2
      INTEGER		CRDRNAX
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
      IF (RNAXI.GT.1) THEN
         CALL ERRREPOR(ERRBDARG, ROUTINE,
     $      'Input array is not 1D')
         GO TO 999
      END IF
      IF (TI.NE.'R') THEN
         CALL ERRREPOR(ERRBDARG, ROUTINE,
     $      'Weight array is not type REAL')
         GO TO 999
      END IF
      NIN = NAXISI(1)
C
C Make output array if it does not exist
C
      IF (.NOT.DATEXIST(OUTPUT)) THEN
         TO = TI
         NAXO = NAXW
         DO 25 I = 1, NAXW
            NAXISO(I) = NAXISW(I)
 25      CONTINUE
         CALL DATMAKAR (OUTPUT, NAXO, NAXISO, TI, ADDOUT)
         RNAXO = RNAXW
         CALL ARRSETCO (OUTPUT, 0.0, 0.0)
      ELSE
         CALL DATGETAR (OUTPUT, NAXO, NAXISO, TO, ADDOUT)
         RNAXO = CRDRNAX(NAXO, NAXISO)
         IF (TO.NE.TI) THEN
            WRITE (MESSAGE, 1050) TI, TO
 1050       FORMAT (
     1         'Array types for input and output image disagree : ',
     2         A1,1X,A1)
            CALL ERRREPOR (ERRBDARG, ROUTINE, MESSAGE)
         END IF
         IF (RNAXO.NE.RNAXW) THEN
            CALL ERRREPOR(ERRBDARG, ROUTINE,
     $         'Output & weight arrays have different dimensions')
            GO TO 999
         END IF
      END IF
C
C Last little bits of bookkeeping
C
      NWT = 1
      DO 30 I = 1, RNAXO
         IF (DODELTA) THEN
            IF (DELTA(I).LT.0) THEN
               CALL ERRREPOR(ERRBDARG, ROUTINE,
     $            'DELTAs can not be negative')
               GO TO 999
            END IF
            IF (NAXISW(I)+DELTA(I).GT.NAXISO(I)) THEN
               CALL ERRREPOR(ERRBDARG, ROUTINE,
     $            'WT array exceeds bounds of OUTPUT')
               GO TO 999
            END IF
         ELSE
            IF (NAXISO(I).NE.NAXISW(I)) THEN
               CALL ERRREPOR(ERRBDARG, ROUTINE,
     $            'Different sizes on axis ' // STRINT(I))
               GO TO 999
            END IF
         END IF
         NWT = NWT * NAXISW(I)
 30   CONTINUE
      DO 40 I = RNAXO+1, SYSMXDIM
         NAXISW(I) = 1
         NAXISO(I) = 1
 40   CONTINUE
C
C Call appropriate routine
C
      IF (TI.EQ.'R') THEN
         CALL PIXRUSCP (MEMR(ADDIN), MEMR(ADDWT), MEMR(ADDOUT),
     $      NAXISO(1),NAXISO(2),NAXISO(3),NAXISO(4),NAXISO(5),
     $      NAXISO(6),NAXISO(7),NAXISW(1),NAXISW(2),NAXISW(3),
     $	    NAXISW(4),NAXISW(5),NAXISW(6),NAXISW(7),
     $      NWT, DELTA, NIN, NCOP)
      ELSE IF (TI.EQ.'X') THEN
         CALL PIXXSCPY (MEMX(ADDIN), MEMR(ADDWT), MEMX(ADDOUT),
     $      NWT, NIN)
      ELSE IF (TI.EQ.'I') THEN
         CALL PIXISCPY (MEMI(ADDIN), MEMR(ADDWT), MEMI(ADDOUT),
     $      NWT, NIN)
      ELSE IF (TI.EQ.'D') THEN
         CALL PIXDSCPY (MEMD(ADDIN), MEMR(ADDWT), MEMD(ADDOUT),
     $      NWT, NIN)
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Array type '//TI//
     1      'Not supported')
      END IF
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
