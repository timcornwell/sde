C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgfitto.f	1.4    10/22/93
C
      SUBROUTINE IMGFITTO (IN, EXAMPLE, OUT)
C
CD Expands IN to fit EXAMPLE; must have same DELT; OUT cannot exist previously
C
C	IN	CH*(*)	input	Input IMAGE name
C	EXAMPLE	CH*(*)	input	example IMAGE name
C	OUT	CH*(*)	output	output IMAGE name
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	Dec 18 1990
C	The headers are dealt with more systematically.  Previously,
C	the header of EXAMPLE was simply copied to OUT.  Now, we
C	create OUT with the coordinates and dimension of EXAMPLE,
C	copy any additional header items over from IN, and then
C	copy any further additional items over from EXAMPLE.
C				D.S.Briggs	Apr 30 1993
C	Fixed calculation of image center for ODD images
C				D.S.Briggs	June 9 1993
C	Convert IN to type of EXAMPLE, if necessary
C				D.S.Briggs	Oct 20 1993
C-----------------------------------------------------------------------
#include		"stdinc.h"
C
      CHARACTER*(*)	IN, OUT, EXAMPLE
C
      CHARACTER*(*)	ROUTINE

      PARAMETER		(ROUTINE = 'IMGFITTO')
C
      CHARACTER*(*)     TEMPNAME
      PARAMETER         (TEMPNAME = ROUTINE//'-TMP')
C
      INTEGER		IADD, EADD
      CHARACTER*1	ITYPE, ETYPE
      INTEGER           INAX, INAXIS(SYSMXDIM)
      DOUBLE PRECISION  IRVAL(SYSMXDIM)
      REAL              IRPIX(SYSMXDIM), IDELT(SYSMXDIM), 
     $   		IROTA(SYSMXDIM)
      CHARACTER*8       TYPE(SYSMXDIM)
      INTEGER           ENAX, ENAXIS(SYSMXDIM)
      DOUBLE PRECISION  ERVAL(SYSMXDIM)
      REAL              ERPIX(SYSMXDIM), EDELT(SYSMXDIM), 
     $   		EROTA(SYSMXDIM)
      INTEGER           SNAX, SNAXIS(SYSMXDIM)
      DOUBLE PRECISION  SRVAL(SYSMXDIM)
      REAL              SRPIX(SYSMXDIM), SDELT(SYSMXDIM), 
     $   		SROTA(SYSMXDIM)
      INTEGER		ANAX, ANAXIS(SYSMXDIM)
      REAL		ARPIX(SYSMXDIM), ADELT(SYSMXDIM),
     $   		AROTA(SYSMXDIM)
      CHARACTER*8	ATYPE(SYSMXDIM)
      DOUBLE PRECISION	ARVAL(SYSMXDIM)
      REAL		RPDELT(SYSMXDIM)
C
      DOUBLE PRECISION	TOL
      INTEGER		PNAXIS(SYSMXDIM), TRC(SYSMXDIM), BLC(SYSMXDIM)
      INTEGER           IAX, NAX, OUTSIDE(SYSMXDIM, 2)
      LOGICAL           DATEXIST, TOOBIG, TOOSMALL,
     $   		MIXED, NOOVERLA
C=======================================================================
      IF (ERROR) GO TO 999
C
C Are the images compatible?
C
      TOL = 0.1D0/3600.D0
      IF (DATEXIST(OUT)) THEN
         CALL ERRREPOR (ERRLOGIC, ROUTINE,
     $      'OUT cannot exist prior to running IMGFITTO')
         GOTO 990
      ENDIF
C
      CALL DATGETAR (IN, INAX, INAXIS, ITYPE, IADD)
      CALL DATGETAR (EXAMPLE, ENAX, ENAXIS, ETYPE, EADD)
      IF (ITYPE .NE. ETYPE) THEN
         CALL DATRENAM (IN, TEMPNAME)
         IF (ETYPE.EQ.'R') THEN
            CALL ARRCVTR (TEMPNAME, IN)
         ELSE IF (ETYPE.EQ.'D') THEN
            CALL ARRCVTD (TEMPNAME, IN)
         ELSE IF (ETYPE.EQ.'X') THEN
            CALL ARRCVTX (TEMPNAME, IN)
         ELSE
            CALL ERRREPOR (ERRBDARG, ROUTINE,
     $         'Arrays are of wrong type')
         END IF
         CALL CRDCOPY (TEMPNAME, IN)
         IF (ERROR) GO TO 990
      ENDIF
C
      CALL CRDGET (IN, INAX, TYPE, INAXIS, IRVAL, IRPIX, IDELT, IROTA)
      CALL CRDGET (EXAMPLE, ENAX, TYPE, ENAXIS, ERVAL, ERPIX, EDELT, 
     $   EROTA)
      CALL CRDGET (EXAMPLE, ANAX, ATYPE, ANAXIS, ARVAL, ARPIX, ADELT, 
     $   AROTA)
      DO 10 IAX = 1, ENAX
         IF (ENAXIS(IAX) .GT. 1) NAX = IAX
 10   CONTINUE
      DO 15 IAX = 1, NAX
         IF (ABS (IDELT(IAX) - EDELT(IAX)) .GT. TOL) THEN
            CALL ERRREPOR (ERRBDARG, ROUTINE,
     $         'Images have different cell sizes')
            GOTO 990
         ENDIF
         IF (ABS(IRVAL(IAX) - ERVAL(IAX)) .GT. TOL) THEN
            CALL ERRREPOR (ERRBDARG, ROUTINE,
     $         'Images have different reference values')
            GOTO 990
         ENDIF
 15   CONTINUE
C
C Get reference pixels of IN and EXAMPLE, shift to center of EXAMPLE
C
      DO 20 IAX = 1, NAX
         RPDELT(IAX) = (ENAXIS(IAX)+1)/2 - ERPIX(IAX)
         ERPIX(IAX) = ERPIX(IAX) + RPDELT(IAX)
         IRPIX(IAX) = IRPIX(IAX) + RPDELT(IAX)
 20   CONTINUE
      CALL CRDPUT (IN, INAX, TYPE, INAXIS, IRVAL, IRPIX, IDELT, IROTA)
      CALL CRDPUT (EXAMPLE, ENAX, TYPE, ENAXIS, ERVAL, ERPIX, EDELT, 
     $   EROTA)
C
C Determine situation
C 
      NOOVERLA = .FALSE.
C
C OUTSIDE:  1 = I outside E, -1 = I inside E, 0 = I on side of E
C second subscript: 1 = lower edge, 2 = upper edge
C
      DO 30 IAX = 1, NAX
         IF (IRPIX(IAX) .GT. ERPIX(IAX))  THEN
            OUTSIDE(IAX, 1) = 1
         ELSE IF (IRPIX(IAX) .LT. ERPIX(IAX)) THEN
            OUTSIDE(IAX, 1) = -1
         ELSE
            OUTSIDE(IAX, 1) = 0
         ENDIF
         IF ((INAXIS(IAX) - IRPIX(IAX)) .GT. 
     $      (ENAXIS(IAX) - ERPIX(IAX)) )   THEN
            OUTSIDE(IAX, 2) = 1
         ELSE IF ((INAXIS(IAX) - IRPIX(IAX)) .LT. 
     $      (ENAXIS(IAX) - ERPIX(IAX)) ) THEN
            OUTSIDE(IAX, 2) = -1
         ELSE
            OUTSIDE(IAX, 2) = 0
         ENDIF
         IF ((IRPIX(IAX) - INAXIS(IAX) .GT. ERPIX(IAX) ) .OR.
     $       (-IRPIX(IAX) .GT. ENAXIS(IAX) - ERPIX(IAX) ) ) THEN
            NOOVERLA = .TRUE.
         ENDIF
 30   CONTINUE
C
C Take appropriate action
C
      TOOBIG = .TRUE.
      TOOSMALL = .TRUE.
      MIXED = .FALSE.
      DO 40 IAX = 1, NAX
         IF (OUTSIDE(IAX, 1) .GT. 0) TOOSMALL = .FALSE.
         IF (OUTSIDE(IAX, 2) .GT. 0) TOOSMALL = .FALSE.
         IF (OUTSIDE(IAX, 1) .LT. 0) TOOBIG = .FALSE.
         IF (OUTSIDE(IAX, 2) .LT. 0) TOOBIG = .FALSE.
 40   CONTINUE
      IF (.NOT. TOOSMALL .AND. .NOT. TOOBIG) MIXED = .TRUE.
C
      IF (NOOVERLA) THEN
         CALL MSGPUT ('No Overlap in Images', 'W')
         CALL IMGCLONE (EXAMPLE, OUT)
         CALL ARRSETCO (OUT, 0.0, 0.0)
      ELSE IF (TOOSMALL) THEN
         DO 50 IAX = 1, NAX
            PNAXIS(IAX) = ENAXIS(IAX)
 50      CONTINUE
         CALL IMGPAD (IN, OUT, PNAXIS, 0.0)
      ELSE IF (TOOBIG) THEN
         IF (.NOT.DATEXIST('FitWindow')) CALL DATCREAT ('FitWindow')
         DO 60 IAX = 1, NAX
            BLC(IAX) = IRPIX(IAX) - ERPIX(IAX) + 1
            TRC(IAX) = IRPIX(IAX) - ERPIX(IAX) + ENAXIS(IAX)
 60      CONTINUE
         CALL DATPUTI ('FitWindow', 'BLC', BLC, SYSMXDIM)
         CALL DATPUTI ('FitWindow', 'TRC', TRC, SYSMXDIM)
         CALL ARRSUBSE (IN, OUT, 'FitWindow')
      ELSE IF (MIXED) THEN
         DO 70 IAX = 1, NAX
            PNAXIS(IAX) = MAX(2*ENAXIS(IAX), 2*INAXIS(IAX))
 70      CONTINUE
         CALL IMGPAD (IN, 'Scratch', PNAXIS, 0.0)
         CALL CRDGET ('Scratch', SNAX, TYPE, SNAXIS, SRVAL, SRPIX, 
     $      SDELT, SROTA)
         IF (.NOT.DATEXIST('FitWindow')) CALL DATCREAT ('FitWindow')
         DO 80 IAX = 1, NAX
            BLC(IAX) = SRPIX(IAX) - ERPIX(IAX) + 1
            TRC(IAX) = SRPIX(IAX) - ERPIX(IAX) + ENAXIS(IAX)
 80      CONTINUE
         CALL DATPUTI ('FitWindow', 'BLC', BLC, SYSMXDIM)
         CALL DATPUTI ('FitWindow', 'TRC', TRC, SYSMXDIM)
         CALL ARRSUBSE ('Scratch', OUT, 'FitWindow')
         CALL DATDELET ('Scratch')
      ENDIF
      IF (DATEXIST('FitWindow')) CALL DATDELET ('FitWindow')
C
C Shift reference pixels back
C
      DO 120 IAX = 1, NAX
         ERPIX(IAX) = ERPIX(IAX) - RPDELT(IAX)
         IRPIX(IAX) = IRPIX(IAX) - RPDELT(IAX)
 120   CONTINUE
      CALL CRDPUT (IN, INAX, TYPE, INAXIS, IRVAL, IRPIX, IDELT, IROTA)
      CALL CRDPUT (EXAMPLE, ANAX, ATYPE, ANAXIS, ARVAL, ARPIX, ADELT, 
     $   AROTA)
      CALL CRDPUT (OUT, ANAX, ATYPE, ANAXIS, ARVAL, ARPIX, ADELT, 
     $   AROTA)
C
      CALL HEDACOPY (IN, OUT)
      CALL HEDACOPY (EXAMPLE, OUT)
C
      IF (DATEXIST (TEMPNAME)) CALL DATDELET (TEMPNAME)
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
