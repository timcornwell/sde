C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrlist2.f	1.1    10/29/91
C
      SUBROUTINE ARRLIST2 (ARRAY, WHAT, DOSUB, IMININ, IMAXIN, DOSKIP,
     $   ISKIPIN, DORANGE, DOEXCLD, RMIN, RMAX)
C
CD List out arrays on MSGPUT  (Many argument version)
C
C	ARRAY	CH*(*)	input	Name of array
C	WHAT	CH*(*)	input	Identifying comment
C	DOSUB	LOG	input	IMIN, IMAX are ignored if false
C	IMIN	INT(*)	input	print subsection of array from
C	IMAX	INT(*)	input	 IMIN to IMAX on each axis
C	DOSKIP	LOG	input	ISKIP is ignored if false
C	ISKIP	INT(*)	input	skip this many between printouts
C	DORANGE	LOG	input	RMIN, RMAX are ignored if false
C	DOEXCLD LOG	input	reverse sense of RMIN & RMAX
C	RMIN	DBLE	input	only values >= RMIN
C	RMAX	DBLE	input	 and <= RMAX are printed
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				D.S.Briggs	17-Sept-1991
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	ARRAY, WHAT
      INTEGER		IMININ(SYSMXDIM), IMAXIN(SYSMXDIM),
     $   		ISKIPIN(SYSMXDIM)
      DOUBLE PRECISION	RMIN, RMAX
      LOGICAL		DOSKIP, DOSUB, DORANGE, DOEXCLD
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'ARRLIST2')
C
      INTEGER		IMIN(SYSMXDIM), IMAX(SYSMXDIM),
     $   		ISKIP(SYSMXDIM)
      INTEGER		NAX, NAXIS(SYSMXDIM), AADD, I
      CHARACTER*1	ATYPE
C=======================================================================
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (ARRAY, NAX, NAXIS, ATYPE, AADD)
      DO 100 I = 1, NAX
         IF (DOSKIP) THEN
            ISKIP(I) = ISKIPIN(I)
         ELSE
            ISKIP(I) = 1
         END IF
         IF (DOSUB) THEN
            IMIN(I) = MAX(1, MIN(IMININ(I),NAXIS(I)))
            IMAX(I) = MAX(1, MIN(IMAXIN(I),NAXIS(I)))
         ELSE
            IMIN(I) = 1
            IMAX(I) = NAXIS(I)
         END IF
 100  CONTINUE
C
      IF (ATYPE .EQ. 'I') THEN
         CALL PIXILIS2 (MEMI(AADD), NAX, NAXIS, WHAT, IMIN, IMAX,
     $      ISKIP, DORANGE, DOEXCLD, NINT(RMIN), NINT(RMAX))
      ELSE IF (ATYPE .EQ. 'R') THEN
         CALL PIXRLIS2 (MEMR(AADD), NAX, NAXIS, WHAT, IMIN, IMAX,
     $      ISKIP, DORANGE, DOEXCLD, REAL(RMIN), REAL(RMAX))
      ELSE IF (ATYPE .EQ. 'X') THEN
         CALL PIXXLIS2 (MEMX(AADD), NAX, NAXIS, WHAT, IMIN, IMAX,
     $      ISKIP, DORANGE, DOEXCLD, CMPLX(RMIN), CMPLX(RMAX))
      ELSE IF (ATYPE .EQ. 'D') THEN
         CALL PIXDLIS2 (MEMD(AADD), NAX, NAXIS, WHAT, IMIN, IMAX,
     $      ISKIP, DORANGE, DOEXCLD, RMIN, RMAX)
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $      'ARRLIST doesn''t deal with type '//ATYPE)
         GOTO 990
      ENDIF
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
