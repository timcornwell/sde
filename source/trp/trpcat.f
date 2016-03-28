C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)trpcat.f	1.1    12/19/90
C
       SUBROUTINE TRPCAT (NAME1, NAME2, NAME3, SUB, STOKES)
C
CD Concatenate triple product data
C
C
C	NAMEn	CH*(*)	input	Names of directory entry
C	SUB	CH*(*)	input	Name of sub-class e.g. OBS
C	STOKES	CH*(*)	input	Names of Stokes parameters to combine
C				e.g. IQUV
C Audit trail:
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	NAME1, NAME2, NAME3, SUB, STOKES
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'TRPCAT')
C
      INTEGER		NAX, NAXIS(SYSMXDIM)
      REAL		RPIX (SYSMXDIM), DELT(SYSMXDIM),
     1			ROTA (SYSMXDIM)
      DOUBLE PRECISION	RVAL(SYSMXDIM)
      CHARACTER*8	TYPE(SYSMXDIM)
      CHARACTER*(SYSMXNAM)	STRM2, STRM3, NSUBS1, NSUBS2, NSUBS3
      INTEGER		NNAMES, INAMES, IS, STRLEN
C
      PARAMETER		(NNAMES=8)
      CHARACTER*8	NAMES(NNAMES)
      DATA		NAMES /'UU1', 'VV1', 'WW1', 'TIME', 'TRIPLE',
     $   'UU2', 'VV2', 'WW2'/
C=======================================================================
C
      IF (ERROR) GO TO 999
C
C Make output visibility
C
      CALL DATCREAT (NAME3)
      CALL DATCREAT (STRM2(NAME3, SUB))
C
C Concatenate u,v,w,time,baseline, etc
C
      DO 10 INAMES = 1, NNAMES
         CALL ARRCAT (STRM2(NAME1, NAMES(INAMES)),
     $                STRM2(NAME2, NAMES(INAMES)),
     $                STRM2(NAME3, NAMES(INAMES)))
 10   CONTINUE
C
C Copy header items
C
      CALL HEDCOPY (NAME1, NAME3)
      CALL HEDCOPY (NAME2, NAME3)
C
C Loop over all stokes parameters
C
      DO 20 IS = 1, STRLEN(STOKES)
C
C Make useful strings
C
         NSUBS1 = STRM3 (NAME1, SUB, STOKES(IS:IS))
         NSUBS2 = STRM3 (NAME2, SUB, STOKES(IS:IS))
         NSUBS3 = STRM3 (NAME3, SUB, STOKES(IS:IS))
         CALL DATCREAT (NSUBS3)
C
C Concatenate visibility parts
C
         CALL ARRCAT (STRM2(NSUBS1, 'TRP'), STRM2(NSUBS2, 'TRP'),
     $      STRM2(NSUBS3, 'TRP'))
         CALL ARRCAT (STRM2(NSUBS1, 'WT'), STRM2(NSUBS2, 'WT'),
     $      STRM2(NSUBS3, 'WT'))
C
C Should check here for identity
C
         CALL CRDGET (NSUBS1, NAX, TYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
         CALL CRDPUT (NSUBS3, NAX, TYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
C
 20   CONTINUE
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
