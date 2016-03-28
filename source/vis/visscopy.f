C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)visscopy.f	1.1    6/7/93
C
       SUBROUTINE VISSCOPY (INNAME, OUTNAME, SUB, STOKES)
C
CD Copy visibilities subject to positive weights
C
C	INNAME	CH*(*)	input	Input directory name
C	OUTNAME	CH*(*)	input	Output directory name
C	SUB	CH*(*)	input	Name of sub-class e.g. OBS
C	STOKES	CH*(*)	input	Names of Stokes parameters to combine
C				e.g. IQUV
C
C Since we must maintain the correspondence between the header arrays
C and the visibilities, all selection is done on the basis of the
C weights in the first stokes parameter.  The copied visibilities all
C carry the proper weights, of course.
C
C Audit trail:
C	Original version
C				D.S.Briggs	May 15 1993
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	INNAME, OUTNAME, SUB, STOKES
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISSCOPY')
C
      INTEGER		NAX, NAXIS(SYSMXDIM)
      REAL		RPIX (SYSMXDIM), DELT(SYSMXDIM),
     1			ROTA (SYSMXDIM)
      DOUBLE PRECISION	RVAL(SYSMXDIM)
      CHARACTER*8	TYPE(SYSMXDIM)
      CHARACTER*(SYSMXNAM)	NSUBSIN, NSUBSOUT, WTNAM
      INTEGER		NNAMES, INAMES, IS, STRLEN
C
      CHARACTER*(SYSMXNAM)	STRM2, STRM3, STRM4
C
      PARAMETER		(NNAMES=5)
      CHARACTER*8	NAMES(NNAMES)
      DATA		NAMES /'UU', 'VV', 'WW', 'TIME', 'BASELINE'/
C=======================================================================
      IF (ERROR) GO TO 999
C
C Make output visibility
C
      CALL DATCREAT (OUTNAME)
      CALL DATCREAT (STRM2(OUTNAME, SUB))
C
C Concatenate u,v,w,time,baseline, etc
C
      WTNAM = STRM4(INNAME, SUB, STOKES(1:1), 'WT')
      DO 10 INAMES = 1, NNAMES
         CALL ARRSCOPY (STRM2(INNAME, NAMES(INAMES)),
     $      		WTNAM, STRM2(OUTNAME, NAMES(INAMES)))
 10   CONTINUE
C
C Copy header items
C
      CALL HEDCOPY (INNAME, OUTNAME)
C
C Loop over all stokes parameters
C
      DO 20 IS = 1, STRLEN(STOKES)
C
C Make useful strings
C
         NSUBSIN = STRM3 (INNAME, SUB, STOKES(IS:IS))
         NSUBSOUT = STRM3 (OUTNAME, SUB, STOKES(IS:IS))
         CALL DATCREAT (NSUBSOUT)
C
C Copy the visibility records, using weights from first stokes
C
         CALL ARRSCOPY (STRM2(NSUBSIN, 'VIS'), WTNAM,
     $      STRM2(NSUBSOUT, 'VIS'))
         CALL ARRSCOPY (STRM2(NSUBSIN, 'WT'), WTNAM,
     $      STRM2(NSUBSOUT, 'WT'))
C
C Should check here for identity
C
         CALL CRDGET (NSUBSIN, NAX, TYPE, NAXIS, RVAL, RPIX, DELT,
     $      ROTA)
         CALL CRDPUT (NSUBSOUT, NAX, TYPE, NAXIS, RVAL, RPIX, DELT,
     $      ROTA)
C
 20   CONTINUE
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
