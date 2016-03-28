C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)visav.f	1.2    6/12/94
C
       SUBROUTINE VISAV (INNAME, OUTNAME, CLASS, STOKES, TINT,
     $   SIGMAUW, WTMODE, DOMEDIAN)
C
CD Copy visibilities subject to positive weights
C
C	INNAME	CH*(*)	input	Name of input directory entry
C	OUTNAME	CH*(*)	input	Name of output directory to be created
C	CLASS	CH*(*)	input	Class, eg 'OBS'
C	STOKES	CH*(*)	input	Stokes parameters to be considered
C	TINT	REAL	input	Integration time in minutes
C	SIGMAUW	REAL	input	Sigma for unit weight
C	WTMODE	CH*(*)	input	'RMS', 'WT', 'VB-RMS', 'VB-WT'
C	DOMEDIAN LOG	input	Use median averaging?
C
C Since we must maintain the correspondence between the header arrays
C and the visibilities, all selection is done on the basis of the
C weights in the first stokes parameter.  No flagged visibilities in the
C other stokes parameters will be used, but not all good data will
C necessarily be used.
C
C WTMODE controls what is done with the weights in the output database
C The input weights are used to compute the weighted average.  If the
C mode is 'RMS' or 'VB-RMS', the output weights are computed from the
C internal scatter of the points used in the average.  If the mode
C begins with 'VB', then they are munged with a constant factor to make
C them come out correctly on AIPS VBPLTs.
C
C Audit trail:
C	Original version
C				D.S.Briggs	May 20 1993
C	Added SIGMAUW estimation for 'RMS' mode.  Only in for the non-median
C	averaging, though.  I don't see that the other would be useful.
C				D.S.Briggs	May 30 1994
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	INNAME, OUTNAME, CLASS, STOKES, WTMODE
      REAL		TINT, SIGMAUW
      LOGICAL		DOMEDIAN
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISAV')
C
      INTEGER		NAX, NAXIS(SYSMXDIM)
      REAL		RPIX (SYSMXDIM), DELT(SYSMXDIM),
     1			ROTA (SYSMXDIM)
      DOUBLE PRECISION	RVAL(SYSMXDIM)
      CHARACTER*8	TYPE(SYSMXDIM)
C
      REAL		TINTD, SSIGMA, SSIGUW
      CHARACTER*(SYSMXNAM)	NSUBSIN, NSUBSOUT, WTNAM
      INTEGER		IS, NIN, NOUT, NANT,
     $   		UADDI, VADDI, WADDI, TADDI, BADDI,
     $   		UADDO, VADDO, WADDO, TADDO, BADDO,
     $   		VISADDI, VISADDO, WTADDI, WTADDS, WTADDO
C
      CHARACTER*(SYSMXNAM)	STRM2, STRM3, STRM4
      INTEGER		ARRNPIX, DATADD, STRLEN
C=======================================================================
      IF (ERROR) GO TO 999
C
C Make output visibility
C
      CALL DATCREAT (OUTNAME)
      CALL DATCREAT (STRM2(OUTNAME, CLASS))
C
      UADDI = DATADD(STRM2(INNAME,'UU'))
      VADDI = DATADD(STRM2(INNAME,'VV'))
      WADDI = DATADD(STRM2(INNAME,'WW'))
      TADDI = DATADD(STRM2(INNAME,'TIME'))
      BADDI = DATADD(STRM2(INNAME,'BASELINE'))
      WTNAM = STRM4(INNAME, CLASS, STOKES(1:1), 'WT')
      WTADDS = DATADD(WTNAM)
      NIN = ARRNPIX(STRM2(INNAME,'TIME'))
      TINTD = TINT/86400.0
C
C Create header arrays for output
C
      CALL VISAVP0 (MEMR(TADDI), MEMR(BADDI), MEMR(WTADDS),
     $   TINTD, NIN, NOUT, NANT)
C
      CALL DATMAKAR (STRM2(OUTNAME, 'UU'), 1, NOUT, 'R', UADDO)
      CALL DATMAKAR (STRM2(OUTNAME, 'VV'), 1, NOUT, 'R', VADDO)
      CALL DATMAKAR (STRM2(OUTNAME, 'WW'), 1, NOUT, 'R', WADDO)
      CALL DATMAKAR (STRM2(OUTNAME, 'TIME'), 1, NOUT, 'R', TADDO)
      CALL DATMAKAR (STRM2(OUTNAME, 'BASELINE'), 1, NOUT, 'R', BADDO)
C
C Average up the header stuff
C
      CALL VISAVP1 (MEMR(UADDI), MEMR(UADDO), MEMR(VADDI), MEMR(VADDO),
     $   	    MEMR(WADDI), MEMR(WADDO), MEMR(TADDI), MEMR(TADDO),
     $		    MEMR(BADDI), MEMR(BADDO), MEMR(WTADDS),
     $   	    TINTD, NIN, NOUT, NANT)
C
C Copy header items
C
      CALL HEDACOPY (INNAME, OUTNAME)
C
C Loop over all stokes parameters
C
      DO 20 IS = 1, STRLEN(STOKES)
C
         NSUBSIN = STRM3 (INNAME, CLASS, STOKES(IS:IS))
         NSUBSOUT = STRM3 (OUTNAME, CLASS, STOKES(IS:IS))
         VISADDI = DATADD(STRM2(NSUBSIN, 'VIS'))
         WTADDI = DATADD(STRM2(NSUBSIN, 'WT'))
         CALL DATCREAT (NSUBSOUT)
         CALL DATMAKAR (STRM2(NSUBSOUT,'VIS'), 1, NOUT, 'X', VISADDO)
         CALL DATMAKAR (STRM2(NSUBSOUT,'WT'), 1, NOUT, 'R', WTADDO)
C
         IF (DOMEDIAN) THEN
            CALL VISAVP3 (MEMX(VISADDI), MEMX(VISADDO), MEMR(WTADDI),
     $         MEMR(WTADDS), MEMR(WTADDO), MEMR(TADDI), MEMR(BADDI),
     $         TINTD, SIGMAUW, NIN, NOUT, NANT, WTMODE)
         ELSE
            CALL VISAVP2 (MEMX(VISADDI), MEMX(VISADDO), MEMR(WTADDI),
     $         MEMR(WTADDS), MEMR(WTADDO), MEMR(TADDI), MEMR(BADDI),
     $         TINTD, SIGMAUW, SSIGMA, SSIGUW, NIN, NOUT, NANT, WTMODE)
         END IF
C
         CALL CRDGET (NSUBSIN, NAX, TYPE, NAXIS, RVAL, RPIX, DELT,
     $      ROTA)
         CALL CRDPUT (NSUBSOUT, NAX, TYPE, NAXIS, RVAL, RPIX, DELT,
     $      ROTA)
C
 20   CONTINUE
C
      IF ((WTMODE.EQ.'RMS').OR.(WTMODE.EQ.'VB-RMS')) THEN
         WRITE (MESSAGE, 1000) SSIGMA
 1000    FORMAT ('Measured average dispersion is',1PE12.4)
         CALL MSGPUT (MESSAGE,'I')
         IF (.NOT.DOMEDIAN) THEN
            WRITE (MESSAGE, 1010) SSIGUW
 1010       FORMAT ('Measured sigma for unit weight is',1PE12.4)
            CALL MSGPUT (MESSAGE,'I')
         END IF
      END IF
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
