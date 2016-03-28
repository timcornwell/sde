C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrfftr.f	1.6    5/25/94
C
      SUBROUTINE ARRFFTR (RARRAY, XARRAY, DIR)
C
CD Real to complex Fourier transform. Operates on SDE arrays.
C
C	RARRAY	CH*(*)	input	Name of real array
C	XARRAY	CH*(*)	input	Name of complex array
C	DIR	CH*(*)	input	Direction of transform FPS convention
C	
C
C No packing is performed so the transform of a REAL array of size
C NX, NY is a COMPLEX array of size NX/2+1, NY. FFTIMG calls this
C routine to do the work, and FFTCONJA to calculate new 
C headers.
C      This version will expand and contract arrays as required,
C zero-padding and avoiding extra transforms. The phase center of the
C transforms is at NX/2, NY/2, NZ/2, etc. in the image plane, and at
C 1, NV/2, NW/2, etc. in the transform plane.
C  This version will automatically detect equivalent third axes and
C not transform. We should generalize this one day...
C
C In a really ugly problem, the axis information in NAXIS does not
C contain enough information to properly conjugate the first axis.
C No way to tell if it should be 2*NU-2 or 2*NU-1.  As a consequence,
C all subsequent routines that take NU now require it to be FNU, or
C the full sized U axis.  We default to the former, and allow it to
C be overridden in the XARRAY/FNU.  Presumably this will be set where
C appropriate by FFTCONJA.
C
C Audit trail:
C	Now will not transform equal third axes
C				T.J.Cornwell	Feb 18 1989
C	Changed to allow 8192 transforms
C				T.J.Cornwell	Oct 30 1989
C	Support added for odd sized images
C				D.S.Briggs	Jan 21 1992
C	Call to FFTR1D was incorrect
C				D.S.Briggs	Oct 21 1992
C	Address of work array not being stored properly.
C				D.S.Briggs	May 25 1994
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	RARRAY, XARRAY
      INTEGER		DIR
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'ARRFFTR')
C
      INTEGER		MAXGAMMA, MAXN
      PARAMETER		(MAXGAMMA = 13)
      PARAMETER		(MAXN = 2**MAXGAMMA+1)
C
      CHARACTER		ATYPE*1
      INTEGER		RNAX, XNAX, IAX, XNAXIS(SYSMXDIM),
     1			RNAXIS(SYSMXDIM)
      INTEGER		NDUMMY, RADD, XADD, XWORKADD, RWORKADD, OFFR,
     1			OFFX, IZ, RXNAX, FNU
      CHARACTER*8	RCTYPE(SYSMXDIM), XCTYPE(SYSMXDIM)
      LOGICAL		SLOWZ
C
      INTEGER		CRDRNAX, DATFGETI, DATADD
      LOGICAL		DATEXIST
      CHARACTER*(SYSMXNAM)	STRM2
C=======================================================================
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (RARRAY, RNAX, RNAXIS, ATYPE, RADD)
      IF (ATYPE.NE.'R') THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Bad array type '//ATYPE)
         GO TO 990
      END IF
C
      CALL DATGETAR (XARRAY, XNAX, XNAXIS, ATYPE, XADD)
      IF (ATYPE.NE.'X') THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Bad array type '//ATYPE)
         GO TO 990
      END IF
C
      RXNAX = CRDRNAX(XNAX, XNAXIS)
      IF (RXNAX.LE.0) THEN
         CALL ERRREPOR (ERRLOGIC, ROUTINE, 
     1      'No real axes for complex image')
         GO TO 990
      END IF
      DO 10 IAX = 1, RXNAX
         IF(XNAXIS(IAX).LE.1) THEN
            CALL ERRREPOR (ERRLOGIC, ROUTINE, 
     1         'Trying to transform complex axis with only one pixel')
            GO TO 999
         END IF
         IF(RNAXIS(IAX).LE.1) THEN
            CALL ERRREPOR (ERRLOGIC, ROUTINE, 
     1         'Trying to transform real axis with only one pixel')
            GO TO 999
         END IF
  10  CONTINUE
C
C Deal with conjugation problem
C
      FNU = 2 * (XNAXIS(1) - 1)
      IF (DATEXIST(STRM2(XARRAY,'FNU')))
     $   FNU = DATFGETI(XARRAY,'FNU')
C
C Create work arrays
C
      IF (.NOT.DATEXIST('SCRATCH/FFTX')) THEN
         CALL DATCREAT ('SCRATCH')
         CALL DATMAKAR ('SCRATCH/FFTX', 1, 2*MAXN, 'X', XWORKADD)
         CALL DATMAKAR ('SCRATCH/FFTR', 1, 2*MAXN, 'R', RWORKADD)
      ELSE
         XWORKADD = DATADD ('SCRATCH/FFTX')
         RWORKADD = DATADD ('SCRATCH/FFTR')
      END IF
C
C Find out if the third axis should be transformed: this really doesn't
C belong here. We should find a better way of doing this.
C
      IF(DATEXIST(STRM2(RARRAY, 'CTYPE')).AND.
     $   DATEXIST(STRM2(XARRAY, 'CTYPE'))) THEN
         CALL DATGETC (RARRAY, 'CTYPE', RCTYPE, SYSMXDIM, NDUMMY)
         CALL DATGETC (XARRAY, 'CTYPE', XCTYPE, SYSMXDIM, NDUMMY)
         SLOWZ = RCTYPE(3).EQ.XCTYPE(3)
      ELSE
         SLOWZ =  .FALSE.
      ENDIF
C
C Now call pixel level routine
C
      IF (RXNAX.EQ.1) THEN
         CALL FFTR1D2 (MEMR(RADD), MEMX(XADD), MEMX(XWORKADD),
     1      MEMX(XWORKADD+MAXN), MEMR(RWORKADD), RNAXIS(1), FNU, DIR)
      ELSEIF (RXNAX.EQ.2) THEN
         CALL FFTR2D (MEMR(RADD), MEMX(XADD), MEMX(XWORKADD), 
     1      MEMX(XWORKADD+MAXN), MEMR(RWORKADD), RNAXIS(1), 
     2      RNAXIS(2), FNU, XNAXIS(2), DIR)
      ELSEIF (RXNAX.EQ.3) THEN
         IF(SLOWZ) THEN
            DO 100 IZ = 1, XNAXIS(3)
               OFFR = RNAXIS(1) * RNAXIS(2) * (IZ - 1)
               OFFX = XNAXIS(1) * XNAXIS(2) * (IZ - 1)
               CALL FFTR2D (MEMR(RADD + OFFR), MEMX(XADD+OFFX), 
     1         MEMX(XWORKADD), MEMX(XWORKADD+MAXN), 
     2         MEMR(RWORKADD), RNAXIS(1), 
     3         RNAXIS(2), FNU, XNAXIS(2), DIR)
  100       CONTINUE
         ELSE
            CALL FFTR3D (MEMR(RADD), MEMX(XADD), MEMX(XWORKADD), 
     1         MEMX(XWORKADD+MAXN), MEMR(RWORKADD), RNAXIS(1), 
     2         RNAXIS(2), RNAXIS(3), FNU, XNAXIS(2), XNAXIS(3),
     3         DIR)
         END IF
      ELSE 
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Illegal dimension')
         GO TO 999
      END IF
C
  990 IF (ERROR) CALL ERRTRACE (ROUTINE)
C
  999 CONTINUE
      END
