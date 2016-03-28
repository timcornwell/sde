C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imggridc.f	1.5    2/18/92
C
      SUBROUTINE IMGGRIDC (IMG, CORIMG, MODE)
C
CD Perform grid correction for an image.
C
C	IMG	CH*(*)	input	Name of image to correct
C	CORIMG	CH*(*)	input	Name of corrected image
C	MODE	CH*(*)	input	APPLY|CORRECT
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Hard-wired REFPIX to be NX/2, NY/2 *** Because IMGFFT
C	hardwires NX/2, NY/2.
C				M.A.Holdaway	March 20 1991
C	Changed to REFPIX calculations to allow for odd sized images
C				D.S.Briggs	Jan 3 1991
C------------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	IMG, CORIMG, MODE
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGGRIDC')
C
      INTEGER		NAX, NAXIS (SYSMXDIM), NREAL, IADD, CCFADD,
     1			NDUMMY, CCFLEN, OFFSET, IZ, I,
     2			CNAX, CCFNAX, CNAXIS (SYSMXDIM), CADD,
     3			FFTPWR2
      LOGICAL		DATEXIST, SLOWZ
      REAL		RPIX(SYSMXDIM)
      CHARACTER		ATYPE*1, CFATYPE*1, STRM2*(SYSMXNAM),
     1			CFTYPE*(SYSMXNAM)
C=======================================================================
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (IMG, NAX, NAXIS, ATYPE, IADD)
      CALL DATGETAR (CORIMG, CNAX, CNAXIS, ATYPE, CADD)
      DO 10 I = 1, NAX
         RPIX(I) = (NAXIS(I)+1) / 2
 10   CONTINUE
C
C Was this made with slow transform in Z?
C
      IF (ERROR) GO TO 990
      CALL DATGETL (IMG, 'SLOWZ', SLOWZ, 1, NDUMMY)
      IF (ERROR) THEN
         SLOWZ = .FALSE.
         CALL ERRCANCE
      END IF
C
C Set up default for convolution function
C
      IF(.NOT.DATEXIST(STRM2(IMG, 'CFTYPE'))) THEN
         CFTYPE = 'SF'
         CALL DATPUTC (IMG, 'CFTYPE', CFTYPE, 1)
      ELSE
         CALL DATGETC (IMG, 'CFTYPE', CFTYPE, 1, NDUMMY)
      END IF
C
C Now take care of convolution function
C
      IF(.NOT.DATEXIST(STRM2(IMG, 'CCFFN'))) THEN
         CCFLEN = 1 + FFTPWR2(MAX(CNAXIS(1), CNAXIS(2)))/2
         CCFNAX = 1
         CFATYPE = 'R'
         CALL DATMAKAR (STRM2(IMG, 'CCFFN'), CCFNAX, CCFLEN, CFATYPE,
     1      CCFADD)
         CALL GRDCCF (CFTYPE, MEMR(CCFADD), CCFLEN)
      ELSE 
         CALL DATGETAR (STRM2(IMG, 'CCFFN'), CCFNAX, CCFLEN, CFATYPE,
     1      CCFADD)
         CALL GRDCCF (CFTYPE, MEMR(CCFADD), CCFLEN)
      END IF
C
      NREAL = 0
  1   CONTINUE
      NREAL = NREAL + 1
      IF (NAXIS(NREAL).NE.CNAXIS(NREAL)) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Axes differ')
         GO TO 999
      END IF
      IF (NAXIS(NREAL+1).GT.1) GO TO 1
C
      IF (NREAL.EQ.1) THEN
         CALL PIXCOR1D (MEMR(IADD), NAXIS(1), MEMR(CCFADD),
     1      CCFLEN, MODE, MEMR(CADD), RPIX(1))
      ELSEIF (NREAL.EQ.2) THEN
         IF(ATYPE.EQ.'R') THEN
            CALL PIXCOR2D (MEMR(IADD), NAXIS(1), NAXIS(2), MEMR(CCFADD),
     1         CCFLEN, MODE, MEMR(CADD), RPIX(1), RPIX(2))
         ELSEIF(ATYPE.EQ.'X') THEN
            CALL PIXCORX2 (MEMX(IADD), NAXIS(1), NAXIS(2), MEMR(CCFADD),
     1         CCFLEN, MODE, MEMX(CADD), RPIX(1), RPIX(2))
         END IF
      ELSEIF (NREAL.EQ.3) THEN
         IF (SLOWZ) THEN
            DO 100 IZ = 1, NAXIS(3)
               OFFSET = NAXIS(1) * NAXIS(2) * (IZ - 1)
               CALL PIXCOR2D (MEMR(IADD+OFFSET), NAXIS(1), NAXIS(2), 
     1            MEMR(CCFADD), CCFLEN, MODE, MEMR(CADD+OFFSET),
     $            RPIX(1), RPIX(2))
 100        CONTINUE
         ELSE
            CALL PIXCOR3D (MEMR(IADD), NAXIS(1), NAXIS(2), NAXIS(3),
     1         MEMR(CCFADD), CCFLEN, MODE, MEMR(CADD), RPIX(1), RPIX(2),
     $         RPIX(3))
         END IF
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Dimension not supported')
         GO TO 999
      END IF
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
