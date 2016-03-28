C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgfft.f	1.4	 2/21/91
C
      SUBROUTINE IMGFFT (ARRAYIN, ARRAYOUT)
C
CD FFT an image. This will automatically update the header, find
C the direction of the transform. Can do all forms of transform.
C
C	ARRAYIN		CH*(*)	input	Name of image
C	ARRAYOUT	CH*(*)	input	Name of image
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C	Now will do COMPLEX to COMPLEX also.  
C				M.A.Holdaway	Feb 21 1991
C
C=====================================================================
#include	"stdinc.h"
C
      CHARACTER*(*)	ARRAYIN, ARRAYOUT
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER 	(ROUTINE = 'IMGFFT')
C
      INTEGER		NAX, NAXIS (SYSMXDIM), AADD, DIR
      CHARACTER		STRM2*(SYSMXNAM), INTYPE*1, OUTTYPE*1
      CHARACTER*(SYSMXNAM)      IMTYPIN, IMTYPOUT, VISTYPE
      LOGICAL		DATEXIST, XTX, XTR, RTX
C===================================================================
      IF (ERROR) GO TO 999
C
      XTX = .FALSE.
      XTR = .FALSE.
      RTX = .FALSE.
      CALL IMGTYPE  (ARRAYIN, IMTYPIN)
      CALL DATGETAR (ARRAYIN, NAX, NAXIS, INTYPE, AADD)
C
C Find out types: Real or Complex Image? Hermitian or Non-hermitian VIS
C
      IF (IMTYPIN .EQ. 'IMAGE') THEN
         IF (INTYPE .EQ. 'X') THEN
            XTX = .TRUE.
         ELSE IF (INTYPE .EQ. 'R') THEN
            RTX = .TRUE.
         ELSE
            CALL ERRREPOR (ERRBDARG, ROUTINE,
     $         'Unable to FT image, not R or X')
         ENDIF
      ELSE IF (IMTYPIN .EQ. 'VIS') THEN
         CALL FFTVISTP (ARRAYIN, VISTYPE)
         IF (VISTYPE .EQ. 'H') THEN
            XTR = .TRUE.
         ELSE IF (VISTYPE .EQ. 'N') THEN
            XTX = .TRUE.
         ELSE
            CALL ERRREPOR (ERRBDARG, ROUTINE,
     $         'Unknown visibility type, not I or P')
         ENDIF
      ENDIF
C
C If the output array header exists then we assume that the header is 
C correct, otherwise we create the output array with the correct
C header information
C
      IF (.NOT.DATEXIST (STRM2(ARRAYOUT, 'CRVAL'))) THEN
         CALL DATCREAT (ARRAYOUT)
         IF (XTX) THEN
            CALL FFTCONJX (ARRAYIN, ARRAYOUT, DIR, 0)
         ELSE IF (XTR .OR. RTX) THEN
            CALL FFTCONJA (ARRAYIN, ARRAYOUT, DIR, 0)
         ENDIF
      END IF
C
      CALL IMGTYPE (ARRAYOUT, IMTYPOUT)
      CALL DATGETAR (ARRAYOUT, NAX, NAXIS, OUTTYPE, AADD)
      IF (ERROR) GO TO 990
C
C Complex to Complex
C
      IF (INTYPE .EQ. 'X'  .AND.  OUTTYPE .EQ. 'X') THEN
         IF (IMTYPIN .EQ. 'IMAGE' .AND. IMTYPOUT .EQ. 'VIS') THEN
            CALL ARRFFTX (ARRAYIN, ARRAYOUT, 1)
         ELSE IF (IMTYPIN .EQ.'VIS'.AND.IMTYPOUT .EQ.'IMAGE') THEN
            CALL ARRFFTX (ARRAYOUT, ARRAYIN, -1)
         ELSE
            CALL ERRREPOR (ERRBDARG, ROUTINE,
     $         'Cannot transform NON-image, NON-vis arrays')
            GO TO 999
         ENDIF
C
C Real to Complex
C
      ELSE IF (INTYPE.EQ.'R') THEN
         IF (OUTTYPE.EQ.'X') THEN
            CALL ARRFFTR (ARRAYIN, ARRAYOUT, +1)
         ELSE 
            CALL ERRREPOR (ERRBDARG, ROUTINE,
     1         'Cannot transform '//INTYPE//' to '//OUTTYPE)
            GO TO 999
         END IF
C
C Complex to Real
C
      ELSE IF (INTYPE.EQ.'X') THEN
         IF (OUTTYPE.EQ.'R') THEN
            CALL ARRFFTR (ARRAYOUT, ARRAYIN, -1)
         ELSE
            CALL ERRREPOR (ERRBDARG, ROUTINE,
     1         'Cannot transform '//INTYPE//' to '//OUTTYPE)
            GO TO 999
         END IF
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     1      'Cannot transform image of type '//INTYPE)
         GO TO 999
      END IF
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
