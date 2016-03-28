C
C       National Radio Astronomy Observatory, Socorro, NM 87801
C       Software Development Environment (SDE)
C++
C @(#)imgmschk.f	1.3    8/27/92
C
      SUBROUTINE IMGMSCHK (IMAGE, MASK, ERRLEV, RESULT)
C
CD Check mask for compatability with image.
C
C       IMAGE   CH*(*)  input   Name of image
C       MASK    CH*(*)  input   Name of image mask
C       ERRLEV  CHAR*5  input   Level of compatability demanded
C       RESULT  CHAR*5  output  Compatability code
C
C       RESULT = 'EXACT' Exactly compatable
C       RESULT = 'FIT'   May be fixed with IMGFITTO (PAD or SUBSE)
C       RESULT = 'FATAL' No way to fit them short of interpolation.
C                        (Rotations or delta don't match.  Mask not
C                        real.  Different ref values.)
C
C       ERRLEV is the level compatability that is demanded.  If the
C       result is more serious than this, ERRREPOR will be called.
C	ERRLEV = ' ' is interpreted as 'FIT'
C
C Audit trail:
C       Original version:
C                               D.S.Briggs      April 2 1992
C       Not so original version.  I managed to delete 3/4 of the
C       source, and this is a rewrite.  :(
C				D.S.Briggs	May 18 1992
C	Mask may have "smaller" dimensionality than Image.  Damn
C	pseudo axes anyway!  Mask must be same type as image, instead
C	of strictly real.
C				D.S.Briggs	August 20 1992
C-----------------------------------------------------------------------
#include        "stdinc.h"
C
      CHARACTER*(*)     ROUTINE
      PARAMETER         (ROUTINE = 'IMGMSCHK')
C
      CHARACTER*(*)     IMAGE, MASK, ERRLEV, RESULT
C
      INTEGER           NDUMMY, IAX, IERRLEV, IRESULT, IRNAX, MRNAX,
     $   		INAX, INAXIS(SYSMXDIM),
     $                  MNAX, MNAXIS(SYSMXDIM)
      CHARACTER*1       IATYPE, MATYPE
      CHARACTER*8       ITYPE(SYSMXDIM), MTYPE(SYSMXDIM)
      DOUBLE PRECISION  IRVAL(SYSMXDIM), MRVAL(SYSMXDIM)
      REAL              IRPIX(SYSMXDIM), MRPIX(SYSMXDIM), 
     $   		IDELT(SYSMXDIM), MDELT(SYSMXDIM),
     $   		IROTA(SYSMXDIM), MROTA(SYSMXDIM)
      DOUBLE PRECISION	TOL
C
      INTEGER		CRDRNAX
      LOGICAL		DATEXIST
      REAL		DATFGETR
      CHARACTER*(SYSMXNAM)	STRM2
C=======================================================================
      IF (ERROR) GO TO 999
C
      IERRLEV = -1
      IF (ERRLEV.EQ.'FATAL') IERRLEV = 3
      IF (ERRLEV.EQ.'FIT') IERRLEV = 2
      IF (ERRLEV.EQ.' ') IERRLEV = 2
      IF (ERRLEV.EQ.'EXACT') IERRLEV = 1
      IF (IERRLEV.LT.0) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Unrecognized ERRLEV')
         GO TO 999
      END IF
C
C These may generate an error if the item is not found.
C
      CALL DATGETAR (IMAGE, INAX, INAXIS, IATYPE, NDUMMY)
      CALL DATGETAR (MASK, MNAX, MNAXIS, MATYPE, NDUMMY)
      CALL CRDGET (IMAGE, INAX, ITYPE, INAXIS, IRVAL, IRPIX, IDELT,
     $   IROTA)
      CALL CRDGET (MASK, MNAX, MTYPE, MNAXIS, MRVAL, MRPIX, MDELT,
     $   MROTA)
      IF (ERROR) GO TO 990
      IRNAX = CRDRNAX (INAX, INAXIS)
      MRNAX = CRDRNAX (MNAX, MNAXIS)
C
C Check for FATAL stuff first
C
      RESULT = 'FATAL'
      IRESULT = 3
C
      TOL = 0.1D0/3600.D0
C
      IF (IRNAX.LT.MRNAX) THEN
         IF (IRESULT.GT.IERRLEV)
     $      CALL ERRREPOR(ERRBDARG, ROUTINE,
     $      'Fewer number of real axes in image than mask')
         GO TO 999
      END IF
C
      IF (MATYPE.NE.IATYPE) THEN
         IF (IRESULT.GT.IERRLEV)
     $      CALL ERRREPOR(ERRBDARG, ROUTINE,
     $      'Mask must be of same type as image')
         GO TO 999
      END IF
C
      DO 10 IAX = 1, IRNAX
C
         IF (ABS (IDELT(IAX) - MDELT(IAX)) .GT. TOL) THEN
            IF (IRESULT.GT.IERRLEV)
     $         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $         'Image and mask have different cell sizes')
            GOTO 999
         ENDIF
C
         IF (ABS(IRVAL(IAX) - MRVAL(IAX)) .GT. TOL) THEN
            IF (IRESULT.GT.IERRLEV)
     $         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $         'Image and mask have different reference values')
            GOTO 999
         ENDIF
C
         IF (ABS(IROTA(IAX) - MROTA(IAX)) .GT. TOL) THEN
            IF (IRESULT.GT.IERRLEV)
     $         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $         'Image and mask have different rotations')
            GOTO 999
         ENDIF
C
 10   CONTINUE
C
C Now stuff that can be mitigated with IMGFITTO
C
      RESULT = 'FIT'
      IRESULT = 2
C
      DO 20 IAX = 1, IRNAX
C
         IF (INAXIS(IAX).NE.MNAXIS(IAX)) THEN
            IF (IRESULT.GT.IERRLEV)
     $         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $         'Image and mask have different sizes')
            GOTO 999
         ENDIF
C
 20   CONTINUE
C
C Print some warnings if values in mask are unusual.
C
      IF (.NOT.DATEXIST(STRM2(MASK,'ARRMAX')))
     $   CALL ARRSTAT(MASK, ' ')
C
      IF (DATFGETR(MASK, 'ARRMAX').GT.1.0) THEN
         MESSAGE = 'Mask has maximum value greater than 1.0'
         CALL MSGPUT(MESSAGE, 'W')
      END IF
      IF (DATFGETR(MASK, 'ARRMIN').LT.0.0) THEN
         MESSAGE = 'Mask has negative minumum value'
         CALL MSGPUT(MESSAGE, 'W')
C
      END IF
C
C Looks OK
C
      RESULT = 'EXACT'
      IRESULT = 1
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
