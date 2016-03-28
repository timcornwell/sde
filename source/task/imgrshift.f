C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgrshift.f	1.2    12/10/93
C
      SUBROUTINE SDEMAIN
C
CD Program to shift reference pixel of one image to another
C
C  The image pixels are not modified, and the only restriction is that the
C  the two images must have a compatable set of axis types.  Use this
C  with care.  It can easily result in non-integral referene pixels, which
C  will confuse some tasks.
C
C	Original version
C				D.S.Briggs	April 1993
C	If no template image, just shift the reference pixel to the
C	standard place, (NAXIS(I)+1)/2
C				D.S.Briggs	Dec 10 1993
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGRSHIFT')
C
      CHARACTER*(SYSMXNAM) 	INFILE, REFFILE, OUTFILE
C
      INTEGER           INAX, INAXIS(SYSMXDIM)
      DOUBLE PRECISION  IRVAL(SYSMXDIM)
      REAL              IRPIX(SYSMXDIM), IDELT(SYSMXDIM), 
     $   		IROTA(SYSMXDIM)
      CHARACTER*8       ITYPE(SYSMXDIM)
      INTEGER           RNAX, RNAXIS(SYSMXDIM)
      DOUBLE PRECISION  RRVAL(SYSMXDIM)
      REAL              RRPIX(SYSMXDIM), RDELT(SYSMXDIM), 
     $   		RROTA(SYSMXDIM)
      CHARACTER*8       RTYPE(SYSMXDIM)
C
      DOUBLE PRECISION	WORLD(SYSMXDIM)
      REAL		PIXEL(SYSMXDIM)
      INTEGER		NDUMMY, IAX, REALNAX
C
      CHARACTER*(SYSMXDIM)	STRINT
C==================================================================
C
      CALL MSGWELCO ('I take absolute values of images')
      CALL USRCTL
      CALL USRGETL ('Debug', SYSDEBUG, 1, NDUMMY)
      CALL USRGETC ('Image', INFILE, 1, NDUMMY)
      CALL USRGETC ('Reference', REFFILE, 1, NDUMMY)
      CALL USRGETC ('Output', OUTFILE, 1, NDUMMY)
C
C Get Image
C
      CALL FILIMGGE ('Image', INFILE, ' ')
      IF (REFFILE.NE.' ') CALL FILIMGGE ('Reference', REFFILE, ' ')
      IF (ERROR) GO TO 999
C
      CALL MSGPUT ('Image','I')
      CALL CRDLIST ('Image')
      CALL CRDGET ('Image', INAX, ITYPE, INAXIS, IRVAL, IRPIX, IDELT,
     $   IROTA)
C
      DO 100 IAX = 1, SYSMXDIM
         IF (INAXIS(IAX).EQ.0) INAXIS(IAX) = 1
         IF (INAXIS(IAX).GT.1) REALNAX = IAX
 100  CONTINUE
C
      IF (REFFILE.NE.' ') THEN
         CALL MSGPUT ('Reference', 'I')
         CALL CRDLIST ('Reference')
         CALL CRDGET ('Reference', RNAX, RTYPE, RNAXIS, RRVAL, RRPIX,
     $      RDELT, RROTA)
C
         DO 110 IAX = 1, SYSMXDIM
            IF (RNAXIS(IAX).EQ.0) RNAXIS(IAX) = 1
            IF (INAXIS(IAX).GT.1) THEN
               IF (ITYPE(IAX).NE.RTYPE(IAX)) THEN
                  CALL ERRREPOR (ERRBDARG, ROUTINE,
     $               'Axis types not equal on axis ' // STRINT(IAX))
                  GO TO 999
               END IF
            END IF
 110     CONTINUE
      END IF
C
      IF (REFFILE.NE.' ') THEN
         DO 200 IAX = 1, REALNAX
            WORLD(IAX) = RRVAL(IAX)
 200     CONTINUE
C
         CALL CRDDWTOP ('Image', WORLD, PIXEL)
C
         DO 220 IAX = 1, REALNAX
            IRPIX(IAX) = PIXEL(IAX)
            IRVAL(IAX) = RRVAL(IAX)
 220     CONTINUE
      ELSE
         DO 300 IAX = 1, REALNAX
            PIXEL(IAX) = (INAXIS(IAX)+1)/2
 300     CONTINUE
C
         CALL CRDDPTOW ('Image', PIXEL, WORLD)
C
         DO 310 IAX = 1, REALNAX
            IRPIX(IAX) = PIXEL(IAX)
            IRVAL(IAX) = WORLD(IAX)
 310     CONTINUE
      END IF
C
      CALL CRDPUT ('Image', INAX, ITYPE, INAXIS, IRVAL, IRPIX, IDELT,
     $   IROTA)
      CALL DATRENAM ('Image', 'Output')
      IF (ERROR) GO TO 999
C
      CALL MSGPUT ('Output','I')
      CALL CRDLIST ('Output')
C        
C Bare minimum history
C
      CALL HISINPUT ('Output')
C
C Write result 
C
      IF (OUTFILE.EQ.'*') OUTFILE = INFILE
      IF (OUTFILE.NE.' ') CALL FILIMGPU ('Output', OUTFILE, ' ')
C
  999 CONTINUE
      END
