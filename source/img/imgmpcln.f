C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgmpcln.f	1.1    11/21/94
C
      SUBROUTINE IMGMPCLN (DRT, PSF, PBOX, CLN, RES, BOX)
C
CD Clean an image.  Multi PSF
C
C	DRT	CH*(*)	input	Name of Dirty image
C	PSF	CH*(*)	input	Name of Point Spread Function Stack
C	PBOX	CH*(*)	input	Name of PBox stack
C	CLN	CH*(*)	input	Name of Clean image
C	RES	CH*(*)	input	Name of Residual image
C	BOX	CH*(*)	input	Name of Clean Box image
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines.
C				D.S.Briggs	Nov 20 1994
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	DRT, PSF, PBOX, CLN, RES, BOX
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGMPCLN')
C
      REAL		GAIN, FLUX, MAXRES, TFLUX, RPIX(SYSMXDIM)
      INTEGER		NSUB, BSUB, ANSUB, POADD, NPSF
      CHARACTER*1	DATYPE, PATYPE, CATYPE, RATYPE, ATYPE, BATYPE,
     $   		PBATYPE
      INTEGER		DNAX, DADD, DNAXIS(SYSMXDIM)
      INTEGER		PNAX, PADD, PNAXIS(SYSMXDIM)
      INTEGER		PBNAX, PBADD, PBNAXIS(SYSMXDIM)
      INTEGER		CNAX, CADD, CNAXIS(SYSMXDIM)
      INTEGER		RNAX, RADD, RNAXIS(SYSMXDIM)
      INTEGER		BNAX, BADD, BNAXIS(SYSMXDIM)
      INTEGER		IAX, NREAL, NDUMMY, NAX, NAXIS(SYSMXDIM)
      INTEGER		CLADD, CXLADD, CYLADD, CZLADD, PSFRX, PSFRY
      LOGICAL		DATEXIST
      CHARACTER*(SYSMXNAM)	STRM2
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (DRT, DNAX, DNAXIS, DATYPE, DADD)
      IF (ERROR) GO TO 990
      IF (DATYPE.NE.'R') THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1      'Bad array type '//DATYPE//' for Dirty Image')
         GO TO 990
      END IF
C
      CALL DATGETAR (PSF, PNAX, PNAXIS, PATYPE, PADD)
      IF (ERROR) GO TO 990
      IF (PATYPE.NE.'R') THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1      'Bad array type '//PATYPE//' for PSF')
         GO TO 990
      END IF
C
      CALL DATGETAR (PBOX, PBNAX, PBNAXIS, PBATYPE, PBADD)
      IF (ERROR) GO TO 990
      IF (PBATYPE.NE.'R') THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1      'Bad array type '//PBATYPE//' for PBOX')
         GO TO 990
      END IF
C
      CALL DATGETAR (CLN, CNAX, CNAXIS, CATYPE, CADD)
      IF (ERROR) GO TO 990
      IF (CATYPE.NE.'R') THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1      'Bad array type '//CATYPE//' for CLEAN Image')
         GO TO 990
      END IF
C
      CALL DATGETAR (RES, RNAX, RNAXIS, RATYPE, RADD)
      IF (ERROR) GO TO 990
      IF (RATYPE.NE.'R') THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1      'Bad array type '//RATYPE//' for Residual Image')
         GO TO 990
      END IF
C
      CALL DATGETAR (BOX, BNAX, BNAXIS, BATYPE, BADD)
      IF (ERROR) GO TO 990
      IF (BATYPE.NE.'R') THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1      'Bad array type '//BATYPE//' for Clean Box Image')
         GO TO 990
      END IF
C
      NREAL = 0
      DO 10 IAX = 1, SYSMXDIM
         IF (CNAXIS(IAX).NE.DNAXIS(IAX)) THEN
            CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1         'Clean and Dirty Axes disagree')
            GO TO 990
         END IF
         IF (RNAXIS(IAX).NE.DNAXIS(IAX)) THEN
            CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1         'Residual and Dirty Axes disagree')
            GO TO 999
         END IF
         IF (BNAXIS(IAX).NE.DNAXIS(IAX)) THEN
            CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1         'Clean Box and Dirty Axes disagree')
            GO TO 999
         END IF
         IF ((PBNAXIS(IAX).NE.DNAXIS(IAX)).AND.(DNAXIS(IAX).GT.1)) THEN
            CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1         'PSF Box and Dirty Axes disagree')
            GO TO 999
         END IF
         IF (DNAXIS(IAX).GT.1) THEN
            NREAL = NREAL + 1
            IF (PNAXIS(IAX).LT.(2*DNAXIS(IAX)+1)) THEN
               CALL MSGPUT ('Warning: PSF may be too small', 'W')
               GO TO 11
            END IF
         ELSE
            GO TO 11
         END IF
  10  CONTINUE
  11  CONTINUE
C
      CALL DATGETR (PSF,'CRPIX',RPIX, SYSMXDIM, NDUMMY)
      PSFRX = RPIX(1)
      PSFRY = RPIX(2)
      NPSF = PNAXIS(NREAL+1)
      CALL DATMAKAR ('PSFOffset', 1, NREAL*NPSF, 'I', POADD)
C
C
C Get control parameters
C
      IF (DATEXIST(STRM2(CLN, 'NITER'))) THEN
         CALL DATGETI(CLN, 'NITER', NSUB, 1, NDUMMY)
      ELSE
         NSUB = 100
         CALL DATPUTI(CLN, 'NITER', NSUB, 1)
      END IF
      IF (DATEXIST(STRM2(CLN, 'BITER'))) THEN
         CALL DATGETI(CLN, 'BITER', BSUB, 1, NDUMMY)
      ELSE
         BSUB = 1
         CALL DATPUTI(CLN, 'BITER', BSUB, 1)
      END IF
      IF (DATEXIST(STRM2(CLN, 'GAIN'))) THEN
         CALL DATGETR(CLN, 'GAIN', GAIN, 1, NDUMMY)
      ELSE
         GAIN = 0.1
         CALL DATPUTR(CLN, 'GAIN', GAIN, 1)
      END IF
      IF (DATEXIST(STRM2(CLN, 'FLUX'))) THEN
         CALL DATGETR(CLN, 'FLUX', FLUX, 1, NDUMMY)
      ELSE
         FLUX = 0.0
         CALL DATPUTR(CLN, 'FLUX', FLUX, 1)
      END IF
      IF (DATEXIST(STRM2(CLN, 'TFLUX'))) THEN
         CALL DATGETR(CLN, 'TFLUX', TFLUX, 1, NDUMMY)
      ELSE
         TFLUX = 0.0
         CALL DATPUTR(CLN, 'TFLUX', TFLUX, 1)
      END IF
C
C Make clean component list
C
      IF (.NOT.DATEXIST (STRM2(CLN, 'PIXLIST'))) THEN
         NAX = 1
         NAXIS(1) = ABS(NSUB)
         ATYPE = 'R'
         CALL DATMAKAR (STRM2(CLN, 'PIXLIST'), NAX, NAXIS, ATYPE,
     1     CLADD)
         NAX = 2
         ATYPE = 'I'
         NAXIS(2) = NREAL
         CALL DATMAKAR (STRM2(CLN, 'PIXLOC'), NAX, NAXIS, ATYPE,
     1     CXLADD)
         CYLADD = CXLADD + NAXIS(1)
         CZLADD = CXLADD + 2*NAXIS(1)
      ELSE
         CALL DATGETAR (STRM2(CLN, 'PIXLIST'), NAX, NAXIS, ATYPE,
     1     CLADD)
         CALL DATGETAR (STRM2(CLN, 'PIXLOC'), NAX, NAXIS, ATYPE,
     1     CXLADD)
         CYLADD = CXLADD + NAXIS(1)
         CZLADD = CXLADD + 2*NAXIS(1)
      END IF
C
C Finally do something
C
      IF (NREAL.EQ.2) THEN
         CALL PIX2RMCL (MEMR(DADD), MEMR(PADD), MEMR(PBADD), MEMR(BADD),
     $      DNAXIS(1), DNAXIS(2), PNAXIS(1), PNAXIS(2), PNAXIS(3),
     $	    PSFRX, PSFRY, MEMI(POADD), MEMI(POADD+NPSF), GAIN, NSUB,
     $      BSUB, FLUX, MEMR(CADD), MEMR(RADD),
     $      ANSUB, MAXRES, TFLUX,
     $      MEMR(CLADD), MEMI(CXLADD), MEMI(CYLADD))
      ELSE
         WRITE (MESSAGE, 1000) NREAL
 1000    FORMAT ('Cannot clean ',I1,'-D images')
         CALL ERRREPOR (ERRBDARG, ROUTINE, MESSAGE)
         GO TO 990
      END IF
C
C Store goodies
C
      CALL DATPUTI (CLN, 'NITER', ANSUB, 1)
      CALL DATPUTR (CLN, 'FLUX', MAXRES, 1)
      CALL DATPUTR (CLN, 'TFLUX', TFLUX, 1)
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
