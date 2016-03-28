C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)lnls.f	1.3    05 Jun 1993
C
      SUBROUTINE SDEMAIN
C
CD Program to make log N/Log S faithful image
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell    Jan 22 1992
C	Added optional output to SDE modelfile
C				D.S.Briggs	Mar 1993
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'LNLS')
C
      REAL                      CELLSIZE(3), SHIFT(3)
      REAL		        RA, DEC, FREQ, SRANGE(2), BETA, DENS
      CHARACTER*(SYSMXNAM)	IMGFILE, MODFILE
      INTEGER		        NDUMMY, IMSIZE(3), SEED
C==================================================================
      CALL MSGWELCO ('I make LOG N/LOG S faithful images')
      CALL USRCTL
C
C Get input parameters
C
      CALL USRGETI('Imsize', IMSIZE, 3, NDUMMY)
      CALL USRGETR('Cellsize', CELLSIZE, 3, NDUMMY)
      CALL USRGETC('Image', IMGFILE, 1, NDUMMY)
      CALL USRGETC('Modfile', MODFILE, 1, NDUMMY)
      CALL USRGETR('Dec', DEC, 1, NDUMMY)
      CALL USRGETR('Freq', FREQ, 1, NDUMMY)
      CALL USRGETR('N', DENS, 1, NDUMMY)
      CALL USRGETR('S', SRANGE, 2, NDUMMY)
      CALL USRGETR('Slope', BETA, 1, NDUMMY)
      CALL USRGETI('Seed', SEED, 1, NDUMMY)
      CALL USRGETL('Debug',SYSDEBUG,1,NDUMMY) 
C
      CALL MSGPUT ('Finding model image', 'I')
      CALL IMGOMAKE (DBLE(RA), DBLE(DEC), DBLE(FREQ), CELLSIZE, 
     1   IMSIZE, SHIFT, 'R', 'Image')
C
      CALL LNSIMG ('Image', DENS, SRANGE, BETA, SEED, MODFILE)
C
C Now write to disk
C
      IF (IMGFILE.NE.' ') THEN
         CALL FILIMGPU ('Image', IMGFILE, ' ')
      END IF
C
 999  CONTINUE
      END
C
      SUBROUTINE LNSIMG (IMAGE, DENS, SRANGE, BETA, SEED, MODFILE)
C
CD Make an image for the LOG N/ LOG S
C
C	IMAGE	CH*(*)	input	Name of directory entry
C	SRANGE	REAL	input	Cut off in Flux
C	BETA	REAL	input	BETA
C	MODFILE	CH*(*)	input	Name of output model file
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 22 1992
C	MODFILE added
C				D.S.Briggs	Apr 8 1993
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	IMAGE, MODFILE
      REAL		SRANGE(2), BETA, DENS
      INTEGER		SEED
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'LNSIMG')
C
      REAL		IMRPIX (SYSMXDIM), IMDELT(SYSMXDIM),
     1			IMROTA (SYSMXDIM)
      DOUBLE PRECISION	IMRVAL(SYSMXDIM)
      CHARACTER*8	IMTYPE(SYSMXDIM), HANDLE
      INTEGER		IMNAX, IMNAXIS(SYSMXDIM), IMADD, DATADD
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      CALL CRDGET (IMAGE, IMNAX, IMTYPE, IMNAXIS, IMRVAL, IMRPIX, 
     1   IMDELT, IMROTA)
C
      IMADD = DATADD(IMAGE)
C
      HANDLE = ' '
      IF (MODFILE.NE.' ') THEN
         HANDLE = 'Modfile'
         CALL FILDEL (MODFILE)
         CALL TXTOPEN (HANDLE, MODFILE, 'WRITE')
      END IF
C
      IF ((IMNAX.EQ.2).OR.((IMNAX.GE.3).AND.(IMNAXIS(3).EQ.1))) THEN
         CALL LNSIMG2D (DENS, SRANGE, BETA, SEED, MEMR(IMADD), 
     1      IMNAXIS(1), IMNAXIS(2), IMRPIX(1), 3600.0*IMDELT(1), 
     2      IMRPIX(2), 3600.0*IMDELT(2), HANDLE)
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1      'Only support 2 dimensions')
         GO TO 999
      END IF
C
      CALL DATPUTC (IMAGE, 'BUNIT', 'JY/PIXEL', 1)
C
      IF (HANDLE.NE.' ') CALL TXTCLOSE (HANDLE)
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
      SUBROUTINE LNSIMG2D (DENS, SRANGE, BETA, SEED, IMAGE, NX, NY, 
     &   REFX, DELTX, REFY, DELTY, HANDLE)
C
CD Make an LOG N/LOG S image 

C	IMAGE	REAL	output	Output image
C	NX, NY	INT	input	Number of pixels on x, y axes
C	REFX	REAL	input	Reference pixel
C	DELTX	REAL	input	Increment in X
C	HANDLE	CH*(*)	input	Handle for text output file
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 22 1992
C---------------------------------------------------------------------
#include	"stdinc.h"
C
C
      INTEGER		NX, NY, SEED
      REAL		DENS, BETA, SRANGE(2)
      REAL		REFX, REFY, DELTX, DELTY, IMAGE(NX, *)
      CHARACTER*(*)	HANDLE
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'LNSIMG2D')
C
      INTEGER		IX, IY, NTOT, I
      REAL		R, OMEGA, R, SI, ALPHA, F, DX, DY
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      IF (DELTX.EQ.0.0) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Zero increment in X')
         GO TO 999
      END IF
      IF (DELTY.EQ.0.0) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Zero increment in Y')
         GO TO 999
      END IF
      IF (BETA.LE.1.0) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Illegal slope')
         GO TO 999
      END IF
C
      DO 2 IY = 1, NY
         DO 1 IX = 1, NX
            IMAGE(IX, IY) = 0.0
 1       CONTINUE
 2    CONTINUE
C
      ALPHA=1.0/(1.0-BETA)
      OMEGA = NX * NY * ABS(DELTX) * ABS(DELTY) 
     $   * (4.0*ATAN(1.0)/(180.0*3600.0))**2
      F = (SRANGE(1)**(1.0-BETA)-SRANGE(2)**(1.0-BETA))
      NTOT = DENS * F * OMEGA / (BETA-1.0)
      WRITE (MESSAGE, 1000) NTOT, OMEGA
 1000 FORMAT ('Making ',I5,' point sources in solid angle ',1PE12.3,
     $   ' sr')
      CALL MSGPUT (MESSAGE, 'I')
      DO 10 I = 1, NTOT
         CALL UTLRAND(R,SEED)
         DX = 1 + (FLOAT(NX)-1.0)*R
         IX = NINT(DX)
         DX = (DX - REFX) * DELTX
         CALL UTLRAND(R,SEED)
         DY = 1 + (FLOAT(NX)-1.0)*R
         IY = NINT(DY)
         DY = (DY - REFY) * DELTY
         CALL UTLRAND(R,SEED) 
         SI = (SRANGE(1)**(1.0-BETA) - F * R)**ALPHA
         IMAGE(IX, IY) = IMAGE(IX,IY) + SI
C
         IF (HANDLE.NE.' ') THEN
            WRITE (MESSAGE, 1010) SI, DX, DY, 0.0, 0.0, 0.0, 'POINT'
 1010       FORMAT(6(G11.5, ','), '''', A4,'''')
            CALL TXTWRITE(HANDLE, MESSAGE)
         END IF
 10   CONTINUE
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
