C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)flydoc.f	1.12    24 Feb 1995
C
      SUBROUTINE SDEMAIN
C
CD Advise on fly usage
C
C Audit trail:
C	Redefined niter to be consistent with use in fly i.e. number
C	of clean components is independent of number of patches.
C					T.J. Cornwell July 16, 1992
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FLYDOC')
C
      INTEGER 		NX, NY, NZ, NDUMMY, NFACET, I, NXT, NVIS, NITER
      REAL		WAVE, BASE, FOV, CELL, NWORDS, PHASE, D
      CHARACTER*(SYSMXNAM)	CONF
C
      REAL		R2S, PI
C==================================================================
C
      PI = 4.0*ATAN(1.0)
      R2S = 180.0*3600.0/PI
C
      CALL MSGWELCO('I advise on wide-field imaging')
 1    CONTINUE
      CALL USRCTL
C
C Get information from user
C
      CALL USRGETR ('Wavelength', WAVE, 1, NDUMMY)
      CALL USRGETC ('Configuration', CONF, 1, NDUMMY)
      CALL USRGETR ('D', D, 1, NDUMMY)
      CALL USRGETR ('Fov', FOV, 1, NDUMMY)
      CALL USRGETR ('Cellsize', CELL, 1, NDUMMY)
      CALL USRGETI ('Nvis', NVIS, 1, NDUMMY)
      CALL USRGETI ('Niter', NITER, 1, NDUMMY)
      CALL USRGETR ('Phase', PHASE, 1, NDUMMY)
      IF(ERROR) GO TO 999
C
C Configuration
C
      IF (CONF(1:1).EQ.'D') THEN
         BASE=1000.0
      ELSEIF (CONF(1:1).EQ.'C') THEN
         BASE=3500.0
      ELSEIF (CONF(1:1).EQ.'B') THEN
         BASE=10000.0
      ELSEIF (CONF(1:1).EQ.'A') THEN
         BASE=35000.0
      ELSE
         BASE=1000.0
         CONF='D'
      ENDIF
      CALL MSGPUT ('Configuration '//CONF(1:1), 'I')
C
C Wavelength
C
      WRITE (MESSAGE, 1005) WAVE
 1005 FORMAT ('Wavelength = ',F7.3,' [m]')
      CALL MSGPUT (MESSAGE, 'I')
C
C How much peeling is allowed?
C
      WRITE (MESSAGE, 1050) PHASE
 1050 FORMAT ('Phase error limited to ',F7.3,' radians')
      CALL MSGPUT (MESSAGE, 'I')
C
C Find field of view
C
      IF(FOV.LE.0.0) THEN
         FOV = 2.0*WAVE/D
      ELSE
         FOV = FOV/R2S
      ENDIF
      WRITE (MESSAGE, 1010) R2S * FOV
 1010 FORMAT ('FOV  = ',F12.2,' [arcsec]')
      CALL MSGPUT (MESSAGE, 'I')
C
C Find cellsize
C
      IF(CELL.LE.0.0) THEN
         CELL = WAVE/(BASE*3.0)
      ELSE
         CELL = CELL/R2S
      ENDIF
      WRITE (MESSAGE, 1020) R2S * CELL
 1020 FORMAT ('Cellsize = ',F12.2,' [arcsec]')
      CALL MSGPUT (MESSAGE, 'I')
C
C Find total number of pixels across field
C
      NXT = FOV/CELL
      WRITE (MESSAGE, 1200) NXT 
 1200 FORMAT ('Number of pixels across whole field of view = ',I5)
      CALL MSGPUT (MESSAGE, 'I')
C
C Find NX
C
C
C Now solve for NX and NFACET using the relationship:
C      NZ = NX * FOV / (8 * F)
C
      CALL MSGPUT ('  NZ       NX,  NY        Facets      Memory usage',
     $   'I')
      CALL MSGPUT ('                                      (Mwords)',
     $   'I')
      DO 100 I = 1, 20
         NZ = 2**(I-1)
         NX = MAX(1, INT(SQRT(2*NZ*PHASE*WAVE/(PI*CELL**2*BASE))))
         NY = NX
         NFACET = MAX(1,1+NXT/NX)
         NWORDS = (4.0*FLOAT(NITER)+FLOAT(NFACET)**2*2.0*
     $      FLOAT(NX*NY)+FLOAT(NX*NY)*(3.0*FLOAT(NZ)+1.0)+
     $      9.0*FLOAT(NVIS))/1000000.0
         WRITE (MESSAGE, 1100) NZ, NX, NY, NFACET, NFACET, NWORDS
 1100    FORMAT (1X,I4,3X,I5,',',I5,3X,I3,' by ',I3,3X,F15.3)
         CALL MSGPUT (MESSAGE, 'I')
         IF(NFACET.EQ.1) GO TO 101
 100  CONTINUE
 101  CONTINUE
C
      CALL MSGPUT ('Full 3D requires:', 'I')
      CALL MSGPUT ('    NX,   NY,   NZ       Memory usage', 'I')
      CALL MSGPUT ('                        (Mwords)', 'I')
      NZ = 2 * MAX(1, NINT(FLOAT(NXT**2) * CELL / 8.0))
      NX = 2 * NXT
      NY = 2 * NXT
      NZ = 2 * MAX(1, NINT(FLOAT(NXT**2) * CELL / 8.0))
      NWORDS = (4.0*FLOAT(NITER/NFACET)+FLOAT(NX*NY)+
     $   FLOAT(3*NX*NY)*FLOAT(NZ)+9.0*FLOAT(NVIS))/1000000.0
      WRITE (MESSAGE, 1300) NX, NY, NZ, NWORDS
 1300 FORMAT (1X,I5,',',I5,',',I5,3X,F15.3)
      CALL MSGPUT (MESSAGE, 'I')
C
      IF(ERROR) GO TO 999
      GO TO 1
C
 999  CONTINUE
      END
