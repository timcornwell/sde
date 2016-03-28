C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)visres.f	1.4    2/13/91
C
      SUBROUTINE SDEMAIN
C
CD Program to find visibility residuals
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Changed to VISTOIMG and IMGTOVIS
C				T.J.Cornwell	Aug 8 1989
C	Fixed VISPUT
C				T.J.Cornwell    Nov 1 1990
C	Added primary beam correction
C				T.J.Cornwell    Feb 13 1991
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISRES')
C
      CHARACTER*(SYSMXNAM)	VISFILE, MODFILE, RESFILE
      INTEGER		NDUMMY, I, MNAX, VNAX
      LOGICAL           PBCORR, NOFREQ
      DOUBLE PRECISION	FREQ, VCRVAL(SYSMXDIM), MCRVAL(SYSMXDIM),
     $			OBSRA, OBSDEC
      CHARACTER*8	VCTYPE(SYSMXDIM), MCTYPE(SYSMXDIM)
      DATA		VCRVAL / SYSMXDIM*0.0D0/
      DATA		MCRVAL / SYSMXDIM*0.0D0/
      DATA		VCTYPE / SYSMXDIM*' '/
      DATA		MCTYPE / SYSMXDIM*' '/
C==================================================================
      CALL MSGWELCO ('I find residuals')
      CALL USRCTL
C
C Get input images
C
      CALL USRGETC('Vis', VISFILE, 1, NDUMMY)
      CALL USRGETC('Model', MODFILE, 1, NDUMMY)
      CALL USRGETC('Output', RESFILE, 1, NDUMMY)
      CALL USRGETL('PB', PBCORR, 1, NDUMMY)
      CALL VISGET ('Vis', VISFILE, 'I,Q,U,V', '*', ' ')
      CALL VISCLONE ('Vis', 'OBS', 'I', 'MOD')
      CALL FILIMGGE ('Model', MODFILE, ' ')
C
      CALL MSGPUT ('Model coordinates', 'I')
      CALL CRDLIST ('Model')
      CALL MSGPUT ('Vis coordinates', 'I')
      CALL CRDLIST ('Model')
C
C Do Primary beam correction at model frequency and then undo it
C at the visibility frequency
C
      IF (PBCORR) THEN
         CALL MSGPUT ('Correcting primary beam at model frequency', 'I')
         CALL IMGPB ('Model', 'Model', 'CORRECT')
         CALL DATGETD ('Model', 'CRVAL', MCRVAL, SYSMXDIM, MNAX)
         CALL DATGETD ('Vis/OBS/I', 'CRVAL', VCRVAL, SYSMXDIM, VNAX)
         CALL DATGETC ('Model', 'CTYPE', MCTYPE, SYSMXDIM, MNAX)
         CALL DATGETC ('Vis/OBS/I', 'CTYPE', VCTYPE, SYSMXDIM, VNAX)
         IF(ERROR) GO TO 999
         CALL DATGETD ('Vis', 'OBSRA', OBSRA, 1, NDUMMY)
         CALL DATGETD ('Vis', 'OBSDEC', OBSDEC, 1, NDUMMY)
         IF(ERROR) THEN
            CALL ERRCANCE
            OBSRA  = VCRVAL(1)
            OBSDEC = VCRVAL(2)
            CALL DATPUTD ('Vis', 'OBSRA', OBSRA, 1)
            CALL DATPUTD ('Vis', 'OBSDEC', OBSDEC, 1)
         END IF
         IF ((OBSRA.EQ.0.0D0).AND.(OBSDEC.EQ.0.0D0)) THEN
            CALL MSGPUT ('Observed RA, DEC look suspicious, ignoring',
     $         'I')
            OBSRA  = VCRVAL(1)
            OBSDEC = VCRVAL(2)
            CALL DATPUTD ('Vis', 'OBSRA', OBSRA, 1)
            CALL DATPUTD ('Vis', 'OBSDEC', OBSDEC, 1)
         END IF
         FREQ = 0.0D0
         DO 10 I = 1, VNAX
            IF(VCTYPE(I).EQ.'FREQ') THEN
               FREQ = VCRVAL(I)
            END IF
 10      CONTINUE
         IF(FREQ.EQ.0.0D0) THEN
            CALL ERRREPOR (ERRBDARG, ROUTINE, 'No vis frequency axis')
            GO TO 999
         END IF
         NOFREQ=.TRUE.
         DO 20 I = 1, MNAX
            IF(MCTYPE(I).EQ.'FREQ') THEN
               MCRVAL(I) = FREQ
               NOFREQ = .FALSE.
            END IF
 20      CONTINUE
         IF(NOFREQ) THEN
            CALL ERRREPOR (ERRBDARG, ROUTINE, 'No model frequency axis')
            GO TO 999
         END IF
         CALL DATPUTD ('Model', 'CRVAL', MCRVAL, MNAX)
         CALL MSGPUT ('Applying primary beam at vis frequency', 'I')
         CALL IMGPB ('Model', 'Model', 'APPLY')
      END IF
C
C Now calculate residuals
C
      CALL MSGPUT ('Starting subtraction', 'I')
      CALL IMGTOVIS ('Vis', 'MOD/I', 'Model', 'ModVis', 'Dmodel')
      CALL ARRSUBTR ('Vis/OBS/I/VIS', 'Vis/MOD/I/VIS', 
     1   'Vis/OBS/I/VIS')
      CALL DATDELET ('Vis/MOD')
C
C Output result
C
      CALL VISPUT ('Vis', RESFILE, 'OBS', 'I,Q,U,V', '*', ' ')
C
 999  CONTINUE
      END

