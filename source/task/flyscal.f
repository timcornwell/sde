C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)flyscal.f	1.10    8/14/92
C
      SUBROUTINE SDEMAIN
C
#define maxnpat 225
C
CD Program to selfcal visibility data as produced by fly.
C
C Audit trail:
C	Does not double anymore
C					T.J. Cornwell May 20 1991
C	Can optionally write gains
C					T.J. Cornwell May 19 1992
C	Added separate calibration of amplitude and phase
C					T.J. Cornwell August 11 1992
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FLYSCAL')
C
      INTEGER 		NDUMMY, NSEL, TNITER, TIMR(8) 
      REAL		D2R, TIME(2), UVLIMITS(2), THRES, FMIN, FMAX,
     $                  TPHASE, TAMP
      LOGICAL		DATEXIST
C
      INTEGER           NAX, NAXIS(SYSMXDIM)
      DOUBLE PRECISION  RVAL(SYSMXDIM)
      REAL		RPIX(SYSMXDIM), DELT(SYSMXDIM), ROTA(SYSMXDIM)
      CHARACTER*8       TYPE(SYSMXDIM)
C
C Parameters of patches
C
      INTEGER		IPATCH
      REAL		PMAXRES(maxnpat), TFLUX(maxnpat), TTFLUX
      INTEGER		ANITER(maxnpat)
C
      CHARACTER*(SYSMXNAM)	VISFILE, METHOD, NVISFILE, CMP,
     $				GLFILE, MVS, CHPTFILE, STRM2, MODE
C
      DATA		TIME	/2*0.0/
      DATA		ANITER	/maxnpat * 0/
      DATA		PMAXRES /maxnpat * 0.0/
      DATA		TFLUX /maxnpat * 0.0/
      DATA		TTFLUX /0.0/
C==================================================================
      D2R = ATAN(1.0) / 45.0 
C
      CALL MSGWELCO('I perform wide-field self-calibration')
      CALL USRCTL
C
C Get information from user
C
      CALL USRGETR ('Uvlimits', UVLIMITS, 2, NDUMMY)
      CALL USRGETR ('Flux', FMIN, 1, NDUMMY)
      CALL USRGETC ('Vis', VISFILE, 1, NDUMMY)
      CALL USRGETC ('NewVis', NVISFILE, 1, NDUMMY)
      CALL USRGETC ('Checkpoint', CHPTFILE, 1, NDUMMY)
      CALL USRGETC ('Gains', GLFILE, 1, NDUMMY)
      CALL USRGETI ('Timerange', TIMR, 8, NDUMMY)
      CALL USRGETL ('Debug', SYSDEBUG, 1, NDUMMY)
      CALL USRGETR ('Threshold', THRES, 1, NDUMMY)
      CALL USRGETR ('Tamp', TAMP, 1, NDUMMY)
      CALL USRGETR ('Tphase', TPHASE, 1, NDUMMY)
      IF (TAMP.LE.0.0) TAMP = 10.0
      IF (TPHASE.LE.0.0) TPHASE = 10.0
      CALL USRGETC ('SolType', MODE, 1, NDUMMY)
      CALL USRGETC ('Method', METHOD, 1, NDUMMY)
C
C Now get visibility data 
C
      CALL VISGET ('Vis', VISFILE, 'I', '*', ' ')
C
C Clone model and residual visibilities
C
      CALL DATCREAT ('Vis/MOD')
      CALL DATCREAT ('Vis/MOD/I')
      CALL ARRCOPY ('Vis/OBS/I/VIS', 'Vis/MOD/I/VIS')
      CALL ARRCOPY ('Vis/OBS/I/WT', 'Vis/MOD/I/WT')
      CALL CRDGET ('Vis/OBS/I', NAX, TYPE, NAXIS, RVAL, RPIX, DELT,
     $   ROTA)
      CALL CRDPUT ('Vis/MOD/I', NAX, TYPE, NAXIS, RVAL, RPIX, DELT,
     $   ROTA)
      CALL ARRSETCO ('Vis/MOD/I/VIS', 0.0, 0.0)
C
C Now select data
C
      IF(DATEXIST('Vis/TIME')) THEN
         CALL UTLTTOD (TIMR(1), TIME(1))
         CALL UTLTTOD (TIMR(5), TIME(2))
         CALL VISSEL ('Vis','MOD/I', TIME, UVLIMITS, NSEL)
         IF (ERROR) GO TO 999
         WRITE (MESSAGE, 1000) NSEL
 1000    FORMAT ('Selected ',I7,' visibilities')
         CALL MSGPUT (MESSAGE, 'I')
      ELSE 
         CALL ERRREPOR (ERRLOGIC, ROUTINE, 'No vis Time info')
         GO TO 999
      ENDIF
C
      CALL DATCREAT ('Vis/TMP')
      CALL DATCREAT ('Vis/TMP/I')
      CALL ARRCOPY ('Vis/OBS/I/VIS', 'Vis/TMP/I/VIS')
      CALL DATLNARR ('Vis/OBS/I/WT', 'Vis/TMP/I/WT')
      CALL CRDPUT ('Vis/TMP/I', NAX, TYPE, NAXIS, RVAL, RPIX, DELT,
     $   ROTA)
      CALL ARRSETCO ('Vis/TMP/I/VIS', 0.0, 0.0)
C
C Read checkpoint file
C
      IF(CHPTFILE.EQ.' ') THEN
         CALL MSGPUT ('Using point source model', 'I')
         CALL ARRSETCO ('Vis/MOD/I/VIS', 0.0, FMIN)
      ELSE
         CALL MSGPUT ('Reading from checkpoint file', 'I')
         CALL DATCREAT ('Checkpoint')
         CALL DATREAD ('Checkpoint', CHPTFILE)
         TNITER = 0
         FMAX = 1.0E20
         DO 400 IPATCH = 1, maxnpat
            CALL FLYNAMES (IPATCH, CMP, MVS)
            IF(.NOT.DATEXIST(CMP)) GO TO 410
            IF(ERROR) GO TO 999
            IF(DATEXIST(STRM2(CMP, 'TFLUX'))) THEN
               CALL DATGETI (CMP, 'TFLUX', TFLUX(IPATCH), 1, NDUMMY)
               IF(ERROR) GO TO 999
               IF(TFLUX(IPATCH).EQ.0) THEN
                  WRITE (MESSAGE, 1036) IPATCH
 1036             FORMAT ('Field ', I3,': no comps')
                  CALL MSGPUT (MESSAGE, 'I')
               ELSE
                  CALL DATPUTI (CMP, 'FIELD', IPATCH, NDUMMY)
                  CALL DATPUTI (CMP, 'BITER', 1, NDUMMY)
                  CALL IMGCCIMG (CMP)
                  CALL ARREDIT (CMP, FMIN, FMAX, CMP)
                  WRITE (MESSAGE, 1034) IPATCH
 1034             FORMAT ('Field ', I3)
                  CALL MSGPUT (MESSAGE, 'I')
                  CALL IMGGRIDC (CMP, CMP, 'CORRECT')
                  CALL IMGFFT (CMP, MVS)
                  CALL VISDEGRI ('Vis', 'TMP/I', MVS)
                  CALL DATDELET (MVS)
                  CALL ARRADD ('Vis/MOD/I/VIS', 'Vis/TMP/I/VIS', 
     1               'Vis/MOD/I/VIS')
               END IF
            ELSE
               WRITE (MESSAGE, 1036) IPATCH
               CALL MSGPUT (MESSAGE, 'I')
            END IF
 400     CONTINUE
 410     CONTINUE
      END IF
C
C Now do self-calibration
C
      IF (METHOD.EQ.'Triple') THEN
         CALL MSGPUT ('Using triple-product approach', 'I')
         CALL MSGPUT ('---Be prepared to wait a long time', 'I')
         IF (MODE.EQ.'AMPPHI') THEN
            CALL MSGPUT ('Estimating both amplitude and phase', 'I')
         ELSE
            CALL MSGPUT ('Estimating phase only', 'I')
         END IF
         CALL DATPUTR ('Vis', 'TINT', TPHASE, 1)
         CALL VISTRP ('Vis', 'OBS/I', 'MOD/I', 'OBS/I', MODE)
      ELSE
         CALL MSGPUT ('Using self-calibration approach', 'I')
         IF (MODE.EQ.'AMPPHI') THEN
            IF(TAMP.EQ.TPHASE) THEN
               CALL MSGPUT ('Correcting amplitude and phase together', 
     &            'I')
               CALL DATPUTR ('Vis', 'TINT', TAMP, 1)
               CALL VISSCAL ('Vis', 'OBS/I', 'MOD/I', 'OBS/I', MODE)
            ELSE 
               CALL MSGPUT ('Correcting amplitude and phase separately', 
     &            'I')
               CALL MSGPUT ('First amplitude', 'I')
               CALL DATPUTR ('Vis', 'TINT', TAMP, 1)
               CALL VISSCAL ('Vis', 'OBS/I', 'MOD/I', 'OBS/I', 'AMP')
               CALL DATDELET ('Vis/OBS/I/ANTGAIN')
               CALL DATDELET ('Vis/OBS/I/GAINTIME')
               CALL DATDELET ('Vis/OBS/I/ORES')
               CALL DATDELET ('Vis/OBS/I/NRES')
               CALL MSGPUT ('Now phase', 'I')
               CALL DATPUTR ('Vis', 'TINT', TPHASE, 1)
               CALL VISSCAL ('Vis', 'OBS/I', 'MOD/I', 'OBS/I', ' ')
            ENDIF
         ELSE
            CALL MSGPUT ('Correcting phase only', 'I')
            CALL DATPUTR ('Vis', 'TINT', TPHASE, 1)
            CALL VISSCAL ('Vis', 'OBS/I', 'MOD/I', 'OBS/I', MODE)
         END IF
      END IF
C
C Write out derived gains
C
      IF (GLFILE.NE.' ') THEN
         CALL TXTOPEN ('Gains', GLFILE, 'WRITE')
         CALL CALLIST ('Gains', 'Vis', 'OBS/I')
         CALL TXTCLOSE ('Gains')
      END IF
C
C Do data editting
C
      IF(THRES.GT.0.0) THEN
         CALL MSGPUT ('Flagging on discrepancy from model', 'I')
         CALL VISEDIT ('Vis', 'OBS/I', 'MOD/I', 'OBS/I', THRES, ' ')
      END IF
C
C Write answers
C
      IF (NVISFILE.NE.' ') THEN
         CALL HISOPEN ('Vis')
         CALL HISINPUT ('Vis')
         CALL VISPUT ('Vis', NVISFILE, 'OBS', 'I', '*', ' ')
      END IF
C
 999  CONTINUE
      END
C++
      SUBROUTINE FLYNAMES (I, CMP, MVS)
C
C	I	INT	input	Patch number
C	CMP	CHAR	output	Name of components image for this patch
C
C Audit trail:
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FLYNAMES')
      INTEGER	I
      CHARACTER*(*)	CMP, MVS
      CHARACTER*6	STRINT
C==================================================================
C
      CMP = 'Checkpoint/Comps'//STRINT(I)
      MVS = 'MVis'//STRINT(I)
C
      END
