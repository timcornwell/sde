C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)flysub.f	1.10   6/2/92
C
      SUBROUTINE SDEMAIN
C
#define maxnpat 121
C
CD Program to subtract visibility data as produced by fly.
C
C Audit trail:
C	Can add as well as subtract
C				T.J. Cornwell	11 April 1991
C	Initialized BITER
C				T.J. Cornwell	6 June 1991
C	Corrects for different frequency model in PB
C				T.J. Cornwell	14 June 1991
C	Can ignore specified patch
C				T.J. Cornwell	12 March 1992
C	Only read and write I
C				T.J. Cornwell	30 April 1992
C	Edit specified patch, control PB correction
C				T.J. Cornwell	18 May 1992
C	Changed to IMGNWIN for blanking
C				T.J. Cornwell 	2 June 1992
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FLYSUB')
C
      INTEGER 		NDUMMY, DIR, NSEL, TNITER, TIMR(8), STRSEARC
      REAL		D2R, TIME(2), UVLIMITS(2), STEP(SYSMXDIM),
     2			BLC (SYSMXDIM), TRC (SYSMXDIM)
      LOGICAL		DATEXIST, DOPB
C
      INTEGER           NAX, NAXIS(SYSMXDIM)
      DOUBLE PRECISION  RVAL(SYSMXDIM), VFREQ
      REAL		RPIX(SYSMXDIM), DELT(SYSMXDIM), ROTA(SYSMXDIM)
      CHARACTER*8       TYPE(SYSMXDIM)
      LOGICAL		ADD
C
C Parameters of patches
C
      INTEGER		IPATCH, DPATCH
      REAL		PMAXRES(maxnpat), TFLUX(maxnpat), TTFLUX
      INTEGER		ANITER(maxnpat)
C
      CHARACTER*(SYSMXNAM)	VISFILE, UTL, NVISFILE, CMP,
     $				MVS, CHPTFILE, STRM2
C
      DATA		TIME	/2*0.0/
      DATA		ANITER	/maxnpat * 0/
      DATA		PMAXRES /maxnpat * 0.0/
      DATA		TFLUX /maxnpat * 0.0/
      DATA		TTFLUX /0.0/
      DATA		STEP	/SYSMXDIM*1/
C==================================================================
      D2R = ATAN(1.0) / 45.0 
C
      CALL MSGWELCO('I perform wide-field visibility subtraction')
      CALL USRCTL
C
C Get information from user
C
      CALL USRGETR ('Uvlimits', UVLIMITS, 2, NDUMMY)
      CALL USRGETC ('Vis', VISFILE, 1, NDUMMY)
      CALL USRGETC ('NewVis', NVISFILE, 1, NDUMMY)
      CALL USRGETC ('Checkpoint', CHPTFILE, 1, NDUMMY)
      CALL USRGETI ('Timerange', TIMR, 8, NDUMMY)
      CALL USRGETL ('PBCorrect', DOPB, 1, NDUMMY)
      CALL USRGETL ('Add', ADD, 1, NDUMMY)
      CALL USRGETI ('DPatch', DPATCH, 1, NDUMMY)
      IF (ERROR) GO TO 999
      CALL USRGETI ('BLC', BLC, SYSMXDIM, NDUMMY)
      CALL ERRCANCE
      CALL USRGETI ('TRC', TRC, SYSMXDIM, NDUMMY)
      CALL ERRCANCE
      CALL USRGETL ('Debug', SYSDEBUG, 1, NDUMMY)
C
      CALL DATCREAT ('Window')
      CALL DATPUTI ('Window', 'BLC', BLC, SYSMXDIM)
      CALL DATPUTI ('Window', 'TRC', TRC, SYSMXDIM)
C
C Now get visibility data and select it. Then clone model visibility.
C
      CALL VISGET ('Vis', VISFILE, 'I', '*', ' ')
      IF(DATEXIST('Vis/TIME')) THEN
         CALL UTLTTOD (TIMR(1), TIME(1))
         CALL UTLTTOD (TIMR(5), TIME(2))
         CALL VISSEL ('Vis','OBS/I', TIME, UVLIMITS, NSEL)
         IF (ERROR) GO TO 999
         WRITE (MESSAGE, 1000) NSEL
 1000    FORMAT ('Selected ',I7,' visibilities')
         CALL MSGPUT (MESSAGE, 'I')
      ELSE 
         CALL ERRREPOR (ERRLOGIC, ROUTINE, 'No vis Time info')
         GO TO 999
      ENDIF
C
C Clone visibilities
C
C
      CALL DATCREAT ('Vis/TMP')
      CALL DATCREAT ('Vis/TMP/I')
      CALL ARRCOPY ('Vis/OBS/I/VIS', 'Vis/TMP/I/VIS')
      CALL DATLNARR ('Vis/OBS/I/WT', 'Vis/TMP/I/WT')
      CALL CRDGET ('Vis/OBS/I', NAX, TYPE, NAXIS, RVAL, RPIX, DELT,
     $   ROTA)
      VFREQ = RVAL(STRSEARC ('FREQ', TYPE, NAX))
      CALL CRDPUT ('Vis/TMP/I', NAX, TYPE, NAXIS, RVAL, RPIX, DELT,
     $   ROTA)
      CALL ARRSETCO ('Vis/TMP/I/VIS', 0.0, 0.0)
C
      IF(DOPB) THEN
         CALL MSGPUT ('Correcting frequency dependent primary beam','I')
      END IF
C
C Read checkpoint file
C
      CALL MSGPUT ('Reading from checkpoint file', 'I')
      CALL DATCREAT ('Checkpoint')
      CALL DATREAD ('Checkpoint', CHPTFILE)
      TNITER = 0
      DO 400 IPATCH = 1, maxnpat
         CALL FLYNAMES (IPATCH, CMP, UTL, MVS)
         IF(.NOT.DATEXIST(CMP)) GO TO 400
         IF(ERROR) GO TO 999
         IF(DATEXIST(STRM2(CMP, 'NITER'))) THEN
            CALL DATGETI (CMP, 'NITER', ANITER(IPATCH), 1, NDUMMY)
            IF(ERROR) GO TO 999
            IF(ANITER(IPATCH).LE.0) THEN
               WRITE (MESSAGE, 1036) IPATCH
 1036          FORMAT ('Field ', I2,': no comps')
               CALL MSGPUT (MESSAGE, 'I')
            ELSE
               CALL DATPUTI(CMP, 'BITER', 1, 1)
               CALL IMGCCIMG (CMP)
               CALL DATGETR (CMP, 'TFLUX', TFLUX(IPATCH), 1, NDUMMY)
               WRITE (MESSAGE, 1034) IPATCH, ANITER(IPATCH),
     $              TFLUX(IPATCH)
 1034          FORMAT ('Field ', I2,': found ',I6,' comps, Flux = ',
     $            1PE12.3)
               CALL MSGPUT (MESSAGE, 'I')
               IF(DPATCH.NE.0) THEN
                  IF(IPATCH.EQ.DPATCH) THEN
                     WRITE(MESSAGE, 1029) IPATCH
 1029                FORMAT ('Editing patch ', I4)
                     CALL MSGPUT (MESSAGE, 'I')
                     CALL ARRSTAT (CMP, ' ')
                     CALL DATGETR (CMP, 'ARRSUM', TFLUX(IPATCH), 1,
     $                  NDUMMY)
                     WRITE (MESSAGE, 1038) TFLUX(IPATCH)
 1038                FORMAT ('Before editing, Flux = ', 1PE12.3)
                     CALL MSGPUT (MESSAGE, 'I')
                     CALL IMGNWIN (CMP, 'Window', 0.0, CMP)
                     CALL ARRSTAT (CMP, ' ')
                     CALL DATGETR (CMP, 'ARRSUM', TFLUX(IPATCH), 1,
     $                  NDUMMY)
                     WRITE (MESSAGE, 1037) TFLUX(IPATCH)
 1037                FORMAT ('After editing,  Flux = ', 1PE12.3)
                     CALL MSGPUT (MESSAGE, 'I')
                  END IF
               ENDIF
               IF(DOPB) THEN
                  CALL DATPUTC (CMP, 'TELESCOP', 'AIRY', 1)
                  CALL DATPUTR (CMP, 'TELDIAM', 25.0, 1)
                  CALL IMGPB (CMP, CMP, 'CORRECT')
                  CALL CRDGET (CMP, NAX, TYPE, NAXIS, RVAL, RPIX, DELT,
     $               ROTA)
                  RVAL(STRSEARC ('FREQ', TYPE, NAX)) = VFREQ
                  CALL CRDPUT (CMP, NAX, TYPE, NAXIS, RVAL, RPIX, DELT,
     $               ROTA)
                  CALL IMGPB (CMP, CMP, 'APPLY')
               END IF
               CALL IMGDOUBL (CMP, UTL)
               CALL IMGGRIDC (UTL, UTL, 'CORRECT')
               NDUMMY = 2
               CALL FFTCONJA (UTL, MVS, DIR, NDUMMY)
               CALL IMGFFT (UTL, MVS)
               CALL VISDEGRI ('Vis', 'TMP/I', MVS)
               CALL DATDELET (UTL)
               CALL DATDELET (MVS)
               IF(ADD) THEN
                  CALL ARRADD ('Vis/OBS/I/VIS', 'Vis/TMP/I/VIS', 
     1               'Vis/OBS/I/VIS')
               ELSE
                  CALL ARRSUBTR ('Vis/OBS/I/VIS', 'Vis/TMP/I/VIS', 
     1               'Vis/OBS/I/VIS')
               END IF
            END IF
         ELSE
            WRITE (MESSAGE, 1036) IPATCH
            CALL MSGPUT (MESSAGE, 'I')
         END IF
 400  CONTINUE
 410  CONTINUE
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
      SUBROUTINE FLYNAMES (I, CMP, UTL, MVS)
C
C	I	INT	input	Patch number
C	CMP	CHAR	output	Name of components image for this patch
C
C Audit trail:
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FLYNAMES')
      INTEGER	I
      CHARACTER*(*)	CMP, UTL, MVS
      CHARACTER*6	STRINT
C
C==================================================================
C
      CMP = 'Checkpoint/Comps'//STRINT(I)
      MVS = 'MVis'//STRINT(I)
      UTL = 'Util'//STRINT(I)
C
      END
