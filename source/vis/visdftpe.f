C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)visdftpe.f	1.1   5/17/91
C
      SUBROUTINE VISDFTPE (VIS, CLASS, IMAGE, TELESCOP)
C
CD Direct Fourier transform to Vis. data to image--pointing errors
C are taken into account via the symmetric voltage patterns
C
C
C	VIS		CH*(*)	input	Name of visibility data
C	CLASS		CH*(*)	input	Class of visibility data
C	IMAGE		CH*(*)	input	Name of Image
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	May 10 1991
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	IMAGE, VIS, CLASS
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISDFTPE')
C
      CHARACTER*1	VATYPE
      CHARACTER*8	VTYPE(SYSMXDIM)
      REAL		VRPIX(SYSMXDIM), VDELT(SYSMXDIM),
     1			VROTA(SYSMXDIM)
      DOUBLE PRECISION	VRVAL(SYSMXDIM)
      INTEGER		INAX, INAXIS(SYSMXDIM), ONAX, ONAXIS(SYSMXDIM)
      CHARACTER*1	OTYPE
      CHARACTER*8	ITYPE(SYSMXDIM)
      REAL		IRPIX(SYSMXDIM), IDELT(SYSMXDIM),
     1			IROTA(SYSMXDIM)
      DOUBLE PRECISION	IRVAL(SYSMXDIM), FREQ
      INTEGER		VNAX, VNAXIS(SYSMXDIM)
      INTEGER		NVP
      INTEGER 		NDUMMY, CRDRNAX, I, STRSEARC, RNAX
      INTEGER		VSADD, IADD, WTADD, UADD, VADD, DATADD,
     1			VPADD, TADD, BADD, ROADD, DOADD
      CHARACTER*(SYSMXNAM)	SVIS, STRM2, TELESCOP
C
      REAL		RADMAX, RTOA, RCONST, BMX, BMY, TELDIAM
C
      REAL	PI
      PARAMETER	(PI=3.14159274101257)
C
      DOUBLE PRECISION	OBSRA, OBSDEC
C==================================================================
      IF (ERROR) GO TO 999
C
      CALL CRDGET (IMAGE, INAX, ITYPE, INAXIS, IRVAL, IRPIX, IDELT, 
     1   IROTA)
C
      RTOA = 180.0 * 3600.0 / PI
C
C      IF (DATEXIST (STRM2(VIS, 'TELESCOP'))) THEN
C         CALL DATGETC (VIS, 'TELESCOP', TELESCOP, 1, NDUMMY)
C      ELSE
C         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Telescope not known')
C         GO TO 999
C      END IF
C
      RNAX = CRDRNAX(INAX, INAXIS)
C
      CALL DATGETD (IMAGE, 'CRVAL', IRVAL, SYSMXDIM, NDUMMY)
      CALL DATGETC (IMAGE, 'CTYPE', ITYPE, SYSMXDIM, NDUMMY)
      IF (ERROR) GO TO 990
      I = STRSEARC ('FREQ', ITYPE, NDUMMY)
      IF (I.EQ.0) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'No frequency axis')
         GO TO 999
      ELSE
         FREQ = IRVAL(I) / 1E9
      END IF
C
      IF (ERROR) GO TO 990
      CALL DATGETD (IMAGE, 'OBSRA', OBSRA, 1, NDUMMY)
      IF (ERROR) THEN
         CALL ERRCANCE
         CALL DATPUTD (IMAGE, 'OBSRA', IRVAL(1), 1)
         OBSRA = IRVAL(1)
      END IF
      CALL DATGETD (IMAGE, 'OBSDEC', OBSDEC, 1, NDUMMY)
      IF (ERROR) THEN
         CALL ERRCANCE
         CALL DATPUTD (IMAGE, 'OBSDEC', IRVAL(2), 1)
         OBSDEC = IRVAL(2)
      END IF
C
C Now get UV data
C
      SVIS = STRM2 (VIS, CLASS)
      CALL CRDGET (SVIS, VNAX, VTYPE, VNAXIS, VRVAL, VRPIX, VDELT, 
     1   VROTA)
C
      CALL DATGETAR (STRM2(SVIS, 'VIS'), VNAX, VNAXIS, VATYPE,
     1   VSADD)
      WTADD =  DATADD (STRM2(SVIS, 'WT'))
      IADD = DATADD (IMAGE)
C
      IF (INAXIS(1).EQ.1) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Can only DFTPE 2-D')
         GO TO 999
      ELSEIF (INAXIS(2).EQ.1) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Can only DFTPE 2-D')
         GO TO 999
      ELSEIF (INAXIS(3).EQ.1) THEN
C
C ***********************  Two dimensions *************************
C
         UADD = DATADD (STRM2(VIS, 'UU'))
         VADD = DATADD (STRM2(VIS, 'VV'))
         BADD = DATADD (STRM2(VIS, 'BASELINE'))
         TADD = DATADD (STRM2(VIS, 'TIME'))
         CALL DATGETAR (STRM2(VIS, 'RAOFF'), ONAX, ONAXIS, OTYPE, 
     $      ROADD)
         CALL DATGETAR (STRM2(VIS, 'DECOFF'), ONAX, ONAXIS, OTYPE,
     $      DOADD)
         WRITE(MESSAGE, '('' VISDFTPE found '',I5,
     $      '' integration intervals '')') ONAXIS(2)
         CALL MSGPUT (MESSAGE, 'I')
         WRITE(MESSAGE, '('' VISDFTPE found '',I5,
     $      '' ANTENNAS '')') ONAXIS(1)
         CALL MSGPUT (MESSAGE, 'I')
         IF (ERROR) GOTO 990
         IF (TELESCOP(1:3).EQ.'VLA') THEN
C            RCONST = 60*44.255/FREQ
C            BMX = (44.255*3600.0*IDELT(1)/RCONST)**2
C            BMY = (44.255*3600.0*IDELT(2)/RCONST)**2
             CALL ERRREPOR(ERRBDARG, ROUTINE,
     $         'Voltage Pattern Code Nonexistent '//TELESCOP)
         ELSEIF (TELESCOP(1:2).EQ.'SI') THEN
            CALL ARRAIRYV (NVP, RADMAX, VPADD)
            RCONST = RTOA * (3E8/(FREQ*1E9))/(PI * 0.20)
            BMX = (3600.0*IDELT(1)/RCONST)**2
            BMY = (3600.0*IDELT(2)/RCONST)**2
         ELSEIF (TELESCOP(1:3).EQ.'MMA') THEN
            CALL ARRAIRBV (NVP, RADMAX, VPADD)
            RCONST = RTOA * (3E8/(FREQ*1E9))/(PI * 7.5)
            BMX = (3600.0*IDELT(1)/RCONST)**2
            BMY = (3600.0*IDELT(2)/RCONST)**2
         ELSEIF (TELESCOP(1:4).EQ.'AIRY') THEN
            CALL ARRAIRV (NVP, RADMAX, VPADD)
            CALL DATGETR(VIS, 'TELDIAM', TELDIAM, 1, NDUMMY)
            RCONST = RTOA * (3E8/(FREQ*1E9))/(PI * TELDIAM)
            BMX = (3600.0*IDELT(1)/RCONST)**2
            BMY = (3600.0*IDELT(2)/RCONST)**2
         ELSEIF (TELESCOP(1:5).EQ.'AIRYB') THEN
            CALL ARRAIRBV (NVP, RADMAX, VPADD)
            CALL DATGETR(VIS, 'TELDIAM', TELDIAM, 1, NDUMMY)
            RCONST = RTOA * (3E8/(FREQ*1E9))/(PI * TELDIAM)
            BMX = (3600.0*IDELT(1)/RCONST)**2
            BMY = (3600.0*IDELT(2)/RCONST)**2
         ELSEIF (TELESCOP(1:4).EQ.'DISH') THEN
            CALL ARRAIRBV (NVP, RADMAX, VPADD)
            RCONST = RTOA * (3E8/(FREQ*1E9))/(PI * 15.)
            BMX = (3600.0*IDELT(1)/RCONST)**2
            BMY = (3600.0*IDELT(2)/RCONST)**2
         ELSEIF (TELESCOP(1:3).EQ.'12M') THEN
            CALL ARRAIRBV (NVP, RADMAX, VPADD)
            RCONST = RTOA * (3E8/(FREQ*1E9))/(PI * 12.)
            BMX = (3600.0*IDELT(1)/RCONST)**2
            BMY = (3600.0*IDELT(2)/RCONST)**2
         ELSEIF (TELESCOP(1:4).EQ.'BIMA') THEN
            CALL ARRGVP (NVP, RADMAX, VPADD)
C
C The first term is an empirical term to get FWHM = 150 arcsec at 86GHz
C
            RCONST = 2.438 * RTOA * (3E8/(FREQ*1E9))/(PI * 6.1)
            BMX = (3600.0*IDELT(1)/RCONST)**2
            BMY = (3600.0*IDELT(2)/RCONST)**2
         ELSE
            CALL ERRREPOR (ERRBDARG, ROUTINE,
     1         'Cannot treat telescope '//TELESCOP)
            GO TO 999
         END IF
C 
C this stuff provides a histogram of the pointing errors in RA and DEC
C and is a pain to get all of this output in normal situations.
C
C         AMIN = 0.
C         AMAX = 0.
C         I1 = INDEX(VIS, '/PC')+1
C         J1 = STRLEN(VIS)
C         PC = VIS(I1:J1)
C         CALL PIXPGHIS (ONAXIS(1)*ONAXIS(2), MEMR(ROADD), AMIN, AMAX, 
C     $      10, 'RAHIST'//PC//'.PLOT/QMS',
C     $      'RA POINTING OFFSET', 'HISTOGRAM OF RA POINTING OFFSET')
C         AMIN = 0.
C         AMAX = 0.
C         CALL PIXPGHIS (ONAXIS(1)*ONAXIS(2), MEMR(DOADD), AMIN, AMAX,
C     $      10, 'DECHIST'//PC//'.PLOT/QMS',
C     $      'DEC POINTING OFFSET', 'HISTOGRAM OF DEC POINTING OFFSET')
C
         CALL VISDFTE2 (MEMX(VSADD), VNAXIS(1), MEMR(WTADD), 
     1      MEMR(UADD), MEMR(VADD), MEMR(TADD), MEMR(BADD),
     $      MEMR(ROADD), MEMR(DOADD), ONAXIS(1), ONAXIS(2), MEMR(IADD),
     2      INAXIS(1), INAXIS(2), IRPIX(1), IDELT(1), IRPIX(2), 
     3      IDELT(2), OBSRA, OBSDEC, BMX, BMY, NVP, MEMR(VPADD),
     4      RADMAX, IMAGE )
C
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Can only DFTPE 2-D')
         GO TO 999
      END IF
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
