C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgsvdcl.f	1.3 24 Feb 1995
C
      SUBROUTINE IMGSVDCL (DRT, PSF, XFR, CCLEAN, RES, BOX, MVIS)
C
CD SVD-Clean an image -- version 0
C
C	DRT	CH*(*)	input	Name of Dirty image
C	PSF	CH*(*)	input	Name of Point Spread Function
C	XFR	CH*(*)	input	Name of Transfer Function
C	CCLEAN	CH*(*)	input	Name of CCLEAN image
C	RES	CH*(*)	input	Name of Residual image
C       BOX     CH*(*)  input   Name of Clean Box image
C	MVIS	CH*(*)	input	Name of FT file
C	ALG	CH*(*)	input	Algorithm
C
C In this version of the algorithm, the beam patch is circular, and
C decomposed once at the beginning of the main loop.  The center of
C the patch is constrained to be within BOX, but portions of the
C returned CCs may lay outside of it.
C
C Audit trail:
C	Cloned from imgcclea.f
C				D.S.Briggs	3 Dec 1992
C	Eliminate double declaration of variable, unused vars.
C				M. Stupar	28 Dec 1994
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	DRT, PSF, XFR, CCLEAN, RES, BOX, MVIS
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGSVDCL')
C
      REAL		GAIN, FLUX, MAXRES, TFLUX
      INTEGER		NITER, BITER, RITER, ANITER, NLOC
      CHARACTER*1	DATYPE, PATYPE, CATYPE, RATYPE, BATYPE, MATYPE
      CHARACTER*1	ATYPE
      CHARACTER*(SYSMXNAM)	TEMPFILE
      INTEGER		DNAX, DADD, DNAXIS(SYSMXDIM)
      INTEGER		PNAX, PADD, PNAXIS(SYSMXDIM)
      INTEGER		CNAX, CADD, CNAXIS(SYSMXDIM)
      INTEGER		RNAX, RADD, RNAXIS(SYSMXDIM)
      INTEGER		BNAX, BADD, BNAXIS(SYSMXDIM)
      INTEGER		MNAX, MADD, MNAXIS(SYSMXDIM)
      INTEGER		NAXIS(SYSMXDIM), TADD
      INTEGER		CLADD, CXLADD, CYLADD, CZLADD
      INTEGER		RLADD, RXLADD, RYLADD, RZLADD, BP, SVADD
      INTEGER		IAX, NREAL, NDUMMY, NL, NAX, NCMP
      INTEGER		IPEAK, PMX, PMY, RNX, RNY, CNX, CNY
      INTEGER		NIWIN, NOWIN, DELTA(7), NSV, NEDIT, I
      REAL		RBP, MINSVR, MAXSV
      LOGICAL		DODBLE
C
      CHARACTER*(SYSMXNAM)	STRM2
      LOGICAL		DATEXIST
      REAL		DATFGETR
      INTEGER		DATFGETI, DATADD, PIXISAMA
C=====================================================================
      IF (ERROR) GO TO 999
C
C Get control parameters
C
      DODBLE = .FALSE.
      MINSVR = 1.E-6
C
      IF (DATEXIST (STRM2 (CCLEAN, 'CCNL'))) THEN
         CALL DATGETI (CCLEAN, 'CCNL', NL, 1, NDUMMY)
      ELSE
         NL = 16384
      END IF
      IF (DATEXIST(STRM2(CCLEAN, 'NITER'))) THEN
         CALL DATGETI(CCLEAN, 'NITER', NITER, 1, NDUMMY)
      ELSE
         NITER = 10
         CALL DATPUTI(CCLEAN, 'NITER', NITER, 1)
      END IF
      IF (DATEXIST(STRM2(CCLEAN, 'BITER'))) THEN
         CALL DATGETI(CCLEAN, 'BITER', BITER, 1, NDUMMY)
      ELSE
         BITER = 1
         CALL DATPUTI(CCLEAN, 'BITER', BITER, 1)
      END IF
      IF (DATEXIST(STRM2(CCLEAN, 'GAIN'))) THEN
         CALL DATGETR(CCLEAN, 'GAIN', GAIN, 1, NDUMMY)
      ELSE
         GAIN = 0.5
         CALL DATPUTR(CCLEAN, 'GAIN', GAIN, 1)
      END IF
      IF (DATEXIST(STRM2(CCLEAN, 'FLUX'))) THEN
         CALL DATGETR(CCLEAN, 'FLUX', FLUX, 1, NDUMMY)
      ELSE
         FLUX = 0.0
         CALL DATPUTR(CCLEAN, 'FLUX', FLUX, 1)
      END IF
      IF (DATEXIST(STRM2(CCLEAN, 'BP'))) THEN
         CALL DATGETI(CCLEAN, 'BP', BP, 1, NDUMMY)
      ELSE
         BP = 10
         CALL DATPUTI(CCLEAN, 'BP', BP, 1)
      END IF
      RBP = BP
C
C Make lists
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
      CALL DATGETAR (CCLEAN, CNAX, CNAXIS, CATYPE, CADD)
      IF (ERROR) GO TO 990
      IF (CATYPE.NE.'R') THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1      'Bad array type '//CATYPE//' for CLEAN Image')
         GO TO 990
      END IF
      CNX = CNAXIS(1)
      CNY = CNAXIS(2)
C
      CALL DATGETAR (RES, RNAX, RNAXIS, RATYPE, RADD)
      IF (ERROR) GO TO 990
      IF (RATYPE.NE.'R') THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1      'Bad array type '//RATYPE//' for Residual Image')
         GO TO 990
      END IF
      RNX = RNAXIS(1)
      RNY = RNAXIS(2)
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
      NLOC = 1
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
         IF (DNAXIS(IAX).GT.1) THEN
            NREAL = NREAL + 1
            NLOC = NLOC * DNAXIS(IAX)
         ELSE
            GO TO 11
         END IF
  10  CONTINUE
  11  CONTINUE
C
C Make locations for components list
C
      IF (.NOT.DATEXIST (STRM2(CCLEAN, 'PIXLIST'))) THEN
         NAX = 1
         NAXIS(1) = ABS(NITER)
         ATYPE = 'R'
         CALL DATMAKAR (STRM2(CCLEAN, 'PIXLIST'), NAX, NAXIS, ATYPE,
     1     CLADD)
         ATYPE='I'
         NAX = 2
         ATYPE = 'I'
         NAXIS(2) = NREAL
         CALL DATMAKAR (STRM2(CCLEAN, 'PIXLOC'), NAX, NAXIS, ATYPE,
     1     CXLADD)
         CYLADD = CXLADD + NAXIS(1)
         CZLADD = CXLADD + 2 * NAXIS(1)
      ELSE
         CALL DATGETAR (STRM2(CCLEAN, 'PIXLIST'), NAX, NAXIS, ATYPE,
     1     CLADD)
         CALL DATGETAR (STRM2(CCLEAN, 'PIXLOC'), NAX, NAXIS, ATYPE,
     1     CXLADD)
         CYLADD = CXLADD + NAXIS(1)
         CZLADD = CXLADD + 2 * NAXIS(1)
      END IF
C
C Now make locations for residual list
C
      IF (.NOT.DATEXIST (STRM2(RES, 'PIXLIST'))) THEN
         NAX = 1
         NAXIS(1) = NL
         ATYPE = 'R'
         CALL DATMAKAR (STRM2(RES, 'PIXLIST'), NAX, NAXIS, ATYPE,
     1     RLADD)
         NAX = 2
         ATYPE = 'I'
         NAXIS(2) = NREAL
         CALL DATMAKAR (STRM2(RES, 'PIXLOC'), NAX, NAXIS, ATYPE,
     1     RXLADD)
         RYLADD = RXLADD + NAXIS(1)
         RZLADD = RXLADD + 2 * NAXIS(1)
      ELSE
         CALL DATGETAR (STRM2(RES, 'PIXLIST'), NAX, NAXIS, ATYPE,
     1     RLADD)
         CALL DATGETAR (STRM2(RES, 'PIXLOC'), NAX, NAXIS, ATYPE,
     1     RXLADD)
         RYLADD = RXLADD + NAXIS(1)
         RZLADD = RXLADD + 2 * NAXIS(1)
      END IF
C
C If beam matrix is invariant, make it now
C
      MATYPE = 'R'
      MNAX = 2
      MNAXIS(1) = 2*BP+1
      MNAXIS(2) = 2*BP+1
      CALL DATMAKAR ('PatchWindow', MNAX, MNAXIS, MATYPE, MADD)
      CALL ARRSETCO ('PatchWindow', 0.0, 0.0)
      CALL PIXDRFEL (MEMR(MADD), MNAXIS(1), MNAXIS(2), RBP+1., RBP+1.,
     $   RBP+.1, RBP+.1, 0.0, 1.0)
      CALL ARRSTAT ('PatchWindow',' ')
      NIWIN = NINT(DATFGETR('PatchWindow','ARRSUM'))
      NOWIN = NIWIN
      IF (ERROR) GO TO 990
C
C And decompose it
C
      CALL MSGPUT ('Creating Beam matrix','I')
      CALL ARRMBMAT ('BeamMat', 'PatchWindow', 'PatchWindow',
     $   PSF, .FALSE.)
C
      CALL MSGPUT ('Doing Singular Value Decomposition','I')
      CALL MATSVD ('BeamMat', 'U', 'W', 'V')
      IF (ERROR) GO TO 999
C
C Edit the Singular Values
C
      NSV = NIWIN
      SVADD = DATADD('W')
      NEDIT = 0
      CALL ARRSTAT ('W',' ')
      MAXSV = ABS(DATFGETR('W','ARRMAX'))
      IF (ABS(DATFGETR('W','ARRMIN')).GT.MAXSV)
     $   MAXSV = ABS(DATFGETR('W','ARRMIN'))
      WRITE (MESSAGE,1000) MAXSV
 1000 FORMAT ('Max Singular Value is',1PE14.6)
      CALL MSGPUT(MESSAGE, 'I')
      DO 100 I = 1, NOWIN
         IF (DODBLE) THEN
            IF (ABS(MEMD(SVADD+I-1)/MINSVR).LT.MAXSV) THEN
               MEMD(SVADD+I-1) = 0.0
               NEDIT = NEDIT + 1
            END IF
         ELSE
            IF (ABS(MEMR(SVADD+I-1)/MINSVR).LT.MAXSV) THEN
               MEMR(SVADD+I-1) = 0.0
               NEDIT = NEDIT + 1
            END IF
         END IF
 100  CONTINUE
      WRITE (MESSAGE, 1010) NEDIT
 1010 FORMAT ('Edited',I5,' singular values')
      CALL MSGPUT(MESSAGE, 'I')
      NSV = NSV - NEDIT
      WRITE (MESSAGE, 1020) NSV
 1020 FORMAT ('Using',I5,' singular values')
      CALL MSGPUT(MESSAGE, 'I')
C
C Finally do something
C
      CALL MSGPUT (
     1   '  Niter   Nlocations   Residual(Jy/beam)   Total Flux(Jy)',
     2   'I')
C
C ********************* Start of major cycles ************************
C
      CALL IMGRESID (CCLEAN, DRT, XFR, RES, MVIS)
      RITER = BITER
C
C Apply the Clean Box to the residual. The Clean Box is an image which is
C 1.0 at locations where cleaning can occur and 0.0 at other locations.
C
  1   CONTINUE
C
         IF (SYSDEBUG) THEN
            CALL STRNUMFL ('RES.FULL.#', RITER, TEMPFILE)
            CALL FILIMGPU (RES, TEMPFILE, ' ')
         END IF
C
         CALL ARRCOPY (RES, 'TEMP-RES')
         TADD = DATADD('TEMP-RES')
         CALL PIXRMULT (MEMR(TADD), MEMR(BADD), MEMR(TADD), NLOC)
C
         IF (SYSDEBUG) THEN
            CALL STRNUMFL ('RES.BOX.#', RITER, TEMPFILE)
            CALL FILIMGPU ('TEMP-RES', TEMPFILE, ' ')
         END IF
C
         IF (NREAL.EQ.2) THEN
C
C Find peak of Residuals
C
            IPEAK = PIXISAMA(RNX*RNY, MEMR(TADD), 1) - 1
            PMX = 1 + MOD(IPEAK, RNX)
            PMY = 1 + (IPEAK - PMX + 1)/RNX
            MAXRES = MEMR(RADD+(PMY-1)*RNX+PMX-1)
C
C Go get the residual pixels into a linear array
C
            DELTA(1) = PMX - BP - 1
            DELTA(2) = PMY - BP - 1
            CALL DATPUTI ('PatchWindow', 'DELTA', DELTA, SYSMXDIM)
            CALL ARRSCOPY (RES, 'PatchWindow', 'WRes')
C
c         CALL STRNUMFL ('WRES.#', RITER, TEMPFILE)
c         CALL FILIMGPU ('WRes', TEMPFILE, ' ')
C
C SVD deconvolve the beam patch
C
            CALL MATSVBKS ('U','W','V','WRes','WComp')
C
C Multiply by gain, and add back to component image
C
            CALL ARRSETCO ('WRes', 0.0, 0.0)
            CALL ARRSCALE ('WComp', GAIN, 0.0, 'WComp')
            CALL ARRUSCPY ('WComp', 'PatchWindow', CCLEAN)

            IF (SYSDEBUG) THEN
               CALL STRNUMFL ('CMP.#', RITER, TEMPFILE)
               CALL FILIMGPU (CCLEAN, TEMPFILE, ' ')
            END IF
C
C Enforce support constraints on components
C
c            CALL PIXRMULT (MEMR(CADD), MEMR(BADD), MEMR(CADD), NLOC)
C
         ELSE
            CALL ERRREPOR(ERRBDARG, ROUTINE, 'Illegal dimension')
            GO TO 999
         END IF
         RITER = RITER + 1
C
C Now find the updated residual image using an FFT convolution
C
         CALL IMGRESID (CCLEAN, DRT, XFR, RES, MVIS)
         IF (ERROR) GO TO 999
C
C Get statistics from CMP
C
         CALL ARRSTAT (CCLEAN, ' ')
         NCMP = DATFGETI (CCLEAN, 'ARRNLOC')
         TFLUX = DATFGETR (CCLEAN, 'ARRSUM')
         IPEAK = PIXISAMA(RNX*RNY, MEMR(RADD), 1) - 1
         PMX = 1 + MOD(IPEAK, RNX)
         PMY = 1 + (IPEAK - PMX + 1)/RNX
         MAXRES = MEMR(RADD+(PMY-1)*RNX+PMX-1)
C
C Write summary message
C
         WRITE (MESSAGE, 1100) RITER, NCMP, MAXRES, TFLUX
 1100    FORMAT (2X,I6,6X,I7,6X,1PE12.4,7X,1PE12.4)
         CALL MSGPUT (MESSAGE, 'I')
C
C Go back for more?
C
      IF (SYSINTRP.AND.SYSINTAC.EQ.'QUIT') GO TO 2
c      IF ((MAXRES.LE.0.0).AND.(FLUX.LE.0.0)) GO TO 2
      IF ((ABS(MAXRES).GT.FLUX).AND.(RITER.LT.ABS(NITER))) GO TO 1
 2    CONTINUE
C
C ********************* End of major cycles ************************
C
C Store goodies
C
      CALL DATPUTR (CCLEAN, 'TFLUX', TFLUX, 1)
      CALL DATPUTR (CCLEAN, 'FLUX', MAXRES, 1)
      CALL DATPUTI (CCLEAN, 'NITER', ANITER, 1)
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
