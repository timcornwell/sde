C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgsvdc1.f	1.3 24 Feb 1995
C
      SUBROUTINE IMGSVDC1 (DRT, PSF, XFR, CCLEAN, RES, BOX, MVIS)
C
CD SVD-Clean an image -- version 1   (SLOW)
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
C In this version of the algorithm, the beam patch is circular, but
C decomposed when necessary within the main loop.  Both the center
C of the patch, and all pixels within it are constrained to lie within
C BOX.  Any pixels that happen to lie outside BOX are simply not
C solved for.
C
C The algorithm is reasonably smart about not solving beams that it's
C already seen, but it as it stashes everything, it might conceivably
C run out of memory in large cases.
C
C Audit trail:
C	Cloned from imgsvdcl
C				D.S.Briggs	6 Dec 1992
C	Eliminate double declaration of variable.
C				M. Stupar	28 Dec 1994
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	DRT, PSF, XFR, CCLEAN, RES, BOX, MVIS
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGSVDC1')
C
      REAL		GAIN, FLUX, MAXRES, TFLUX
      INTEGER		NITER, BITER, RITER, ANITER, NLOC
      CHARACTER*1	DATYPE, PATYPE, CATYPE, RATYPE, BATYPE, MATYPE
      CHARACTER		ATYPE*1, BEAMDIR*15
      CHARACTER*(SYSMXNAM)	TEMPFILE
      INTEGER		DNAX, DADD, DNAXIS(SYSMXDIM)
      INTEGER		PNAX, PADD, PNAXIS(SYSMXDIM)
      INTEGER		CNAX, CADD, CNAXIS(SYSMXDIM)
      INTEGER		RNAX, RADD, RNAXIS(SYSMXDIM)
      INTEGER		BNAX, BADD, BNAXIS(SYSMXDIM)
      INTEGER		MNAX, MADD, MNAXIS(SYSMXDIM)
      INTEGER		NAXIS(SYSMXDIM)
      INTEGER		HADD, HASHVAL, OLDHASH
      INTEGER		CLADD, CXLADD, CYLADD, CZLADD
      INTEGER		RLADD, RXLADD, RYLADD, RZLADD, BP
      INTEGER		IAX, NREAL, NDUMMY, NL, NAX, NCMP
      INTEGER		IPEAK, PMX, PMY, RNX, RNY, CNX, CNY
      INTEGER		NIWIN, NOWIN, DELTA(7), NSV
      INTEGER		BLC(SYSMXDIM), TRC(SYSMXDIM)
      REAL		RBP, MINSVR
      LOGICAL		DODBLE, DODCMP
C
      CHARACTER*(SYSMXNAM)	STRM2, STRINT
      LOGICAL		DATEXIST, ARREQ
      REAL		DATFGETR
      INTEGER		DATFGETI, PIXISAMA, ARRBHASH
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
C The patch is invariant
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
      WRITE (MESSAGE, 1005) NIWIN
 1005 FORMAT (I5,' pixels in basic beam patch') 
      CALL MSGPUT (MESSAGE,'I')
      IF (ERROR) GO TO 990
C
C Make the array of hash values, same size as residual image
C
      CALL DATMAKAR ('HashVal', RNAX, RNAXIS, 'I', HADD)
      CALL ARRSETCO ('HashVal', 0.0, -1.0)
C
C Make some temporary arrays and directories
C
      CALL IMGCLONE ('PatchWindow', 'BoxedPatch')
      CALL DATCREAT ('Window')
C
C Finally do something
C
C ********************* Start of major cycles ************************
C
      CALL IMGRESID (CCLEAN, DRT, XFR, RES, MVIS)
      RITER = BITER
  1   CONTINUE
C
         IF (SYSDEBUG) THEN
            CALL STRNUMFL ('RES.FULL.#', RITER, TEMPFILE)
            CALL FILIMGPU (RES, TEMPFILE, ' ')
         END IF
C
C Apply the Clean Box to the residual.
C
         CALL PIXRMULT (MEMR(RADD), MEMR(BADD), MEMR(RADD), NLOC)
C
         IF (SYSDEBUG) THEN
            CALL STRNUMFL ('RES.BOX.#', RITER, TEMPFILE)
            CALL FILIMGPU (RES, TEMPFILE, ' ')
         END IF
C
         IF (NREAL.EQ.2) THEN
C
C Find peak of Residuals
C
            IPEAK = PIXISAMA(RNX*RNY, MEMR(RADD), 1) - 1
            PMX = 1 + MOD(IPEAK, RNX)
            PMY = 1 + (IPEAK - PMX + 1)/RNX
            MAXRES = MEMR(RADD+(PMY-1)*RNX+PMX-1)
C
C Find the intersection of the shifted patch and box
C
            BLC(1) = PMX - BP
            BLC(2) = PMY - BP
            BLC(3) = 1
            TRC(1) = PMX + BP
            TRC(2) = PMY + BP
            TRC(3) = 1
            CALL DATPUTI ('Window', 'BLC', BLC, SYSMXDIM)
            CALL DATPUTI ('Window', 'TRC', TRC, SYSMXDIM)
            CALL ARRSUBSE (BOX, 'BoxedPatch', 'Window')
            CALL ARRMULT ('PatchWindow', 'BoxedPatch', 'BoxedPatch')
            CALL ARRSTAT ('BoxedPatch', ' ')
            NIWIN = DATFGETI ('BoxedPatch', 'ARRNLOC')
            WRITE (MESSAGE, 1007) NIWIN
 1007       FORMAT (I5,' pixels in shifted beam patch') 
            CALL MSGPUT (MESSAGE,'I')
C
            IF (SYSDEBUG) THEN
               CALL STRNUMFL ('BOX.#', RITER, TEMPFILE)
               CALL FILIMGPU ('BoxedPatch', TEMPFILE, ' ')
            END IF
            IF (ERROR) GO TO 999
C
C Have we visited this location before?
C
            OLDHASH = MEMI(HADD+(PMY-1)*RNX+PMX-1)
            HASHVAL = ARRBHASH ('BoxedPatch')
            IF ((OLDHASH.GE.0).AND.(HASHVAL.EQ.OLDHASH)) THEN
               BEAMDIR = 'B' // STRINT(HASHVAL)
               DODCMP = .FALSE.
               WRITE (MESSAGE, 1006) PMX, PMY, HASHVAL
 1006          FORMAT ('Revisiting pixel',I4,',',I4,'  Hashval =',I7)
               CALL MSGPUT (MESSAGE,'I')
            ELSE
               BEAMDIR = 'B' // STRINT(HASHVAL)
               IF (DATEXIST(BEAMDIR)) THEN
                  IF (ARREQ(STRM2(BEAMDIR,'BoxedPatch'),
     $                                    'BoxedPatch')) THEN
                     WRITE (MESSAGE, 1008) HASHVAL, PMX, PMY
 1008                FORMAT ('Reusing stored beam SVD',I7,
     $                  ' for pixel center ',I4,',',I4)
                     CALL MSGPUT (MESSAGE, 'I')
                     DODCMP = .FALSE.
                  ELSE
                     WRITE (MESSAGE, 1009) HASHVAL, PMX, PMY
 1009                FORMAT ('Hash value collision for value',I7,
     $                  ' at pixel center ',I4,',',I4)
                     CALL MSGPUT (MESSAGE, 'I')
                     CALL DATDELET (STRM2(BEAMDIR,'U'))
                     CALL DATDELET (STRM2(BEAMDIR,'W'))
                     CALL DATDELET (STRM2(BEAMDIR,'V'))
                     CALL DATDELET (STRM2(BEAMDIR,'BoxedPatch'))
                     DODCMP = .TRUE.
                  END IF
               ELSE
                  WRITE (MESSAGE, 1010) PMX, PMY, HASHVAL
 1010             FORMAT ('Computing new SVD for beam at ',I4,
     $               ',',I4,'  Hashval =',I7)
                  CALL MSGPUT (MESSAGE, 'I')
                  DODCMP = .TRUE.
               END IF
               MEMI(HADD+(PMY-1)*RNX+PMX-1) = HASHVAL
            END IF
C
C And decompose it
C
            IF (DODCMP) THEN
               CALL MSGPUT ('Creating Beam matrix','I')
               CALL ARRMBMAT ('BeamMat', 'BoxedPatch', 'BoxedPatch',
     $            PSF, .FALSE.)
C
               CALL MSGPUT ('Doing Singular Value Decomposition','I')
               CALL MATSVD ('BeamMat', STRM2(BEAMDIR,'U'),
     $            STRM2(BEAMDIR,'W'), STRM2(BEAMDIR,'V'))
               CALL ARRCOPY ('BoxedPatch', STRM2(BEAMDIR,'BoxedPatch'))
               CALL DATDELET ('BeamMat')
               IF (ERROR) GO TO 999
C
C Edit the Singular Values
C
               CALL ARRSVEDT (STRM2(BEAMDIR,'W'), MINSVR,
     $                        STRM2(BEAMDIR,'W'), NSV)
            END IF
C
C Go get the residual pixels into a linear array
C
            DELTA(1) = PMX - BP - 1
            DELTA(2) = PMY - BP - 1
            CALL DATPUTI ('BoxedPatch', 'DELTA', DELTA, SYSMXDIM)
            CALL ARRSCOPY (RES, 'BoxedPatch', 'WRes')
C
C
C SVD deconvolve the boxed beam patch
C
            CALL MATSVBKS (STRM2(BEAMDIR,'U'), STRM2(BEAMDIR,'W'),
     $         STRM2(BEAMDIR,'V'), 'WRes', 'WComp')
C
C Multiply by gain, and add back to component image
C
            CALL ARRSETCO ('WRes', 0.0, 0.0)
            CALL ARRSCALE ('WComp', GAIN, 0.0, 'WComp')
            CALL ARRUSCPY ('WComp', 'BoxedPatch', CCLEAN)
C
            IF (SYSDEBUG) THEN
               CALL STRNUMFL ('CMP.#', RITER, TEMPFILE)
               CALL FILIMGPU (CCLEAN, TEMPFILE, ' ')
            END IF
C
         ELSE
            CALL ERRREPOR(ERRBDARG, ROUTINE, 'Illegal dimension')
            GO TO 999
         END IF
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
         CALL MSGPUT (
     1      '  Niter   Nlocations   Residual(Jy/beam)   Total Flux(Jy)',
     2      'I')
C
         WRITE (MESSAGE, 1100) RITER, NCMP, MAXRES, TFLUX
 1100    FORMAT (2X,I6,6X,I7,6X,1PE12.4,7X,1PE12.4)
         CALL MSGPUT (MESSAGE, 'I')
         RITER = RITER + 1
C
C delete arrays whose size may change
C
         CALL DATDELET ('WComp')
         CALL DATDELET ('WRes')
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
