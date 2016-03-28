C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)svdconv.f	1.4    12/1/94
C
      SUBROUTINE SDEMAIN
C
CD Program to perform direct deconvolution via SVD decomposition
C
C TRC & BLC only affects the image size on output.  The actual convolutions
C to create the image and residuals are done at full PSF size internally
C
C The NNSVD option just isn't quite working correctly.  The code seems fine,
C but the actual Lawson & Hanson subroutine LDP seems to be producing an
C incorrect answer in some cases.  
C
C Audit trail:
C	Original version
C				D.S.Briggs	Nov 9, 1992
C	NNLS option added
C				D.S.Briggs	Feb 20, 1993
C	NNSVD option added
C				D.S.Briggs	2 March, 1993
C	TempFormat added
C				D.S.Briggs	23 March 1993
C	User selectable beam fitting algorithm.  Trim to half plane
C	option on output.
C				D.S.Briggs	June 13 1994
C	Add inputs to history.  Rename inputs a smidge
C				D.S.Briggs	July 13 1994
C	Added MaxIter for NNLS algorithm.  May also be specified by
C	NIend.  MaxIter is scaled by the size of the Flux Window, NIend
C	is simply the number of iterations.
C				D.S.Briggs	Nov 29 1994
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'SVDCONV')
C
      INTEGER		NITER, NDUMMY,
     $     		BLC(SYSMXDIM), TRC(SYSMXDIM)
      REAL		RNITER, BEAM(4), MINSV, MAXITER
      CHARACTER*(SYSMXNAM)	IMGFILE, RESFILE, DRTFILE, PSFFILE,
     $			CMPFILE, IWINFILE, OWINFILE, TMPFILE, ALG,
     $   		TMPFMT, FITALG
      LOGICAL		DOWRSVD, DORESVD, DODBLE, DOHALF
C
      INTEGER		I, NAX, NAXIS(SYSMXDIM), NIWIN, NOWIN, NPIX,
     $   		PADD, DADD, WDADD, SVADD, TADD,
     $   		ISVADD, NIBEGIN, NIEND, NEDIT, NSV, ITMAX
      REAL		NIINC, MAXSV
      CHARACTER		ATYPE*1, CTYPE*1
C
      INTEGER		DATADD, CRDRNAX
      REAL		DATFGETR
      LOGICAL		DATEXIST
C==================================================================
      CALL MSGWELCO ('I perform SVD deconvolution')
      CALL USRCTL
C
C Get input parameters
C
      CALL USRGETC ('Dirty', DRTFILE, 1, NDUMMY)
      CALL USRGETC ('PSF', PSFFILE, 1, NDUMMY)
      CALL USRGETC ('Residual', RESFILE, 1, NDUMMY)
      CALL USRGETC ('Components', CMPFILE, 1, NDUMMY)
      CALL USRGETC ('DataWindow', IWINFILE, 1, NDUMMY)
      CALL USRGETC ('FluxWindow', OWINFILE, 1, NDUMMY)
      CALL USRGETC ('Image', IMGFILE, 1, NDUMMY)
      CALL USRGETI ('BLC', BLC, SYSMXDIM, NDUMMY)
      CALL ERRCANCE
      CALL USRGETI ('TRC', TRC, SYSMXDIM, NDUMMY)
      CALL ERRCANCE
      CALL USRGETL ('Half', DOHALF, 1, NDUMMY)
      CALL USRGETC ('Algorithm', STRBUF, 1, NDUMMY)
      CALL STRUC (STRBUF, ALG)
      CALL USRGETL ('Double', DODBLE, 1, NDUMMY)
      CALL USRGETI ('NIbegin', NIBEGIN, 1, NDUMMY)
      CALL USRGETI ('NIend', NIEND, 1, NDUMMY)
      CALL USRGETR ('NIinc', NIINC, 1, NDUMMY)
      CALL USRGETR ('Beam', BEAM, 4, NDUMMY)
      CALL USRGETC ('FitAlg', FITALG, 1, NDUMMY)
      CALL USRGETR ('MinSV', MINSV, 1, NDUMMY)
      CALL USRGETR ('MaxIter', MAXITER, 1, NDUMMY)
      CALL USRGETL ('WriteSVD', DOWRSVD, 1, NDUMMY)
      CALL USRGETL ('ReadSVD', DORESVD, 1, NDUMMY)
      CALL USRGETC ('Format', TMPFMT, 1, NDUMMY)
      CALL USRGETL ('DEBUG', SYSDEBUG, 1, NDUMMY)
      IF (ERROR) GO TO 999
C
C Algorithm type?
C
      IF (ALG.EQ.' ') ALG = 'SVD'
      IF (ALG.EQ.'SVD') THEN
         CALL MSGPUT ('SVD algorithm', 'I')
      ELSE IF (ALG.EQ.'NNLS') THEN
         CALL MSGPUT ('NNLS algorithm','I')
      ELSE IF (ALG.EQ.'NNSVD') THEN
         CALL MSGPUT ('NNSVD algorithm','I')
      ELSE
         CALL ERRREPOR (ERRBADID, ROUTINE,
     $        'Algorithm type not recognized')
         GO TO 999
      END IF
C
      IF (DODBLE) THEN
         CTYPE = 'D'
         CALL MSGPUT (
     $      'Will do beam decomposition in DOUBLE precision', 'I')
      ELSE
         CTYPE = 'R'
         CALL MSGPUT (
     $      'Will do beam decomposition in SINGLE precision', 'I')
      END IF
C
C Get images
C
      CALL FILIMGGE ('Dirty', DRTFILE, ' ')
      CALL FILIMGGE ('PSF', PSFFILE, ' ')
      CALL FILMASGE ('DataWindow', IWINFILE, 'Dirty')
      CALL FILMASGE ('FluxWindow', OWINFILE, 'Dirty')
      CALL FILBEMGE ('PSF', BEAM, FITALG, 'BEAM')
      IF (ERROR) GO TO 999
C
      CALL DATGETAR ('PSF', NAX, NAXIS, ATYPE, PADD)
      NPIX = NAXIS(1)
      IF ((ATYPE.NE.'R').AND.(ATYPE.NE.CTYPE)) THEN
         CALL ERRREPOR (ERRFATAL, ROUTINE, 'PSF is bad type')
         GO TO 999
      END IF
      IF (CRDRNAX(NAX,NAXIS).GT.2) THEN
         CALL ERRREPOR (ERRFATAL, ROUTINE, 'Only 1 or 2-D images!')
         GO TO 999
      END IF
C
      IF ((TRC(1).EQ.1).AND.(TRC(2).EQ.1)) THEN
         TRC(1) = NAXIS(1)
         TRC(2) = NAXIS(2)
      END IF
      IF (DOHALF) CALL CRDHALF ('Dirty', BLC, TRC)
      CALL DATCREAT ('Window')
      CALL DATPUTI ('Window', 'TRC', TRC, SYSMXDIM)
      CALL DATPUTI ('Window', 'BLC', BLC, SYSMXDIM)
C
      CALL DATGETAR ('Dirty', NAX, NAXIS, ATYPE, DADD)
      IF ((ATYPE.NE.'R').AND.(ATYPE.NE.CTYPE)) THEN
         CALL ERRREPOR (ERRFATAL, ROUTINE, 'Dirty image is bad type')
         GO TO 999
      END IF
      IF (NAXIS(1).NE.NPIX) THEN
         CALL ERRREPOR (ERRFATAL, ROUTINE,
     $                  'Dirty image and PSF are different sizes')
         GO TO 999
      END IF
C
      IF (DODBLE) THEN
         CALL ARRCVTD ('Dirty','Dirty')
         DADD = DATADD ('Dirty')
         CALL ARRCVTD ('PSF','PSF')
         PADD = DATADD ('PSF')
      END IF
C
      CALL ARRDIV ('DataWindow','DataWindow','DataWindow')
      CALL ARRSTAT ('DataWindow',' ')
      NIWIN = NINT(DATFGETR('DataWindow','ARRSUM'))
      WRITE (MESSAGE, 1001) NIWIN
 1001 FORMAT ('Data Window has',I5,' pixels')
      CALL MSGPUT (MESSAGE, 'I')
      IF (ERROR) GO TO 999
C
      CALL ARRDIV ('FluxWindow','FluxWindow','FluxWindow')
      CALL ARRSTAT ('FluxWindow',' ')
      NOWIN = NINT(DATFGETR('FluxWindow','ARRSUM'))
      WRITE (MESSAGE, 1002) NOWIN
 1002 FORMAT ('Flux Window has',I5,' pixels')
      CALL MSGPUT (MESSAGE, 'I')
      IF (ERROR) GO TO 999
C
C Deal with MaxIter
C
      IF (ALG.EQ.'NNLS') THEN
         IF ((MAXITER.LE.0.0).AND.(NIEND.LE.1)) MAXITER = 3.0
         IF (MAXITER.GT.0.0) THEN
            WRITE (MESSAGE, 1003) MAXITER
 1003       FORMAT ('Allowing',F8.3,' * Nflux iterations of main loop')
            ITMAX = MAXITER * NOWIN
         ELSE
            WRITE (MESSAGE, 1004) NIEND
 1004       FORMAT ('Allowing ',I8,' iterations of main loop')
            ITMAX = NIEND
         END IF
         CALL MSGPUT (MESSAGE,'I')
      END IF
C
C Create a windowed dirty map
C
      CALL DATMAKAR ('WDirty', 1, NIWIN, CTYPE, WDADD)
      CALL ARRSETCO ('WDirty', 0.0, 0.0)
      CALL ARRSCOPY ('Dirty', 'DataWindow', 'WDirty')
      IF (ERROR) GO TO 999
C
C SV decompose the beam, or read in old SV decomposition.  (Note that most
C things beyond simple sv editing will change the beam matrix, and it should
C be recomputed.  Use the read-in feature with caution.  In particular,
C double precision beams must be stored as SDE files, or things will break
C in a non-obvious manner.
C
      IF ((ALG.EQ.'SVD').OR.(ALG.EQ.'NNSVD')) THEN
         IF (DORESVD) THEN
            CALL MSGPUT ('Reading in old Singular Value Decomposition',
     $           'I')
            CALL GETARRAY ('BeamMat', TMPFMT)
            CALL GETARRAY ('U', TMPFMT)
            CALL GETARRAY ('W', TMPFMT)
            CALL GETARRAY ('V', TMPFMT)
         ELSE
            CALL MSGPUT ('Creating Beam matrix','I')
            CALL ARRMBMAT ('BeamMat', 'DataWindow', 'FluxWindow',
     $         'PSF', DODBLE)
C     
            CALL MSGPUT ('Doing Singular Value Decomposition','I')
            CALL MATSVD ('BeamMat', 'U', 'W', 'V')
         END IF
         IF (ERROR) GO TO 999
C
C Write out the SVD arrays, if requested
C
         IF (DOWRSVD) THEN
            CALL PUTARRAY ('U', TMPFMT)
            CALL PUTARRAY ('W', TMPFMT)
            CALL PUTARRAY ('V', TMPFMT)
C     
            CALL MATTRANS ('U','Ut')
            CALL MATTRANS ('V','Vt')
            CALL MATMULT ('Ut','U', 'UI')
            CALL MATMULT ('Vt','V', 'VI')
C     
            CALL PUTARRAY ('UI', TMPFMT)
            CALL PUTARRAY ('VI', TMPFMT)
C     
            CALL PUTARRAY ('BeamMat', TMPFMT)
C     
            CALL MATDIAG ('W','DW')
            CALL MATMULT ('U','DW', 'T')
            CALL MATMULT ('T','Vt','SVD')
            CALL DATDELET ('T')
            CALL ARRLC ('BeamMat', 1.0, 'SVD', -1.0, 'SVDres')
C     
            CALL PUTARRAY ('DW', TMPFMT)
            CALL PUTARRAY ('SVDres', TMPFMT)
         END IF
         IF (ERROR) GO TO 999
C
C Edit Singular Values
C
         NSV = NOWIN
         SVADD = DATADD('W')
         IF (MINSV.GT.0.0) THEN
            NEDIT = 0
            CALL ARRSTAT ('W',' ')
            MAXSV = ABS(DATFGETR('W','ARRMAX'))
            IF (ABS(DATFGETR('W','ARRMIN')).GT.MAXSV)
     $           MAXSV = ABS(DATFGETR('W','ARRMIN'))
            WRITE (MESSAGE,1000) MAXSV
 1000       FORMAT ('Max Singular Value is',1PE14.6)
            CALL MSGPUT(MESSAGE, 'I')
            DO 100 I = 1, NOWIN
               IF (DODBLE) THEN
                  IF (ABS(MEMD(SVADD+I-1)/MINSV).LT.MAXSV) THEN
                     MEMD(SVADD+I-1) = 0.0
                     NEDIT = NEDIT + 1
                  END IF
               ELSE
                  IF (ABS(MEMR(SVADD+I-1)/MINSV).LT.MAXSV) THEN
                     MEMR(SVADD+I-1) = 0.0
                     NEDIT = NEDIT + 1
                  END IF
               END IF
 100        CONTINUE
            WRITE (MESSAGE, 1010) NEDIT
 1010       FORMAT ('Edited',I5,' singular values')
            CALL MSGPUT(MESSAGE, 'I')
            NSV = NSV - NEDIT
         END IF
C
C Setup for loop.  (A little convoluted, I grant you.)
C
         IF (NIBEGIN.LE.0) THEN
            IF (NIEND.LE.0) THEN
               NIBEGIN = NSV
            ELSE
               NIBEGIN = 1
            END IF
         END IF
         NIBEGIN = MIN (NIBEGIN, NSV)
         NIEND = MIN (NIEND, NSV)
         NIEND = MAX (NIEND, NIBEGIN)
         RNITER = NIBEGIN
         NITER = NINT(RNITER)
C     
         CALL ARRCOPY ('W','InitialW')
         ISVADD = DATADD('InitialW')
C
         IF (ALG.EQ.'NNSVD') THEN
            CALL DATCREAT ('ArrWindow')
         END IF
C
      ELSE IF (ALG.EQ.'NNLS') THEN
         CALL MSGPUT ('Creating Beam matrix','I')
         CALL ARRMBMAT ('BeamMat', 'DataWindow', 'FluxWindow', 'PSF',
     $        DODBLE)
         NIBEGIN = 1
         NIEND = 1
         RNITER = 1
         NITER = 1
         NIINC = 1
      END IF
C
C Main Loop begins here
C
 200  CONTINUE
C
C Loop over number of singular values included
C
      IF (ALG.EQ.'SVD') THEN
         WRITE (MESSAGE,1200) NITER
 1200    FORMAT ('Using',I4,' singular values')
         CALL MSGPUT (MESSAGE,'I')
         CALL ARRSETCO ('W', 0.0, 0.0)
         IF (DODBLE) THEN
            CALL PIXDCOPY (MEMD(ISVADD), 1, MEMD(SVADD), 1, NITER)
         ELSE
            CALL PIXRCOPY (MEMR(ISVADD), 0, 1, MEMR(SVADD), 0, 1, NITER)
         END IF
C     
C     Backsubstitute to get solution.
C
         CALL MATSVBKS ('U','W','V','WDirty','WComp')
C
      ELSE IF (ALG.EQ.'NNSVD') THEN
         WRITE (MESSAGE,1200) NITER
         CALL MSGPUT (MESSAGE,'I')
C
         CALL DATMAKAR ('Temp', 1, NITER, CTYPE, TADD)
         DO 300 I = 1, NITER
            IF (DODBLE) THEN
               MEMD(TADD+I-1) = 1.0 / MEMD(ISVADD+I-1)
            ELSE
               MEMR(TADD+I-1) = 1.0 / MEMR(ISVADD+I-1)
            END IF
 300     CONTINUE
         CALL MATDIAG ('Temp', 'WI')
         CALL DATDELET ('Temp')
C
         CALL DATGETAR ('U', NAX, NAXIS, ATYPE, NDUMMY)
         NAXIS(2) = NITER
         CALL DATPUTI ('ArrWindow', 'TRC', NAXIS, SYSMXDIM)
         CALL ARRSUBSE ('U', 'U1', 'ArrWindow')
         CALL MATTRANS ('U1', 'U1T')
C
         CALL DATGETAR ('V', NAX, NAXIS, ATYPE, NDUMMY)
         NAXIS(2) = NITER
         CALL DATPUTI ('ArrWindow', 'TRC', NAXIS, SYSMXDIM)
         CALL ARRSUBSE ('V', 'V1', 'ArrWindow')
C
         CALL MATMULT ('V1', 'WI', 'G')
         CALL MATMULT ('U1T', 'WDirty', 'U1TD')
         CALL MATMULT ('WI', 'U1TD', 'Temp')
         CALL MATMULT ('V1', 'Temp', 'H')
         CALL ARRSCALE ('H', -1.0, 0.0, 'H')
         CALL DATDELET ('Temp')
         CALL DATDELET  ('U1TD')
C
         CALL MATLDP ('G', 'Z', 'H')
c
         call matmult ('G', 'Z', 'TMP')
         call filimgpu ('TMP', 'TMP', ' ')
         call filimgpu ('H', 'H', ' ')
C
         CALL MATMULT ('G', 'Z', 'WComp')
         CALL ARRLC ('WComp', 1.0, 'H', -1.0, 'WComp')
C
         CALL DATDELET ('U1')
         CALL DATDELET ('U1T')
         CALL DATDELET ('V1')
         CALL DATDELET ('WI')
         CALL DATDELET ('G')
         CALL DATDELET ('H')
         CALL DATDELET ('Z')
C
      ELSE IF (ALG.EQ.'NNLS') THEN
         CALL DATPUTI ('BeamMat', 'ITMAX', ITMAX, 1)
         CALL  MATNNLS ('BeamMat', 'WComp', 'WDirty')
      END IF
C     
      IF (DODBLE) THEN
         CALL ARRCVTR ('WComp','WComp')
         CALL ARRCVTR ('PSF','PSF')
         CALL ARRCVTR ('Dirty','Dirty')
      END IF
C
C Reassemble components into a full sized image
C
      CALL ARRUSCPY ('WComp', 'FluxWindow', 'Components')
      CALL HEDCOPY ('Dirty','Components')
      CALL DATDELET ('WComp')
C
C Write answer: First do Components file
C
      IF (CMPFILE.NE.' ') THEN
         CALL DATPUTC ('Components', 'BUNIT', 'JY/PIXEL', 1)
         CALL IMGSUBSE ('Components', 'Comps', 'Window')
         CALL STRNUMFL (CMPFILE, NITER, TMPFILE)
         CALL HISINPUT ('Comps')
         CALL FILIMGPU ('Comps', TMPFILE, ' ')
         CALL DATDELET ('Comps')
      END IF
C
C Residual image
C
      CALL DATPUTC ('PSF', 'FFTSIZE', 'EXACT', 1)
      CALL IMGMAKEX ('PSF', 'XFR')
      CALL IMGCLONE ('XFR', 'ModVis')
      CALL DATPUTC ('Components', 'FFTSIZE', 'EXACT', 1)
      CALL IMGFFT ('Components', 'ModVis')
      CALL ARRMULT ('ModVis', 'XFR', 'ModVis')
      CALL DATPUTC ('ModVis', 'FFTSIZE', 'EXACT', 1)
      CALL IMGFFT ('ModVis', 'DComp')
      CALL IMGCLONE ('Dirty', 'Residual')
      CALL ARRLC ('Dirty', 1.0, 'DComp', -1.0, 'Residual')
      CALL DATDELET ('DComp')
      CALL DATDELET ('XFR')
      CALL DATDELET ('ModVis')
      IF (RESFILE.NE.' ') THEN
         CALL DATPUTC ('Residual', 'BUNIT', 'JY/BEAM', 1)
         CALL IMGSUBSE ('Residual', 'Res', 'Window')
         CALL STRNUMFL (RESFILE, NITER, TMPFILE)
         CALL HISINPUT ('Res')
         CALL FILIMGPU ('Res', TMPFILE, ' ')
         CALL DATDELET ('Res')
      END IF
C
C Now do conventional CLEAN image if required
C
      IF (IMGFILE.NE.' ') THEN
         CALL IMGCLONE ('Components', 'Clean')
         CALL IMGSMOOT ('Components', BEAM, 'Clean', 'ModVis')
         CALL DATDELET ('ModVis')
         CALL IMGP2PB ('Clean', BEAM, 'Clean')
         CALL ARRLC ('Clean', 1.0, 'Residual', 1.0, 'Clean')
         CALL DATPUTC ('Clean', 'BUNIT', 'JY/BEAM', 1)
         CALL IMGSUBSE ('Clean', 'Cln', 'Window')
         CALL STRNUMFL (IMGFILE, NITER, TMPFILE)
         CALL HISINPUT ('Cln')
         CALL FILIMGPU ('Cln', TMPFILE, ' ')
         CALL DATDELET ('Cln')
      END IF
      IF (ERROR) GO TO 999
C
      IF (DATEXIST('Components')) CALL DATDELET ('Components')
      IF (DATEXIST('Residual')) CALL DATDELET ('Residual')
      IF (DATEXIST('Clean')) CALL DATDELET ('Clean')
C
C Calculate next value of NITER, and possibly loopback
C
      IF (NIINC .LT. 0.0) THEN
 400     RNITER = RNITER * ABS(NIINC)
         IF (NINT(RNITER).EQ.NITER) GO TO 400
         NITER = NINT(RNITER)
      ELSE IF (NIINC .GT. 0.0) THEN
         RNITER = RNITER + NIINC
         NITER = NINT(RNITER)
      ELSE
         NITER = NIEND + 1
      END IF
      IF (NITER .LE. NIEND) GO TO 200
C
 999  CONTINUE
      END
C
      SUBROUTINE PUTARRAY (A,FMT)
#include	"stdinc.h"
      CHARACTER*(*) A,FMT
      CHARACTER*(SYSMXNAM) FNAME, EXT
C
      CALL DATRENAM (A, 'TEMP-ARRAY')
      CALL IMGCLONE ('TEMP-ARRAY', A)
      CALL ARRCOPY ('TEMP-ARRAY', A)
      CALL STRINSRT (FMT, A, FNAME)
      CALL FILSYSEX (FNAME, EXT)
      IF (EXT.EQ.'FTS') CALL ARRCVTR (A, A)
      CALL MATIMHDR (A)
      CALL HISINPUT (A)
      CALL FILIMGPU (A, FNAME, ' ')
      CALL DATDELET (A)
      CALL DATRENAM ('TEMP-ARRAY', A)
      END
C
      SUBROUTINE GETARRAY (A,FMT)
      CHARACTER*(*) A,FMT
      CHARACTER*64 FNAME
C
      CALL STRINSRT (FMT,A,FNAME)
      CALL FILIMGGE (A, FNAME, ' ')
      END

