C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)bignnls.f	1.1    2/8/95
C
      SUBROUTINE SDEMAIN
C
CD Cut down version of SVDCONV (NNLS option) for use on big images.
C There are absolutely no frills, in the interest of saving memory.  The
C image & residuals must be created by cleanwin, if it will handle them,
C or by hand with imgconv, smooth & imglc if it won't.
C
C Audit trail:
C	Cloned from SVDCONV
C				D.S.Briggs	Jan 22 1995
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'BIGNNLS')
C
      INTEGER		NITER, NDUMMY,
     $     		BLC(SYSMXDIM), TRC(SYSMXDIM)
      REAL		RNITER, BEAM(4), MINSV, MAXITER
      CHARACTER*(SYSMXNAM)	IMGFILE, RESFILE, DRTFILE, PSFFILE,
     $			CMPFILE, IWINFILE, OWINFILE
      LOGICAL		DOWRSVD, DORESVD, DODBLE, DOHALF
C
      INTEGER		I, NAX, NAXIS(SYSMXDIM), NIWIN, NOWIN, NPIX,
     $   		PADD, DADD, WDADD, SVADD, TADD, IDELT(SYSMXDIM),
     $   		ISVADD, NIBEGIN, NIEND, NEDIT, NSV, ITMAX
      REAL		NIINC, MAXSV
      CHARACTER		ATYPE*1, CTYPE*1
C
      INTEGER		DATADD, CRDRNAX
      REAL		DATFGETR
      LOGICAL		DATEXIST
C==================================================================
      CALL MSGWELCO (
     $   'I perform no-frills NNLS deconvolution on big images')
      CALL USRCTL
C
C Get input parameters
C
      CALL USRGETC ('Dirty', DRTFILE, 1, NDUMMY)
      CALL USRGETC ('PSF', PSFFILE, 1, NDUMMY)
      CALL USRGETC ('Components', CMPFILE, 1, NDUMMY)
      CALL USRGETC ('DataWindow', IWINFILE, 1, NDUMMY)
      CALL USRGETC ('FluxWindow', OWINFILE, 1, NDUMMY)
      CALL USRGETI ('BLC', BLC, SYSMXDIM, NDUMMY)
      CALL ERRCANCE
      CALL USRGETI ('TRC', TRC, SYSMXDIM, NDUMMY)
      CALL ERRCANCE
      CALL USRGETL ('Half', DOHALF, 1, NDUMMY)
      CALL USRGETL ('Double', DODBLE, 1, NDUMMY)
      CALL USRGETI ('NIend', NIEND, 1, NDUMMY)
      CALL USRGETR ('MaxIter', MAXITER, 1, NDUMMY)
      CALL USRGETL ('DEBUG', SYSDEBUG, 1, NDUMMY)
      IF (ERROR) GO TO 999
C
      IF (DODBLE) THEN
         CTYPE = 'D'
         CALL MSGPUT (
     $      'Will do deconvolution in DOUBLE precision', 'I')
      ELSE
         CTYPE = 'R'
         CALL MSGPUT (
     $      'Will do deconvolution in SINGLE precision', 'I')
      END IF
C
C Get images
C
      CALL FILIMGGE ('Dirty', DRTFILE, ' ')
      IF ((TRC(1).EQ.1).AND.(TRC(2).EQ.1)) THEN
         TRC(1) = NAXIS(1)
         TRC(2) = NAXIS(2)
      END IF
      IF (DOHALF) CALL CRDHALF ('Dirty', BLC, TRC)
      CALL DATCREAT ('Window')
      CALL DATPUTI ('Window', 'TRC', TRC, SYSMXDIM)
      CALL DATPUTI ('Window', 'BLC', BLC, SYSMXDIM)
      IF ((BLC(1).NE.1).OR.(TRC(1).NE.NAXIS(1)).OR.
     $    (BLC(2).NE.1).OR.(TRC(2).NE.NAXIS(2))) THEN
         CALL DATRENAM ('Dirty','RawDirty')
         CALL IMGSUBSE ('RawDirty','Dirty','Window')
         CALL DATDELET ('RawDirty')
      END IF
C
      CALL FILMASGE ('DataWindow', IWINFILE, 'Dirty')
      CALL IMGSTRIM ('DataWindow')
      CALL IMGIDELT ('Dirty', 'DataWindow', IDELT)
      CALL DATPUTI ('DataWindow', 'DELTA', IDELT, SYSMXDIM)
      CALL FILMASGE ('FluxWindow', OWINFILE, 'Dirty')
      CALL IMGSTRIM ('FluxWindow')
      CALL IMGIDELT ('Dirty', 'FluxWindow', IDELT)
      CALL DATPUTI ('FluxWindow', 'DELTA', IDELT, SYSMXDIM)
      IF (ERROR) GO TO 999
C
      CALL DATGETAR ('Dirty', NAX, NAXIS, ATYPE, PADD)
      NPIX = NAXIS(1)
      IF ((ATYPE.NE.'R').AND.(ATYPE.NE.CTYPE)) THEN
         CALL ERRREPOR (ERRFATAL, ROUTINE, 'Dirty is bad type')
         GO TO 999
      END IF
      IF (CRDRNAX(NAX,NAXIS).GT.2) THEN
         CALL ERRREPOR (ERRFATAL, ROUTINE, 'Only 1 or 2-D images!')
         GO TO 999
      END IF
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
C Create a windowed dirty map
C
      CALL DATMAKAR ('WDirty', 1, NIWIN, CTYPE, WDADD)
      CALL ARRSETCO ('WDirty', 0.0, 0.0)
      CALL ARRSCOPY ('Dirty', 'DataWindow', 'WDirty')
      CALL DATDELET ('Dirty')
      IF (ERROR) GO TO 999
C
      CALL FILIMGGE ('PSF', PSFFILE, ' ')
      IF (ERROR) GO TO 999
C
C Get the PSF and deal with it
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
      IF (DODBLE) THEN
         CALL ARRCVTD ('PSF','PSF')
         PADD = DATADD ('PSF')
      END IF
C
      CALL MSGPUT ('Creating Beam matrix','I')
      CALL ARRMBMAT ('BeamMat', 'DataWindow', 'FluxWindow', 'PSF',
     $   DODBLE)
C
C At this point, we've probably past the worst of the memory problems
C
      CALL DATDELET ('PSF')
      CALL DATDELET ('DataWindow')
C
C Deal with MaxIter
C
      IF ((MAXITER.LE.0.0).AND.(NIEND.LE.1)) MAXITER = 3.0
      IF (MAXITER.GT.0.0) THEN
         WRITE (MESSAGE, 1003) MAXITER
 1003    FORMAT ('Allowing',F8.3,' * Nflux iterations of main loop')
         ITMAX = MAXITER * NOWIN
      ELSE
         WRITE (MESSAGE, 1004) NIEND
 1004    FORMAT ('Allowing ',I8,' iterations of main loop')
         ITMAX = NIEND
      END IF
      CALL MSGPUT (MESSAGE,'I')
C
      NIEND = 1
C
C Do the actual deconvolutio
C
      CALL DATPUTI ('BeamMat', 'ITMAX', ITMAX, 1)
      CALL MATNNLS ('BeamMat', 'WComp', 'WDirty')
      CALL DATDELET ('BeamMat')
C
C Do something with the components
C
      CALL FILIMGGE ('Dirty', DRTFILE, ' ')
      CALL IMGSUBSE ('Dirty','Components','Window')
      IF (DODBLE) CALL ARRCVTR ('WComp','WComp')
C
C Reassemble components into a full sized image
C
      CALL ARRSETCO ('Components', 0.0, 0.0)
      CALL ARRUSCPY ('WComp', 'FluxWindow', 'Components')
      CALL DATDELET ('WComp')
C
C Write answer: First do Components file
C
      IF (CMPFILE.NE.' ') THEN
         CALL DATPUTC ('Components', 'BUNIT', 'JY/PIXEL', 1)
         CALL HISINPUT ('Components')
         CALL FILIMGPU ('Components', CMPFILE, ' ')
         CALL DATDELET ('Components')
      END IF
C
 999  CONTINUE
      END

