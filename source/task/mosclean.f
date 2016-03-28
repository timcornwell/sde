C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)mosclean.f	1.7	 6/13/92
C
      SUBROUTINE SDEMAIN
C
CD Given mosaic database, we make dirty maps, clean, subtract from vis,
C  and print out linear mosaics of the clean maps and the clean components
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	May 31 1991
C	Added 5 possible square regions whose CLEAN components
C	will be subtracted from the VIS.  A linear mosaic
C	of the subtracted CLEAN components is also written out.
C				M.A.Holdaway	June 21 1991
C	Corrected error in subsequent subtraction boxes
C				M.A.Holdaway	Aug 7 1991
C	Added PBCLIP for use with polarization
C				M.A.Holdaway	Aug 8 1991
C	Now the clean components are made more consistent
C	via primary beam knowledge.  Also, reduced
C	NC (number of clean areas) to 2 because of difficulties
C	in running the code with 10
C				M.A.Holdaway	Jan 2 1992
C	Tracked change to IMGCLEAN.  Soft CLEAN boxes not enabled, tho'
C				D.S.Briggs	June 12 1992
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'MOSCLEAN')
C
      CHARACTER*(SYSMXNAM)	MOSFILE, NMOSFILE, STOKES
      CHARACTER*(SYSMXNAM)	CLNFILE, COMPFILE, SENSFILE
      CHARACTER*(SYSMXNAM)	SCOMPFIL, MASKFILE
      INTEGER		IMSIZE(3)
      REAL		SHIFT(3), CELLSIZE(3), UVLIMITS(2), TAPER(3),
     $   		SENSCLIP, BEAM(4), PBCLIP
      INTEGER		TIMR(8), NITER
      REAL		FOV, FLUX, GAIN, PBCORR
      INTEGER		NW
      PARAMETER		(NW = 30)
      INTEGER		BLC(SYSMXDIM), TRC(SYSMXDIM), NWRITE(NW)
      INTEGER		SBLC1(SYSMXDIM), STRC1(SYSMXDIM)
      INTEGER		SBLC2(SYSMXDIM), STRC2(SYSMXDIM)
      INTEGER		SBLC3(SYSMXDIM), STRC3(SYSMXDIM)
      INTEGER		SBLC4(SYSMXDIM), STRC4(SYSMXDIM)
      INTEGER		SBLC5(SYSMXDIM), STRC5(SYSMXDIM)
      INTEGER		SBLC6(SYSMXDIM), STRC6(SYSMXDIM)
      INTEGER		SBLC7(SYSMXDIM), STRC7(SYSMXDIM)
      INTEGER		SBLC8(SYSMXDIM), STRC8(SYSMXDIM)
      INTEGER		SBLC9(SYSMXDIM), STRC9(SYSMXDIM)
      INTEGER		SBLC10(SYSMXDIM), STRC10(SYSMXDIM)
C
      INTEGER		NC
      PARAMETER		(NC = 2)
      INTEGER		SBLC(7,NC), STRC(7,NC)
      CHARACTER*(SYSMXNAM)	CFTYPE, SUBCLASS, MOSS, STRM2,
     $   			CTIME, STRM3, MODCLASS
      REAL		TIME(2), SUMWT, MAXPSF, MAXRES, MAXSEN
      INTEGER		NPC, IPC, DIR, NSEL, NDUMMY, ANITER, I
      DOUBLE PRECISION	RVAL(SYSMXDIM)
C
      CHARACTER*7	STRINT, IPCSTR
      LOGICAL		DATEXIST, DOSUB
      REAL		DATFGETR
C
      DATA		IMSIZE /3 * 1/
      DATA		CELLSIZE /3 * 1.0/
      DATA		SBLC  /14 * 1/
      DATA		STRC  /14 * 1/
C==================================================================
      CALL MSGWELCO ('I clean mosaic data bases')
      CALL USRCTL
C
C Get input parameters
C
      CALL USRGETC ('Mos', MOSFILE, 1, NDUMMY)
      CALL USRGETC ('NewMos', NMOSFILE, 1, NDUMMY)
      CALL USRGETC ('Clean', CLNFILE, 1, NDUMMY)
      CALL USRGETC ('Comps', COMPFILE, 1, NDUMMY)
      CALL USRGETC ('SComps', SCOMPFIL, 1, NDUMMY)
      CALL USRGETC ('Sens', SENSFILE, 1, NDUMMY)
      CALL USRGETI ('Nwrite', NWRITE, NW, NDUMMY)
C
      CALL USRGETI ('Imsize', IMSIZE, 3, NDUMMY)
      CALL USRGETR ('Cellsize', CELLSIZE, 3, NDUMMY)
      CALL USRGETR ('Shift', SHIFT, 3, NDUMMY)
      CALL USRGETC ('Stokes', STOKES, 1, NDUMMY)
      CALL USRGETR ('FOV', FOV, 1, NDUMMY)
      CALL USRGETR ('Timerange', TIMR, 8, NDUMMY)
      CALL USRGETR ('Uvlimits', UVLIMITS, 2, NDUMMY)
      CALL USRGETR ('Filter', TAPER, 3, NDUMMY)
      CALL USRGETR ('Beam', BEAM, 4, NDUMMY)
      CALL USRGETC ('ConvType', CFTYPE, 1, NDUMMY)
C
      CALL USRGETC ('Mask', MASKFILE, 1, NDUMMY)
      CALL USRGETI ('BLC', BLC, SYSMXDIM, NDUMMY)
      CALL USRGETI ('TRC', TRC, SYSMXDIM, NDUMMY)
      CALL USRGETI ('SBLC1', SBLC1, SYSMXDIM, NDUMMY)
      CALL USRGETI ('SBLC2', SBLC2, SYSMXDIM, NDUMMY)
      CALL USRGETI ('STRC1', STRC1, SYSMXDIM, NDUMMY)
      CALL USRGETI ('STRC2', STRC2, SYSMXDIM, NDUMMY)
C
      CALL USRGETI ('Niter', NITER, 1, NDUMMY)
      CALL USRGETR ('Flux', FLUX, 1, NDUMMY)
      CALL USRGETR ('PBcorr', PBCORR, 1, NDUMMY)
      CALL USRGETR ('Gain', GAIN, 1, NDUMMY)
      CALL USRGETR ('MinSENS', SENSCLIP, 1, NDUMMY)
      CALL USRGETR ('MinPB', PBCLIP, 1, NDUMMY)
      CALL USRGETL ('DEBUG', SYSDEBUG, 1, NDUMMY)
C
      SUBCLASS = 'OBS/'//STOKES(1:1)
C
      IMSIZE(1) = IMSIZE(1)*2
      IMSIZE(2) = IMSIZE(2)*2
      DO 100 I = 1, 2
         SBLC(I,1) = SBLC1(I)
         SBLC(I,2) = SBLC2(I)
         STRC(I,1) = STRC1(I)
         STRC(I,2) = STRC2(I)
 100  CONTINUE
C
C Get mosaic file
C
      NPC = 0
      CALL DATCREAT ('Mos')
      IF (MOSFILE .NE. ' ') THEN
         CALL VISMOSGE ('Mos', MOSFILE)
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'No Mosaic FILE')
         GOTO 999
      ENDIF
C
      IF (SYSDEBUG) THEN
         CALL MSGPUT ('Listing for Vis', 'D')
         CALL CRDLIST  ('Mos/PC1/OBS/'//STOKES(1:1))
      ENDIF
C
C Make some big images
C
      CALL IMGMAKE ('Mos/PC1/OBS/'//STOKES(1:1), CELLSIZE, IMSIZE, 
     $   SHIFT, 'R', 'Dirty')
      CALL IMGCLONE ('Dirty', 'PSF')
      CALL MSGPUT ('Listing for PSF', 'D')
      CALL CRDLIST ('PSF')
      NDUMMY = 2
      CALL FFTCONJA ('PSF', 'XFR', DIR, NDUMMY)
      CALL MSGPUT ('Listing for XFR', 'D')
      CALL CRDLIST ('XFR')
      CALL IMGCLONE ('XFR', 'GriddedVis')
      CALL ARRSETCO ('Dirty', 0.0, 0.0)
      CALL ARRSETCO ('PSF', 0.0, 0.0)
      CALL ARRSETCO ('XFR', 0.0, 0.0)
      CALL ARRSETCO ('GriddedVis', 0.0, 0.0)
C
C Make some smaller images
C 
      CALL CRDNHALF ('Dirty', BLC, TRC)
      CALL DATCREAT ('Window')
      CALL DATCREAT ('SWindow')
      CALL DATPUTI  ('Window', 'BLC', BLC, SYSMXDIM)
      CALL DATPUTI  ('Window', 'TRC', TRC, SYSMXDIM)
      CALL IMGSUBSE ('Dirty', 'SmallDirty', 'Window')
      CALL IMGCLONE ('SmallDirty', 'Residual')
      CALL IMGCLONE ('SmallDirty', 'Components')
      CALL IMGCLONE ('SmallDirty', 'SComponents')
      CALL IMGCLONE ('SmallDirty', 'MosClean')
      CALL IMGCLONE ('SmallDirty', 'MosComp')
      CALL IMGCLONE ('SmallDirty', 'MosSComp')
      CALL IMGCLONE ('SmallDirty', 'Flat')
      CALL IMGCLONE ('SmallDirty', 'Util')
      CALL IMGCLONE ('SmallDirty', 'Denom')
      CALL ARRSETCO ('Residual', 0.0, 0.0)
      CALL ARRSETCO ('Components', 0.0, 0.0)
      CALL ARRSETCO ('SComponents', 0.0, 0.0)
      CALL ARRSETCO ('MosClean', 0.0, 0.0)
      CALL ARRSETCO ('MosComp', 0.0, 0.0)
      CALL ARRSETCO ('MosSComp', 0.0, 0.0)
      CALL ARRSETCO ('Util', 0.0, 0.0)
      CALL ARRSETCO ('Denom', 0.0, 0.0)
      CALL ARRSETCO ('Flat', 0.0, 1.0)

      IF (MASKFILE.NE.' ') THEN
         CALL FILMASGE ('Box', MASKFILE, 'SmallDirty')
      ELSE
         CALL FILMASGE ('Box', ' ', 'SmallDirty')
      ENDIF
      IF (ERROR) GO TO 999
C
      CALL DATGETI ('Mos', 'NPC', NPC, 1, NDUMMY)
      DO 5000 IPC = 1, NPC
         IPCSTR = STRINT(IPC)
         CALL MSGPUT (STRINT(IPC)//': Begin Pointing '
     $      //STRINT(IPC), 'I')
         MOSS = STRM2 ('Mos', 'PC'//STRINT(IPC))
         CALL HEDSETOB (MOSS, 'Components')
         CALL HEDSETOB (MOSS, 'SComponents')
C
C Step 1: select the data
C
         IF (DATEXIST ('WTSave')) CALL DATDELET ('WTSave')
         CALL ARRCOPY (STRM3(MOSS, SUBCLASS, 'WT'), 'WTSave')
         IF(DATEXIST(STRM2(MOSS, 'TIME'))) THEN
            CALL UTLTTOD (TIMR(1), TIME(1))
            CALL UTLTTOD (TIMR(5), TIME(2))
            CALL VISSEL (MOSS, SUBCLASS, TIME, UVLIMITS, NSEL)
            IF (ERROR) GO TO 999
            WRITE (MESSAGE, 1030) IPCSTR, NSEL
 1030       FORMAT (A7,': Selected ',I7,' visibilities')
            CALL MSGPUT (MESSAGE, 'I')
         ELSE
            CALL MSGPUT ('No TIME information; UVlimits not processed',
     $         'W')
         END IF
C
C Step 2: Now grid data and weights
C
         CALL ARRSETCO ('XFR', 0., 0.)
         IF (FOV.NE.0.0) THEN
            CALL DATPUTR ('XFR', 'WTFOV', FOV, 1)
            CALL GRDUWT (MOSS, SUBCLASS, 'XFR')
            IF (ERROR) GOTO 999
            CALL MSGPUT (MESSAGE, 'I')
            CALL SYSETIME (CTIME)
            CALL MSGPUT (STRINT(IPC)//': Weighting completed: '//CTIME,
     $         'I')
         END IF
C
C Step 3: Apply Taper
C
         IF ((TAPER(1).NE.0.0).OR.(TAPER(2).NE.0.0)) THEN
            CALL VISTAPER (MOSS, SUBCLASS, TAPER, SUBCLASS)
         END IF
C
C Step 4: Make PSF
C
         IF (ERROR) GO TO 999
         CALL DATPUTC ('PSF', 'CFTYPE', CFTYPE, 1)
         CALL DATPUTC ('XFR', 'CFTYPE', CFTYPE, 1)
         IF (ERROR) GO TO 999
         CALL VISTOIMG (MOSS, SUBCLASS, 'PSF', 'XFR', .TRUE.)
         IF (ERROR) GO TO 999
         CALL DATGETR ('XFR', 'SUMWT', SUMWT, 1, NDUMMY)
         CALL DATPUTR ('PSF', 'SUMWT', SUMWT, 1)
         WRITE (MESSAGE,1200) IPCSTR, SUMWT
 1200    FORMAT (A7,': Sum of weights = ',1PE15.6)
         CALL MSGPUT (MESSAGE, 'I')
         CALL ARRSTAT ('PSF', ' ')
         CALL DATGETR ('PSF', 'ARRMAX', MAXPSF, 1, NDUMMY)
         IF (MAXPSF.EQ.0.0) THEN
            CALL ERRREPOR (ERRNTFND, ROUTINE, 'Peak of PSF zero')
            GO TO 999
         ELSE
             CALL ARRSCALE('PSF',1.0/MAXPSF,0.0,'PSF')
         END IF
C
C Step 5: Make Dirty Map
C
         CALL VISTOIMG (MOSS, SUBCLASS, 'Dirty', 'GriddedVis', 
     $      .FALSE.)
         IF (MAXPSF.NE.0.0) THEN
            CALL ARRSCALE('Dirty',1.0/MAXPSF,0.0,'Dirty')
         END IF
         CALL SYSETIME (CTIME)
         CALL MSGPUT (STRINT(IPC)//': Made PSF and DIRTY: '//CTIME, 'I')
C
C Step 6: Begin CLEAN
C
         CALL DATDELET ('SmallDirty')
         CALL IMGSUBSE ('Dirty', 'SmallDirty', 'Window')
         CALL ARRSETCO ('Components', 0.0, 0.0)
C
C We should do something like adjust the FLUX limit based upon
C the PB level
C
         CALL DATPUTI ('Components', 'NITER', NITER, 1)
         CALL DATPUTR ('Components', 'FLUX', FLUX, 1)
         CALL DATPUTR ('Components', 'TFLUX', 0.0, 1)
         CALL DATPUTR ('Components', 'GAIN', GAIN, 1)
         CALL IMGCLEAN ('SmallDirty', 'PSF', 'Components', 'Residual',
     $      'Box')
         CALL DATGETI ('Components', 'NITER', ANITER, 1, NDUMMY)
         CALL DATGETR ('Components', 'FLUX', MAXRES, 1, NDUMMY)
         IF (ERROR) GO TO 999
C
         DO 4000 I = 1, NW
            IF (IPC .EQ. NWRITE(I)) THEN
               CALL HEDSETOB (MOSS, 'Components')
               CALL HEDSETOB (MOSS, 'Dirty')
               CALL FILIMGPU ('Dirty', 'DIRTY.'//STRINT(IPC), ' ')
               CALL FILIMGPU ('Components', 'COMPS.'//STRINT(IPC), ' ')
               GOTO 4001
            ENDIF
 4000    CONTINUE
 4001    CONTINUE
C
         CALL SYSETIME (CTIME)
         CALL MSGPUT (STRINT(IPC)//': Finished CLEAN: '//CTIME, 'I')
C
C Step 7: Remove the components from the observed visibilities
C	  Two ways of accomplishing this:
C		1) By Box
C		2) By PBCLIP
C
         IF (NMOSFILE.NE.' ') THEN
C
C  form clean components to subtract
C
            IF (PBCLIP .GT. 0.0) THEN
               DOSUB = .TRUE.
               CALL ARRCOPY ('Components', 'SComponents')
               CALL IMGPBCLP ('SComponents', PBCLIP, 'ABOVE')
               WRITE (MESSAGE, 1835) PBCLIP
 1835          FORMAT('Subtracting components beyond PB level ',
     $            F5.3)
               CALL MSGPUT (MESSAGE, 'I')
            ELSE
               DOSUB = .FALSE.
               CALL ARRSETCO ('SComponents', 0.0, 0.0)
               DO 200 I = 1, NC
                  IF (SBLC(1,I) .NE. 1 .OR. SBLC(2,I) .NE. 1 .OR.
     $               STRC(1,I) .NE. 1 .OR. STRC(2,I) .NE. 1) THEN
                     DOSUB = .TRUE.
                     CALL DATPUTI  ('SWindow', 'BLC', SBLC(1,I), 
     $                  SYSMXDIM)
                     CALL DATPUTI  ('SWindow', 'TRC', STRC(1,I), 
     $                  SYSMXDIM)
                     IF (DATEXIST('SUtil')) CALL DATDELET ('SUtil')
                     CALL IMGSUBSE ('Components', 'SUtil', 'SWindow')
                     CALL IMGFITTO ('SUtil', 'Components', 'SSUtil')
                     CALL ARRLC    ('SComponents', 1.0, 'SSUtil', 1.0,
     $                  'SComponents')
                     CALL DATDELET ('SSUtil')
                  ENDIF
 200           CONTINUE
               IF (DOSUB) THEN
                  CALL MSGPUT ('Subtracting components in boxes', 'I')
               ENDIF
            ENDIF
C
            IF (DOSUB) THEN
               CALL ARRCLIP  ('SComponents', 0.0, 1E+20, 'SComponents')
               CALL IMGCLONE ('SComponents', 'MASK')
               CALL IMGCLONE ('SComponents', 'MASKPB')
               CALL ARRMASK  ('SComponents', 'MASK', 0.5*FLUX, 1E+20, 
     $            0.0, 1.0, 0.0)
               CALL IMGPB    ('MASK', 'MASKPB', 'APPLY')
               CALL ARRLC    ('SComponents', 1.0, 'MASKPB', 
     $            -FLUX/PBCORR, 'SComponents')
               CALL DATDELET ('MASK')
               CALL DATDELET ('MASKPB')
               CALL ARRCOPY  ('WTSave', STRM3(MOSS, SUBCLASS, 'WT'))         
               CALL VISCLONE (MOSS, 'OBS', STOKES(1:1), 'MOD')
               MODCLASS = STRM2 ('MOD', STOKES(1:1))
               CALL IMGTOVIS (MOSS, MODCLASS, 'SComponents', 'ModVis', 
     $            'Dmodel')
               CALL DATDELET ('Dmodel')
               CALL ARRSUBTR (STRM3(MOSS, SUBCLASS, 'VIS'), 
     $            STRM3(MOSS, MODCLASS, 'VIS'),
     $            STRM3(MOSS, SUBCLASS, 'VIS') )
               CALL DATDELET ( STRM2(MOSS, 'MOD') )
               WRITE (MESSAGE, 2000) IPCSTR, ANITER, MAXRES
 2000          FORMAT (A7, ': Subtracted ',I6,' comps: max residual ',
     1            1PE9.2,' Jy/beam')
               CALL MSGPUT (MESSAGE, 'I')
            ENDIF
         ENDIF
C
C Step 8: Make the convolved CLEAN image with residuals
C
         IF (PBCLIP .GT. 0.0) THEN
            CALL IMGPBCLP ('Components', PBCLIP, 'BELOW')
         ENDIF
         IF (CLNFILE .NE. ' ') THEN
            IF (IPC .EQ. 1) THEN
               IF (BEAM(1) .EQ. 0. ) THEN
                  CALL IMGBMSHP( 'PSF' )
                  IF (ERROR) GO TO 999
                  BEAM(1) =  DATFGETR( 'PSF', 'BMAJ')*3600.
                  BEAM(2) =  DATFGETR( 'PSF', 'BMIN')*3600.
                  BEAM(3) =  DATFGETR( 'PSF', 'BPA')
                  BEAM(4) =  DATFGETR( 'PSF', 'BZ')*3600.
               ENDIF
               CALL DATPUTR ('MosClean', 'BMAJ', BEAM(1)/3600., 1)
               CALL DATPUTR ('MosClean', 'BMIN', BEAM(2)/3600., 1)
               CALL DATPUTR ('MosClean', 'BPA', BEAM(3), 1)
               CALL DATPUTR ('MosClean', 'BZ', BEAM(4)/3600., 1)
            ENDIF
            IF (DATEXIST('Clean')) CALL DATDELET ('Clean')
            CALL DATPUTI  ('Components', 'NITER', ANITER, 1)
            CALL IMGCLONE ('Components', 'Clean')
            CALL IMGSMOOT ('Components', BEAM, 'Clean', 'MVis')
            CALL DATDELET ('MVis')
            CALL IMGP2PB  ('Clean', BEAM, 'Clean')
            CALL ARRLC    ('Clean', 1.0, 'Residual', 1.0, 'Clean')
            CALL DATPUTC  ('Clean', 'BUNIT', 'JY/BEAM', 1)
            IF (PBCLIP .GT. 0.0) THEN
               CALL IMGPBCLP ('Clean', PBCLIP, 'BELOW')
            ENDIF
         END IF
C
C Step 9: Make Linear COMPS and CLEAN images
C
         IF (CLNFILE .NE. ' ' .OR. COMPFILE .NE. ' ') THEN
            CALL HEDSETOB (MOSS, 'Flat')
            CALL HEDSETOB (MOSS, 'Util')
            CALL IMGPB ('Flat', 'Util', 'APPLY')
            CALL IMGPB ('Util', 'Util', 'APPLY')
            IF (PBCLIP .GT. 0.0) THEN
               CALL IMGPBCLP ('Util', PBCLIP, 'BELOW')
            ENDIF
            CALL ARRLC ('Denom', 1.0, 'Util', SUMWT, 'Denom')
         ENDIF
         IF (CLNFILE .NE. ' ') THEN
            CALL HEDSETOB (MOSS, 'Clean')
            CALL IMGPB ('Clean', 'Clean', 'APPLY')
            CALL ARRLC ('MosClean', 1.0, 'Clean', SUMWT, 'MosClean')
         ENDIF
         IF (COMPFILE .NE. ' ') THEN
            CALL HEDSETOB (MOSS, 'Components')
            CALL IMGPB ('Components', 'Components', 'APPLY')
            CALL ARRLC ('MosComp', 1.0, 'Components', SUMWT, 
     $         'MosComp')
        ENDIF
         IF (SCOMPFIL .NE. ' ') THEN
            CALL IMGPB ('SComponents', 'SComponents', 'APPLY')
            CALL ARRLC ('MosSComp', 1.0, 'SComponents', SUMWT, 
     $         'MosSComp')
         ENDIF
C
 5000 CONTINUE
C
C Step 10: Divide by denom to get MosClean and MosComp
C	   Write out whatever we should
C
      IF (CLNFILE .NE. ' ' .OR. COMPFILE .NE. ' ' .OR. 
     $   SCOMPFIL .NE. ' ') THEN
         CALL ARRSTAT ('Denom', ' ')
         CALL DATGETR ('Denom', 'ARRMAX', MAXSEN, 1, NDUMMY)
         IF(ERROR) GO TO 999
         IF(MAXSEN.EQ.0.0) THEN
            CALL ERRREPOR (ERRNTFND, ROUTINE, 'Zero sensitivity')
            GO TO 999
C         ELSE
C            WRITE (MESSAGE, 1100) MAXSEN
C 1100       FORMAT  ('Maximum sensitivity = ',F7.3,
C     $         ' Primary beam units')
C            CALL MSGPUT (MESSAGE, 'I')
         ENDIF
      END IF
      IF (CLNFILE.NE.' ' ) THEN
         CALL ARRCDIV ('MosClean', 'Denom', SENSCLIP*MAXSEN, 'MosClean')
         CALL DATGETD ('MosClean', 'CRVAL', RVAL, SYSMXDIM, NDUMMY)
         CALL DATPUTD ('MosClean', 'OBSRA', RVAL(1), 1)
         CALL DATPUTD ('MosClean', 'OBSDEC', RVAL(2), 1)
         CALL DATPUTC ('MosClean', 'BUNIT', 'JY/BEAM', 1)
         CALL FILIMGPU ('MosClean', CLNFILE, ' ')
      END IF
      IF (COMPFILE.NE.' ' ) THEN
         CALL ARRCDIV ('MosComp', 'Denom', SENSCLIP*MAXSEN, 'MosComp')
         CALL DATGETD ('MosComp', 'CRVAL', RVAL, SYSMXDIM, NDUMMY)
         CALL DATPUTD ('MosComp', 'OBSRA', RVAL(1), 1)
         CALL DATPUTD ('MosComp', 'OBSDEC', RVAL(2), 1)
         CALL DATPUTC ('MosComp', 'BUNIT', 'JY/PIXEL', 1)
         CALL FILIMGPU ('MosComp', COMPFILE, ' ')
      END IF
      IF (SCOMPFIL.NE.' ' ) THEN
         CALL ARRCDIV ('MosSComp', 'Denom', SENSCLIP*MAXSEN, 'MosSComp')
         CALL DATGETD ('MosSComp', 'CRVAL', RVAL, SYSMXDIM, NDUMMY)
         CALL DATPUTD ('MosSComp', 'OBSRA', RVAL(1), 1)
         CALL DATPUTD ('MosSComp', 'OBSDEC', RVAL(2), 1)
         CALL DATPUTC ('MosSComp', 'BUNIT', 'JY/PIXEL', 1)
         CALL FILIMGPU ('MosSComp', SCOMPFIL, ' ')
      END IF
      IF (SENSFILE.NE.' ') THEN
         CALL DATRENAM ('Denom', 'Sens')
         CALL HISOPEN ('Sens')
         CALL HISINPUT ('Sens')
         CALL DATGETD ('Sens', 'CRVAL', RVAL, SYSMXDIM, NDUMMY)
         CALL DATPUTD ('Sens', 'OBSRA', RVAL(1), 1)
         CALL DATPUTD ('Sens', 'OBSDEC', RVAL(2), 1)
         CALL DATPUTC ('Sens', 'BUNIT', 'WEIGHT', 1)
         CALL FILIMGPU ('Sens', SENSFILE, ' ')
      END IF
C
      IF (NMOSFILE.NE.' ') THEN
         CALL VISMOSPU ('Mos', NMOSFILE)
      END IF
C
 999  CONTINUE
      END

