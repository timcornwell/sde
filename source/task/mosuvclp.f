C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)mosuvclp.f	1.4	 7/5/95
C
      SUBROUTINE SDEMAIN
C
CD Plots |Vis| (sqrt(u^2 + v^2), clips w.r.t a model
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	April 9 1993
C	Fixed the DOIT business, added VIS CLIPPING capability,
C	added graph limits setting from USR parms.
C				M.A. Holdaway	Aug 15, 1994
C	Added POL stuff
C				M.A. Holdaway	Jul 5 1995
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'MOSUVCLP')
C
      CHARACTER*(SYSMXNAM)	MOSFILE, MODFILE, MODE, TITLE,
     1			MOSS, STRM2, NMOSFILE, STRM4
      REAL		GRAPHLIM(4)
      REAL		TIME(2), UVLIM(2), UVLIM0(2), THRESH(2)
      INTEGER		IPOINT(20), ISKIP
      CHARACTER*(SYSMXNAM)	OUTDEV, STOKESDO, OSTOKES
      CHARACTER*1	FSTOKES(4), STOKES
      INTEGER		NDUMMY, TIMR(8)
      CHARACTER*6	STRINT
      LOGICAL		DOIT
      DOUBLE PRECISION	OBSRA, OBSDEC
      INTEGER		NPC, IPC, JPC, NSEL
      INTEGER		NAX, NAXIS(SYSMXDIM), ADD, IS
      CHARACTER*1	T
      CHARACTER*(SYSMXNAM)	TELESCOP, YLABEL, OUTDEV2
C
      CHARACTER*(SYSMXNAM)	STRRMBL
C
      DATA		GRAPHLIM /0.0,0.0,0.0,0.0/
      DATA		UVLIM0 / 0.0, 1E+10/
C==================================================================
      CALL MSGWELCO ('I do mosaic UV plots and clip wrt a model')
      CALL USRCTL
C
C Get input parameters
C
      CALL USRGETC ('Mos', MOSFILE, 1, NDUMMY)
      CALL USRGETC ('Model', MODFILE, 1, NDUMMY)
      CALL USRGETC ('Mode', MODE, 1, NDUMMY)
      CALL USRGETC ('Stokes', STOKESDO, 1, NDUMMY)
      CALL USRGETC ('OtherStokes', OSTOKES, 1, NDUMMY)
      CALL USRGETI ('Timerange', TIMR, 8, NDUMMY)
      CALL USRGETI ('Skip', ISKIP, 1, NDUMMY)
      CALL USRGETI ('WhichPoint', IPOINT, 20, NDUMMY)
      CALL USRGETC ('Dev', OUTDEV, 1, NDUMMY)
C
      CALL USRGETC ('NewMos', NMOSFILE, 1, NDUMMY)
      CALL USRGETR ('Thresh', THRESH, 2, NDUMMY)
      CALL USRGETR ('UVlimits', UVLIM, 2, NDUMMY)
      CALL USRGETR ('GraphLimits', GRAPHLIM, 4, NDUMMY)
C
C Get mosaic file
C
      STOKES = STOKESDO(1:1)
      FSTOKES(1) = OSTOKES(1:1)
      FSTOKES(2) = OSTOKES(2:2)
      FSTOKES(3) = OSTOKES(3:3)
      FSTOKES(4) = OSTOKES(4:4)
      CALL VISMOSGE ('Mos', MOSFILE)
C
C Get model image
C
      IF (MODE(1:3) .EQ. 'DIV') THEN
         CALL FILIMGGE ('IModel', MODFILE, ' ')
         CALL IMGDOUBL ('IModel', 'Model')
         CALL DATDELET ('IModel')
         CALL IMGGRIDC ('Model', 'Model', 'CORRECT')
         CALL IMGCLONE ('Model', 'PBModel')
      ENDIF
C
C Loop over pointings: 
C
      CALL DATGETI ('Mos', 'NPC', NPC, 1, NDUMMY)
      DO 10 IPC = 1, NPC
         DOIT = .FALSE.
         IF (IPOINT(1) .LE. 0) DOIT = .TRUE.
         DO 9 JPC = 1, 20
            IF (IPOINT(JPC) .EQ. IPC) DOIT = .TRUE.
 9       CONTINUE
         IF (.NOT. DOIT) GOTO 10
C
         WRITE (MESSAGE, 1000) IPC
 1000    FORMAT ('Pointing ',I3)
         CALL MSGPUT (MESSAGE, 'I')
         MOSS = STRM2 ('Mos', 'PC'//STRINT(IPC))

         CALL DATGETD (MOSS, 'OBSRA', OBSRA, 1, NDUMMY)
         CALL DATGETD (MOSS, 'OBSDEC', OBSDEC, 1, NDUMMY)
         CALL DATGETC (MOSS, 'TELESCOP', TELESCOP, 1, NDUMMY)
         WRITE (MESSAGE, 1100) OBSRA, OBSDEC
 1100    FORMAT ('Observed RA, DEC = ',F10.4,1X,F10.4)
         CALL MSGPUT (MESSAGE, 'I')
         CALL UTLTTOD (TIMR(1), TIME(1))
         CALL UTLTTOD (TIMR(5), TIME(2))
         CALL VISSEL (MOSS, 'OBS/'//STOKES, TIME, UVLIM0, NSEL)
         CALL VISCLONE (MOSS, 'OBS', STOKES, 'MOD')
         IF (MODE(1:3) .EQ. 'DIV') THEN
            CALL DATPUTD ('Model', 'OBSRA', OBSRA, 1)
            CALL DATPUTD ('Model', 'OBSDEC', OBSDEC, 1)
            CALL DATPUTC ('Model', 'TELESCOP', TELESCOP, 1)
            CALL IMGPB ('Model', 'PBModel', 'APPLY')
            CALL IMGFFT ('PBModel', 'ModMos')
            CALL VISDEGRI (MOSS, 'MOD/'//STOKES, 'ModMos')
C
            IF (THRESH(2) .GT. 0.0) THEN
               CALL VISCLIP (MOSS, 'OBS/'//STOKES, 'MOD/'//STOKES, 
     $              'OBS/'//STOKES, UVLIM, 
     $            THRESH, 'DIV')
            ENDIF
C
            YLABEL = 'ABS( Data Vis / Model Vis )'
            WRITE (TITLE, 1010) IPC, MOSFILE(1:15), MODFILE(1:15)
 1010       FORMAT ('Vis/Model for PC',I4,' MOS:',A15,' MOD:',A15)
            CALL ARRCDIV (STRM2(MOSS, 'OBS/'//STOKES//'/VIS'), 
     $         STRM2(MOSS, 'MOD/'//STOKES//'/VIS'), 1.E-6, 'Data')
            CALL ARRX2AP ('Data', 'Data1', 'Trash')
            CALL DATDELET (STRM2(MOSS, 'MOD'))
            CALL DATDELET ('Data')
            CALL DATDELET ('Trash')
         ELSE
            CALL ARRSETCO (STRM2(MOSS, 'MOD/'//STOKES//'/VIS'), 
     $           0.0, 1.0)
            IF (THRESH(2) .GT. 0.0) THEN
               CALL VISCLIP (MOSS, 'OBS/'//STOKES, 'MOD/'//STOKES, 
     $              'OBS/'//STOKES, UVLIM, THRESH, 'CLIP')
            ENDIF
            YLABEL = 'ABS( Data Vis )'
            WRITE (TITLE, 1015) IPC, MOSFILE(1:15), MODFILE(1:15)
 1015       FORMAT ('Vis for PC',I4,' MOS:',A15,' MOD:',A15)
            CALL ARRX2AP (STRM2(MOSS, 'OBS/'//STOKES//'/VIS'), 
     $           'Data1', 'Trash')
            CALL DATDELET ('Trash')
         ENDIF
C
C Deal with other stokes
C
         IF (THRESH(2) .GT. 0.0) THEN
            CALL ARRCLIP(STRM4(MOSS, 'OBS', STOKES, 'WT'),
     $           0.0, 1.0E-6, 'Trash')
            CALL ARRSCALE( 'Trash', 1.0E+6, 0.0, 'Trash')
            DO 765 IS = 1, 4
               IF (FSTOKES(IS) .NE. ' ') THEN
                  CALL ARRMULT (STRM4(MOSS,'OBS',FSTOKES(IS),'WT'),
     $                 'Trash', STRM4(MOSS,'OBS',FSTOKES(IS),'WT'))
               ENDIF
 765        CONTINUE
            CALL DATDELET ('Trash')
         ENDIF
C
C Make plot
C
         CALL ARRQU2X (STRM2(MOSS, 'UU'), STRM2(MOSS, 'VV'), 'X')
         CALL ARRX2AP ('X', 'R', 'THETA')
         CALL ARRSCOPY ('Data1', STRM2(MOSS, 'OBS/'//STOKES//'/WT'), 
     $        'Data2')
         CALL ARRSCOPY ('R', STRM2(MOSS, 'OBS/'//STOKES//'/WT'), 'R2')
         CALL DATCREAT ('Window')
         CALL DATPUTI ('Window', 'STEP', ISKIP, 1)
         CALL ARRSUBSE ('R2', 'R3', 'Window')
         CALL ARRSUBSE ('Data2', 'Data3', 'Window')
         CALL DATGETAR ('R3', NAX, NAXIS, T, ADD)
         IF (OUTDEV .NE. ' ') THEN
            IF (OUTDEV .NE. '/xw') THEN
               OUTDEV2 = STRRMBL( STRINT(IPC)// OUTDEV)
               CALL MSGPUT ('Plot in file '//OUTDEV2, 'I')
            ELSE
               OUTDEV2 = OUTDEV
            ENDIF
            CALL ARRPGGRF (NAXIS(1), 'R2', 'Data2', OUTDEV2, 
     $         'Radial UV Dist', YLABEL, TITLE, 1, 0,
     $           GRAPHLIM(1),GRAPHLIM(2),GRAPHLIM(3),GRAPHLIM(4))
         ENDIF
C       
         CALL DATDELET ('Window')
         CALL DATDELET ('X')
         CALL DATDELET ('R')
         CALL DATDELET ('R2')
         CALL DATDELET ('R3')
         CALL DATDELET ('THETA')
         CALL DATDELET ('Data1')
         CALL DATDELET ('Data2')
         CALL DATDELET ('Data3')
C
         IF (ERROR) GO TO 999
  10  CONTINUE

C
      IF ((MODE .EQ. 'DIV' .OR. MODE .EQ. 'CLIP') 
     $     .AND. THRESH(2) .GT. 0.0 .AND. NMOSFILE.NE.' ') THEN
            CALL VISMOSPU ('Mos', NMOSFILE)
      ENDIF
C
 999  CONTINUE
      END
