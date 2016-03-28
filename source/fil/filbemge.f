C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)filbemge.f	1.4    6/14/94
C
      SUBROUTINE FILBEMGE (BEAMFILE, BEAM, FITMODE, TYPE)
C
CD Get beam/taper parameters.
C
C	BEAMFILE CH*(*)	input	Name of beam file/image
C	BEAM	R(4)	output	Parameters of Gaussian Beam
C	FITMODE	CH*(*)	input	'LINEAR', 'NONLINEAR', 'ARCHIVE'
C				'HDR-LINEAR', 'HDR-NONLINEAR', 'HDR-ARCHIVE'
C	TYPE	CH*(*)	input	'BEAM' or 'TAPER'
C
C The only difference between BEAM and TAPER is that negative or zero
C values for a BEAM will generate an error, but are acceptable for a TAPER
C
C If BEAMFILE does not exist as a directory entry, it is read in from disk
C as a datafile.  If a beam exists in the header, it is extracted.  Otherwise
C IMGBMSHP is called.  If the file was read in from disk, then the
C db entry is deleted.
C
C modes LINEAR, NONLINEAR, ARCHIVE will always try to fit from the image
C and will ignore the header information.  The HDR- modes will use the
C header information first, and only fit if needed.  The default, and
C previous behavior, is HDR-LINEAR.  For convenience, FITMODE may also be
C abbreviated L, NL, A, HL, HNL, HA
C
C Audit trail:
C	Original version
C				D.S.Briggs	1 May 1992
C	Tweaked up explicit beam override a little
C				D.S.Briggs	21 Feb 1993
C	Change warning for negative beam sizes -- they're now legal
C	and correspond to inverse tapering.
C				D.S.Briggs	Oct 22 1993
C	Pass in flags to indicate whether we want a beam or a taper,
C	and what sort of fit we should do if we have to.
C				D.S.Briggs	June 12 1994
C-------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*) 	BEAMFILE, TYPE, FITMODE
      REAL		BEAM(4)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FILBEMGE')
C
      CHARACTER*(SYSMXNAM)	NAME, TTYPE, TFITMODE
      LOGICAL		ISFIT
C
      CHARACTER*(SYSMXNAM)	STRM2
      LOGICAL		DATEXIST
      REAL		DATFGETR
      INTEGER		STRLEN
C==================================================================
      IF (ERROR) GO TO 999
C
      TTYPE = TYPE
      IF (TTYPE.EQ.' ') TTYPE = 'BEAM'
      TFITMODE = FITMODE
      IF (FITMODE.EQ.'L') TFITMODE = 'LINEAR'
      IF (FITMODE.EQ.'NL') TFITMODE = 'NONLINEAR'
      IF (FITMODE.EQ.'A') TFITMODE = 'ARCHIVE'
      IF (FITMODE.EQ.'HL') TFITMODE = 'HDR-LINEAR'
      IF (FITMODE.EQ.'HNL') TFITMODE = 'HDR-NONLINEAR'
      IF (FITMODE.EQ.'HA') TFITMODE = 'HDR-ARCHIVE'
      IF (FITMODE.EQ.' ') TFITMODE = 'HDR-LINEAR'
C
      IF ((TTYPE.NE.'BEAM').AND.(TTYPE.NE.'TAPER')) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Unrecognized TYPE')
         GO TO 999
      END IF
C
      IF ((TFITMODE.NE.'HDR-LINEAR').AND.(TFITMODE.NE.'HDR-NONLINEAR')
     $    .AND.(TFITMODE.NE.'HDR-ARCHIVE').AND.(TFITMODE.NE.'LINEAR')
     $    .AND.(TFITMODE.NE.'NONLINEAR').AND.(TFITMODE.NE.'ARCHIVE'))
     $   THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Unrecognized FITMODE')
         GO TO 999
      END IF
C
      ISFIT = .FALSE.
      IF ((BEAMFILE.EQ.' ').OR.(BEAM(1).NE.0.0).OR.
     $     (BEAM(2).NE.0.0)) THEN
         CALL MSGPUT ('Using explicit BEAM','I')
         GO TO 800
      END IF
C
      IF (DATEXIST(BEAMFILE)) THEN
         NAME = BEAMFILE
      ELSE
         NAME = 'TempBeam'
         CALL FILIMGGE (NAME, BEAMFILE, ' ')
      END IF
C
      IF (DATEXIST(STRM2(NAME,'BMAJ')).AND.
     $   (TFITMODE(1:4).EQ.'HDR-')) THEN
         CALL MSGPUT ('Using precalculated BEAM from BEAMFILE','I')
      ELSE
         IF (TFITMODE(1:4).EQ.'HDR-') THEN
            STRBUF = TFITMODE(5:)
            TFITMODE = STRBUF
         END IF
         CALL MSGPUT ('Must fit BEAM to PSF','I')
         MESSAGE = 'Using ' // TFITMODE(1:STRLEN(TFITMODE)) //
     $      ' fitting algorithm'
         CALL MSGPUT (MESSAGE,'I')
         CALL DATPUTC (NAME, 'FIT-ALG', TFITMODE, 1)
         CALL IMGBMSHP (NAME)
         ISFIT = .TRUE.
      END IF
      IF (ERROR) GO TO 990
C
      BEAM(1) =  DATFGETR(NAME, 'BMAJ')*3600.0
      BEAM(2) =  DATFGETR(NAME, 'BMIN')*3600.0
      BEAM(3) =  DATFGETR(NAME, 'BPA')
      BEAM(4) =  DATFGETR(NAME, 'BZ')*3600.0
C
 800  CONTINUE
      IF ((BEAM(1).EQ.0.0).OR.(BEAM(2).EQ.0.0))
     $   CALL MSGPUT ('Questionable Beam Size!','W')
      IF ((BEAM(1).LT.0.0).OR.(BEAM(2).LT.0.0)) THEN
         IF (TTYPE.EQ.'TAPER') THEN
            CALL MSGPUT ('Using inverse taper','I')
         ELSE
            CALL ERRREPOR (ERRBDARG, ROUTINE,
     $         'Can''t inverse taper a BEAM')
         END IF
      END IF
C
 900  CONTINUE
      IF (.NOT.ISFIT) THEN
 1900    FORMAT ('Beam (FWHM in asec) = ',2F12.5,F7.1,F12.5)
         IF (BEAM(4).EQ.0.0) THEN
            WRITE (MESSAGE, 1900) BEAM(1), BEAM(2), BEAM(3)
         ELSE
            WRITE (MESSAGE, 1900) BEAM(1), BEAM(2), BEAM(3), BEAM(4)
         END IF
         CALL MSGPUT (MESSAGE, 'I')
      END IF
C
      IF (NAME.EQ.'TempBeam') CALL DATDELET (NAME)
C
 990  CONTINUE
      IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
 999  CONTINUE
      END
