C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)beamcalc.f	1.3    9/5/94
C
      SUBROUTINE SDEMAIN
C
CD Calculate gaussian (de)convolutions
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				D.S.Briggs	Oct 18 1993
C	Track changes to FILBEMGE
C				D.S.Briggs	Sept 3 1994
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'BEAMCALC')
C
      INTEGER 		NDUMMY
      CHARACTER*(SYSMXNAM)	INBFILE, OUTBFILE, TFILE, OUTFILE,
     $   		MODE, FITMODE
      REAL		INBEAM(4), OUTBEAM(4), TAPER(4)
C==================================================================
      CALL MSGWELCO ('I calculate Gaussian (de)convolution parameters')
      CALL USRCTL
C
C Get inputs
C
      CALL USRGETR ('InBeam', INBEAM, 4, NDUMMY)
      CALL USRGETC ('InBFile', INBFILE, 1, NDUMMY)
      CALL USRGETR ('OutBeam', OUTBEAM, 4, NDUMMY)
      CALL USRGETC ('OutBFile', OUTBFILE, 1, NDUMMY)
      CALL USRGETR ('Taper', TAPER, 4, NDUMMY)
      CALL USRGETC ('TFile', TFILE, 1, NDUMMY)
      CALL USRGETC ('Mode', STRBUF, 1, NDUMMY)
      CALL USRGETC ('FitMode', FITMODE, 1, NDUMMY)
      CALL STRUC (STRBUF, MODE)
      CALL USRGETC ('Outfile', OUTFILE, 1, NDUMMY)
C
      IF (MODE.EQ.' ') MODE = 'CONV'
      IF ((MODE(1:3).NE.'CON').AND.(MODE(1:3).NE.'DEC')) THEN
         CALL MSGPUT ('MODE forced to CONV','W')
         MODE = 'CONV'
      END IF
C
      CALL MSGPUT ('Input Beam:','I')
      CALL FILBEMGE (INBFILE, INBEAM, FITMODE, 'TAPER')
      IF (MODE(1:3).EQ.'CON') THEN
         CALL MSGPUT ('Taper:','I')
         CALL FILBEMGE (TFILE, TAPER, FITMODE, 'TAPER')
      ELSE
         CALL MSGPUT ('Output Beam:','I')
         CALL FILBEMGE (OUTBFILE, OUTBEAM, FITMODE, 'TAPER')
      END IF
C
C Do the (de)convolution
C
      CALL MSGPUT (' ','I')
      IF (MODE(1:3).EQ.'CON') THEN
         CALL MSGPUT ('Convolving INBEAM with TAPER','I')
      ELSE
         CALL MSGPUT ('Deconvolving INBEAM from OUTBEAM giving TAPER',
     $      'I')
         TAPER(1) = -INBEAM(1)
         INBEAM(1) = OUTBEAM(1)
         TAPER(2) = -INBEAM(2)
         INBEAM(2) = OUTBEAM(2)
         TAPER(3) = INBEAM(3)
         INBEAM(3) = OUTBEAM(3)
      END IF
      CALL UTLGCONV (INBEAM, TAPER, OUTBEAM)
      IF (ERROR) GO TO 999
C
C Write out answer
C
      CALL MSGPUT (' ','I')
      WRITE (MESSAGE, 2000) OUTBEAM(1), OUTBEAM(2), OUTBEAM(3)
 2000 FORMAT ('Result = ',1P2E13.5,0PF9.3)
      CALL MSGPUT (MESSAGE, 'I')
      IF (OUTFILE.NE.' ') THEN
         CALL TXTOPEN ('Output', OUTFILE, 'WRITE')
         WRITE (MESSAGE, 2010) OUTBEAM(1), OUTBEAM(2), OUTBEAM(3)
 2010 FORMAT (' Filter = ',1PE13.5,',',E13.5,',',E13.5)
         CALL TXTWRITE ('Output', MESSAGE)
         CALL TXTCLOSE ('Output')
      END IF
C
 999  CONTINUE
      END
