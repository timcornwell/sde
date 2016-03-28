C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)sd2mos.f	1.4	 4/8/93
C
      SUBROUTINE SDEMAIN
C
CD Convert SD image into a Mosaic Database
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	March 25 1993
C	Added WT
C				M.A.H.		March 25 1993
C	Changed 'Amtfile' to 'Antfile' (how did it ever work before?)
C				M.A.H.		April 3 1993
C	Added Stokes
C				M.A.H.		April 6 1993
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'SD2MOS')
C
      CHARACTER*(SYSMXNAM)	SDIMAGE, MOS, TELE, ANTFILE, BUNIT
      CHARACTER*(SYSMXNAM)	STOKES
      REAL			TELDIAM, FREQ, WT
      INTEGER			BLC(2), TRC(2), SKIP(2)
C
      INTEGER			NDUMMY
C==================================================================
C
      CALL MSGWELCO ('I make Mosaic Databases from 2-D Single Dish Img')
      CALL USRCTL
C
      CALL USRGETC('SDImage', SDIMAGE, 1, NDUMMY)
      CALL USRGETC('Stokes', STOKES, 1, NDUMMY)
      CALL USRGETC('MosOut', MOS, 1, NDUMMY)
      CALL USRGETC('Telescop', TELE, 1, NDUMMY)
      CALL USRGETR('Teldiam', TELDIAM, 1, NDUMMY)
      CALL USRGETR('Freq', FREQ, 1, NDUMMY)
      CALL USRGETR('Wt', WT, 1, NDUMMY)
      CALL USRGETC('Antfile', ANTFILE, 1, NDUMMY)
      CALL USRGETI('BLC', BLC, 2, NDUMMY)
      CALL USRGETI('TRC', TRC, 2, NDUMMY)
      CALL USRGETI('SKIP', SKIP, 2, NDUMMY)
C
      CALL FILIMGGE ('SDImage', SDIMAGE, ' ')
      CALL DATGETC ('SDImage', 'BUNIT', BUNIT, 1, NDUMMY)
      IF (BUNIT .NE. 'JY/BEAM') THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $      'SD MAP Units must be JY/BEAM')
         GOTO 999
      ENDIF
      CALL FILGETAN ('Antfile', ANTFILE)
      CALL DATCREAT ('Mos')
C
      IF (ERROR) GOTO 999
      CALL ARRSD2MS ('SDImage', 'Mos', 'Antfile', FREQ, WT, BLC, TRC, 
     $   SKIP, TELE, TELDIAM, STOKES)
C
      IF (MOS.NE.' ') THEN
         CALL MSGPUT ('Writing new mosaic file as SDE file', 'I')
         CALL VISMOSPU ('Mos', MOS)
      ENDIF
C
 999  CONTINUE
      END





