C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)moslist.f	1.5	 11/22/94
C
      SUBROUTINE SDEMAIN
C
CD Program to list mosaic data
C
C Audit trail:
C	New task
C				T.J.Cornwell	Jan 24 1989
C	Added VISMOSGE
C				T.J. Cornwell	Feb 3 1989
C
C	Added SUMMARY MODE, added STOKES, added TELESCOP printing,
C	print number of visibilities in each pointing
C				M.A.Holdaway	April 1 1993
C	Fixed Nlist to be an INT
C				M.A. Holdaway	Nov 22 1994
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'MOSLIST')
C
      CHARACTER*(SYSMXNAM)	MOSFILE, OUTFILE, MOSS, STRM2, MODE
      CHARACTER*(SYSMXNAM)	TELESCOP, STOKES, SUB, STRM3
      REAL			TELDIAM
      INTEGER			NLIST
      LOGICAL			DATEXIST
      CHARACTER*6	STRINT
      DOUBLE PRECISION	OBSRA, OBSDEC
      INTEGER		NDUMMY, NPC, IPC, NAX, NAXIS(SYSMXDIM), ADD
      CHARACTER*1	T
C==================================================================
      CALL MSGWELCO ('I list mosaic visibility data')
      CALL USRCTL
C
C Get input parameters
C
      CALL USRGETC('Mos', MOSFILE, 1, NDUMMY)
      CALL USRGETC('Outfile', OUTFILE, 1, NDUMMY)
      CALL USRGETC('Stokes', STOKES, 1, NDUMMY)
      CALL USRGETC('Mode', MODE, 1, NDUMMY)
      CALL USRGETI('Nlist', NLIST, 1, NDUMMY)
C
      CALL VISMOSGE ('Mos', MOSFILE)
C
      SUB = 'OBS/'//STOKES(1:1)
      CALL TXTOPEN ('Listing', OUTFILE, 'WRITE')
      IF (MODE(1:1) .EQ. 'S') THEN
         WRITE(MESSAGE, 1200) MOSFILE
 1200    FORMAT ('Summary of visibilities in mosaic file ',A)
      ELSE
         WRITE (MESSAGE, 1201) MOSFILE
 1201    FORMAT ('Listing of visibilities in mosaic file ',A)
      ENDIF
      CALL TXTWRITE ('Listing', MESSAGE)
C
      CALL TXTWRITE ('Listing', 'Subclass = '//SUB)
      CALL TXTWRITE ('Listing', ' ')
C
      CALL DATGETI ('Mos', 'NPC', NPC, 1, NDUMMY)
      DO 10 IPC = 1, NPC
         CALL TXTWRITE ('Listing', ' ')
         WRITE (MESSAGE, 1000) IPC
 1000    FORMAT ('Pointing ',I3)
         CALL TXTWRITE ('Listing', MESSAGE)
         MOSS = STRM2 ('Mos', 'PC'//STRINT(IPC))
C
         CALL DATGETD (MOSS, 'OBSRA', OBSRA, 1, NDUMMY)
         CALL DATGETD (MOSS, 'OBSDEC', OBSDEC, 1, NDUMMY)
         WRITE (MESSAGE, 1100) OBSRA, OBSDEC
 1100    FORMAT ('Observed RA, DEC = ',F10.4,1X,F10.4)
         CALL TXTWRITE ('Listing', MESSAGE)
C
         CALL DATGETC (MOSS, 'TELESCOP', TELESCOP, 1, NDUMMY)
         IF (DATEXIST(STRM2(MOSS, 'TELDIAM'))) THEN
            CALL DATGETR (MOSS, 'TELDIAM', TELDIAM, 1, NDUMMY)
            WRITE (MESSAGE, 1300) TELESCOP(1:10), TELDIAM
 1300       FORMAT ('Observed with ', A10, ' with diameter ',F5.2,' m')
         ELSE
            WRITE (MESSAGE, 1301) TELESCOP(1:10)
 1301       FORMAT ('Observed with ',A10)
         ENDIF
         CALL TXTWRITE ('Listing', MESSAGE)
C
         CALL DATGETAR (STRM3(MOSS, SUB, 'VIS'), NAX, NAXIS, T, ADD)
         WRITE(MESSAGE, 1400) NAXIS(1)
 1400    FORMAT('There are ', I7, ' visibilities in this pointing')
         CALL TXTWRITE ('Listing', MESSAGE)
C
         IF (MODE(1:1) .NE. 'S') THEN
            CALL TXTWRITE ('Listing', ' ')
            CALL VISLISTB ('Listing', MOSS, SUB, NLIST, .TRUE.)
         ENDIF
         IF (ERROR) GOTO 999
 10   CONTINUE
      CALL TXTCLOSE ('Listing')
      CALL MSGPUT ('Mosaic Listing in text file '//OUTFILE, 'I')
C
 999  CONTINUE
      END
