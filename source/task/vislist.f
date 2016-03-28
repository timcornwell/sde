C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)vislist.f	1.10    11/27/93
C
      SUBROUTINE SDEMAIN
C
CD Program to list visibility data
C
C Audit trail:
C	New task
C				T.J.Cornwell	Jan 24 1989
C      Open outfile explicitly
C				T.J.Cornwell	March 20 1989
C	Vislist can now sort by baseline
C				M.A.Holdaway	???
C       Add filename to output
C                               D.S.Briggs      August 14 1992
C	Added STOKES to input list
C				M.A.Holdaway	Sept 9 1992
C	Actually made STOKES work
C				T.J.Cornwell	Sept 14 1992
C	Added simple data selection
C				D.S.Briggs	May 25 1993
C	Added OVERWRITE option
C				D.S.Briggs	Nov 27 1993
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISLIST')
C
      CHARACTER*(SYSMXNAM)	VISFILE, OUTFILE, ORDER, STOKES,
     $   			SUBCLASS
      INTEGER		NDUMMY, NS, IS, TIMR(8), NSEL, NLIST
      REAL		TIME(2), UVLIMITS(2)
      LOGICAL		DOALL, DOOVR
C
      LOGICAL		DATEXIST
      INTEGER		STRLEN
C==================================================================
      CALL MSGWELCO ('I list visibility data')
      CALL USRCTL
C
C Get input parameters
C
      CALL USRGETC('Vis', VISFILE, 1, NDUMMY)
      CALL USRGETC('Outfile', OUTFILE, 1, NDUMMY)
      CALL USRGETC('Order', ORDER, 1, NDUMMY)
      CALL USRGETR('Uvlimits', UVLIMITS, 2, NDUMMY)
      CALL USRGETI('Timerange', TIMR, 8, NDUMMY)
      CALL USRGETC('Stokes', STOKES, 1, NDUMMY)
      CALL USRGETI('NList', NLIST, 1, NDUMMY)
      CALL USRGETL('DoAll', DOALL, 1, NDUMMY)
      CALL USRGETL('Overwrite', DOOVR, 1, NDUMMY)
C
      IF (DOALL) THEN
         WRITE (MESSAGE, 1001) NLIST
 1001    FORMAT ('Will list ALL visibilities  (Maximum of',I7,')')
      ELSE
         WRITE (MESSAGE, 1002) NLIST
 1002    FORMAT ('Will list visibilities with weight>0  ',
     $      '(Maximum of',I7,')')
      END IF
      CALL MSGPUT (MESSAGE, 'I')
C
      CALL VISGET ('Vis', VISFILE, STOKES, '*', ' ')
      IF(DATEXIST('Vis/TIME')) THEN
         CALL UTLTTOD (TIMR(1), TIME(1))
         CALL UTLTTOD (TIMR(5), TIME(2))
         DO 50 IS = 1, 4
            IF (STOKES(IS:IS).NE.' ') THEN
               SUBCLASS = 'OBS/' // STOKES(IS:IS)
               CALL VISSEL ('Vis', SUBCLASS, TIME, UVLIMITS, NSEL)
               IF (ERROR) GO TO 999
               WRITE (MESSAGE, 1000) NSEL
 1000          FORMAT ('Selected ',I7,' visibilities')
               CALL MSGPUT (MESSAGE, 'I')
            END IF
 50      CONTINUE
      ELSE
         CALL MSGPUT ('No TIME information', 'W')
      END IF
C
      IF (DOOVR) CALL FILDEL (OUTFILE)
      CALL TXTOPEN ('Listing', OUTFILE, 'WRITE')
      MESSAGE = 'Listing to ' // OUTFILE
      CALL MSGPUT (MESSAGE, 'I')
C
      MESSAGE = 'Filename = "' // VISFILE(1:STRLEN(VISFILE)) // '"'      
      CALL TXTWRITE ('Listing', MESSAGE)
      MESSAGE = ' '
      CALL TXTWRITE ('Listing', MESSAGE)
C
      DO 100 IS = 1, 4
         IF(STOKES(IS:IS).EQ.' ') GOTO 110
         MESSAGE = 'Stokes = "' // STOKES(IS:IS) // '"'
         CALL TXTWRITE ('Listing', MESSAGE)
         MESSAGE = ' '
         CALL TXTWRITE ('Listing', MESSAGE)
         MESSAGE = 'Listing Stokes ' // STOKES(IS:IS)
         CALL MSGPUT (MESSAGE, 'I')
         IF (ORDER(1:4) .EQ. 'TIME') THEN
            CALL VISLISTT ('Listing', 'Vis', 'OBS/'//STOKES(IS:IS),
     $         NLIST, DOALL)
         ELSE
            CALL VISLISTB ('Listing', 'Vis', 'OBS/'//STOKES(IS:IS),
     $         NLIST, DOALL)
         ENDIF
         MESSAGE = ' '
         CALL TXTWRITE ('Listing', MESSAGE)
 100  CONTINUE
 110  CONTINUE
      CALL TXTCLOSE ('Listing')
C
 999  CONTINUE
      END
