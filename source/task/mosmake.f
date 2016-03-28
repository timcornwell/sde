C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)mosmake.f	1.7	 7/17/95
C
      SUBROUTINE SDEMAIN
C
CD Program to assemble uv data into a mosaic
C
C Audit trail:
C	Now copies OBSRA, OBSDEC if not in header
C					T.J. Cornwell April 30 1991
C	Now can read input VIS file names from a TXT File
C					M>A> HOLDAWY	May 1 1995
C	Option to delete stokes Q and U from the mosaic database
C					M.A. Holdaway	July 17 1995
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'MOSMAKE')
C
      INTEGER 		NDB, IDB, MAXINP, MAXLIST, NDUMMY
      PARAMETER         (MAXINP=100)
      PARAMETER         (MAXLIST=500)
C
      CHARACTER*(SYSMXNAM)	VISINP(MAXINP), STRM2, MOSFILE, IVIS,
     $                          TELESCOP, LISTFILE, VISLIST(MAXLIST),
     $     			STRM3
      INTEGER			I, STRSEARC
      CHARACTER*8		CTYPE(SYSMXDIM)
      DOUBLE PRECISION		CRVAL(SYSMXDIM)
      LOGICAL			DATEXIST
      REAL                      REFDATE
      CHARACTER*6               STRINT
      CHARACTER*132	LINE
      LOGICAL		EOF, QUDEL
      INTEGER		NCHAR, STRLEN
C==================================================================
      CALL MSGWELCO ('I assemble u,v data into mosaic data bases')
      CALL USRCTL
C
C Get input parameters
C
      CALL USRGETC ('Vis', VISINP, MAXINP, NDB)
      CALL USRGETC ('ListFile', LISTFILE, 1, NDUMMY)
      CALL USRGETC ('Mosaic', MOSFILE, 1, NDUMMY)
      CALL USRGETC ('Telescope', TELESCOP, 1, NDUMMY)
      CALL USRGETL ('DeletePOL', QUDEL, 1, NDUMMY)
C
      IF (LISTFILE .EQ. ' ') THEN
         CALL MSGPUT ('Vis Files determined by INPUTS', 'I')
         DO 2 IDB = 1, MAXINP
            VISLIST(IDB) = VISINP(IDB)
            IF(VISINP(IDB).EQ.' ') THEN 
               NDB = IDB-1
               GO TO 3
            END IF
 2       CONTINUE
 3       CONTINUE
      ELSE         
         CALL MSGPUT ('Vis Files determined by List File: '//LISTFILE,
     $        'I')
         NDB = 0
         CALL TXTOPEN (ROUTINE, LISTFILE, 'READ')
 1       CONTINUE
            CALL TXTREAD (ROUTINE, LINE, NCHAR, EOF)
            IF (ERROR) GOTO 999
            IF (EOF) GO TO 50
            IF (LINE(1:1).EQ.'#') GO TO 1
            NDB = NDB+1
            IF (NDB .GT. MAXLIST) THEN
               CALL ERRREPOR (ERRLOGIC, ROUTINE,
     $              'More than 100 pointings in '//LISTFILE)
               GOTO 999
            ENDIF
            READ (LINE(:STRLEN(LINE)), *, ERR = 200) VISLIST(NDB)
         GOTO 1
 50      CONTINUE
         CALL TXTCLOSE (ROUTINE)
      ENDIF
C
      CALL DATCREAT ('M')
      CALL DATPUTI ('M', 'NPC', NDB, 1)
      DO 10 IDB = 1, NDB
         IVIS = 'M/PC'//STRINT(IDB)
         CALL DATCREAT (IVIS)
         IF (IDB.NE.1) CALL DATPUTR (IVIS, 'REFDATE', REFDATE, 1)
         CALL VISGET (IVIS, VISLIST(IDB), '*', '*', ' ')
         IF (IDB.EQ.1) CALL DATGETR (IVIS, 'REFDATE', REFDATE, 1,
     $      NDUMMY)
         IF (QUDEL) THEN
            CALL DATDELET (STRM3( IVIS, 'OBS', 'Q' ))
            CALL DATDELET (STRM3( IVIS, 'OBS', 'U' ))
         ENDIF
         IF (TELESCOP.NE.' ') THEN
            CALL DATPUTC (IVIS, 'TELESCOP', TELESCOP, 1)
         END IF
         CALL DATGETD (IVIS, 'CRVAL', CRVAL, SYSMXDIM, NDUMMY)
         CALL DATGETC (IVIS, 'CTYPE', CTYPE, SYSMXDIM, NDUMMY)
         IF (.NOT.DATEXIST(STRM2(IVIS, 'OBSRA'))) THEN
            I = STRSEARC('RA', CTYPE, SYSMXDIM)
            CALL DATPUTD (IVIS, 'OBSRA', CRVAL(I), 1)
         END IF
         IF (.NOT.DATEXIST(STRM2(IVIS, 'OBSDEC'))) THEN
            I = STRSEARC('DEC', CTYPE, SYSMXDIM)
            CALL DATPUTD (IVIS, 'OBSDEC', CRVAL(I), 1)
         END IF
 10   CONTINUE
C
      CALL DATSETTP ('M', 'VISMOSAIC')
C
      CALL VISMOSPU ('M', MOSFILE)
      GOTO 999
 200  CONTINUE
      CALL ERRREPOR(ERRBDARG, ROUTINE,
     $     'Error reading from textfile: '//LINE)
      GOTO 999
C
 999  CONTINUE
      END
