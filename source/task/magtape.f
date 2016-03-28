C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)magtape.f	1.3    3/23/91
C
      SUBROUTINE SDEMAIN
C
CD Program to perform magtape handling.
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'MAGTAPE')
C
      INTEGER		NDUMMY, STAT, SYSSYSTM, GETPID, IFILE, NFILE,
     1			NSKIP, DENSITY, LISTLUN, FILGETLU
      LOGICAL		STRMATCH, EOF
      CHARACTER*(SYSMXNAM)	GOCMD
      CHARACTER*6	STRINT
      CHARACTER*256	SYSCMD
      CHARACTER*(SYSMXNAM)	SDETAPE, FILENAME, TRNSFLNM,
     1			TMPFLNAM, LISTFILE, LNAME
      DATA		SDETAPE		/'Sdetape.'/
      DATA		LISTFILE	/'Listfile.'/
C==================================================================
C
C Make unique names
C
      CALL STRAPPEN (SDETAPE, STRINT (GETPID(NDUMMY)))
      CALL STRAPPEN (LISTFILE, STRINT (GETPID(NDUMMY)))
C
      CALL MSGWELCO ('I handle magnetic tapes')
 1    CONTINUE
C
      CALL USRCTL
C
C Get input parameters
C
      CALL USRGETI ('Nfiles', NFILE, 1, NDUMMY)
      CALL USRGETI ('Nskip', NSKIP, 1, NDUMMY)
      CALL USRGETI ('Density', DENSITY, 1, NDUMMY)
      CALL USRGETC ('File', FILENAME, 1, NDUMMY)
      CALL USRGETC ('Tape', LNAME, 1, NDUMMY)
      IF (ERROR) GO TO 999
C
C Set defaults
C
      IF (LNAME.NE.' ') THEN
         SDETAPE = LNAME
      END IF
      IF (FILENAME.NE.' ') THEN
         CALL SYSTRANS (FILENAME, TRNSFLNM)
      ELSE
         TRNSFLNM = 'Sdefile'
      END IF
C
C Get go command
C
      CALL USRGETGO (GOCMD)
C
C ******************* Start of command execution ******************
C
C Mounting tape
C
      IF (STRMATCH('mount',GOCMD)) THEN
         CALL MSGPUT ('Allocating tape drive', 'I')
         SYSCMD = 'tpalloc -r -d '//STRINT(DENSITY)//' '//SDETAPE
         STAT = SYSSYSTM (SYSCMD)
C
C Dismounting tape
C
      ELSEIF (STRMATCH('dismount',GOCMD)) THEN
         CALL MSGPUT ('Deallocating tape drive', 'I')
         SYSCMD = 'tpdealloc -k '//SDETAPE
         STAT = SYSSYSTM (SYSCMD)
C
C Status of tape
C
      ELSEIF (STRMATCH('status',GOCMD)) THEN
         CALL MSGPUT ('Status of tape drive', 'I')
         SYSCMD = 'sdemt -f '//SDETAPE
         CALL STRAPPEN (SYSCMD, ' status')
         STAT = SYSSYSTM (SYSCMD)
C
C Rewind tape
C
      ELSEIF (STRMATCH('rewind',GOCMD)) THEN
         CALL MSGPUT ('Rewinding tape drive', 'I')
         SYSCMD = 'sdemt -f '//SDETAPE
         CALL STRAPPEN (SYSCMD, ' rewind')
         STAT = SYSSYSTM (SYSCMD)
C
C Skip files
C
      ELSEIF (STRMATCH('skip',GOCMD)) THEN
         CALL MSGPUT ('Skipping '//STRINT(NSKIP)//
     1      'files on tape drive', 'I')
         SYSCMD = 'sdemt -f '//SDETAPE
         IF (NSKIP.GT.0) THEN
            CALL STRAPPE2 (SYSCMD, ' fsf ', STRINT(NSKIP))
         ELSE
            CALL STRAPPE2 (SYSCMD, ' bsf ', STRINT(-NSKIP))
         END IF
         STAT = SYSSYSTM (SYSCMD)
C
C List files
C
      ELSEIF (STRMATCH('list',GOCMD)) THEN
         CALL MSGPUT ('Listing selected files', 'I')
         SYSCMD = 'ls -l1rt '//TRNSFLNM
         CALL STRAPPEN (SYSCMD,' | more')
         STAT = SYSSYSTM (SYSCMD)
C
C Read files
C
      ELSEIF (STRMATCH('load',GOCMD)) THEN
         CALL MSGPUT ('Loading files', 'I')
         DO 100 IFILE = 1, NFILE
            SYSCMD = 'sdedd ibs=2880 obs=2880 if='//SDETAPE
            TMPFLNAM = TRNSFLNM
            IF (NFILE.GT.1) THEN
               CALL STRAPPEN (TMPFLNAM, STRINT(IFILE))
            END IF
            CALL STRAPPE2 (SYSCMD,' of=',TMPFLNAM)
            CALL MSGPUT ('Loading '//TMPFLNAM, 'I')
            STAT = SYSSYSTM (SYSCMD)
 100     CONTINUE
C
C Write files
C
      ELSEIF (STRMATCH('save',GOCMD)) THEN
         CALL MSGPUT ('Saving files:', 'I')
         SYSCMD = 'ls -1rt '//TRNSFLNM
         CALL STRAPPE2 (SYSCMD,' > ',LISTFILE)
         STAT = SYSSYSTM (SYSCMD)
         IFILE = 0
         CALL TXTOPEN ('List', LISTFILE, 'READ')
         IF (ERROR) GO TO 990
 200     CONTINUE
            CALL TXTREAD('List', TRNSFLNM, NDUMMY, EOF)
            IF (EOF) GO TO 210
            CALL MSGPUT ('Saving '//TRNSFLNM, 'I')
            IFILE = IFILE + 1
            SYSCMD = 'sdedd ibs=2880 obs=2880 of='//SDETAPE
            CALL STRAPPE2 (SYSCMD,' if=',TRNSFLNM)
            STAT = SYSSYSTM (SYSCMD)
            IF (IFILE.GE.NFILE) GO TO 210
            GO TO 200
 210     CONTINUE
         CALL MSGPUT ('Wrote '//STRINT(IFILE)//' files', 'I')
         CALL TXTCLOSE ('List')
         SYSCMD = 'rm '//LISTFILE
         STAT = SYSSYSTM (SYSCMD)
C
C Not known !
C
      ELSE
         CALL MSGPUT ('Unknown command: '//GOCMD, 'W')
         SYSCMD = GOCMD
         STAT = SYSSYSTM (SYSCMD)
      END IF
      GO TO 1
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
