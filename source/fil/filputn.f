C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)filputn.f	1.1	 10/4/94
C
      SUBROUTINE FILPUTN (NAME, FILE, NWRITE)
C
CD Write N arrays hanging off NAME to an ASCII file
C
C	NAME	CH*(*)	inp	Directory to write
C	FILE	CH*(*)	inp	Output File Name
C	NWRITE	INT	inp	Write this many lines (<=0 => all)
C
C	We assume that NAME has an array of characters,
C	NAME/WRITENAMES (if WRITENAMES isn't there, it looks
C	in ARRAYNAMES)
C	
C	and N arrays, STRM2(NAME, WRITENAMES(I))
C
C
C       4 
C       'X'    'Y'     'Z'    'DIAM'
C       # This is a comment
C       21.3   52.1    72.3   8.0
C       -16.2  15.7    14.2   26.0
C       11.3   22.1    52.3   8.0
C
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A. Holdaway	April 27 1994
C
C---------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	NAME, FILE
      INTEGER		NWRITE
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FILPUTN')
C
      CHARACTER*1	ATYPE
      CHARACTER*(SYSMXNAM)	STRM2
      CHARACTER*132	LINE, COMMENT
      INTEGER		MAXTHING, NTHINGS, ITHING, IROW
      PARAMETER		(MAXTHING = SYSMXDIM)
      INTEGER		NAX, NAXIS(MAXTHING), NROWS, NDUMMY
      CHARACTER*8	ARRNAMES(MAXTHING)
      INTEGER		IADD(MAXTHING)
      CHARACTER*15	HEADER(MAXTHING), TEMP
C
      DATA		ARRNAMES / MAXTHING * ' '/
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999

C
      CALL DATGETC (NAME, 'WRITENAMES', ARRNAMES, MAXTHING, NDUMMY)
      IF (ERROR) THEN
         CALL ERRCANCE
         CALL DATGETC (NAME, 'ARRAYNAMES', ARRNAMES, MAXTHING, NDUMMY)
      ENDIF
      NTHINGS = MAXTHING
      DO 5 ITHING=MAXTHING, 1, -1
         IF (ARRNAMES(ITHING) .EQ. ' ') NTHINGS = ITHING-1
 5    CONTINUE
C
C Write Header
C
      CALL TXTOPEN (ROUTINE, FILE, 'WRITE')
      WRITE (LINE, *) NTHINGS
      CALL TXTWRITE (ROUTINE, LINE)
C
      DO 10 ITHING = 1, NTHINGS
         TEMP = ''''//ARRNAMES(ITHING)
         TEMP(INDEX(TEMP, ' '):INDEX(TEMP, ' '))= ''''
         HEADER(ITHING) = TEMP
 10   CONTINUE
      WRITE (LINE, *) (HEADER(ITHING), ITHING = 1, NTHINGS)
      CALL TXTWRITE (ROUTINE, LINE)
C
      IF (ERROR) GOTO 990
      CALL DATGETC (NAME, 'COMMENT', COMMENT, 1, NDUMMY)
      IF (ERROR) THEN
         CALL ERRCANCE
      ELSE
         CALL TXTWRITE (ROUTINE, COMMENT)
      ENDIF
C
C Get and Check the Data
C     
      DO 20 ITHING = 1, NTHINGS
         CALL DATGETAR (STRM2(NAME, ARRNAMES(ITHING)),
     $        NAX, NAXIS, ATYPE, IADD(ITHING))
         IF (ITHING .GT. 1 .AND. NAXIS(1) .NE. NROWS) THEN
            CALL ERRREPOR (ERRLOGIC, ROUTINE,
     $           'Arrays have inconsistent lengths')
            GOTO 990
         ENDIF
         IF (NAX .GT. 1) THEN
            CALL ERRREPOR (ERRLOGIC, ROUTINE,
     $           'Can only deal with 1-D arrays')
            GOTO 990
         ENDIF
         NROWS = NAXIS(1)
 20   CONTINUE
      IF (NWRITE .LE. 0) NWRITE = NROWS
C
C Write the Data
C
      DO 100 IROW = 1, NWRITE
         WRITE (LINE, 27) (MEMR(IADD(ITHING)+IROW-1), 
     $        ITHING = 1, NTHINGS)
         CALL TXTWRITE (ROUTINE, LINE)
  100 CONTINUE
 27   FORMAT (10(F12.5))
      CALL TXTCLOSE(ROUTINE)
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
