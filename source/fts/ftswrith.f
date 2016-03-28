C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)ftswrith.f	1.8	 3/18/91
C
      SUBROUTINE FTSWRITH (NAME)
C
CD Write out header to FITS file.
C
C 	NAME	CH*(*)	input	Name of directory entry
C
C Audit trail:
C	HISTORY cards have program name inserted here instead of
C	in HISPUT as before.
C				T.J.Cornwell	Jan 9 1989
C	Changed FTIPARSE to FTIPARSO.
C				T.J.Cornwell	Jan 13 1989
C	Don't write program name as part of history card
C				T.J.Cornwell	July 10 1990
C	Make sure the last block is ALWAYS written
C				R.G. Marson	Nov 20 1990
C	Error in clearing FTSBUFF: was overwriting ROTA
C	when the header information exceeded 1 buffer
C				M.A.Holdaway	Feb 27 1991
C	Previously found error was a bit more subtle than 
C	thought: failed on IBM because the last statement
C	was writing beyond the end of the buffer
C				T.J.Cornwell    March 3 1991
C	Introduce convention that PTYPE's with #'s get them reset
C	to #. Needed to write duplicate name arrays properly.
C				T.J.Cornwell    March 18 1991
C
C---------------------------------------------------------------------
#include	"stdinc.h"
C
#include	"ftsinc.h"
C
      CHARACTER*(*)	NAME
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FTSWRITH')
C
      CHARACTER			KEYWORD*(FTSLNKEY)
      CHARACTER			OBJECT*(FTSLNOBJ)
      CHARACTER			KTYPE*1
C
      REAL		RVALUE
      INTEGER		IVALUE
      DOUBLE PRECISION	DVALUE
      LOGICAL		LVALUE
      COMPLEX		XVALUE
      CHARACTER*(SYSMXNAM)	CVALUE
C
      INTEGER		NCARD, ICARD, ISTART, STARTCRD
      INTEGER		FILEID, DATFGETI, ANVALS, I
C
      INTEGER		IAX, NAX, NAXIS(SYSMXDIM), AADD
      REAL		RPIX (SYSMXDIM), DELT (SYSMXDIM),
     1			ROTA (SYSMXDIM)
      DOUBLE PRECISION	RVAL (SYSMXDIM)
      CHARACTER*8	TYPE (SYSMXDIM)
      CHARACTER*1	ATYPE
C
      CHARACTER*(FTSLEN)	CARD, HISCARD
      INTEGER		NIO, IHISCARD
      INTEGER		FTSBUFF (FTSBLOCK / SYSCHINT)
      CHARACTER*(SYSMXNAM)	STRTMP, STRM2
      LOGICAL		STRMATCH, DATEXIST, XTENSION
C====================================================================
C
      IF (ERROR) GO TO 999
C
C Put origin info
C
      CALL DATPUTC (NAME, 'ORIGIN', SYSORIG, 1)
      CALL SYSDATEC (CVALUE)
      CALL DATPUTC (NAME, 'DATE', CVALUE, 1)
      CALL DATPUTL (NAME, 'BLOCKED', .TRUE., 1)
C
C Is this an extension?
C
      XTENSION = DATEXIST (STRM2(NAME, 'XTENSION'))
C
C Get array info
C
      IF(.NOT.XTENSION) THEN
         CALL FTIINITI('SIMPLE')
         CALL CRDGET (NAME, NAX, TYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
         CALL DATGETAR (NAME, NAX, NAXIS, ATYPE, AADD)
      ELSE 
         CALL FTIINITI('XTENSION')
      END IF
C
C Get logical unit number for access
C
      FILEID = DATFGETI (NAME, 'FILEID')
      IF (ERROR) GO TO 990
C
C Initialize buffer
C
      CARD = ' '
      DO 10 ICARD = 1, FTSCARD
         ISTART = (ICARD-1)*FTSLEN/SYSCHINT + 1
         CALL STRCHINT (CARD, FTSBUFF (ISTART), FTSLEN)
  10  CONTINUE
C
C Start reading in FITS blocks 
C
      ICARD = 0
      NCARD = 0
      IHISCARD = 0
C
   1  CONTINUE
      KEYWORD = FTSKEYS (1, NCARD+1)
      KTYPE   = FTSKEYS (2, NCARD+1)
      CARD = ' '
C
C All done ?
C
      IF (KEYWORD.EQ.' ') THEN
         NCARD = NCARD + 1
         GO TO 1
      ELSEIF (KEYWORD.EQ.'END') THEN
         GO TO 200
      ELSEIF (KEYWORD.EQ.'SIMPLE') THEN
         LVALUE = .TRUE.
      ELSEIF (KEYWORD.EQ.'NAXIS') THEN
         IVALUE = NAX
      ELSEIF (KEYWORD(1:5).EQ.'NAXIS') THEN
         READ (KEYWORD(6:6), '(I1)') IAX
         IF (IAX.GT.NAX) THEN
            NCARD = NCARD + 1
            GO TO 1
         END IF
         IVALUE = NAXIS(IAX)
      ELSEIF (KEYWORD(1:5).EQ.'CRVAL') THEN
         READ (KEYWORD(6:6), '(I1)') IAX
         IF (IAX.GT.NAX) THEN
            NCARD = NCARD + 1
            GO TO 1
         END IF
         DVALUE = RVAL(IAX)
      ELSEIF (KEYWORD(1:5).EQ.'CTYPE') THEN
         READ (KEYWORD(6:6), '(I1)') IAX
         IF (IAX.GT.NAX) THEN
            NCARD = NCARD + 1
            GO TO 1
         END IF
         CVALUE = TYPE(IAX)
      ELSEIF (KEYWORD(1:5).EQ.'CRPIX') THEN
         READ (KEYWORD(6:6), '(I1)') IAX
         IF (IAX.GT.NAX) THEN
            NCARD = NCARD + 1
            GO TO 1
         END IF
         RVALUE = RPIX(IAX)
      ELSEIF (KEYWORD(1:5).EQ.'CROTA') THEN
         READ (KEYWORD(6:6), '(I1)') IAX
         IF (IAX.GT.NAX) THEN
            NCARD = NCARD + 1
            GO TO 1
         END IF
         RVALUE = ROTA(IAX)
      ELSEIF (KEYWORD(1:5).EQ.'CDELT') THEN
         READ (KEYWORD(6:6), '(I1)') IAX
         RVALUE = DELT(IAX) 
         IF (IAX.GT.NAX) THEN
            NCARD = NCARD + 1
            GO TO 1
         END IF
      ELSEIF (KEYWORD.EQ.'HISTORY') THEN
         IHISCARD = IHISCARD + 1
         IF (ERROR) GO TO 990
         CALL HISGET (NAME, HISCARD, IHISCARD)
         IF(ERROR) THEN
            CALL ERRCANCE
            IHISCARD = -1
         END IF
         IF ((IHISCARD.GT.0).AND.(HISCARD.NE.' ')) THEN
            CARD = 'HISTORY '//HISCARD
            NCARD = NCARD - 1
         ELSE
            NCARD = NCARD + 1
            GO TO 1
         END IF
      ELSEIF (KEYWORD(1:5).EQ.'PTYPE') THEN
         CVALUE = ' '
         CALL DATGETC(NAME, KEYWORD, CVALUE, 1, ANVALS)
         DO 15 I = 1, 8
            IF(CVALUE(I:I).EQ.'#') CVALUE(I:I) = ' '
 15      CONTINUE
      ELSEIF (KTYPE.EQ.'R') THEN
         CALL DATGETR(NAME, KEYWORD, RVALUE, 1, ANVALS)
      ELSEIF (KTYPE.EQ.'I') THEN
         CALL DATGETI(NAME, KEYWORD, IVALUE, 1, ANVALS)
      ELSEIF (KTYPE.EQ.'D') THEN
         CALL DATGETD(NAME, KEYWORD, DVALUE, 1, ANVALS)
      ELSEIF (KTYPE.EQ.'L') THEN
         CALL DATGETL(NAME, KEYWORD, LVALUE, 1, ANVALS)
      ELSEIF (KTYPE.EQ.'C') THEN
         CVALUE = ' '
         CALL DATGETC(NAME, KEYWORD, CVALUE, 1, ANVALS)
      END IF
C
C Keyword may be missing. If so, then cancel error and continue
C
      IF (ERROR) THEN
         CALL ERRREASO (STRTMP)
         IF ((STRMATCH(STRTMP,ERRNTFND).AND.
     1      (NCARD+1.GT.3))) THEN
            CALL ERRCANCE
            NCARD = NCARD + 1
            GO TO 1
         ELSE
            GO TO 990
         END IF
      ELSE
         IF (CARD.EQ.' ') THEN
            CALL FTIPARSO (CARD, KEYWORD, OBJECT, RVALUE, IVALUE,
     $         DVALUE, XVALUE, LVALUE, CVALUE, KTYPE)
         END IF
         IF (SYSDEBUG) THEN
            CALL MSGPUT (CARD, 'D')
         END IF
         ISTART = ICARD*FTSLEN/SYSCHINT + 1
         CALL STRCHINT (CARD, FTSBUFF (ISTART), FTSLEN)
      END IF
      ICARD = ICARD + 1
      NCARD = NCARD + 1
C
C Write another block if the buffer is full, then reset buffer
C
      IF (ICARD.GE.FTSCARD) THEN
         CALL FILCWRIT (FTSBUFF, FTSBLOCK, FILEID, NIO)
         IF(NIO.NE.FTSBLOCK) THEN
            CALL ERRREPOR (ERROUTPT, ROUTINE,
     $         'Could not write all bytes')
            GOTO 999
         END IF
         CARD = ' '
         DO 20 ICARD = 1, FTSCARD
            ISTART = (ICARD-1)*FTSLEN/SYSCHINT + 1
            CALL STRCHINT (CARD, FTSBUFF (ISTART), FTSLEN)
  20     CONTINUE
         ICARD = 0
      END IF
      IF (ERROR) GO TO 990
      GO TO 1
C
  200 CONTINUE
C
C Finish off by writing END card
C
      KEYWORD = 'END'
      OBJECT = ' '
      CVALUE = ' '
      CALL FTIPARSO (CARD, KEYWORD, OBJECT, RVALUE, IVALUE, DVALUE,
     $     XVALUE, LVALUE, CVALUE, KTYPE)
      ISTART = ICARD*FTSLEN/SYSCHINT + 1
      CALL STRCHINT (CARD, FTSBUFF (ISTART), FTSLEN)
      ICARD = ICARD + 1
      NCARD = NCARD + 1
      CALL FILCWRIT (FTSBUFF, FTSBLOCK, FILEID, NIO)
      IF (NIO.NE.FTSBLOCK) THEN
         CALL ERRREPOR (ERROUTPT, ROUTINE,
     $        'Could not write all bytes')
         GOTO 999
      END IF
C         
 990  CONTINUE
      IF (ERROR) CALL ERRTRACE(ROUTINE)
C
 999  CONTINUE
      END
