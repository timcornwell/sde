C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)hdr.f	1.4    8/14/94
C
      SUBROUTINE SDEMAIN
C
CD Program to manipulate headers
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Now can change coordinate variables and add HISTORY cards
C				T.J.Cornwell	July 28 1990
C	Error in length of Keytype
C				T.J.Cornwell	November 24 1993
C	Added history list capability
C				D.S.Briggs	Aug 14 1994
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'HDR')
C
      CHARACTER*8	NLIST(1024)
      CHARACTER*1	NTYPE(1024)
      INTEGER		INODE, NODES, NSIZE(1024)
      CHARACTER*(SYSMXNAM) 	INFILE,OUTFILE
      INTEGER                   NDUMMY, I, IND
      CHARACTER*(SYSMXNAM)	GOCMD
      CHARACTER*(SYSMXNAM)	KEYSTR
      CHARACTER*(SYSMXNAM)	KEYWORD
      CHARACTER*1		KEYTYPE
      REAL		KEYREAL
      INTEGER		KEYINT
      DOUBLE PRECISION	KEYDBL
      LOGICAL		KEYLOG
      LOGICAL		STRMATCH
      INTEGER		NAX, NAXIS(SYSMXDIM)
      DOUBLE PRECISION	CRVAL(SYSMXDIM)
      REAL		CRPIX(SYSMXDIM), CDELT(SYSMXDIM),
     $			CROTA(SYSMXDIM)
      CHARACTER*(8)	CTYPE(SYSMXDIM)
C==================================================================
C
      CALL MSGWELCO ('I manipulate headers')
      CALL USRCTL
C
C Get inputs
C
      CALL USRGETC ('Infile', INFILE, 1, NDUMMY)
C
C Get input image
C
      CALL FILIMGGE ('In', INFILE, '*')
      IF (ERROR) GO TO 999
C
C Open history
C
      CALL HISOPEN ('In')
C
C Dump header
C
      CALL DATHEDLI ('In', NODES, NLIST, NTYPE, NSIZE)
      CALL MSGPUT ('Header keywords', 'I')
      DO 5 INODE = 1, NODES, 6
         WRITE (MESSAGE, 2000) (NTYPE(I), NLIST(I), I=INODE,INODE+5)
 2000    FORMAT (6(A1, 1X, A8, 2X))
         CALL MSGPUT (MESSAGE, 'I')
 5    CONTINUE
C
C Get user command
C
 100  CONTINUE
      CALL USRCTL
      CALL USRGETGO (STRBUF)
      CALL STRUC (STRBUF, GOCMD)
      IF (ERROR) GO TO 999
C
      KEYTYPE = ' '
      CALL USRGETC ('Word', KEYWORD, 1, NDUMMY)
      DO 10 INODE = 1, NODES
         IF (STRMATCH(NLIST(INODE),KEYWORD)) THEN
            KEYTYPE = NTYPE(INODE)
            GO TO 11
         END IF
  10  CONTINUE
  11  CONTINUE
C
      IF (GOCMD(1:3).EQ.'PUT') THEN
         IF(KEYWORD.EQ.'NAXIS') THEN
            CALL CRDGET ('In', NAX, CTYPE, NAXIS, CRVAL, CRPIX, CDELT,
     1         CROTA)
            CALL USRGETI ('Int', NAX, 1, NDUMMY)
            CALL CRDPUT ('In', NAX, CTYPE, NAXIS, CRVAL, CRPIX, CDELT,
     1         CROTA)
         ELSE
     $   IF((KEYWORD(1:5).EQ.'CRVAL').OR.(KEYWORD(1:5).EQ.'CTYPE').OR.
     $      (KEYWORD(1:5).EQ.'CRPIX').OR.(KEYWORD(1:5).EQ.'CDELT').OR.
     $      (KEYWORD(1:5).EQ.'CROTA').OR.(KEYWORD(1:5).EQ.'NAXIS')) THEN
            CALL CRDGET ('In', NAX, CTYPE, NAXIS, CRVAL, CRPIX, CDELT,
     1         CROTA)
            READ(KEYWORD(6:6), '(I1)') IND
            IF(KEYWORD(1:5).EQ.'CRVAL') THEN
               CALL USRGETD ('Double', CRVAL(IND), 1, NDUMMY)
            ELSEIF(KEYWORD(1:5).EQ.'CTYPE') THEN
               CALL USRGETC ('String', KEYSTR, 1, NDUMMY)
               CTYPE(IND) = KEYSTR(1:8)
            ELSEIF(KEYWORD(1:5).EQ.'CRPIX') THEN
               CALL USRGETR ('Real', CRPIX(IND), 1, NDUMMY)
            ELSEIF(KEYWORD(1:5).EQ.'CDELT') THEN
               CALL USRGETR ('Real', CDELT(IND), 1, NDUMMY)
            ELSEIF(KEYWORD(1:5).EQ.'CROTA') THEN
               CALL USRGETR ('Real', CROTA(IND), 1, NDUMMY)
            ELSEIF(KEYWORD(1:5).EQ.'NAXIS') THEN
               CALL USRGETI ('Int', NAXIS(IND), 1, NDUMMY)
            ENDIF
            CALL CRDPUT ('In', NAX, CTYPE, NAXIS, CRVAL, CRPIX, CDELT,
     1         CROTA)
         ELSEIF(KEYWORD.EQ.'HISTORY') THEN
            CALL USRGETC ('String', KEYSTR, 1, NDUMMY)
            CALL HISPUT ('In', KEYSTR)
         ELSE 
            IF (KEYTYPE.EQ.' ') THEN
               CALL USRGETC ('Type', KEYSTR, 1, NDUMMY)
               KEYTYPE=KEYSTR(1:1)
            END IF
            IF (KEYTYPE.EQ.'C') THEN
               CALL USRGETC ('String', KEYSTR, 1, NDUMMY)
               IF (ERROR) GO TO 200
               CALL DATPUTC ('In', KEYWORD, KEYSTR, 1)
            ELSE IF (KEYTYPE.EQ.'I') THEN
               CALL USRGETI ('Int', KEYINT, 1, NDUMMY)
               IF (ERROR) GO TO 200
               CALL DATPUTI ('In', KEYWORD, KEYINT, 1)
            ELSE IF (KEYTYPE.EQ.'R') THEN
               CALL USRGETR ('Real', KEYREAL, 1, NDUMMY)
               IF (ERROR) GO TO 200
               CALL DATPUTR ('In', KEYWORD, KEYREAL, 1)
            ELSE IF (KEYTYPE.EQ.'D') THEN
               CALL USRGETD ('Double', KEYDBL, 1, NDUMMY)
               IF (ERROR) GO TO 200
               CALL DATPUTD ('In', KEYWORD, KEYDBL, 1)
            ELSE IF (KEYTYPE.EQ.'L') THEN
               CALL USRGETL ('Log', KEYLOG, 1, NDUMMY)
               IF (ERROR) GO TO 200
               CALL DATPUTL ('In', KEYWORD, KEYLOG, 1)
            ELSE
               CALL MSGPUT ('Unknown Key word '//KEYWORD, 'W')
            END IF
         END IF
      ELSE IF (GOCMD(1:3).EQ.'GET') THEN
         IF(KEYWORD.EQ.'NAXIS') THEN
            CALL CRDGET ('In', NAX, CTYPE, NAXIS, CRVAL, CRPIX, CDELT,
     1         CROTA)
            WRITE (MESSAGE, 1100) KEYWORD, NAX
            CALL MSGPUT (MESSAGE, 'I')
         ELSE
     $   IF((KEYWORD(1:5).EQ.'CRVAL').OR.(KEYWORD(1:5).EQ.'CTYPE').OR.
     $      (KEYWORD(1:5).EQ.'CRPIX').OR.(KEYWORD(1:5).EQ.'CDELT').OR.
     $      (KEYWORD(1:5).EQ.'CROTA').OR.(KEYWORD(1:5).EQ.'NAXIS')) THEN
            CALL CRDGET ('In', NAX, CTYPE, NAXIS, CRVAL, CRPIX, CDELT,
     1         CROTA)
            CALL CRDLIST ('In')
            READ(KEYWORD(6:6), '(I1)') IND
            IF(KEYWORD(1:5).EQ.'CRVAL') THEN
               WRITE (MESSAGE, 1300) KEYWORD(1:6), CRVAL(IND)
               CALL MSGPUT (MESSAGE, 'I')
               CALL USRPUTD ('Double', CRVAL(IND), 1)
            ELSEIF(KEYWORD(1:5).EQ.'CTYPE') THEN
               WRITE (MESSAGE, 1000) KEYWORD(1:6), CTYPE(IND)
               CALL MSGPUT (MESSAGE, 'I')
               CALL USRPUTC ('String', CTYPE(IND), 1)
            ELSEIF(KEYWORD(1:5).EQ.'CRPIX') THEN
               WRITE (MESSAGE, 1200) KEYWORD(1:6), CRPIX(IND)
               CALL MSGPUT (MESSAGE, 'I')
               CALL USRPUTR ('Real', CRPIX(IND), 1)
            ELSEIF(KEYWORD(1:5).EQ.'CDELT') THEN
               WRITE (MESSAGE, 1200) KEYWORD(1:6), CDELT(IND)
               CALL MSGPUT (MESSAGE, 'I')
               CALL USRPUTR ('Real', CDELT(IND), 1)
            ELSEIF(KEYWORD(1:5).EQ.'CROTA') THEN
               WRITE (MESSAGE, 1200) KEYWORD(1:6), CROTA(IND)
               CALL MSGPUT (MESSAGE, 'I')
               CALL USRPUTR ('Real', CROTA(IND), 1)
            ELSEIF(KEYWORD(1:5).EQ.'NAXIS') THEN
               WRITE (MESSAGE, 1100) KEYWORD(1:6), NAXIS(IND)
               CALL MSGPUT (MESSAGE, 'I')
               CALL USRPUTI ('Int', NAXIS(IND), 1)
            ENDIF
         ELSE
            IF (KEYTYPE.EQ.'C') THEN
               CALL DATGETC ('In', KEYWORD, KEYSTR, 1, NDUMMY)
               WRITE (MESSAGE, 1000) KEYWORD(1:8), KEYSTR
 1000          FORMAT (A,' = ',A)
               CALL MSGPUT (MESSAGE, 'I')
               CALL USRPUTC ('String', KEYSTR, 1)
            ELSE IF (KEYTYPE.EQ.'I') THEN
               CALL DATGETI ('In', KEYWORD, KEYINT, 1, NDUMMY)
               WRITE (MESSAGE, 1100) KEYWORD(1:8), KEYINT
 1100          FORMAT (A,' = ',I8)
               CALL MSGPUT (MESSAGE, 'I')
               CALL USRPUTI ('Int', KEYINT, 1)
            ELSE IF (KEYTYPE.EQ.'R') THEN
               CALL DATGETR ('In', KEYWORD, KEYREAL, 1, NDUMMY)
               WRITE (MESSAGE, 1200) KEYWORD(1:8), KEYREAL
 1200          FORMAT (A,' = ',1PE12.4)
               CALL MSGPUT (MESSAGE, 'I')
               CALL USRPUTR ('Real', KEYREAL, 1)
            ELSE IF (KEYTYPE.EQ.'D') THEN
               CALL DATGETD ('In', KEYWORD, KEYDBL, 1, NDUMMY)
               WRITE (MESSAGE, 1300) KEYWORD(1:8), KEYDBL
 1300          FORMAT (A,' = ',1PE18.7)
               CALL MSGPUT (MESSAGE, 'I')
               CALL USRPUTD ('Double', KEYDBL, 1)
            ELSE IF (KEYTYPE.EQ.'L') THEN
               CALL DATGETL ('In', KEYWORD, KEYLOG, 1, NDUMMY)
               IF (KEYLOG) THEN
                  WRITE (MESSAGE, 1400) KEYWORD(1:8)
 1400             FORMAT (A,' = T')
               ELSE
                  WRITE (MESSAGE, 1410) KEYWORD
 1410             FORMAT (A,' = F')
               END IF
               CALL MSGPUT (MESSAGE, 'I')
               CALL USRPUTL ('Log', KEYLOG, 1)
            ELSE
               CALL MSGPUT ('Unknown Key word '//KEYWORD, 'W')
            END IF
         END IF
      ELSE IF (GOCMD(1:4).EQ.'SAVE') THEN
         CALL USRGETC ('Out', OUTFILE, 1, NDUMMY)
         IF (OUTFILE.NE.' ') THEN
            CALL DATRENAM ('In', 'Out')
            CALL FILIMGPU('Out', OUTFILE, ' ')
            CALL DATRENAM ('Out', 'In')
         END IF
      ELSE IF (GOCMD.EQ.'HISLIST') THEN
         CALL HISLIST ('In')
      ELSE IF (GOCMD.EQ.' ') THEN
         CALL DATHEDLI ('In', NODES, NLIST, NTYPE, NSIZE)
         CALL MSGPUT ('Header keywords', 'I')
         DO 6 INODE = 1, NODES, 8
            WRITE (MESSAGE, 2000) (NLIST(I), I=INODE,INODE+7)
            CALL MSGPUT (MESSAGE, 'I')
 6       CONTINUE
      ELSE
         CALL MSGPUT ('Unknown command '//GOCMD, 'W')
      END IF
      GO TO 100
C
C Handle errors
C
 200  CONTINUE
      CALL ERRCANCE
      CALL MSGPUT ('Unknown keyword '//KEYWORD, 'I')
      GO TO 100
C
  999 CONTINUE
      END
