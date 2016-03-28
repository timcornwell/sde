C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)ftsreadh.f	1.5    1/7/94
C
      SUBROUTINE FTSREADH (NAME, ISEOF)
C
CD Read in header from FITS file and either translate to
C known FITS keywords or write card images to NAME directory.
C On exit, if there was no error then the file will be left at
C the next block after the END card.
C
C
C 	NAME	CH*(*)	input	Name of directory entry
C	ISEOF	LOG	output	Is EOF
C
C Audit trail:
C	Added detection of XTENSION files. Also put in proper
C	handling of HISTORY cards
C				T.J.Cornwell	Jan 7 1989
C	Put back FTIINITI call to allow recognition of cards by
C	FTIPARSI
C				T.J.Cornwell	Jan 12 1989
C	Added COMMAND cards
C                              T.J. Cornwell   May 3 1989
C	Use HISAPUT to avoid adding the name of the current program
C                              T.J. Cornwell   July 10 1990
C	Skip cards with a /
C                              T.J. Cornwell   August 14 1990
C	Set default value of XTENSION
C				T.J. Cornwell   May 28 1991
C	Added default co-ordinates values
C				R. G. Marson    Dec 1993
C
C---------------------------------------------------------------------
#include	"stdinc.h"
#include	"ftsinc.h"
C
      CHARACTER*(*)	NAME
      LOGICAL		ISEOF
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FTSREADH')
C
      CHARACTER			KEYWORD*(FTSLNKEY)
      CHARACTER			OBJECT*(FTSLNOBJ)
      CHARACTER			KTYPE*1, HISCARD*80
C
      REAL		RVALUE
      INTEGER		IVALUE
      DOUBLE PRECISION	DVALUE
      LOGICAL		LVALUE
      COMPLEX		XVALUE
      CHARACTER	*(SYSMXNAM)	CVALUE
C
      INTEGER		IAX, NAX, NAXIS(SYSMXDIM)
      REAL		RPIX (SYSMXDIM), DELT (SYSMXDIM),
     1			ROTA (SYSMXDIM)
      DOUBLE PRECISION	RVAL (SYSMXDIM)
      CHARACTER*8	TYPE (SYSMXDIM)
      CHARACTER*1	ATYPE
C
      INTEGER		NCARD, ICARD, ISTART
      INTEGER		FILEID, DATFGETI
C
      CHARACTER*(FTSLEN)	CARD
      INTEGER		NIO
      INTEGER		FTSBUFF (FTSBLOCK / SYSCHINT)
      LOGICAL		XTENSION
C====================================================================
C
      ISEOF = .FALSE.
      XTENSION=.FALSE.
C
C Initialise the co-ords to defaults of RPIX = 1, RVAL = 0, DELT=1
C  and CTYPE to X, Y, Z, AA, BB ....
C This is necessary as not all FITS files have co-ords and lots of SDE 
C  subroutines assume that they can manipulate sensible co-ordinate values
      DO 11 IAX = 1, SYSMXDIM
         RPIX(IAX) = 1.
         RVAL(IAX) = 0.
         DELT(IAX) = 1.
         ROTA(IAX) = 0.
         TYPE(IAX) = CHAR(ICHAR('A') - 4) // CHAR(ICHAR('A') - 4)
 11   CONTINUE
      TYPE(1) = 'X'
      TYPE(2) = 'Y'
      TYPE(3) = 'Z'
C
      IF (ERROR) GO TO 999
C
C Do SIMPLE by default
C
      CALL FTIINITI ('SIMPLE')
C
C Get file id for access
C
      FILEID = DATFGETI (NAME, 'FILEID')
      IF (ERROR) GO TO 990
C
      CALL SYSDATEC (CVALUE)
      CALL DATPUTC (NAME, 'DATE', CVALUE, 1)
C
C Start reading in FITS blocks 
C
      NCARD = 0
      CALL FILCREAD (FTSBUFF, FTSBLOCK, FILEID, NIO)
      IF (NIO.EQ.0) GOTO 100
C
   1  CONTINUE
      DO 30 ICARD = 1, FTSCARD
         ISTART = (ICARD-1)*FTSLEN/SYSCHINT + 1
C
C Parse this into its parts
C
         CALL STRINTCH (FTSBUFF (ISTART), CARD, FTSLEN)
C
         IF (SYSDEBUG) THEN
            CALL MSGPUT (CARD, 'D')
         END IF
C
         CALL FTIPARSI (CARD, KEYWORD, OBJECT, RVALUE, IVALUE, 
     1      DVALUE, XVALUE, LVALUE, CVALUE, KTYPE)
         NCARD = NCARD + 1
C
C A nice little Gotcha for the database system to barf on
C
         IF (INDEX(KEYWORD,'/').NE.0) THEN
            CALL MSGPUT ('Skipping keyword '//KEYWORD, 'W')
            GO TO 30
         END IF
C
C Now interpret Keyword
C
         IF (KEYWORD.EQ.'SIMPLE') THEN
            CALL FTIINITI ('SIMPLE')
         ELSEIF (KEYWORD.EQ.'XTENSION') THEN
            CALL FTIINITI ('XTENSION')
         ELSEIF (KEYWORD.EQ.'END') THEN
            GO TO 200
         ELSEIF ((KEYWORD.EQ.'HISTORY').OR.(KEYWORD.EQ.'COMMAND')) THEN
            HISCARD = ' '
            HISCARD(1:FTSLNOBJ) = OBJECT
            CALL HISAPUT (NAME, HISCARD)
            GO TO 30
         ELSEIF (KEYWORD.EQ.'NAXIS') THEN
            NAX = IVALUE
         ELSEIF (KEYWORD(1:5).EQ.'NAXIS') THEN
            READ (KEYWORD(6:6), '(I1)') IAX
            NAXIS(IAX) = IVALUE
         ELSEIF (KEYWORD(1:5).EQ.'CRVAL') THEN
            READ (KEYWORD(6:6), '(I1)') IAX
            RVAL(IAX) = DVALUE
         ELSEIF (KEYWORD(1:5).EQ.'CTYPE') THEN
            READ (KEYWORD(6:6), '(I1)') IAX
            TYPE(IAX) = CVALUE
         ELSEIF (KEYWORD(1:5).EQ.'CRPIX') THEN
            READ (KEYWORD(6:6), '(I1)') IAX
            RPIX(IAX) = RVALUE
         ELSEIF (KEYWORD(1:5).EQ.'CROTA') THEN
            READ (KEYWORD(6:6), '(I1)') IAX
            ROTA(IAX) = RVALUE
         ELSEIF (KEYWORD(1:5).EQ.'CDELT') THEN
            READ (KEYWORD(6:6), '(I1)') IAX
            DELT(IAX) = RVALUE
         ELSEIF (KEYWORD.EQ.'XTENSION') THEN
            XTENSION = .TRUE.
            CALL DATPUTC (NAME, 'XTENSION', CVALUE, 1)
         ELSEIF (KEYWORD.EQ.' ') THEN
            GO TO 30
         ELSE
C
C If we can translate the keyword then save it attached to the directory
C entry: 
C
            IF (KTYPE.EQ.'R') THEN
               CALL DATPUTR(NAME, KEYWORD, RVALUE, 1)
            ELSEIF (KTYPE.EQ.'I') THEN
               CALL DATPUTI(NAME, KEYWORD, IVALUE, 1)
            ELSEIF (KTYPE.EQ.'D') THEN
               CALL DATPUTD(NAME, KEYWORD, DVALUE, 1)
            ELSEIF (KTYPE.EQ.'L') THEN
               CALL DATPUTL(NAME, KEYWORD, LVALUE, 1)
            ELSEIF (KTYPE.EQ.'C') THEN
               CALL DATPUTC(NAME, KEYWORD, CVALUE, 1)
            END IF
         END IF
         IF (ERROR) THEN
            GO TO 990
         END IF
  30  CONTINUE
C
C Read some more blocks
C
      CALL FILCREAD (FTSBUFF, FTSBLOCK, FILEID, NIO)
      IF (NIO.EQ.0) GOTO 100
      GO TO 1
C
C If we got here then the END card was missing
C
  100 CONTINUE
      IF (SYSDEBUG) CALL MSGPUT ('No END card', 'W')
      ISEOF = .TRUE.
      GO TO 999
C
C If we got here then we must have got an END card
C
  200 CONTINUE
C
C Make array for the data: check for stupid FITS definition of group files
C
      ATYPE = 'R'
      IF(.NOT.XTENSION) THEN
         IF (NAXIS(1).NE.0) THEN
            CALL CRDPUT (NAME, NAX, TYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
         ELSE IF (NAXIS(2).NE.0) THEN
            CALL CRDPUT (NAME, NAX-1, TYPE(2), NAXIS(2), RVAL(2), 
     1         RPIX(2), DELT(2), ROTA(2))
         ELSE
            CALL ERRREPOR (ERRLOGIC, ROUTINE, 'Illegal FITS axes')
            GO TO 999
         END IF
      ELSE
         CALL DATPUTI(NAME, 'NAXIS1', NAXIS(1), 1)
         CALL DATPUTI(NAME, 'NAXIS2', NAXIS(2), 1)
      END IF
C         
 990  CONTINUE
      IF (ERROR) CALL ERRTRACE(ROUTINE)
C
 999  CONTINUE
      END
