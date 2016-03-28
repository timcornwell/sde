C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)ipcssort.f	1.5    7/20/93
C
      SUBROUTINE IPCSSORT (NAME, SORT)
C
CD Sort IPCS data into specified order
C
C
C
C	name		CH*(*)	input	NAME of database entry
C	sort            CH*(*)	input	Sort order required
C Audit trail:
C      Cloned from ipcsput
C                              R.G. Marson     Feb 11 1989
C
C-------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*) 	NAME, SORT
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IPCSSORT')
C
      CHARACTER*(SYSMXNAM) CSORT, STRM2, REFNAME, SRT1NAME, SRT2NAME
      CHARACTER*(SYSMXNAM) INAME
      CHARACTER*1       TYPE
      INTEGER           STRLEN, LEN
      INTEGER           RADD, IADD
      INTEGER           NAX, NAXIS(SYSMXDIM)
C
C==================================================================
C
      IF (ERROR) GO TO 999
C
C     Check that name is of the IPCS type
C
      CALL DATCHKTP(NAME, 'IPCS')
C
C Check that only a primary sort is required, 
C                       or if the sort type is known
C
      IF ((SORT(1:2).NE.'IF').AND.
     $    (SORT(1:2).NE.'IX').AND.
     $    (SORT(1:2).NE.'IY')) THEN
         MESSAGE = 'Unknown search format: '//SORT(1:2)
         CALL ERRREPOR(ERRBDARG, ROUTINE, MESSAGE)
         GOTO 999
      ELSE IF (STRLEN(SORT).NE.2) THEN
         MESSAGE = 'Only primary sorts currently supported'
         CALL ERRREPOR(ERRBDARG, ROUTINE, MESSAGE)
         GOTO 999
      END IF
C
C Check if the data is correctly sorted to begin with
C
      CALL DATGETC(NAME, 'SORT', CSORT, 1, LEN)
      IF (CSORT(1:2).NE.SORT(1:2)) THEN
         WRITE(MESSAGE, '(A, A)') 'Sorting data into ', SORT(1:2)
         CALL MSGPUT(MESSAGE, 'I')
C
C Get the reference array
C
         IF (SORT(2:2).EQ.'F') THEN
            REFNAME = STRM2(NAME, 'FRAME')
            SRT1NAME = STRM2(NAME, 'XPIXEL')
            SRT2NAME = STRM2(NAME, 'YPIXEL')
         ELSE IF (SORT(2:2).EQ.'X') THEN
            REFNAME = STRM2(NAME, 'XPIXEL')
            SRT1NAME = STRM2(NAME, 'FRAME')
            SRT2NAME = STRM2(NAME, 'YPIXEL')
         ELSE
            REFNAME = STRM2(NAME, 'YPIXEL')
            SRT1NAME = STRM2(NAME, 'XPIXEL')
            SRT2NAME = STRM2(NAME, 'FRAME')
         END IF
         CALL DATGETAR(REFNAME,NAX, NAXIS, TYPE, RADD)
C
C Make the index array
C
         INAME = STRM2(NAME, 'INDEX')
         CALL DATMAKAR(INAME, NAX, NAXIS, 'I', IADD)
C
C Sort the reference array (ie. get the index needed to do this)
C
         CALL ARRSORT(REFNAME,INAME)
C
C Use the index to shuffle all the arrays
C
         CALL ARRINDX(INAME,REFNAME)
         CALL ARRINDX(INAME,SRT1NAME)
         CALL ARRINDX(INAME,SRT2NAME)
C
C Delete the index array
C
         CALL DATDELAR(INAME)
C
C Label the new sort order
C
         CALL DATPUTC(NAME, 'SORT', SORT(1:2), 1)
      END IF
C
 990  IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
 999  CONTINUE
      END
