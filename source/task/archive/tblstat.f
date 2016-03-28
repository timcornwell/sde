C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C	(C)opyright 1989 by NRAO; all rights reserved
C
      SUBROUTINE SDEMAIN
C
CD Program to calculate table statistics
C
CS Arguments: CALL SDEMAIN
CA
CA Audit trail:
CA	Minor changes to get it into new format
CA				T.J.Cornwell	Jan 7 1989
CE
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'TBLSTAT')
C
      INTEGER 		NAX, NDUMMY, BLC(SYSMXDIM), 
     1   		TRC(SYSMXDIM), ICOL
      REAL		AVE, RMS, DMAX, DMIN, DATFGETR
      CHARACTER*(SYSMXNAM) 	INFILE,
     1				ARRUNITS,
     2				STRM2, STRM3,
     3				DATFGETC,
     4				TBLNAME,
     5				COLNAME,
     6				ARRNAME,
     7				STRTMP
C==================================================================
C
      CALL MSGWELCO ('I calculate table statistics')
      CALL USRCTL
      CALL USRGETL ('Debug', SYSDEBUG, 1, NDUMMY)
C
C Read data in
C
      CALL USRGETC ('Image', INFILE, 1, NDUMMY)
      CALL USRGETC ('Tables', TBLNAME, 1, NDUMMY)
      CALL FILIMGGE ('Image', INFILE, TBLNAME)
C
C Call main routine to work on the 'IMAGE' array.
C  
      ICOL = 0
      CALL USRGETC ('Columns', STRTMP, 1, NDUMMY)
  1   CONTINUE
      CALL STREXTRA (STRTMP, ICOL+1, COLNAME)
      IF (COLNAME.EQ.' ') GO TO 100
      ICOL = ICOL + 1
      ARRNAME = STRM3('Image',TBLNAME,COLNAME)
      CALL ARRSTAT (ARRNAME, ' ')
      IF (ERROR) GO TO 999
C
C Write results to the message file
C
      WRITE (MESSAGE, 1010) STRM2(TBLNAME,COLNAME)
 1010 FORMAT ('   Table: ',A)
      CALL MSGPUT (MESSAGE, 'I')
C
      ARRUNITS = ' '
      AVE = DATFGETR(ARRNAME, 'ARRAVE')
      RMS = DATFGETR(ARRNAME, 'ARRRMS')
      DMAX = DATFGETR(ARRNAME, 'ARRMAX')
      DMIN = DATFGETR(ARRNAME, 'ARRMIN')
      WRITE (MESSAGE, 1100) AVE, ARRUNITS
 1100 FORMAT ('   Average = ',1PE11.3,' ',A)
      CALL MSGPUT (MESSAGE, 'I')
      WRITE (MESSAGE, 1200) RMS, ARRUNITS
 1200 FORMAT ('   RMS     = ',1PE11.3,' ',A)
      CALL MSGPUT (MESSAGE, 'I')
      WRITE (MESSAGE, 1300) DMAX, ARRUNITS
 1300 FORMAT ('   Maximum = ',1PE11.3,' ',A)
      CALL MSGPUT (MESSAGE, 'I')
      WRITE (MESSAGE, 1400) DMIN, ARRUNITS
 1400 FORMAT ('   Minimum = ',1PE11.3,' ',A)
      CALL MSGPUT (MESSAGE, 'I')
      GO TO 1
  100 CONTINUE
C
      CALL FILCLOSE ('Image')
C
 999  CONTINUE
      END
