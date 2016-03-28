C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)ftsimgre.f	1.3    11/7/90
C
      SUBROUTINE FTSIMGRE (NAME)
C
CD Read in binary data from FITS file into the array specified.
C
C
C 	ARRNAME	CH*(*)	input	Name of array in directory 
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Re-wrote to only use one buffer name, etc.
C				T.J. Cornwell March 30 1989
C	Added IEEE handling
C				T.J.Cornwell	Jan 4 1989
C
C---------------------------------------------------------------------
#include	"stdinc.h"
#include	"ftsinc.h"
C
      CHARACTER*(*)	NAME
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FTSIMGRE')
C
      CHARACTER		ATYPE*1, STRM2*(SYSMXNAM)
      INTEGER		ANAX, ANAXIS(SYSMXDIM)
      INTEGER		AADD
      INTEGER		FILEID, NIO, DATFGETI
      INTEGER		IAX, NREAD, IBLOCK, IADD, 
     1			NELEMENT, NNEXT
C
      INTEGER		BITPIX
      DOUBLE PRECISION	BSCALE, BZERO, DATFGETD
      INTEGER		FTSBUFF (FTSBLOCK/SYSCHINT)
C====================================================================
      IF (ERROR) GO TO 999
C
C Get dimension information and make array to hold the image
C
      CALL DATGETI (STRM2(NAME, 'ARRAY'), 'NAXIS', ANAXIS,
     $   SYSMXDIM, ANAX)
      ATYPE = 'R'
      CALL DATMAKAR (NAME, ANAX, ANAXIS, ATYPE, AADD)
      IF (ERROR) GO TO 990
C
      NELEMENT = 1
      DO 20 IAX = 1, ANAX
         NELEMENT = NELEMENT * ANAXIS(IAX)
  20  CONTINUE
      BITPIX = DATFGETI (NAME, 'BITPIX')
      BSCALE = DATFGETD (NAME, 'BSCALE')
      BZERO  = DATFGETD (NAME, 'BZERO')
      IF (ERROR) GO TO 990
C
C Get logical unit number for access
C
      FILEID = DATFGETI (NAME, 'FILEID')
C
C Now carry on reading FITS cards
C
      IBLOCK = 0
      NREAD = 0
   1  CONTINUE
C
C Read some more blocks into the appropriate place.
C
      IADD = AADD + NREAD 
      NNEXT = MAX (MIN (FTSBLOCK / ABS(BITPIX/8), NELEMENT - NREAD), 0)
C
C If no more data then stop 
C
      IF (NNEXT.LE.0) GO TO 200
C
      CALL FILCREAD (FTSBUFF, FTSBLOCK, FILEID, NIO)
      IF (NIO.EQ.0) GO TO 100
      CALL FTICR (BITPIX/8, BSCALE, BZERO, NNEXT, FTSBUFF, 0, 1, 
     1   MEMR(IADD))
      NREAD = NREAD + NNEXT
      IBLOCK = IBLOCK + 1
      IF (ERROR) GO TO 990
      GO TO 1
C
C If we get here then we ran into an EOF before all the data had been
C read!
C
 100  CONTINUE
      IF(SYSDEBUG) THEN
         WRITE (MESSAGE, 1000) IBLOCK, NREAD
 1000    FORMAT ('Read ',I6,' blocks, and ',I6,' elements')
         CALL MSGPUT (MESSAGE, 'W')
      END IF
      MESSAGE = 'Premature End of File'
      CALL ERRREPOR (ERRLOGIC, ROUTINE, MESSAGE)
      GO TO 999
C
C Found correct number of pixels
C
 200  CONTINUE
C
 990  CONTINUE
      IF (ERROR) CALL ERRTRACE(ROUTINE)
C
 999  CONTINUE
      END
