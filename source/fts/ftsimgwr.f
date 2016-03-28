C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)ftsimgwr.f	1.5    9/3/92
C
      SUBROUTINE FTSIMGWR (NAME)
C
CD Write out binary data to FITS file from the array specified.
C
C
C 	ARRNAME	CH*(*)	input	Name of array in directory 
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Added IEEE handling
C				T.J.Cornwell	Jan 4 1989
C	Control IEEE writing with environment variable
C				T.J.Cornwell	March 19 1990
C	Control IEEE writing with sde.cur variable
C				T.J.Cornwell	April 13 1990
C	Changed USRGETC call to DATGETC ('SYI',... call
C				T.J.Cornwell	July 24 1990
C	Change name of IEEE to FTSTYPE. It seemed to cause some
C	problems on the Convex.
C				T.J.Cornwell	January 14 1991
C	DATAMIN & DATAMAX should always be written, since they might
C	be (incorrectly) left over from previous headers.
C				D.S.Briggs	Sept 3 1992
C---------------------------------------------------------------------
#include	"stdinc.h"
#include	"ftsinc.h"
C
      CHARACTER*(*)	NAME
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FTSIMGWR')
C
      CHARACTER		ATYPE*1
      CHARACTER		FTSTYPE*(SYSMXNAM)
      INTEGER		ANAX, ANAXIS(SYSMXDIM)
      INTEGER		AADD
      INTEGER		FILEID, NIO, DATFGETI
      INTEGER		IAX, NWRITE, IBLOCK, IADD, NELEMENT, NNEXT
      INTEGER		RNAX, CRDRNAX
C
      INTEGER		BITPIX, NDUMMY         
      DOUBLE PRECISION	RSCALE, RZERO, BSCALE, BZERO
      INTEGER		FTSBUFF (FTSBLOCK / SYSCHINT)
      REAL		DMAX, DMIN
C
      INTEGER		STRLEN
      REAL		DATFGETR
C====================================================================
      IF (ERROR) GO TO 999
C
      CALL DATGETC ('SYI', 'FITS', FTSTYPE, 1, NDUMMY)
C
C Cancel any error and set default FITS type to IEEE
C
      IF(ERROR) THEN
         CALL ERRCANCE
         FTSTYPE='IEEE'
      END IF
      IF(FTSTYPE.EQ.'32') THEN
         CALL MSGPUT ('Writing 32-bit scaled-integer format', 'I')
         BITPIX = 32
         CALL FTSFNDSC(NAME, BITPIX)
         CALL DATGETD (NAME, 'BSCALE', BSCALE, 1, NDUMMY)
         CALL DATGETD (NAME, 'BZERO', BZERO, 1, NDUMMY)
         RSCALE = 1.0D0/BSCALE
         RZERO = - RSCALE*BZERO
      ELSE IF(FTSTYPE.EQ.'16') THEN
         CALL MSGPUT ('Writing 16-bit scaled-integer format', 'I')
         BITPIX = 16
         CALL FTSFNDSC(NAME, BITPIX)
         CALL DATGETD (NAME, 'BSCALE', BSCALE, 1, NDUMMY)
         CALL DATGETD (NAME, 'BZERO', BZERO, 1, NDUMMY)
         RSCALE = 1.0D0/BSCALE
         RZERO = - RSCALE*BZERO
      ELSE
         BITPIX = -32
         CALL DATPUTI (NAME, 'BITPIX', BITPIX, 1)
         CALL ARRSTAT (NAME, ' ')
         DMAX = DATFGETR (NAME, 'ARRMAX')
         DMIN = DATFGETR (NAME, 'ARRMIN')
         CALL DATPUTR (NAME, 'DATAMAX', DMAX, 1)
         CALL DATPUTR (NAME, 'DATAMIN', DMIN, 1)
         RSCALE = 1.0D0
         RZERO = 0.0D0
         CALL DATPUTD (NAME, 'BSCALE', RSCALE, 1)
         CALL DATPUTD (NAME, 'BZERO', RZERO, 1)
      END IF
C
C Write the header
C
      CALL FTSWRITH (NAME)
C
C Assemble standard name
C
      CALL DATGETAR (NAME, ANAX, ANAXIS, ATYPE, AADD)
      IF (ATYPE.NE.'R') THEN
         MESSAGE = 'Data type '//ATYPE//' not supported'
         CALL ERRREPOR (ERRBDARG, ROUTINE, MESSAGE)
         GO TO 999
      END IF
      IF (ERROR) GO TO 990
C
      RNAX = CRDRNAX (ANAX, ANAXIS)
      WRITE (MESSAGE, 1000) RNAX, NAME(1:STRLEN(NAME))
 1000 FORMAT ('Writing ',I1,'-dimensional image ',A)
      CALL MSGPUT (MESSAGE, 'I')
      IF (ERROR) GO TO 990
C
      NELEMENT = 1
      DO 10 IAX = 1, RNAX
         NELEMENT = NELEMENT * ANAXIS(IAX)
  10  CONTINUE
C
C Get logical unit number for access
C
      FILEID = DATFGETI (NAME, 'FILEID')
C
C Now carry on writing FITS cards
C
      IBLOCK = 0
      NWRITE = 0
   1  CONTINUE
C
C Write some more blocks to the appropriate place.
C
      IADD = AADD + NWRITE 
      NNEXT = MAX (MIN (FTSBLOCK / ABS(BITPIX/8), NELEMENT - NWRITE), 0)
C
C If no more data then stop 
C
      IF (NNEXT.LE.0) GO TO 200
C
      CALL FTIDR (BITPIX/8, RSCALE, RZERO, NNEXT, FTSBUFF, 0, 1, 
     1   MEMR(IADD))
      CALL FILCWRIT (FTSBUFF, FTSBLOCK, FILEID, NIO)
      IF(NIO.NE.FTSBLOCK) THEN
         CALL ERRREPOR (ERROUTPT, ROUTINE, 'Could not write all bytes')
         GOTO 999
      END IF
      NWRITE = NWRITE + NNEXT
      IBLOCK = IBLOCK + 1
      IF (ERROR) GO TO 990
      GO TO 1
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
