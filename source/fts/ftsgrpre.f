C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)ftsgrpre.f	1.6    24 Feb 1995
C
      SUBROUTINE FTSGRPRE (NAME, PARLIST)
C
CD Read in GROUPS for FITS file for the random pars specified. To be read
C in the sub-directory for the group must be listed in PARLIST.
C Wild cards are allowed in PARLIST e.g. '*' for all random pars.
C
C
C 	NAME		CH*(*)	input	Name of entry in directory 
C	PARLIST	CH*(*)	input	List of rpars to load
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Added IEEE handling
C				T.J.Cornwell	Jan 4 1989
C	Fixed IEEE handling
C				T.J.Cornwell	Jan 23 1990
C	Inserted ghastly kludge to get around AIPS induced roundoff
C	error in IEEE-based DATEs
C				T.J.Cornwell	March 16 1991
C       Modified to read 'GCOUNT' parameter as integer only on the
C       DEC Alphas due to floating point exceptions.
C                               J.D.Ellithorpe  Oct 24 1994
C---------------------------------------------------------------------
#include	"stdinc.h"
#include	"ftsinc.h"
C
      CHARACTER*(*)	NAME, PARLIST
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FTSGRPRE')
C
      CHARACTER*(SYSMXNAM)	STRM2, STRTMP,
     1				PARNAME(FTSNCOLS)
      CHARACTER			STRINT*6, ATYPE*1
      INTEGER			NDUMMY, GRPSPRRD, TOTGRPS
      INTEGER			IPAR, NPAR, ANPAR, NREG
      INTEGER			NREAD, LGRP, NNEXT
      INTEGER			BITPIX, DATFGETI, FILEID
      INTEGER			REGADD, PARADD(FTSNCOLS),
     1				PAROFF(FTSNCOLS), IREG, NBYTES
      INTEGER 			IAX, ANAX, ANAXIS(SYSMXDIM)
      DOUBLE PRECISION		PSCAL(FTSNCOLS), PZERO(FTSNCOLS),
     1				BSCALE, BZERO, DATFGETD
      LOGICAL			DATEXIST, DATFGETL, STRMATCH
      REAL			DATFGETR
C
C Buffer for reading fits data
C
      INTEGER			NIO
      INTEGER			FTSBUFF(FTSBLOCK / SYSCHINT)
C======================================================================
      IF (ERROR) GO TO 999
C
C Is this a group?
C
      IF (.NOT.DATFGETL (NAME, 'GROUPS').OR.ERROR) THEN
         CALL ERRCANCE
         CALL ERRREPOR (ERRNTFND, ROUTINE, 'Not a groups file')
         GO TO 990
      END IF
C
C Make the group columns first
C
#if COMP_ALPHA
      TOTGRPS = DATFGETI(NAME, 'GCOUNT')
      IF(TOTGRPS.EQ.0) THEN
         CALL ERRREPOR( ERRNTFND, ROUTINE, 'No groups!')
         GOTO 999
      ENDIF
#else
      TOTGRPS = NINT(DATFGETR(NAME, 'GCOUNT'))
      IF (TOTGRPS.EQ.0) THEN
         TOTGRPS = DATFGETI(NAME, 'GCOUNT')
         IF (TOTGRPS.EQ.0) THEN
            CALL ERRREPOR (ERRNTFND, ROUTINE, 'No groups!')
            GO TO 999
         END IF
      END IF
#endif
C
      NPAR = DATFGETI(NAME, 'PCOUNT')
      IF (ERROR) GO TO 990
C
C Find random parameters first
C
      ANAX = 1
      ATYPE = 'R'
      ANAXIS(1) = TOTGRPS
      ANPAR = 0
      DO 10 IPAR = 1, NPAR
C
C PARNAME will be something like: 'UU' or 'BASELINE'
C
         STRTMP = 'PTYPE'
         CALL STRAPPEN (STRTMP, STRINT(IPAR))
         CALL DATGETC (NAME, STRTMP, PARNAME(ANPAR+1), 1, NDUMMY)
C
C Do we really want this ?
C
         IF (.NOT.STRMATCH(PARLIST, PARNAME(ANPAR+1))) GO TO 10
         IF (ERROR) GO TO 990
C
C Add it to list and put NAME in front to get e.g. 'UV/UU'
C
         ANPAR = ANPAR + 1
         PARNAME(ANPAR) = STRM2(NAME, PARNAME(ANPAR))
C
C Is it the same as a previously loaded parameter?
C
  900    CONTINUE
         IF (DATEXIST(PARNAME(ANPAR))) THEN
            CALL STRAPPEN(PARNAME(ANPAR),'#')
            GO TO 900
         END IF
C
C Make array entry for each column, and find address.
C
         CALL DATMAKAR (PARNAME(ANPAR), ANAX, ANAXIS, ATYPE,
     1      PARADD(ANPAR))
         PAROFF(ANPAR) = IPAR
C
C Get scaling parameters
C
         STRTMP = 'PSCAL'
         CALL STRAPPEN (STRTMP, STRINT(IPAR))
         CALL DATGETD (NAME, STRTMP, PSCAL(ANPAR), 1, NDUMMY)
         STRTMP = 'PZERO'
         CALL STRAPPEN (STRTMP, STRINT(IPAR))
         CALL DATGETD (NAME, STRTMP, PZERO(ANPAR), 1, NDUMMY)
C
C Terrible kludge to get around scaling problem of IEEE Date
C
         IF((PARNAME(ANPAR).EQ.STRM2(NAME,'DATE#')).AND.
     $      (PZERO(ANPAR).GT.2400000.5)) THEN
            CALL DATPUTR(NAME, 'REFDATE', SNGL(PZERO(ANPAR)), 1)
            PZERO(ANPAR) = 0.0D0
         END IF
C
         IF (ERROR) GO TO 990
  10  CONTINUE
C
C Were any wanted random parameters found?
C
      IF (ANPAR.EQ.0) THEN
         CALL ERRREPOR (ERRNTFND, ROUTINE, 'Random parameters '//
     1      'not found')
         GO TO 999
      END IF
C
C Now get regular array stuff.
C
      BSCALE = DATFGETD (NAME, 'BSCALE')
      BZERO =  DATFGETD (NAME, 'BZERO')
      NREG = 1
      CALL DATGETI (STRM2(NAME, 'ARRAY'), 'NAXIS', ANAXIS, SYSMXDIM,
     $   ANAX)
      IF (ERROR) GOTO 990
      DO 20 IAX = 1, ANAX
         NREG = NREG * ANAXIS(IAX)
 20   CONTINUE
      ANAX = ANAX + 1
      ANAXIS(ANAX) = TOTGRPS
C
C Make an array to hold the regular array stuff
C
      CALL DATMAKAR(NAME, ANAX, ANAXIS, ATYPE, REGADD)
C
C Get logical unit number for access
C
      FILEID = DATFGETI (NAME, 'FILEID')
C
C Now start reading FITS cards
C
      NREAD = 0
C
C LGRP is the number of values in a group
C
      LGRP = NPAR + NREG
      BITPIX = DATFGETI(NAME, 'BITPIX')
      IF (ERROR) GO TO 990
      IF (ABS(BITPIX).EQ.16) THEN
C
C GRPSPRRD is the number of groups per block: we get this by dividing
C the length of a FITS block by the number of bytes in a group
C
         GRPSPRRD = INT(FLOAT(FTSBLOCK) / FLOAT(2*LGRP))
      ELSEIF (ABS(BITPIX).EQ.32) THEN
         GRPSPRRD = INT(FLOAT(FTSBLOCK) / FLOAT(4*LGRP))
      ELSE
         MESSAGE = 'Cannot read '//STRINT(BITPIX)//'-bit files'
         CALL ERRREPOR (ERRBDARG, ROUTINE, MESSAGE)
         GO TO 999
      END IF
C
C Only read integral number of words. This means that we cannot
C read tapes easily.
C
C Start of reading loop
C
   1  CONTINUE
C
C How many groups remain to be read?
C
      NNEXT = MAX (MIN (GRPSPRRD, TOTGRPS - NREAD), 0)
C
C NBYTES is the number of bytes to read next
C
      IF (ABS(BITPIX).EQ.16) THEN
         NBYTES = NNEXT * LGRP * 2
      ELSE
         NBYTES = NNEXT * LGRP * 4
      END IF
C
C If no more data then stop 
C
      IF (NBYTES.LE.0) GO TO 200
C
C Read into a buffer, then into the actual arrays
C
      CALL FILCREAD (FTSBUFF, NBYTES, FILEID, NIO)         
      IF (NIO.EQ.0) GO TO 100
      DO 110 IPAR = 1, ANPAR
         CALL FTICR (BITPIX/8, PSCAL(IPAR), PZERO(IPAR), NNEXT, 
     1       FTSBUFF, PAROFF(IPAR) - 1, LGRP, 
     2       MEMR(PARADD(IPAR)+NREAD))
  110 CONTINUE
C
C Now do regular array
C
      DO 120 IREG = 1, NNEXT
         CALL FTICR (BITPIX/8, BSCALE, BZERO, NREG,
     1      FTSBUFF, NPAR+LGRP*(IREG-1), 1, 
     2      MEMR(REGADD+NREG*(NREAD+IREG-1)))
  120 CONTINUE
C 
      IF (ERROR) GO TO 990
      NREAD = NREAD + NNEXT
C
C Go back for more
C
      GO TO 1
C
C If we get here then we ran into an EOF before all the data had been
C read!
C
 100  CONTINUE
      WRITE (MESSAGE, 1000) NREAD, TOTGRPS
 1000 FORMAT ('Could only read ',I8,' out of ',I8,' groups')
      CALL MSGPUT (MESSAGE, 'W')
      GO TO 999
C
C Found correct number of groups
C
 200  CONTINUE
C
 990  CONTINUE
      IF (ERROR) CALL ERRTRACE(ROUTINE)
C
 999  CONTINUE
      END
