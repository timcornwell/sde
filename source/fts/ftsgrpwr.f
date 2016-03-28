C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)ftsgrpwr.f	1.7    3/18/91
C
      SUBROUTINE FTSGRPWR (NAME, PARLIST)
C
CD Write GROUPS into FITS file for the random pars specified.
C
C 	NAME    CH*(*)  input   Name of entry in directory 
C	PARLIST CH*(*)  input   List of rpars to load
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C      Modified arguemnts of filcwrite to correctly handle bitpix=16
C                              R.G Marson      Feb 9 1990
C      Modified to take into account Floating point FITS
C                              R.G. Marson      Nov 2 1990
C	Removed X.EQ..TRUE. constructs which cause IBM compiler to
C	barf. Also de-DO-END DO'ed.
C				T.J.Cornwell	Jan 22 1991
C	Added control via Fits type from SYI
C				T.J.Cornwell	March 16 1991
C
C---------------------------------------------------------------------
#include	"stdinc.h"
#include	"ftsinc.h"
C
      CHARACTER*(*)	NAME, PARLIST
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FTSGRPWR')
C
      CHARACTER*(SYSMXNAM)	STRM2, STRTMP, PARNAME(FTSNCOLS)
      CHARACTER*6		STRINT*6
      CHARACTER*8               CTYPE(SYSMXDIM)
      CHARACTER*(SYSMXNAM)      PAR(FTSNCOLS), FTSTYPE
      DOUBLE PRECISION		PSCALE(FTSNCOLS), PZERO(FTSNCOLS)
      DOUBLE PRECISION          BSCALE, BZERO, RSCALE, RZERO
      DOUBLE PRECISION          XSCALE(SYSMXDIM), XZERO(SYSMXDIM)
      DOUBLE PRECISION          DATFGETD, CRVAL(SYSMXDIM)
      INTEGER                   IOBUFF  (FTSBLOCK / SYSCHINT)
      REAL                      CRPIX(SYSMXDIM), CDELT(SYSMXDIM)
      REAL                      CROTA(SYSMXDIM)
      INTEGER			TOTGRPS, NGRP, CARDCTR, GRPCTR, GRPS
      INTEGER                   RBLOCK
      INTEGER                   VISADD, DATADD
      INTEGER                   NDUMMY
      INTEGER			IPAR, NPAR, ANPAR, NBYTES
      INTEGER			BITPIX, DATFGETI, FILEID
      INTEGER			PARADD(SYSMXDIM), I
      INTEGER 			NAX, NAXIS(SYSMXDIM)
      LOGICAL			DATEXIST, DATEXIAR, STRMATCH
      INTEGER			NIO
C======================================================================
      IF (ERROR) GO TO 999
C
C Find out how mant groups there are
C
      TOTGRPS = DATFGETI(NAME, 'GCOUNT')
      IF ((TOTGRPS.EQ.0.0).OR.ERROR) THEN
         CALL ERRREPOR (ERRNTFND, ROUTINE, 'No groups !')
         GO TO 999
      END IF
C
C Find out how many random parameters there are
C
      NPAR = DATFGETI(NAME, 'PCOUNT')
      IF ((NPAR.EQ.0).OR.ERROR) THEN
         CALL ERRREPOR (ERRNTFND, ROUTINE, 'No parameters!')
         GO TO 999
      END IF
C
      IF(ERROR) GO TO 990
      CALL DATGETC ('SYI', 'FITS', FTSTYPE, 1, NDUMMY)
      IF(ERROR) THEN
         CALL ERRCANCE
         FTSTYPE='IEEE'
      END IF
C
      IF(FTSTYPE.EQ.'32') THEN
         CALL MSGPUT ('Writing 32-bit scaled-integer format', 'I')
         BITPIX = 32
         CALL DATPUTI (NAME, 'BITPIX', BITPIX, 1)
      ELSE IF(FTSTYPE.EQ.'16') THEN
         CALL MSGPUT ('Writing 16-bit scaled-integer format', 'I')
         BITPIX = 16
         CALL DATPUTI (NAME, 'BITPIX', BITPIX, 1)
      ELSE
         BITPIX = -32
         CALL DATPUTI (NAME, 'BITPIX', BITPIX, 1)
      END IF
C
C Find random parameters first
C
      ANPAR = 0
      DO 10 IPAR = 1, NPAR
C
C PARNAME will be something like: 'UU' or 'BASELINE'
C
         STRTMP = 'PTYPE'
         CALL STRAPPEN (STRTMP, STRINT(IPAR))
         CALL DATGETC (NAME, STRTMP, PAR(ANPAR+1), 1, NDUMMY)
         IF (ERROR) THEN
            CALL ERRREPOR (ERRNTFND, ROUTINE, STRTMP)
            GO TO 990
         END IF
C
C Do we really want this ?
C
         IF (.NOT.STRMATCH(PARLIST, PAR(ANPAR+1))) GO TO 10
C
C Add it to list and put NAME in front to get e.g. 'UV/UU'
C
         ANPAR = ANPAR + 1
         PARNAME(ANPAR) = STRM2(NAME, PAR(ANPAR))
         IF(.NOT.DATEXIAR(PARNAME(ANPAR))) THEN
            CALL STRAPPEN(PARNAME(ANPAR),'#')
            IF(.NOT.DATEXIAR(PARNAME(ANPAR))) THEN
               CALL STRAPPEN(PARNAME(ANPAR),'#')
               IF(.NOT.DATEXIAR(PARNAME(ANPAR))) THEN
                  CALL ERRREPOR (ERRNTFND, ROUTINE, PARNAME(ANPAR))
                  GO TO 990
               END IF
            END IF
         END IF
C
C Get scaling parameters for the random parameters arrays
C             (create them if they don't exist)
C
         STRTMP = 'PSCAL'
         CALL STRAPPEN(STRTMP, STRINT(IPAR))
         IF (DATEXIST(STRM2(NAME,STRTMP))) THEN
            PSCALE(ANPAR) = DATFGETD(NAME, STRTMP)
         ELSE
            IF (BITPIX.LT.0) THEN
               PSCALE(ANPAR) = 1.0D0
               CALL DATPUTD(NAME, STRTMP, PSCALE(ANPAR), 1)
            ELSE
               CALL FTSFNDSC(PARNAME(ANPAR), BITPIX)
               PSCALE(ANPAR) = DATFGETD(PARNAME(ANPAR), 'BSCALE')
               CALL DATPUTD(NAME, STRTMP, PSCALE(ANPAR), 1)
               PSCALE(ANPAR) = DATFGETD(NAME, STRTMP)
            END IF
         END IF
         IF (ERROR) GO TO 990
C     
         STRTMP = 'PZERO'
         CALL STRAPPEN (STRTMP, STRINT(IPAR))
         IF (DATEXIST(STRM2(NAME,STRTMP))) THEN
            PZERO(ANPAR) = DATFGETD(NAME, STRTMP)
         ELSE
            IF (BITPIX.LT.0) THEN
               PZERO(ANPAR) = 0.0D0
               CALL DATPUTD(NAME, STRTMP, PZERO(ANPAR), 1)
            ELSE
               CALL FTSFNDSC(PARNAME(ANPAR), BITPIX)
               PZERO(ANPAR) = DATFGETD(PARNAME(ANPAR), 'BZERO')
               CALL DATPUTD(NAME, STRTMP, PZERO(ANPAR), 1)
            END IF
         END IF
         IF (ERROR) GO TO 990
  10  CONTINUE
C
C Get scaling parameters for the Visibility data array
C  (or create them if they don't exist)
C
         IF (DATEXIST(STRM2(NAME,'BSCALE')).AND.
     $      DATEXIST(STRM2(NAME,'BZERO'))) THEN
            BSCALE = DATFGETD(NAME, 'BSCALE')
            BZERO = DATFGETD(NAME, 'BZERO')
         ELSE
            IF (BITPIX.LT.0) THEN
               BZERO = 0.0
               CALL DATPUTD(NAME, 'BZERO', BZERO, 1)
               BSCALE = 1.0
               CALL DATPUTD(NAME, 'BSCALE', BSCALE, 1)
            ELSE
               CALL FTSFNDSC(NAME, BITPIX)
               BSCALE = DATFGETD(NAME, 'BSCALE')
               BZERO = DATFGETD(NAME, 'BZERO')
            END IF
         END IF
C
C Modify header so that first co-ordinate is zero
C
      CALL CRDGET (NAME, NAX, CTYPE, NAXIS, CRVAL, CRPIX, CDELT, CROTA)
      DO 20 I = NAX, 1, -1
         CTYPE(I+1) = CTYPE(I)
         NAXIS(I+1) = NAXIS(I)
         CRVAL(I+1) = CRVAL(I)
         CRPIX(I+1) = CRPIX(I)
         CDELT(I+1) = CDELT(I)
         CROTA(I+1) = CROTA(I)
 20   CONTINUE
      CTYPE(1) = ' '
      NAXIS(1) = 0
      CRVAL(1) = 0
      CRPIX(1) = 1
      CDELT(1) = 1
      CROTA(1) = 0
      CALL CRDPUT(NAME, NAX, CTYPE, NAXIS, CRVAL, CRPIX, CDELT, CROTA)
C
C Write the header
C
      CALL FTSWRITH(NAME)
C
C Were any wanted random parameters found?
C
      IF (ANPAR.EQ.0) THEN
         CALL ERRREPOR (ERRNTFND, ROUTINE, 'No random parameters '//
     1      'not found')
         GO TO 999
      END IF
C
C Find out how many REAL's are in each data group
C
      CALL CRDGET (NAME, NAX, CTYPE, NAXIS, CRVAL, CRPIX, CDELT, CROTA)
      NGRP = 1
      DO 30 I = 2, NAX
         NGRP = NGRP * NAXIS(I)
 30   CONTINUE
C
C Get address of the data array
C
      VISADD = DATADD (NAME)
C
C Get the address of the parameters array(s)
C
      DO 40 I = 1, NPAR
         PARADD(I) = DATADD (PARNAME(I))
 40   CONTINUE
C
C Get logical unit number for access
C
      FILEID = DATFGETI (NAME, 'FILEID')
C
C Scaling parameters as required by ftidr
C
      RSCALE = 1D0 / BSCALE
      RZERO = - BZERO / BSCALE
      DO 50 I = 1, NPAR
         XSCALE(I) = 1D0 / PSCALE(I)
         XZERO(I) = - PZERO(I) / PSCALE(I)
 50   CONTINUE
C
C Pack the data into ftsbuff, then convert each block into 
C  integers (iobuff) and write it to disk
C
      CARDCTR = 0
      GRPCTR = 0
      GRPS = 0
      NBYTES = BITPIX/8
      RBLOCK = FTSBLOCK/ABS(NBYTES)
      DO 60 GRPCTR = 0, TOTGRPS - 1
         DO 70 I = 1, NPAR
            CALL FTIDR (NBYTES, XSCALE(I), XZERO(I), 1, IOBUFF,
     $           CARDCTR, 1, MEMR (PARADD(I) + GRPCTR))
            CARDCTR = CARDCTR + 1
            IF (CARDCTR.GE.RBLOCK) THEN
               CALL FILCWRIT (IOBUFF, FTSBLOCK, FILEID, NIO)
               CARDCTR = 0
            END IF
 70      CONTINUE
         DO 80 I = 0, NGRP - 1
            CALL FTIDR (NBYTES, RSCALE, RZERO, 1, IOBUFF,
     $           CARDCTR, 1, MEMR (VISADD + GRPS + I))
            CARDCTR = CARDCTR + 1
            IF (CARDCTR.GE.RBLOCK) THEN
               CALL FILCWRIT (IOBUFF, FTSBLOCK, FILEID, NIO)
               CARDCTR = 0
            END IF
 80      CONTINUE
         GRPS = (GRPCTR + 1) * NGRP
 60   CONTINUE
      IF (CARDCTR.GT.0) THEN
         DO 90 I = CARDCTR + 1, (FTSBLOCK/SYSCHINT) 
            IOBUFF(I) = 0
 90      CONTINUE
         CALL FILCWRIT (IOBUFF, FTSBLOCK, FILEID, NIO)
      END IF
C
 990  CONTINUE
      IF (ERROR) CALL ERRTRACE(ROUTINE)
C
 999  CONTINUE
      END
