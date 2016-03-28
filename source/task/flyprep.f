C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)flyprep.f	1.4    11/7/90
C
      SUBROUTINE SDEMAIN
C
CD Program to prepare visibility data for fly
C
C Audit trail:
C	New task
C				T.J.Cornwell	April 18 1990
C	Fixed VISPUT
C				T.J.Cornwell    Nov 1 1990
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISPREP')
C
      CHARACTER*(SYSMXNAM)	VISFILE, OUTFILE
      INTEGER		NDUMMY
C==================================================================
      CALL MSGWELCO ('I prepare visibility data for fly')
      CALL USRCTL
C
C Get input parameters
C
      CALL USRGETC('Vis', VISFILE, 1, NDUMMY)
      CALL USRGETC('Outfile', OUTFILE, 1, NDUMMY)
C
C Get the original visibility files
C
      CALL FLYGET ('Vis', VISFILE, 'I', 'UU,VV,WW', ' ')
C
C Put the visibility file
C
      CALL VISPUT ('Vis', OUTFILE, 'OBS', 'I', '*', ' ')
C
 999  CONTINUE
      END
C++
C
      SUBROUTINE FLYGET (NAME, FILENAME, STOKES, GRPLIST, TBLLIST)
C
C Get visibility data from a file. 
C
C
C	NAME		CH*(*)	input	NAME of file as specified to user
C	FILENAME	CH*(*)	input	Name of file
C	STOKES		CH*(*)	input	List of Stokes parameters to load
C	GRPLIST	CH*(*)	input	List of groups to load
C	TBLLIST	CH*(*)	input	List of tables to load
C Audit trail:
C	Added SDETYPE specification
C				T.J.Cornwell	Feb 3 1989
C
C-------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*) 	NAME, FILENAME, STOKES, GRPLIST, TBLLIST
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FLYGET')
C
      CHARACTER		FILESYS*(SYSMXNAM),
     2			STRM2*(SYSMXNAM),
     3			ACCESS*(SYSMXNAM)
C
      INTEGER		NDUMMY
      LOGICAL		STRMATCH
C
      DATA		ACCESS / 'READ' /
C==================================================================
C
      IF (ERROR) GO TO 999
C
C Find out what sort of file it is
C
      CALL FILSYSEX (FILENAME, FILESYS)
C
C If this is an SDE file then just get it
C
      IF (STRMATCH(FILESYS, 'SDE')) THEN
         CALL DATCREAT(NAME)
         CALL DATREAD (NAME, FILENAME)
      ELSE IF (STRMATCH(FILESYS, 'FTS')) THEN
C
C Get FITS groups file
C
         CALL FILGRPGE (NAME, FILENAME, GRPLIST, TBLLIST, FILESYS)
C
C Now filter to get required STOKES
C
         CALL FLYSTD (NAME, STOKES)
C
         CALL DATSETTP (NAME, 'VIS')
C
      ELSE
         MESSAGE = 'File system: '//FILESYS//' not supported'
         CALL ERRREPOR (ERRWRGTP, ROUTINE, MESSAGE)
      END IF
C
      IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
 999  CONTINUE
      END
C++
C
      SUBROUTINE FLYSTD (NAME, STOKES)
C
C Re-format a uv file just read from FITS file into a more sensible
C format: namely put each correlator into a separate directory. The
C names are of the form: NAME/OBS/I/VIS and NAME/OBS/I/WT for the
C observed Stokes I visibility and weight respectively.
C
C
C	NAME	CH*(*)	input	Name of directory entry
C	STOKES	CH*(*)	input	List of Stokes parameters to be kept
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
       CHARACTER*(*)	NAME, STOKES
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FLYSTD')
C
      INTEGER		IAX, NAX, NAXIS(SYSMXDIM), SNAX,  WPADD,
     1			SNAXIS(SYSMXDIM), AADD, SADD, WADD, ISTOKE,
     2			NSTOKE, STEP, ONAX, ONAXIS(SYSMXDIM)
      REAL		RPIX (SYSMXDIM), DELT(SYSMXDIM),
     1			ROTA (SYSMXDIM)
      DOUBLE PRECISION	RVAL(SYSMXDIM)
      DOUBLE PRECISION	FREQ
      CHARACTER*8	TYPE(SYSMXDIM)
      REAL		ORPIX (SYSMXDIM), ODELT(SYSMXDIM),
     1			OROTA (SYSMXDIM)
      DOUBLE PRECISION	ORVAL(SYSMXDIM)
      CHARACTER*8	OTYPE(SYSMXDIM)
      CHARACTER*6	STRINT
      CHARACTER*1	ATYPE, STYPE, WTYPE
      CHARACTER*(SYSMXNAM)	SNAME, WNAME, STRM2, STRM3, STRM4
      CHARACTER*1	STKTYPE(4)
      LOGICAL		STRMATCH, DATEXIST, PIPELINE, ISIQUV
      INTEGER		NDUMMY, INC, DATADD, I
      REAL		TIMMIN, TIMMAX, REFDATE
      DATA		STKTYPE	/'I','Q','U','V'/
C=======================================================================
      IF (ERROR) GO TO 999
C
      CALL DATDELET (STRM2(NAME, 'HII'))
C
      CALL DATGETAR (NAME, NAX, NAXIS, ATYPE, AADD)
      IF (ATYPE.NE.'R') THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Vis array not real')
         GO TO 999
      END IF
      CALL CRDGET (NAME, NAX, TYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
      IF (ERROR) GO TO 990
C
C Check axes: first does the first axis include a weight?
C
      IF (TYPE(1).NE.'COMPLEX') THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'First axis not COMPLEX')
         GO TO 999
      ELSE
         INC = NAXIS(1)
      END IF
C
C Is a stokes axis present?
C
      DO 10 IAX = 1, NAX
         IF (TYPE(IAX).EQ.'STOKES') GO TO 11
 10   CONTINUE
      CALL ERRREPOR (ERRBDARG, ROUTINE, 'No stokes axes')
      GO TO 999
 11   CONTINUE
      NSTOKE = NAXIS(IAX)
      ISIQUV = RVAL(IAX).GT.0.0
C
C Find scaling numbers
C 
      FREQ = 0.0D0
      DO 15 IAX = 1, NAX
         IF (TYPE(IAX).EQ.'FREQ') THEN
            FREQ = RVAL(IAX)
         END IF
  15  CONTINUE
      IF (FREQ.EQ.0.0D0) THEN
         CALL ERRREPOR (ERRNTFND, ROUTINE, 'No frequency')
         GO TO 999
      END IF
C
C Now scale and rename to get U,V,W in wavelengths
C
      CALL ARRSCALE (STRM2(NAME, 'UU'), SNGL(FREQ), 0.0,
     1   STRM2(NAME, 'UU'))
      CALL ARRSCALE (STRM2(NAME, 'VV'), SNGL(FREQ), 0.0,
     1   STRM2(NAME, 'VV'))
      CALL ARRSCALE (STRM2(NAME, 'WW'), SNGL(FREQ), 0.0,
     1   STRM2(NAME, 'WW'))
C
C Find new header info
C
      ONAX = 0
      DO 30 IAX = 1, NAX-1
         IF ((TYPE(IAX).NE.'COMPLEX').AND.(TYPE(IAX).NE.'STOKES'))
     1      THEN
            ONAX = ONAX + 1
            ONAXIS(ONAX) = NAXIS(IAX)
            OTYPE(ONAX) = TYPE(IAX)
            ORPIX(ONAX) = RPIX(IAX)
            ORVAL(ONAX) = RVAL(IAX)
            ODELT(ONAX) = DELT(IAX)
            OROTA(ONAX) = ROTA(IAX)
         END IF
  30  CONTINUE
C
C Convert to required form in-place
C
C AIPS FORMAT: includes weight
C
      IF (INC.EQ.3) THEN
         IF (NSTOKE.EQ.4) THEN
            IF (ISIQUV) THEN
               CALL MSGPUT ('Visibilities in IQUV format already', 'I')
            ELSE
               CALL MSGPUT ('Converting visibilities to IQUV format', 
     1            'I')
               CALL VISCONVS (MEMR(AADD), NAXIS(NAX), 'STD')
            END IF
         ELSEIF (NSTOKE.EQ.2) THEN
            IF (ISIQUV) THEN
               CALL MSGPUT ('Visibilities in IV format already', 'I')
            ELSE
               CALL MSGPUT ('Converting visibilities to IV format', 
     1            'I')
               CALL VISCONV2 (MEMR(AADD), NAXIS(NAX), 'STD')
            END IF
         END IF
C
C PIPELINE FORMAT: no weight
C
      ELSE IF (INC.EQ.2) THEN
         WPADD = DATADD (STRM2(NAME, 'WEIGHT'))
         IF (NSTOKE.EQ.4) THEN
            CALL VISCONVP (MEMR(AADD), MEMR(WPADD), NAXIS(NAX), 
     1        'STD')
         END IF
C
C UNKNOWN FORMAT:
C
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Unable to read such data')
         GO TO 999
      END IF
      STEP = INC * NSTOKE
C
C Now re-arrange into storage as SDE expects it. e.g. VIS/OBS/I/VIS
C for the visibility data, and VIS/OBS/I/WT for the weights
C
      DO 20 ISTOKE = 1, NSTOKE
C
C Did we want this Stokes parameter?
C
         IF (STOKES.EQ.'*'.OR.STRMATCH (STKTYPE(ISTOKE), STOKES)) THEN
            CALL MSGPUT ('Stokes type '//STKTYPE(ISTOKE), 'I')
            SNAME = STRM3(NAME, 'OBS', STKTYPE(ISTOKE))
            CALL CRDPUT (SNAME, ONAX, OTYPE, ONAXIS, ORVAL, ORPIX, 
     1         ODELT, OROTA)
            SNAX = 1
            SNAXIS(1) = NAXIS(NAX)
C
C Do VIS's
C
            STYPE = 'X'
            SNAME = STRM2 (SNAME, 'VIS')
            CALL DATMAKAR (SNAME, SNAX, SNAXIS, STYPE, SADD)
            CALL PIXRCOPY (MEMR(AADD+INC*(ISTOKE-1)), 0, STEP, 
     1         MEMX(SADD), 0, 2, SNAXIS(1))
            CALL PIXRCOPY (MEMR(AADD+INC*(ISTOKE-1)), 1, STEP, 
     1         MEMX(SADD), 1, 2, SNAXIS(1))
C
C Now do Weights
C
            WTYPE = 'R'
            WNAME = STRM4(NAME, 'OBS', STKTYPE(ISTOKE), 'WT')
            CALL DATMAKAR (WNAME, SNAX, SNAXIS, WTYPE, WADD)
C
C AIPS FORMAT:
C
            IF (INC.EQ.3) THEN
               CALL PIXRCOPY (MEMR(AADD+INC*(ISTOKE-1)+2), 0, STEP, 
     1            MEMR(WADD), 0, 1, SNAXIS(1))
C
C PIPELINE FORMAT:
C
            ELSE IF (INC.EQ.2) THEN
               CALL PIXRCOPY (MEMR(WPADD), 0, 1, MEMR(WADD), 0, 1, 
     1            SNAXIS(1))
C
C UNKNOWN FORMAT:
C
            ELSE
               CALL ERRREPOR (ERRLOGIC, ROUTINE, 'Illegal INC')
               GO TO 999
            END IF
            IF (ERROR) GO TO 990
         END IF
  20  CONTINUE
C
C Get rid of original data and coordinates N.B. DATDELAR deletes only
C the array part of NAME
C
      CALL CRDDEL (NAME)
      CALL DATDELAR (NAME)
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
