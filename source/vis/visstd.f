C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)visstd.f	1.22	 5/1/95
C
      SUBROUTINE VISSTD (NAME, STOKES)
C
CD Re-format a uv file just read from FITS file into a more sensible
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
C	STOKES need no longer be comma separated
C				T.J.Cornwell	Nov 27 1990
C	No longer subtracts 0.5 from day number
C				T.J.Cornwell	Dec 8 1990
C       Knows about the "convention" that UU-L means UU in wavelengths
C                               R.G. Marson     Feb 14 1990
C	Fixed Ralph's fix so that UU is still recognized. Call to
C	ARRSCALE had incorrect first argument.
C                               T.J. Cornwell    Feb 19 1990
C	Now subtracts 0.5 from day number to get MJD. Also
C	inserted ghastly kludge to get around AIPS induced roundoff
C	error in IEEE-based DATEs
C				T.J.Cornwell	March 16 1991
C	Ignore IF axis
C				T.J.Cornwell	May 28 1991
C	Allow for offset RPIX in frequency
C					T.J. Cornwell June 6 1991
C	Loop over all Stokes, not just NSTOKES, when testing against
C	STOKES
C					T.J. Cornwell June 26 1992
C	Changed to be consistent with VISCONVS and VISCONV2 in where the
C	various Stokes parameters are expected to be.
C					T.J. Cornwell Sept 10 1992
C	Changed REFDATE to double
C					T.J. Cornwell Jan 24 1993
C	Can now read new AIPS FITS convention for UU,VV,WW, etc.
C					T.J. Cornwell May 10 1994
C	Added support for UU-L-SIN, etc.
C					T.J. Cornwell Dec 20 1994
C	Allow use of uv fits files that contain only one "DATE" as a
C	PTYPEn; code had assumed that there were 2 PTYPEn's with "DATE",
C	which is true of aips-generated fits files, but not miriad-
C	generated fits files.
C					M. Stupar     Apr 18 1994
C	Added support for 'UU  -SIN', which showed up from AIPS
C					M.A. Holdaway	Apr 30 1995
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
       CHARACTER*(*)	NAME, STOKES
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISSTD')
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
      CHARACTER*1	ATYPE, STYPE, WTYPE
      CHARACTER*(SYSMXNAM)	SNAME, WNAME, STRM2, STRM3, STRM4
      CHARACTER*(SYSMXNAM)	FNAME, TNAME, TNAME2, NNAME, NNAME2,
     $     			NNAME3
      CHARACTER*1	STKTYPE(4)
      CHARACTER*7	DATE
      LOGICAL		DATEXIST, ISIQUV
      INTEGER		NDUMMY, INC, DATADD
      REAL		RTEMP
      DOUBLE PRECISION	REFDATE
      DATA		STKTYPE	/'I','V','Q','U'/
C=======================================================================
      IF (ERROR) GO TO 999
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
            FREQ = RVAL(IAX) + (1.0-RPIX(IAX)) * DELT(IAX)
         END IF
  15  CONTINUE
      IF (FREQ.EQ.0.0D0) THEN
         CALL ERRREPOR (ERRNTFND, ROUTINE, 'No frequency')
         GO TO 999
      END IF
C
C Now scale and rename to get U,V,W in wavelengths (if necessary)
C
      FNAME = STRM2(NAME, 'UU')
      NNAME = STRM2(NAME, 'UU--')
      NNAME2 = STRM2(NAME, 'UU---SIN')
      NNAME3 = STRM2(NAME, 'UU  -SIN')
      TNAME2 = STRM2(NAME, 'UU-L-SIN')
      TNAME = STRM2(NAME, 'UU-L')
      IF (DATEXIST (FNAME)) THEN
         CALL ARRSCALE (FNAME, SNGL(FREQ), 0.0, FNAME)
      ELSEIF (DATEXIST (NNAME)) THEN
         CALL ARRSCALE (NNAME, SNGL(FREQ), 0.0, FNAME)
      ELSEIF (DATEXIST (NNAME2)) THEN
         CALL ARRSCALE (NNAME2, SNGL(FREQ), 0.0, FNAME)
      ELSEIF (DATEXIST (NNAME3)) THEN
         CALL ARRSCALE (NNAME3, SNGL(FREQ), 0.0, FNAME)
      ELSEIF (DATEXIST (TNAME2)) THEN
         CALL DATRENAM (TNAME2, FNAME)
      ELSE IF (DATEXIST (TNAME)) THEN
         CALL DATRENAM (TNAME, FNAME)
      ELSE
         CALL ERRREPOR (ERRNTFND, ROUTINE, 'PTYPE=UU not in header')
         GO TO 999
      END IF
C
      FNAME = STRM2(NAME, 'VV')
      NNAME = STRM2(NAME, 'VV--')
      NNAME2 = STRM2(NAME, 'VV---SIN')
      NNAME3 = STRM2(NAME, 'VV  -SIN')
      TNAME2 = STRM2(NAME, 'VV-L-SIN')
      TNAME = STRM2(NAME, 'VV-L')
      IF (DATEXIST (FNAME)) THEN
         CALL ARRSCALE (FNAME, SNGL(FREQ), 0.0, FNAME)
      ELSE IF (DATEXIST (NNAME)) THEN
         CALL ARRSCALE (NNAME, SNGL(FREQ), 0.0, FNAME)
      ELSE IF (DATEXIST (NNAME2)) THEN
         CALL ARRSCALE (NNAME2, SNGL(FREQ), 0.0, FNAME)
      ELSE IF (DATEXIST (NNAME3)) THEN
         CALL ARRSCALE (NNAME3, SNGL(FREQ), 0.0, FNAME)
      ELSE IF (DATEXIST (TNAME2)) THEN
         CALL DATRENAM (TNAME2, FNAME)
      ELSE IF (DATEXIST (TNAME)) THEN
         CALL DATRENAM (TNAME, FNAME)
      ELSE
         CALL ERRREPOR (ERRNTFND, ROUTINE, 'PTYPE=VV not in header')
         GO TO 999
      END IF
C
      FNAME = STRM2(NAME, 'WW')
      NNAME = STRM2(NAME, 'WW--')
      NNAME2 = STRM2(NAME, 'WW---SIN')
      NNAME3 = STRM2(NAME, 'WW  -SIN')
      TNAME2 = STRM2(NAME, 'WW-L-SIN')
      TNAME = STRM2(NAME, 'WW-L')
      IF (DATEXIST (FNAME)) THEN
         CALL ARRSCALE (FNAME, SNGL(FREQ), 0.0, FNAME)
      ELSE IF (DATEXIST (NNAME)) THEN
         CALL ARRSCALE (NNAME, SNGL(FREQ), 0.0, FNAME)
      ELSE IF (DATEXIST (NNAME2)) THEN
         CALL ARRSCALE (NNAME2, SNGL(FREQ), 0.0, FNAME)
      ELSE IF (DATEXIST (NNAME3)) THEN
         CALL ARRSCALE (NNAME3, SNGL(FREQ), 0.0, FNAME)
      ELSE IF (DATEXIST (TNAME2)) THEN
         CALL DATRENAM (TNAME2, FNAME)
      ELSE IF (DATEXIST (TNAME)) THEN
         CALL DATRENAM (TNAME, FNAME)
      ELSE
         CALL ERRREPOR (ERRNTFND, ROUTINE, 'PTYPE=WW not in header')
         GO TO 999
      END IF
C
C Find new header info: removing COMPLEX, STOKES and IF types
C
      ONAX = 0
      DO 30 IAX = 1, NAX-1
         IF ((TYPE(IAX).NE.'COMPLEX').AND.(TYPE(IAX).NE.'STOKES').AND.
     $      (TYPE(IAX).NE.'IF'))
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
         CALL MSGPUT ('Found separate weights')
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
      DO 20 ISTOKE = 1, 4
C
C Did we want this Stokes parameter?
C
         IF (STOKES.EQ.'*'.OR.
     $      (INDEX (STOKES, STKTYPE(ISTOKE)).NE.0)) THEN
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
               CALL PIXRCOPY (MEMR(AADD+INC*(ISTOKE-1)), 2, STEP, 
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
C Make TIME array
C
      IF(DATEXIST(STRM2(NAME, 'REFDATE'))) THEN
         CALL DATGETD(NAME, 'REFDATE', REFDATE, 1, NDUMMY)
      ELSE 
         CALL ARRSTAT (STRM2(NAME, 'DATE#'), ' ')
         CALL DATGETR (STRM2(NAME, 'DATE#'), 'ARRMIN', RTEMP, 1,
     1      NDUMMY)
         IF (ERROR) GO TO 990
         REFDATE = FLOAT(INT(RTEMP))
         CALL ARRSCALE (STRM2(NAME, 'DATE#'), 1.0, -SNGL(REFDATE),
     1      STRM2(NAME, 'DATE#'))
         REFDATE = REFDATE + 2400000.5D0
         CALL DATPUTD(NAME, 'REFDATE', REFDATE, 1)
      END IF
      CALL UTLMJD2D (REFDATE-2400000.5D0, DATE)
      WRITE (MESSAGE, 1000) DATE
 1000 FORMAT ('First observation on ',A7)
      CALL MSGPUT (MESSAGE, 'I')
C
C Delete NAME/DATE## only if it exists.
C
      IF (DATEXIST(STRM2(NAME, 'DATE##'))) THEN
         CALL ARRADD (STRM2(NAME, 'DATE#'),
     1                STRM2(NAME, 'DATE##'),
     2                STRM2(NAME, 'DATE#'))
         CALL DATDELET (STRM2(NAME, 'DATE##'))
      ENDIF
      CALL DATRENAM (STRM2(NAME, 'DATE#'), STRM2(NAME, 'TIME'))
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
