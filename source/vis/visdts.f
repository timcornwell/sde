C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)visdts.f	1.13    5/12/94
C
      SUBROUTINE VISDTS (NAME, CLASS, STOKES)
C
C    "Re-format a uv file just read from FITS file into a more sensible
C    format: namely put each correlator into a separate directory. The
C    names are of the form: NAME/OBS/I/VIS and NAME/OBS/I/WT for the
C    observed Stokes I visibility and weight respectively."
C    In fact do the opposite ie NAME/OBS/I/VIS -> NAME/ARRAY/DATA
C    
C    name	CH*(*)	input	Name of directory entry
C    stokes	CH*(*)	input	List of Stokes parameters to be kept
C    CLASS	CH*(*)	input	Name of class
C    Audit trail:
C	Cloned from VISSTD with major revisions 
C                                          R.G. Marson     Oct 20 1989
C	Now sets FITS keywords here as last chance. Also now can change
C	CLASS
C					T.J. Cornwell Nov 1 1990
C	Reset GCOUNT here
C					T.J. Cornwell Nov 27 1990
C	Set NAXIS(3) = 1
C					T.J. Cornwell Feb 4 1991
C	Allow for offset RPIX in frequency
C					T.J. Cornwell June 6 1991
C	Cleaned up writing of polarization data
C					T.J. Cornwell Sept 14 1992
C	Changed REFDATE to double
C					T.J. Cornwell Jan 24 1993
C	Can now write all Stokes types   
C					T.J. Cornwell May 10 1994
C	Fixed limited loop over Stokes
C					T.J. Cornwell May 11 1994
C-----------------------------------------------------------------------
#include	"stdinc.h"
#include        "ftsinc.h"
C     
      CHARACTER*(*)	NAME, STOKES, CLASS
C     
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISDTS')
C     
      CHARACTER*(SYSMXNAM) STRM2, STRM3, FTSTYPE
      CHARACTER*6       STRINT
      LOGICAL		STRMATCH, DATEXIST
C
      INTEGER		IRP, NRP
      PARAMETER		(NRP = 6)
      CHARACTER*8	PARNAMES(NRP)
C
      CHARACTER*(SYSMXNAM) STRTMP
      CHARACTER*8	TYPE(SYSMXDIM)
      CHARACTER*2	STKTYP(8)
      CHARACTER*1	ATYPE
      DOUBLE PRECISION	RVAL(SYSMXDIM), FREQ, REFDATE, DATFGETD
      REAL		RPIX (SYSMXDIM), DELT(SYSMXDIM)
      REAL              ROTA (SYSMXDIM)
      REAL		INVFREQ
      INTEGER		NAX, NAXIS(SYSMXDIM), DATADD
      INTEGER           DATEADD, DATTADD, TIMADD, IAX
      INTEGER           NUMSTKS, I, J, TOTGRPS, NUMDONE
      INTEGER           OUTADD, INADD, NDUMMY
      LOGICAL           FOUND(8), WEIGHT(8)
C     
      DATA		STKTYP	/'RR', 'LL', 'RL', 'LR', 'I','Q','U','V'/
      DATA		TOTGRPS	/0/
      DATA		PARNAMES /'UU', 'VV', 'WW', 'BASELINE',
     $			'DATE#', 'DATE##'/
C=======================================================================
      IF (ERROR) GO TO 999
C     
      CALL DATPUTL(NAME, 'GROUPS', .TRUE., 1)
C
C Find out what data exists in data base
C     
      NUMSTKS = 0
      DO 10 I = 1, 8
         IF (STRMATCH (STOKES, STKTYP(I))) THEN
            STRTMP = STRM3(NAME,CLASS,STKTYP(I))
            IF (DATEXIST(STRTMP).AND.
     $           DATEXIST(STRM2(STRTMP,'VIS'))) THEN
               FOUND(I) = .TRUE.
               NUMSTKS = NUMSTKS + 1
               IF (DATEXIST (STRM2 (STRTMP, 'WT'))) THEN
                  WEIGHT(I) = .TRUE.
               ELSE
                  WEIGHT(I) = .FALSE.
                  CALL MSGPUT('No weights found for stokes parameter '//
     $                 STKTYP(I), 'W')
               END IF
            ELSE
               FOUND(I) = .FALSE.
            END IF
         ELSE
            FOUND(I) = .FALSE.
         END IF
 10   CONTINUE
C     
C Check that something was found
C     
      IF (NUMSTKS.EQ.0) THEN 
         CALL ERRREPOR(ERRNTFND, ROUTINE, 'No stokes parameters found')
         GO TO 999
      END IF
      IF (NUMSTKS.GT.4) THEN 
         CALL ERRREPOR(ERRLOGIC, ROUTINE,
     $      'Too many stokes parameters found')
         GO TO 999
      END IF
C     
C Now find co-ords for the data array
C     
      NUMDONE = 0
      DO 20 I = 1, 8
         IF (FOUND(I) .AND. (NUMDONE.EQ.0)) THEN
            STRTMP = STRM3 (NAME, CLASS, STKTYP(I))
            IF(TOTGRPS.EQ.0) THEN
               CALL DATGETAR (STRM2(STRTMP,'VIS'), NAX, NAXIS, ATYPE,
     $            NDUMMY)
               TOTGRPS = NAXIS(1)
            END IF
            NUMDONE = I
            CALL CRDGET (STRTMP, NAX, TYPE, NAXIS, RVAL, RPIX, DELT,
     $         ROTA)
         END IF
 20   CONTINUE
      DO 21 I = NAX, 1, -1
         TYPE(I+2) = TYPE(I)
         NAXIS(I+2) = NAXIS(I)
         RVAL(I+2) = RVAL(I)
         RPIX(I+2) = RPIX(I)
         DELT(I+2) = DELT(I)
         ROTA(I+2) = ROTA(I)
 21   CONTINUE
      NAX = NAX + 3
      TYPE(1) = 'COMPLEX'
      NAXIS(1) = 3
      RVAL(1) = 1
      RPIX(1) = 1
      DELT(1) = 1
      ROTA(1) = 0
C
      TYPE(2) = 'STOKES'
      RPIX(2) = 1
C
C Do all cases exhaustively
C
      IF(FOUND(5).AND..NOT.FOUND(6).AND.
     $   .NOT.FOUND(7).AND..NOT.FOUND(8)) THEN
         CALL MSGPUT ('Writing Stokes I', 'I')
         NAXIS(2) = 1
         RVAL(2) = 1
         DELT(2) = 1
      ELSE IF(FOUND(5).AND.FOUND(6).AND.FOUND(7).AND.FOUND(8)) THEN
         CALL MSGPUT ('Writing Stokes I,Q,U,V', 'I')
         NAXIS(2) = 4
         RVAL(2) = 1
         DELT(2) = 1
      ELSE IF(FOUND(5).AND.
     $      .NOT.FOUND(6).AND..NOT.FOUND(7).AND.FOUND(8)) THEN
         CALL MSGPUT ('Writing Stokes I,V', 'I')
         NAXIS(2) = 2
         RVAL(2) = 1
         DELT(2) = 3
      ELSE IF(FOUND(1).AND.FOUND(2).AND.FOUND(3).AND.FOUND(4)) THEN
         CALL MSGPUT ('Writing Stokes RR,LL,RL,LR', 'I')
         NAXIS(2) = 4
         RVAL(2) = -1
         DELT(2) = -1
      ELSE IF(FOUND(1).AND.FOUND(2).AND.
     $      .NOT.FOUND(3).AND..NOT.FOUND(4)) THEN
         CALL MSGPUT ('Writing Stokes RR,LL', 'I')
         NAXIS(2) = 2
         RVAL(2) = -1
         DELT(2) = -1
      ELSE IF(FOUND(1).AND..NOT.FOUND(2).
     $      AND..NOT.FOUND(3).AND..NOT.FOUND(4)) THEN
         CALL MSGPUT ('Writing Stokes RR', 'I')
         NAXIS(2) = 1
         RVAL(2) = -1
         DELT(2) = -1
      ELSE IF(.NOT.FOUND(1).AND.FOUND(2).
     $      AND..NOT.FOUND(3).AND..NOT.FOUND(4)) THEN
         CALL MSGPUT ('Writing Stokes LL', 'I')
         NAXIS(2) = 1
         RVAL(2) = -2
         DELT(2) = -1
      ELSE
         CALL ERRREPOR (ERRLOGIC, ROUTINE,
     $      'Cannot write this combination of Stokes parameters')
         GO TO 999
      END IF
C
      NAXIS(3) = 1
      NAXIS(NAX) = TOTGRPS
      CALL DATPUTI(NAME, 'GCOUNT', TOTGRPS, 1)
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
      ELSE
         INVFREQ = SNGL(1D0 / FREQ)
      END IF
C
C Now scale and rename to get U,V,W in seconds
C
      CALL ARRSCALE (STRM2(NAME, 'UU'), INVFREQ, 0.0,
     1     STRM2(NAME, 'UU'))
      CALL ARRSCALE (STRM2(NAME, 'VV'), INVFREQ, 0.0,
     1     STRM2(NAME, 'VV'))
      CALL ARRSCALE (STRM2(NAME, 'WW'), INVFREQ, 0.0,
     1     STRM2(NAME, 'WW'))
C     
C Now create an array to hold the data
C     
      ATYPE = 'R'
      IF (.NOT. DATEXIST (STRM2 (NAME, 'ARRAY'))) THEN
         CALL DATMAKAR(NAME, NAX, NAXIS, ATYPE, OUTADD)
         CALL CRDPUT (NAME, NAX, TYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
      ELSE
         CALL DATGETAR(NAME, NAX, NAXIS, ATYPE, OUTADD)
         CALL CRDGET (NAME, NAX, TYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
      END IF
C     
C Put stuff in it
C     
      DO 25 I = 1, 8
         IF (FOUND(I)) THEN
            STRTMP = STRM3 (NAME, CLASS, STKTYP(I))
            INADD = DATADD (STRM2 (STRTMP, 'VIS'))
            DO 24 J = 0, TOTGRPS - 1
              MEMR (OUTADD + 3*NUMSTKS*J) = REAL(MEMX(INADD+J))
              MEMR (OUTADD + 1 + 3*NUMSTKS*J) = AIMAG(MEMX(INADD+J))
 24         CONTINUE
            IF (WEIGHT(I)) THEN
               INADD = DATADD (STRM2 (STRTMP, 'WT'))
               DO 22 J = 0, TOTGRPS - 1
                  MEMR (OUTADD + 2 + 3*NUMSTKS*J) = MEMR(INADD+J)
 22            CONTINUE
            ELSE
               DO 23 J = 0, TOTGRPS - 1
                  MEMR (OUTADD + 2 + 3*NUMSTKS*J) = 1.0
 23            CONTINUE
            END IF
            OUTADD = OUTADD + 3
         END IF
 25   CONTINUE
C
C Now fill in the standard items to write
C
      STRTMP = STRM2(NAME, 'BSCALE')
      IF (DATEXIST(STRTMP)) CALL DATDELET (STRTMP)
      STRTMP = STRM2(NAME, 'BZERO')
      IF (DATEXIST(STRTMP)) CALL DATDELET (STRTMP)
      DO 100 IRP = 1, NRP
         CALL DATPUTC (NAME, 'PTYPE'//STRINT(IRP), PARNAMES(IRP), 1)
         STRTMP = STRM2(NAME, 'PSCAL'//STRINT(IRP))
         IF (DATEXIST(STRTMP)) CALL DATDELET (STRTMP)
         STRTMP = STRM2(NAME, 'PZERO'//STRINT(IRP))
         IF (DATEXIST(STRTMP)) CALL DATDELET (STRTMP)
 100  CONTINUE
      CALL DATPUTI (NAME, 'PCOUNT', NRP, 1)
C     
C Now convert the TIME array to a DATE# and DATE## array
C     
      IF (DATEXIST (STRM2 (NAME, 'TIME'))) THEN
         IF (DATEXIST ( STRM2 (NAME, 'REFDATE'))) THEN
            REFDATE = DATFGETD (NAME, 'REFDATE')
         ELSE
            REFDATE = 2433282.0D0
            CALL MSGPUT('No Reference Date found, assuming 1950',
     $           'W')
         END IF
         CALL DATGETAR (STRM2 (NAME, 'TIME'), NAX, NAXIS, ATYPE,
     $      TIMADD)
         CALL DATMAKAR (STRM2 (NAME, 'DATE#'), NAX, NAXIS, 'R',
     $      DATEADD)
         CALL DATMAKAR (STRM2 (NAME, 'DATE##'), NAX, NAXIS, 'R',
     $      DATTADD)
         IF (DATEXIST('SYI/FITS')) THEN
            CALL DATGETC ('SYI', 'FITS', FTSTYPE, 1, NDUMMY)
         ELSE
            FTSTYPE='IEEE'
         END IF
         IF (FTSTYPE.EQ.'IEEE') THEN
            CALL DATPUTD (NAME, 'BSCALE', 1.0D0, 1)
            CALL DATPUTD (NAME, 'BZERO', 0.0D0, 1)
            CALL DATPUTD (NAME, 'PZERO1', 0.0D0, 1)
            CALL DATPUTD (NAME, 'PZERO2', 0.0D0, 1)
            CALL DATPUTD (NAME, 'PZERO3', 0.0D0, 1)
            CALL DATPUTD (NAME, 'PSCAL4', 1.0D0, 1)
            CALL DATPUTD (NAME, 'PZERO4', 0.0D0, 1)
            CALL DATPUTD (NAME, 'PSCAL5', 1.0D0, 1)
            CALL DATPUTD (NAME, 'PZERO5', REFDATE, 1)
            CALL DATPUTD (NAME, 'PSCAL6', 1.0D0, 1)
            CALL DATPUTD (NAME, 'PZERO6', 0.0D0, 1)
            DO 36 I = 0, NAXIS(1) - 1
               MEMR (DATEADD + I) = SNGL(REFDATE)
               MEMR (DATTADD + I) = MEMR(TIMADD + I)
 36         CONTINUE
         ELSE
            CALL DATPUTD (NAME, 'PZERO1', 0.0D0, 1)
            CALL DATPUTD (NAME, 'PZERO2', 0.0D0, 1)
            CALL DATPUTD (NAME, 'PZERO3', 0.0D0, 1)
            CALL DATPUTD (NAME, 'PSCAL4', 1.0D0, 1)
            CALL DATPUTD (NAME, 'PZERO4', 0.0D0, 1)
            CALL DATPUTD (NAME, 'PSCAL5', 0.25D0, 1)
            CALL DATPUTD (NAME, 'PZERO5', REFDATE, 1)
            DO 35 I = 0, NAXIS(1) - 1
               MEMR (DATEADD + I) = 
     $              FLOAT (INT (4.0 * MEMR (TIMADD + I))) / 4.0
               MEMR (DATTADD + I) =  MEMR (TIMADD + I) -
     $            MEMR (DATEADD + I)
               MEMR (DATEADD + I) = MEMR (DATEADD + I) +
     $            SNGL(REFDATE)
 35         CONTINUE
         END IF
         CALL DATDELET (STRM2(NAME, 'TIME'))
      ELSE
         CALL ERRREPOR (ERRNTFND, ROUTINE, 'Time array not found')
         GOTO 990
      END IF
C     
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C     
 999  CONTINUE
      END
