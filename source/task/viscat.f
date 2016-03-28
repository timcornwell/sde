C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)viscat.f	1.6    10/15/93
C
      SUBROUTINE SDEMAIN
C
CD Program to concatenate visibility data
C
C Audit trail:
C	New task
C				T.J.Cornwell	April 18 1990
C	Fixed VISPUT
C				T.J.Cornwell    Nov 1 1990
C	Allowed multiple concatenations
C				T.J.Cornwell    Nov 27 1990
C	Added CopyAll switch and simple data selection
C				D.S.Briggs	May 15 1993
C	SDE mode in COPYHIS selector copies only HISTORY records
C	that appear to have originated in SDE.  Avoids a zillion
C	calibration records from AIPS that may not be relevant.
C				D.S.Briggs	Oct 15 1993
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISCAT')
C
      CHARACTER*(SYSMXNAM)	VISFILE(SYSMXIMG), OUTFILE, STOKES,
     $   		SUBCLASS, COPYHIS
      REAL		TIME(2), UVLIMITS(2)
      INTEGER		NDUMMY, IVIS, TIMR(8), NVIS, NSEL
      LOGICAL		COPYALL
C
      LOGICAL		DATEXIST
      INTEGER		ARRNPIX
C==================================================================
      CALL MSGWELCO ('I concatenate visibility data')
      CALL USRCTL
C
C Get input parameters
C
      CALL USRGETC('Vis', VISFILE, SYSMXIMG, NDUMMY)
      CALL USRGETC('Stokes', STRBUF, 1, NDUMMY)
      CALL STRUC (STRBUF, STOKES)
      CALL USRGETR ('Uvlimits', UVLIMITS, 2, NDUMMY)
      CALL USRGETI ('Timerange', TIMR, 8, NDUMMY)
      CALL USRGETC('Outfile', OUTFILE, 1, NDUMMY)
      CALL USRGETL('CopyAll', COPYALL, 1, NDUMMY)
      CALL USRGETC('CopyHis', STRBUF, 1, NDUMMY)
      CALL STRUC (STRBUF, COPYHIS)
C
      IF ((COPYHIS.NE.'T').AND.(COPYHIS.NE.'F').AND.
     $    (COPYHIS.NE.'SDE')) THEN
         COPYHIS = 'T'
         CALL MSGPUT ('CopyHis unrecognized -- set to ''T''','W')
      END IF
C
      CALL UTLTTOD (TIMR(1), TIME(1))
      CALL UTLTTOD (TIMR(5), TIME(2))
      SUBCLASS = 'OBS/' // STOKES(1:1)
C
C Get the original visibility files
C
      IVIS=1
      CALL VISGET ('Vis', VISFILE(1), STOKES, '*', ' ')
C
      IF(DATEXIST('Vis/TIME')) THEN
         CALL VISSEL ('Vis', SUBCLASS, TIME, UVLIMITS, NSEL)
         IF (ERROR) GO TO 999
         NVIS = ARRNPIX ('Vis/TIME')
         WRITE (MESSAGE, 1000) NSEL, NVIS
 1000    FORMAT ('Selected',I8,' of',I8,' visibilities')
         CALL MSGPUT (MESSAGE, 'I')
      ELSE
         CALL MSGPUT ('No TIME information', 'W')
      END IF
C
 10   CONTINUE
      IVIS=IVIS+1
      IF(VISFILE(IVIS).EQ.' ') GO TO 20
         CALL VISGET ('VisAdd', VISFILE(IVIS), STOKES, '*', ' ')
C
         IF(DATEXIST('VisAdd/TIME')) THEN
            CALL VISSEL ('VisAdd', SUBCLASS, TIME, UVLIMITS, NSEL)
            IF (ERROR) GO TO 999
            NVIS = ARRNPIX ('VisAdd/TIME')
            WRITE (MESSAGE, 1000) NSEL, NVIS
            CALL MSGPUT (MESSAGE, 'I')
         ELSE
            CALL MSGPUT ('No TIME information', 'W')
         END IF
C
         CALL VISCAT ('Vis', 'VisAdd', 'Vis', 'OBS', STOKES)
         CALL DATDELET ('VisAdd')
         GO TO 10
 20   CONTINUE
C
C Deal with history
C
      IF (COPYHIS.EQ.'F') THEN
         CALL DATDELET ('Vis/HII')
      ELSE IF (COPYHIS.EQ.'SDE') THEN
         CALL HISCOPY ('Vis', 'HisTemp')
         CALL DATDELET ('Vis/HII')
         CALL HISSDCPY ('HisTemp', 'Vis')
         CALL DATDELET ('HisTemp')
      END IF
      CALL HISINPUT ('Vis')
C
C Filter out negative weights if needed
C
      IF (.NOT.COPYALL) THEN
         CALL DATRENAM ('Vis', 'FullVis')
         CALL VISSCOPY ('FullVis', 'Vis', 'OBS', STOKES)
         CALL HISCOPY ('FullVis', 'Vis')
         CALL DATDELET ('FullVis')
      END IF
C
C Put the visibility file
C
      CALL VISPUT ('Vis', OUTFILE, 'OBS', STOKES, '*', ' ')
C
 999  CONTINUE
      END
