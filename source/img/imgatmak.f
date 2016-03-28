C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgatmak.f	1.1    12/27/91
C
      SUBROUTINE IMGATMAK (VIS, SUBCLASS, STAT, MATYPE, MODE, IMG)
C
CD Make an antenna-time image directory entry.
C
C The size of the image to be made is taken from the Gain information
C attached to the visibility directory.  As such, this is a fairly
C special purpose routine.  Space is allocated for the array, but nothing
C is put into it.
C
C	VIS		CH*(*)	input	Name of visibility dir
C	SUBCLASS	CH*(*)	input	Subclass of gain info.
C	MATYPE		CH*(*)  input	Type of image, either 'R' or 'X'
C	STAT		CH*(*)	input	Name of statistics dir
C	STAT/MINANT	INT	input	Minimum antenna number in data
C	STAT/MAXANT	INT	input	Maximum antenna number in data
C	MODE		CH*(*)	input	Fill with ... 'XGain' or ' '
C	IMG		CH*(*)	input	Name of output image
C
C Audit trail:
C	Cloned from IMGMAK
C				D.S.Briggs	Sept 5 1991
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	VIS, SUBCLASS, STAT, MATYPE, MODE, IMG
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGMAKE')
C
      INTEGER		NAX, NAXIS(SYSMXDIM), IADD
      CHARACTER*1	ATYPE
      CHARACTER*8	TYPE(SYSMXDIM)
      REAL		RPIX(SYSMXDIM), DELT(SYSMXDIM),
     1			ROTA(SYSMXDIM)
      DOUBLE PRECISION	RVAL(SYSMXDIM)
      INTEGER 		NDUMMY, GADD, TADD, NUMANT, NUMINT,
     $   		I, IA1, IA2
      DOUBLE PRECISION	DVALUE
      REAL		RVALUE
C
      LOGICAL		DATEXIST
      INTEGER		DATFGETI
      CHARACTER*(SYSMXNAM)	DATE, CVALUE, STRM3
C==================================================================
      IF (ERROR) GO TO 999
C
      IF (.NOT.DATEXIST(STRM3(VIS, SUBCLASS, 'ANTGAIN'))) THEN
         CALL ERRREPOR (ERRNTFND, ROUTINE,
     $                  'No Antenna Gain information')
         GO TO 999
      END IF
C
      CALL DATGETAR (STRM3(VIS, SUBCLASS, 'ANTGAIN'),
     $               NAX, NAXIS, ATYPE, GADD)
      IF ((NAX.NE.2).OR.(ATYPE.NE.'X')) THEN
         CALL ERRREPOR (ERRFATAL, ROUTINE, 'Malformed ANTGAIN array')
         GO TO 999
      END IF
      NUMANT = NAXIS(1)
      NUMINT = NAXIS(2)
C
      CALL DATGETAR (STRM3(VIS, SUBCLASS, 'GAINTIME'),
     $               NAX, NAXIS, ATYPE, TADD)
      IF ((NAX.NE.1).OR.(ATYPE.NE.'R').OR.(NAXIS(1).NE.NUMINT)) THEN
         CALL ERRREPOR (ERRFATAL, ROUTINE, 'Malformed GAINTIME array')
         GO TO 999
      END IF
C
C Put together the axis information
C
      ATYPE = MATYPE
      NAX = 2
C
C Time axis
C
      TYPE(1) = 'TIME'
      DELT(1) = MEMR(TADD+1) - MEMR(TADD)
      RVAL(1) = MEMR(TADD)
      NAXIS(1) = NUMINT
      RPIX(1) = 1.0
      ROTA(1) = 0.0
C
C Antenna axis
C
      IA1 = DATFGETI(STAT,'MINANT')
      IA2 = DATFGETI(STAT,'MAXANT')
      TYPE(2) = 'ANTENNA'
      DELT(2) = 1.0
      RVAL(2) = IA1
      NAXIS(2) = IA2 - IA1 + 1
      RPIX(2) = 1.0
      ROTA(2) = 0.0
C
C Only make the array if required, and fill it if requested
C
      IF(ATYPE.NE.' ') THEN
         CALL DATMAKAR(IMG, NAX, NAXIS, ATYPE, IADD)
C
         IF (MODE.EQ.'XGain') THEN
            IF (ATYPE.NE.'X') THEN
               CALL ERRREPOR(ERRBDARG, ROUTINE,
     $            'Array type must be ''X'' for XGain ATImg')
               GO TO 999
            END IF
C
            CALL MSGPUT('Filling ATImg with complex gains','I')
            DO 100 I = IA1, IA2
               CALL PIXXCOPY (MEMX(GADD+I-1), NUMANT,
     $            MEMX(IADD+(I-1)*NUMINT), 1, NUMINT)
 100        CONTINUE
         END IF
      END IF
C
      CALL CRDPUT (IMG, NAX, TYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
C
C Add in the standard stuff
C
      CALL DATPUTC (IMG, 'BUNIT', 'GAIN', 1)
C
      IF (ERROR) GOTO 990
      CALL DATGETC (VIS, 'OBJECT', CVALUE, 1, NDUMMY)
      IF(ERROR) THEN
         CALL ERRCANCE
         CVALUE = ' '
      END IF
      CALL DATPUTC (IMG, 'OBJECT', CVALUE, 1)
C
      IF (ERROR) GOTO 990
      CALL DATGETC (VIS, 'INSTRUME', CVALUE, 1, NDUMMY)
      IF(ERROR) THEN
         CALL ERRCANCE
         CVALUE = ' '
      END IF
      CALL DATPUTC (IMG, 'INSTRUME', CVALUE, 1)
C
      IF (ERROR) GOTO 990
      CALL DATGETC (VIS, 'TELESCOP', CVALUE, 1, NDUMMY)
      IF(ERROR) THEN
         CALL ERRCANCE
         CVALUE = ' '
      END IF
      CALL DATPUTC (IMG, 'TELESCOP', CVALUE, 1)
C
      IF (ERROR) GOTO 990
      CALL DATGETC (VIS, 'OBSERVER', CVALUE, 1, NDUMMY)
      IF(ERROR) THEN
         CALL ERRCANCE
         CVALUE = ' '
      END IF
      CALL DATPUTC (IMG, 'OBSERVER', CVALUE, 1)
C
      IF (ERROR) GOTO 990
      CALL DATGETC (VIS, 'DATE-OBS', CVALUE, 1, NDUMMY)
      IF(ERROR) THEN
         CALL ERRCANCE
         CVALUE = ' '
      END IF
      CALL DATPUTC (IMG, 'DATE-OBS', CVALUE, 1)
C
      CALL SYSDATEC (DATE)
      CALL DATPUTC (IMG, 'DATE-MAP', DATE, 1)
C
      IF (ERROR) GOTO 990
      CALL DATGETD (VIS, 'OBSRA', DVALUE, 1, NDUMMY)
      IF(ERROR) THEN
         CALL ERRCANCE
         DVALUE = 0.0D0
      END IF
      CALL DATPUTD (IMG, 'OBSRA', DVALUE, 1)
C
      IF (ERROR) GOTO 990
      CALL DATGETD (VIS, 'OBSDEC', DVALUE, 1, NDUMMY)
      IF(ERROR) THEN
         CALL ERRCANCE
         DVALUE = 0.0D0
      END IF
      CALL DATPUTD (IMG, 'OBSDEC', DVALUE, 1)
C
      IF (ERROR) GOTO 990
      CALL DATGETR (VIS, 'EPOCH', RVALUE, 1, NDUMMY)
      IF(ERROR) THEN
         CALL ERRCANCE
         RVALUE = 0.0
      END IF
      CALL DATPUTR (IMG, 'EPOCH', RVALUE, 1)
C
C Copy history info
C
      CALL HISCOPY (VIS, IMG)
C
      CALL DATSETTP (IMG, 'IMAGE')
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
