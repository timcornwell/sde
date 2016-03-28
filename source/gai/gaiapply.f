C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)gaiapply.f	1.2    5/15/92
C
       SUBROUTINE GAIAPPLY (INNAME, INSUB, CALNAME, CALSUB, OUTNAME,
     $   OUTSUB)
C
CD Apply gains to visibility data.
C
C	INNAME	CH*(*)	input	Name of directory entry
C	INSUB	CH*(*)	input	Subclass for VIS data
C	CALNAME	CH*(*)	input	Name of directory with GAIN info
C	CALSUB	CH*(*)	input	Subclass where GAIN into is found
C	OUTNAME	CH*(*)	input	Name of output directory.
C	OUTSUB	CH*(*)	input	Name of output subdir
C	.../GAITYPE	CH*(*)	input	'2PT' or 'BOXCAR'  ('BOXCAR')
C	.../GAITINT	REAL	input	Preaveraging time for gains  (0.0)
C	.../GAIAVG	CH*(*)	input	'AMPPHI' or ' '  (' ')
C
C GAITYPE, GAITINT, and GAIAVG should all hang off of INNAME/INSUB.
C GAITYPE controls the type of interpolation used when applying the gain
C arrays to the data.  'BOXCAR' simply means 'use the nearest gain', while
C '2PT' will do a two point interpolation in both amplitude and phase
C for each visibility point.
C If GAITINT is present and non zero, the gain arrays will be boxcar averaged
C together prior to being passed to the interpolation routines.  Averaging
C time is specified in days. This averaging may be done arithmetically
C (GAIAVG = ' ') or separately in amplitude in phase, (GAIAVG = 'AMPPHI')
C
C Audit trail:
C	New routine
C				D.S.Briggs	10 Sept 1991
C	Preaveraging of gains added.  Mode argument moved to directory
C	entry as GAITYPE.
C				D.S.Briggs	28 Oct 1991
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	INNAME, INSUB, CALNAME, CALSUB, OUTNAME,
     $  		OUTSUB
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'GAIAPPLY')
C
      INTEGER		NAX, NAXIS(SYSMXDIM), GADD, BADD, NVIS,
     $   		NGAIN, IVADD, OVADD, NANT, VTADD, GTADD,
     $			IWADD, OWADD, MINANT, MAXANT, NDUMMY
      CHARACTER*1	ATYPE
      REAL		RPIX (SYSMXDIM), DELT(SYSMXDIM),
     $			ROTA (SYSMXDIM), GAITINT
      DOUBLE PRECISION	RVAL(SYSMXDIM)
      CHARACTER*8	TYPE(SYSMXDIM)
      CHARACTER*11	TSTRING
      CHARACTER*(SYSMXNAM)	INN, OUTN, CALN, GAITYPE, GAIAVG
C
      CHARACTER*(SYSMXNAM)	STRM2
      CHARACTER*12	STRTIMC
      INTEGER		DATADD
      LOGICAL		DATEXIST
C=======================================================================
      IF (ERROR) GO TO 999
C
C Get addresses of all the relevant parts
C
      INN = STRM2(INNAME,INSUB)
      OUTN = STRM2(OUTNAME,OUTSUB)
      CALN = STRM2(CALNAME,CALSUB)
C
      CALL DATGETAR (STRM2(INN,'VIS'), NAX, NAXIS, ATYPE, IVADD)
      NVIS = NAXIS(1)
C
      CALL DATGETAR (STRM2(CALN,'ANTGAIN'), NAX, NAXIS, ATYPE, GADD)
      NANT = NAXIS(1)
      NGAIN = NAXIS(2)
C
      IWADD = DATADD (STRM2(INN, 'WT'))
      GTADD = DATADD (STRM2(CALN, 'GAINTIME'))
      VTADD = DATADD (STRM2(INNAME, 'TIME'))
      BADD = DATADD (STRM2(INNAME, 'BASELINE'))
      IF (ERROR) GO TO 990
C
C Deal with output data set
C
      IF (OUTN.NE.INN) THEN
         IF (.NOT.DATEXIST(OUTNAME)) CALL DATCREAT(OUTNAME)
         IF (.NOT.DATEXIST(OUTN)) CALL DATCREAT(OUTN)
C
         IF (DATEXIST(STRM2(OUTN,'VIS'))) THEN
            CALL DATGETAR (STRM2(OUTN,'VIS'), NAX, NAXIS, ATYPE, OVADD)
            IF ((NAX.NE.1).OR.(NAXIS(1).NE.NVIS)) THEN
               CALL ERRREPOR(ERRBDARG, ROUTINE,
     $            'Output array is wrong size')
               GO TO 999
            END IF
            OWADD = DATADD (STRM2(INN,'WT'))
         ELSE
            CALL DATGETAR (STRM2(INN,'VIS'), NAX, NAXIS, ATYPE, OVADD)
            CALL DATMAKAR (STRM2(OUTN,'VIS'), NAX, NAXIS, ATYPE, OVADD)
            CALL DATGETAR (STRM2(INN,'WT'), NAX, NAXIS, ATYPE, OWADD)
            CALL DATMAKAR (STRM2(OUTN,'WT'), NAX, NAXIS, ATYPE, OWADD)
         END IF
C
         CALL CRDGET (INN, NAX, TYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
         CALL CRDPUT (OUTN, NAX, TYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
      ELSE
         OVADD = IVADD
         OWADD = IWADD
      END IF
C
      IF (OUTNAME.NE.INNAME) THEN
         CALL ARRCOPY(STRM2(INNAME,'UU'), STRM2(OUTNAME,'UU'))
         CALL ARRCOPY(STRM2(INNAME,'VV'), STRM2(OUTNAME,'VV'))
         CALL ARRCOPY(STRM2(INNAME,'WW'), STRM2(OUTNAME,'WW'))
         CALL ARRCOPY(STRM2(INNAME,'TIME'), STRM2(OUTNAME,'TIME'))
      END IF
C
      IF (ERROR) GO TO 990
C
C Dig out averaging modes
C
      IF (DATEXIST(STRM2(INN,'GAITYPE'))) THEN
         CALL DATGETC(INN, 'GAITYPE', GAITYPE, 1, NDUMMY)
         IF ((GAITYPE.NE.'BOXCAR').AND.(GAITYPE.NE.'2PT')) THEN
            MESSAGE = 'Warning: Unknown GAITYPE forced to ''BOXCAR'''
            CALL MSGPUT (MESSAGE, 'W')
            GAITYPE = 'BOXCAR'
         END IF
      ELSE
         GAITYPE = 'BOXCAR'
      END IF
C
      IF (DATEXIST(STRM2(INN,'GAIAVG'))) THEN
         CALL DATGETC(INN, 'GAIAVG', GAIAVG, 1, NDUMMY)
         IF ((GAIAVG.NE.'AMPPHI').AND.(GAIAVG.NE.' ')) THEN
            MESSAGE = 'Warning: Unknown GAIAVG forced to '' '''
            CALL MSGPUT (MESSAGE, 'W')
            GAIAVG = ' '
         END IF
      ELSE
         GAIAVG = ' '
      END IF
C
      IF (DATEXIST(STRM2(INN,'GAITINT'))) THEN
         CALL DATGETR(INN, 'GAITINT', GAITINT, 1, NDUMMY)
      ELSE
         GAITINT = 0.0
      END IF
C
C Preaverage the gains, if needed
C
      IF (GAITINT.GT.0.0) THEN
         TSTRING = STRTIMC (GAITINT)
         MESSAGE = 'Pre-averaging gains with interval '//TSTRING//'.'
         CALL MSGPUT (MESSAGE, 'I')
         IF (GAIAVG.EQ.' ') THEN
            MESSAGE = 'Pre-averaging complex gains'
         ELSE
            MESSAGE = 'Pre-averaging in amplitude and phase'
         END IF
         CALL MSGPUT (MESSAGE, 'I')
C
         CALL VISMXANT (INNAME, MINANT, MAXANT)
         CALL DATPUTI (STRM2(CALN,'ANTGAIN'), 'MIN1', MINANT, 1)
         CALL DATPUTI (STRM2(CALN,'ANTGAIN'), 'MAX1', MAXANT, 1)
         CALL ARRBXCAR (STRM2(CALN,'GAINTIME'), STRM2(CALN,'ANTGAIN'),
     $      ' ', 'Av GainTime', 'Av AntGain', ' ', GAITINT, GAIAVG)
         CALL DATGETAR ('Av GainTime', NAX, NAXIS, ATYPE, GTADD)
         NGAIN = NAXIS(1)
         GADD = DATADD ('Av AntGain')
      END IF
C
C Now actually do the calibration.
C
      CALL GAIAPPLP (MEMX(IVADD), MEMX(OVADD), MEMR(IWADD),
     $   MEMR(OWADD), MEMR(VTADD), MEMR(BADD), MEMX(GADD),
     $   MEMR(GTADD), NVIS, NANT, NGAIN, GAITYPE)
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
