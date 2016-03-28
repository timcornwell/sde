C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)filsmopu.f	1.1    6/9/93
C
C Audit trail:
      SUBROUTINE FILSMOPU (MODEL, TCURFILE, TIMGTMPL)
C
CD Get an incomplete model from SAO image cursor file
C
C	MODEL		CH*(*)	input	Name of model
C	TCURFILE	CH*(*)	input	Filename of cursor file
C	TIMGTMPL	CH*(*)	input	Name/Filename of image template
C
C	MODEL/FLUX	REAL	Set to -1  (Flux not determined)
C	MODEL/RA	REAL	Position of component in Ra offset (asec)
C	MODEL/DEC	REAL	Position of component in Dec offset (asec)
C	MODEL/BMAJ	REAL	Major axis in asec
C	MODEL/BMIN	REAL	Minor axis in asec
C	MODEL/BPA	REAL	Position angle in degrees
C	MODEL/TYPE	CHAR	Type: 'POINT', 'GAUSS', 'RECT', 'DISK'
C
C	If CURFILE is blank, the logical name SDECURSOR will be evaluated
C	for a name.  If this is not found, 'saoimage.reg' is used.  Likewise
C	the database is searched for an existing image of name IMGTMPL.
C	If this is not found, IMGTMPL is interpreted as a filename.
C
C Audit trail:
C	Original version:
C				D.S.Briggs	April 15 1992
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FILSMOPU')
C
      CHARACTER*(*)	TCURFILE, TIMGTMPL, MODEL
C
      CHARACTER*(SYSMXNAM)	CURFILE, IMGTMPL
      CHARACTER		CURTYPE*4, ATYPE*1
      INTEGER		I, NCOMP, FLADD, RAADD, DECADD, BMAJADD,
     $   		BMINADD, BPAADD, TADD
      REAL		RA, DEC, BMAJ, BMIN, BPA
      REAL		PX, PY, PROT
      REAL		PI, D2R, RX, RY
C
      INTEGER		NAX, NAXIS(SYSMXDIM)
      DOUBLE PRECISION	RVAL(SYSMXDIM)
      REAL		RPIX(SYSMXDIM), DELT(SYSMXDIM), ROTA(SYSMXDIM)
      CHARACTER*8	TYPE(SYSMXDIM)
C
      CHARACTER*(SYSMXNAM)	STRM2
      INTEGER		STRLEN, CRDRNAX, DATADD
      LOGICAL		DATEXIST
C==================================================================
      IF (ERROR) GO TO 999
C
      PI = 4.0 * ATAN(1.0)
      D2R = PI/180.0
      CURFILE = TCURFILE
      IMGTMPL = TIMGTMPL
C
C Find a name for the SAOImage cursor file
C
      IF (CURFILE.EQ.' ') THEN
         CALL SYSGTENV ('SDECURSOR', CURFILE)
         IF (CURFILE.EQ.' ') CURFILE = 'saoimage.reg'
      END IF
C
C Dig out a coordinate system
C
      IF (DATEXIST(IMGTMPL)) THEN
         CALL CRDGET (IMGTMPL, NAX, TYPE, NAXIS, RVAL, RPIX, DELT,
     $      ROTA)
         IMGTMPL = '''' // TIMGTMPL // ''''
      ELSE
         CALL FILIMGGE ('TempImage', IMGTMPL, ' ')
         IF (ERROR) GO TO 990
         CALL CRDGET ('TempImage', NAX, TYPE, NAXIS, RVAL, RPIX, DELT,
     $      ROTA)
         CALL DATDELET ('TempImage')
      END IF
      IF (ERROR) GO TO 990
      IF (CRDRNAX(NAX,NAXIS).NE.2) THEN
         CALL ERRREPOR (ERRFATAL, ROUTINE,
     $      'Only 2-D cursor models supported')
         GO TO 999
      END IF
C
C Get the address of the model arrays
C
      CALL DATGETAR (STRM2(MODEL,'FLUX'), NAX, NAXIS, ATYPE, FLADD)
      NCOMP = NAXIS(1)
      RAADD = DATADD (STRM2(MODEL,'RA'))
      DECADD = DATADD (STRM2(MODEL, 'DEC'))
      BMAJADD = DATADD (STRM2(MODEL, 'BMAJ'))
      BMINADD = DATADD (STRM2(MODEL, 'BMIN'))
      BPAADD = DATADD (STRM2(MODEL, 'BPA'))
      TADD = DATADD (STRM2(MODEL, 'TYPE'))
      IF (ERROR) GO TO 990
C
C Initial file manipulation
C
      CALL FILDEL (CURFILE)
      CALL TXTOPEN ('Cursor', CURFILE, 'WRITE')
      MESSAGE = 'Writing model to cursor file ''' //
     $   CURFILE(1:STRLEN(CURFILE)) // ''''
      CALL MSGPUT (MESSAGE,'I')
      WRITE (MESSAGE, 1000) NCOMP
 1000 FORMAT ('Found ',I6,' components')
      CALL MSGPUT (MESSAGE, 'I')
      WRITE (MESSAGE, 1010) IMGTMPL(1:STRLEN(IMGTMPL)),
     $   MODEL(1:STRLEN(MODEL))
 1010 FORMAT ('# ',A,' (SDE image)  ''',A,''' (SDE model)')
      CALL TXTWRITE ('Cursor', MESSAGE)
      MESSAGE = '# '
      CALL SYSDATET (MESSAGE(3:))
      I = STRLEN(MESSAGE)
      IF (ICHAR(MESSAGE(I:I)).LT.ICHAR(' ')) MESSAGE(I:I) = ' '
      CALL TXTWRITE ('Cursor', MESSAGE)
      MESSAGE = '# shape x, y, [x dimension, y dimension], [angle]'
      CALL TXTWRITE ('Cursor', MESSAGE)
C
C Main loop over cursors
C
      DO 500 I = 1, NCOMP
C
 1100    FORMAT (A,'(',F8.3,',',F8.3,')')
 1110    FORMAT (A,'(',F8.3,',',F8.3,',',F8.3,')')
 1120    FORMAT (A,'(',F8.3,',',F8.3,',',F8.3,',',F8.3,',',F8.3,')')
C
C Now actually interpret the cursor
C
         CURTYPE = MEMC(TADD+I-1)(1:4)
         RA = MEMR(RAADD+I-1)
         DEC = MEMR(DECADD+I-1)
         BMAJ = MEMR(BMAJADD+I-1)
         BMIN = MEMR(BMINADD+I-1)
         BPA = MEMR(BPAADD+I-1)
         IF ((CURTYPE.EQ.'GAUS').OR.(CURTYPE.EQ.'RECT').OR.
     $       (CURTYPE.EQ.'DISK')) THEN
            PX = -SIN(BPA*D2R) * BMAJ / (3600.0 * DELT(1))
            PY = COS(BPA*D2R) * BMAJ / (3600.0 * DELT(2))
            RY = SQRT(PX**2 + PY **2)
            PROT = ATAN2(PX, PY) / D2R
            PX = -SIN((BPA-90.0)*D2R) * BMIN / (3600.0 * DELT(1))
            PY = COS((BPA-90.0)*D2R) * BMIN / (3600.0 * DELT(2))
            RX = SQRT(PX**2 + PY**2)
         END IF
         PX = RA / (3600.0 * DELT(1)) + RPIX(1)
         PY = DEC / (3600.0 * DELT(2)) + RPIX(2)
C
         IF (CURTYPE.EQ.'POIN') THEN
            WRITE (MESSAGE,1100) ' POINT', PX, PY
         ELSE IF (CURTYPE.EQ.'GAUS') THEN
            IF (ABS(RX-RY).LT.1.E-4) THEN
               WRITE (MESSAGE, 1110) ' CIRCLE', PX, PY, RX/2.0
            ELSE
               WRITE (MESSAGE, 1120) ' ELLIPSE', PX, PY, RX/2.0,
     $            RY/2.0, PROT
            END IF
         ELSE IF (CURTYPE.EQ.'RECT') THEN
            WRITE (MESSAGE, 1120) ' BOX', PX, PY, RX, RY, PROT
         ELSE IF (CURTYPE.EQ.'DISK') THEN
            IF (ABS(RX-RY).LT.1.E-4) THEN
               WRITE (MESSAGE, 1110) '-CIRCLE', PX, PY, RX/2.0
            ELSE
               WRITE (MESSAGE, 1120) '-ELLIPSE', PX, PY, RX/2.0,
     $            RY/2.0, PROT
            END IF
         ELSE
            CALL ERRREPOR(ERRBDARG, ROUTINE, 'Unrecognized model type')
            GO TO 999
         END IF
         CALL TXTWRITE ('Cursor', MESSAGE)
C
 500  CONTINUE
      CALL TXTCLOSE ('Cursor')
C
 990  CONTINUE
      IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
 999  CONTINUE
      END
