C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)filsmoge.f	1.1    6/9/93
C
C Audit trail:
      SUBROUTINE FILSMOGE (MODEL, TCURFILE, TIMGTMPL)
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
C	If this is not found, IMGTMPL is interpreted as a filename.  If
C	it is blank, the name in the cursor file will be used.
C
C Audit trail:
C	Original version:
C				D.S.Briggs	April 15 1992
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FILSMOGE')
C
      CHARACTER*(*)	TCURFILE, TIMGTMPL, MODEL
C
      CHARACTER*(SYSMXNAM)	CURFILE, IMGTMPL
      CHARACTER		LINE*132, MODTYPE*10
      INTEGER		CURST, CUREND, NUMST, NUMEND
      INTEGER		I, J, NCHAR, NCOMP, FLADD, RAADD,
     $   		DECADD, BMAJADD, BMINADD, BPAADD, TADD
      LOGICAL		EOF, DOEXCLD, LINEERR
      REAL		PX, PY, PXR, PYR, PROT
      REAL		PI, D2R, DX, DY, RX, RY, RROT, T
C
      INTEGER		NAX, NAXIS(SYSMXDIM)
      DOUBLE PRECISION	RVAL(SYSMXDIM)
      REAL		RPIX(SYSMXDIM), 
     1   		DELT(SYSMXDIM), ROTA(SYSMXDIM)
      CHARACTER*8	TYPE(SYSMXDIM)
C
      CHARACTER*(SYSMXNAM)	STRM2
      INTEGER		STRLEN, CRDRNAX
      LOGICAL		DATEXIST
C
C A few defines for later readability
C
#define	CURTYPE		LINE(CURST:CUREND)
#define NUMDATA		LINE(NUMST:NUMEND)
C==================================================================
      IF (ERROR) GO TO 999
C
      PI = 4.0 * ATAN(1.0)
      D2R = PI/180.0
      CURFILE = TCURFILE
      IMGTMPL = TIMGTMPL
C
C Find the SAOImage cursor file
C
      IF (CURFILE.EQ.' ') THEN
         CALL SYSGTENV ('SDECURSOR', CURFILE)
         IF (CURFILE.EQ.' ') CURFILE = 'saoimage.reg'
      END IF
C
      CALL TXTOPEN ('Cursor', CURFILE, 'READ')
      IF (ERROR) GO TO 990
C
C Dig out a coordinate system for use to use
C
      CALL TXTREAD ('Cursor', LINE, NCHAR, EOF)
      IF (DATEXIST(IMGTMPL)) THEN
         CALL CRDGET (IMGTMPL, NAX, TYPE, NAXIS, RVAL, RPIX, DELT,
     $      ROTA)
      ELSE
         IF (IMGTMPL.EQ.' ') IMGTMPL = LINE(3:INDEX(LINE(3:),' ')+1)
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
C Figure out how many components we will need
C
      NCOMP = 0
 100  CONTINUE
C
C Get next non-comment line
C
      CALL TXTREAD ('Cursor', LINE, NCHAR, EOF)
      IF (EOF) GO TO 300
      IF (LINE(1:1).EQ.'#') GO TO 100
C
      I = INDEX(LINE,'(')
      IF (I.GT.1) THEN
         CURST = 2
         CUREND = I-1
         IF ((CURTYPE.EQ.'POINT').OR.
     $       (CURTYPE.EQ.'CIRCLE').OR.
     $       (CURTYPE.EQ.'ELLIPSE').OR.
     $       (CURTYPE.EQ.'BOX')) THEN
            NCOMP = NCOMP + 1
         END IF
      END IF
      GO TO 100
C
 300  CONTINUE
      CALL TXTCLOSE ('Cursor')
      WRITE (MESSAGE, 1300) NCOMP
 1300 FORMAT ('Found ',I6,' components')
      CALL MSGPUT (MESSAGE, 'I')
C
C Make directory
C
      CALL DATCREAT (MODEL)
      CALL DATMAKAR (STRM2(MODEL, 'FLUX'), 1, NCOMP, 'R', FLADD)
      CALL DATMAKAR (STRM2(MODEL, 'RA'), 1, NCOMP, 'R', RAADD)
      CALL DATMAKAR (STRM2(MODEL, 'DEC'), 1, NCOMP, 'R', DECADD)
      CALL DATMAKAR (STRM2(MODEL, 'BMAJ'), 1, NCOMP, 'R', BMAJADD)
      CALL DATMAKAR (STRM2(MODEL, 'BMIN'), 1, NCOMP, 'R', BMINADD)
      CALL DATMAKAR (STRM2(MODEL, 'BPA'), 1, NCOMP, 'R', BPAADD)
      CALL DATMAKAR (STRM2(MODEL, 'TYPE'), 1, NCOMP, 'S', TADD)
C
C Fill the arrays with default values
C
      DO 350 I = 1, NCOMP
         MEMR(FLADD+I-1) = 1.0
         MEMR(RAADD+I-1) = 0.0
         MEMR(DECADD+I-1) = 0.0
         MEMR(BMAJADD+I-1) = 0.0
         MEMR(BMINADD+I-1) = 0.0
         MEMR(BPAADD+I-1) = 0.0
         MEMC(TADD+I-1) = 'UNDEF'
 350  CONTINUE
C
C Main loop over cursors in file.  Now actually *do* something.
C
      CALL TXTOPEN ('Cursor', CURFILE, 'READ')
      CALL MSGPUT ('Constructing model from cursor file','I')
      NCOMP = 0
 400  CONTINUE
C
      LINEERR = .TRUE.
      CALL TXTREAD ('Cursor', LINE, NCHAR, EOF)
      IF (EOF) GO TO 600
      IF (LINE(1:1).EQ.'#') GO TO 400
C
      I = INDEX(LINE,'(')
      J = INDEX(LINE,')')
      IF ((I.LE.2).OR.(J-1.LE.I+1)) GO TO 500
      DOEXCLD = LINE(1:1) .EQ. '-'
      CURST = 2
      CUREND = I-1
      NUMST = I+1
      NUMEND = J-1
C
      PX = 0.0
      PY = 0.0
      PXR = 0.0
      PYR = 0.0
      PROT = 0.0
      READ (NUMDATA, *, ERR=500) PX, PY
      READ (NUMDATA, *, END=410) PX, PY, PXR, PYR, PROT
 410  CONTINUE
      LINEERR = .FALSE.
C
C Now actually interpret the cursor
C
      IF ((CURTYPE.EQ.'POINT').OR.
     $    (CURTYPE.EQ.'CIRCLE').OR.
     $    (CURTYPE.EQ.'ELLIPSE').OR.
     $    (CURTYPE.EQ.'BOX')) THEN
         NCOMP = NCOMP + 1
         IF (CURTYPE.EQ.'CIRCLE') THEN
            PYR = PXR
            CUREND = CUREND + 1
            CURTYPE = 'ELLIPSE'
         END IF
         DX = -SIN(PROT*D2R) * PYR * DELT(1) * 3600.0
         DY = COS(PROT*D2R) * PYR * DELT(2) * 3600.0
         RY = SQRT(DX**2 + DY**2)
         RROT = ATAN2(DX, DY) / D2R
         DX = -SIN((PROT-90.0)*D2R) * PXR * DELT(1) * 3600.0
         DY = COS((PROT-90.0)*D2R) * PXR * DELT(2) * 3600.0
         RX = SQRT(DX**2 + DY**2)
         IF (RX.GT.RY) THEN
            T = RX
            RX = RY
            RY = T
            RROT = RROT + 90.0
         END IF
         IF (CURTYPE.EQ.'POINT') THEN
            MODTYPE = 'POINT'
         ELSE IF (CURTYPE.EQ.'ELLIPSE') THEN
            IF (DOEXCLD) THEN
               MODTYPE = 'DISK'
            ELSE
               MODTYPE = 'GAUSS'
            END IF
            MEMR(BMAJADD+NCOMP-1) = 2.0 * RY
            MEMR(BMINADD+NCOMP-1) = 2.0 * RX
         ELSE IF (CURTYPE.EQ.'BOX') THEN
            MODTYPE = 'RECT'
            MEMR(BMAJADD+NCOMP-1) = RY
            MEMR(BMINADD+NCOMP-1) = RX
         END IF
         MEMR(BPAADD+NCOMP-1) = RROT
         MEMR(RAADD+NCOMP-1) = (PX - RPIX(1)) * DELT(1) * 3600.0
         MEMR(DECADD+NCOMP-1) = (PY - RPIX(2)) * DELT(2) * 3600.0
         MEMC(TADD+NCOMP-1) = MODTYPE
      ELSE IF ((CURTYPE.EQ.'ANNULUS').OR.
     $         (CURTYPE.EQ.'POLYGON')) THEN
            MESSAGE = 'Warning: Skipping // CURTYPE // '!'
            CALL MSGPUT (MESSAGE, 'W')
      ELSE
         LINEERR = .TRUE.
      END IF
C
C Final bookkeeping and error handling
C
 500  CONTINUE
      IF (LINEERR) THEN
         WRITE (MESSAGE,1400) LINE(1:STRLEN(LINE))
 1400    FORMAT ('Cannot parse line ''',A,'''')
         CALL MSGPUT (MESSAGE, 'W')
      END IF
C
C Loopback and exit
C
      GO TO 400
 600  CONTINUE
      CALL TXTCLOSE ('Cursor')
C
C All done
C
 990  CONTINUE
      IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
 999  CONTINUE
      END
