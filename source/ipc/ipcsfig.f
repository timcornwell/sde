C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)ipcsfig.f	1.4    1/28/93
C
      SUBROUTINE IPCSFIG (NAME)
CD     
C    Re-format am ipcs file so that it will be written as a 2 by Nphotons
C    array with imdedded frame markers instead of a groups
C    format file. This is necessary if the file is to read into
C    the FIGARO package and operated on by Tim Bedding's and Gordon
C    Robinsons software.
C    
C    name	CH*(*)	input	Name of directory entry
C    Audit trail:
C    Cloned from VISDTS
C                                          R.G. Marson     Apr 28 1989
C    
C-----------------------------------------------------------------------
#include	"stdinc.h"
#include        "ftsinc.h"
C     
      CHARACTER*(*)	NAME
C     
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IPCSFIG')
C     
      CHARACTER*(SYSMXNAM) STRM2
      CHARACTER         STRINT
      INTEGER           DATFGETI
      LOGICAL           DATEXIST
C     
      CHARACTER*(SYSMXNAM) XNAME, YNAME, FNAME, HCARD
      CHARACTER*8       TYPE(SYSMXDIM)
      CHARACTER         XTYPE, YTYPE, FTYPE, DTYPE
      DOUBLE PRECISION  RVAL(SYSMXDIM)
      REAL              NEWFRAME, CURFRAME, FRAMELSB, FRAMEMSB, TEMP
      REAL              RPIX(SYSMXDIM), DELT(SYSMXDIM), ROTA(SYSMXDIM)
      INTEGER           XNAX, YNAX, FNAX, DNAX, NAX
      INTEGER           XNAXIS(SYSMXDIM), YNAXIS(SYSMXDIM)
      INTEGER           FNAXIS(SYSMXDIM), DNAXIS(SYSMXDIM)
      INTEGER           NAXIS(SYSMXDIM)
      INTEGER		FRSTART, FREND
      INTEGER           XADD, YADD, FADD, DADD
      INTEGER           COUNTER, I
C=======================================================================
      IF (ERROR) GO TO 999
C     
      CALL DATCHKTP(NAME, 'IPCS')
C
      FRSTART = DATFGETI(NAME,'FR_START')
      FREND = DATFGETI(NAME,'FR_END')
      IF (ERROR) THEN
         CALL ERRREPOR (ERRNTFND, ROUTINE, 'Frame data not found')
         GO TO 990
      END IF
      IF (FRSTART.GT.FREND) THEN
         CALL ERRREPOR(ERRLOGIC, ROUTINE, 'Incorrect framing data')
         GOTO 999
      END IF
C
C     Check that the directory enteries we expect exist
C
      XNAME = STRM2(NAME, 'XPIXEL')
      CALL DATGETAR(XNAME, XNAX, XNAXIS, XTYPE, XADD)
      YNAME = STRM2(NAME, 'YPIXEL')
      CALL DATGETAR(YNAME, YNAX, YNAXIS, YTYPE, YADD)
      FNAME = STRM2(NAME, 'FRAME')
      CALL DATGETAR(FNAME, FNAX, FNAXIS, FTYPE, FADD)
      CALL DATGETAR(NAME, DNAX, DNAXIS, DTYPE, DADD)
      IF (ERROR) GOTO 990
      IF ((XNAX.NE.1).OR.(YNAX.NE.1).OR.
     $     (FNAX.NE.1).OR.(DNAX.NE.2)) THEN
         CALL ERRREPOR(ERRWRGTP, ROUTINE, 'Incorrect dimensions')
         GO TO 999
      END IF
      IF ((XNAXIS(1).NE.YNAXIS(1)).OR.(XNAXIS(1).NE.FNAXIS(1)).OR.
     $     (XNAXIS(1).NE.DNAXIS(2)).OR.(DNAXIS(1).NE.1)) THEN
         CALL ERRREPOR(ERRLOGIC, ROUTINE, 'Incompatible array sizes')
         GO TO 999
      END IF
      IF ((XTYPE.NE.'R').OR.(YTYPE.NE.'R').OR.(FTYPE.NE.'R').OR.
     $     (DTYPE.NE.'R')) THEN
         CALL ERRREPOR(ERRWRGTP, ROUTINE, 'Array must be real')
         GO TO 999
      END IF
C
C     Now delete the data array!
C     The data array contains the results of photons being summed
C     The figaro FITS format doesn't accomadate this so all photons
C     are considered to have unity value.
C     I also have to make a new array to replace the old one.
C
      CALL DATDELAR(NAME)
C
C     Now create the new array
C
      NAX = 2
      NAXIS(1) = 2
      NAXIS(2) = FREND - FRSTART + 1 + XNAXIS(1)
      CALL DATMAKAR(NAME, NAX, NAXIS, 'R', DADD)
C
C Now fill up the array with real numbers
C  in the fillowing format: frameMSB, frameLSB, X1, Y1, ...frame2LSB...
C
      COUNTER = 0
      CURFRAME = -9999
      DO I = 0, XNAXIS(1) - 1
         NEWFRAME = MEMR(FADD + I)
         IF (NEWFRAME.NE.CURFRAME) THEN
            FRAMEMSB = MOD(INT(NEWFRAME), 65535)
            FRAMELSB = -32768 + NINT((NEWFRAME - FRAMEMSB)/65535)
            MEMR(DADD + COUNTER) = FRAMEMSB
            MEMR(DADD + COUNTER + 1) = FRAMELSB
            COUNTER = COUNTER + 2
            CURFRAME = NEWFRAME
         END IF
         TEMP = MEMR(XADD + I)
         MEMR(DADD + COUNTER) = TEMP
         TEMP = MEMR(YADD + I)
         MEMR(DADD + COUNTER + 1) = TEMP
         COUNTER = COUNTER + 2
      END DO
C
C Now kludge the array size to be slightly smaller if 
C  there where missing frames
C
      NAXIS(2) = COUNTER/2
      CALL DATPUTI(NAME,'NAXIS', NAXIS, 2)
C
C Now fix up the header
C
      HCARD = STRM2(NAME, 'GROUPS')
      IF (DATEXIST(HCARD)) CALL DATDELET(HCARD)
      HCARD = STRM2(NAME, 'GCOUNT')
      IF (DATEXIST(HCARD)) CALL DATDELET(HCARD)
      HCARD = STRM2(NAME, 'PCOUNT')
      IF (DATEXIST(HCARD)) CALL DATDELET(HCARD)
      DO I = 1,3
         HCARD = STRM2(NAME, 'PTYPE'//STRINT(I))
         IF (DATEXIST(HCARD)) CALL DATDELET(HCARD)
         HCARD = STRM2(NAME, 'PSCAL'//STRINT(I))
         IF (DATEXIST(HCARD)) CALL DATDELET(HCARD)
         HCARD = STRM2(NAME, 'PZERO'//STRINT(I))
         IF (DATEXIST(HCARD)) CALL DATDELET(HCARD)
      END DO
      TYPE(1) = 'XY'
      TYPE(2) = 'PIXEL'
      RVAL(1) = 1.0
      RVAL(2) = 1.0
      RPIX(1) = 1.0
      RPIX(2) = 1.0
      DELT(1) = 1.0
      DELT(2) = 1.0
      ROTA(1) = 0.0
      ROTA(2) = 0.0
      CALL CRDPUT(NAME, 2, TYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C     
 999  CONTINUE
      END
