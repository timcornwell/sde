C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)filgetan.f	1.11	 1
C
      SUBROUTINE FILGETAN (ANT, ANTFILE)
C
CD Read an array specification ASCII file.  Several coord systems recognized
C
C The coordinate system is
C chosen so that x points to the local meridian i.e. for ha = 0, the
C u component of the baseline is given by the difference in y components
C of the two antennas (see fred.f in the sim directory).
C
C      ANT/NANT        INT     Number of antennas
C      ANT/SITELAT     DBLE    Site latitude
C      ANT/SITELONG    DBLE    Site longitude
C      ANT/LAT         DBLE    Antenna latitude
C      ANT/LONG        DBLE    Antenna longitude, relative to SITELONG
C      ANT/LX          DBLE    Earth-based cartesian x coord. of ants.
C      ANT/LY          DBLE    Earth-based cartesian y coord. of ants.
C      ANT/LZ          DBLE    Earth-based cartesian z coord. of ants.
C      ANT/RX          REAL    Local cartesian x coord. of ants.
C      ANT/RY          REAL    Local cartesian y coord. of ants.
C      ANT/RZ          REAL    Local cartesian z coord. of ants.
C      ANT/DIAM        DBLE    Diameter of ants.
C      ANT/UANT        DBLE    Re-usable array to hold u coord.
C      ANT/VANT        DBLE    Re-usable array to hold v coord.
C      ANT/WANT        DBLE    Re-usable array to hold w coord.
C      ANT/SHANT       LOG     Re-usable array to hold shadowing info.
C
C	ANT	CH*(*)	input	Name of directory entry
C	ANTFILE	CH*(*)	input	Name of input file
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Added capability to take number based on coordinate system
C	which is tangent to the surface of the earth: simply put @
C	at the beginning of the line
C                              T.J. Cornwell   April 3 1989
C	Fixed to rotate about X-axis
C                              T.J. Cornwell   April 9 1989
C	Stop read if an error
C                              T.J. Cornwell   Feb 6 1990
C	Lost scaling
C                              T.J. Cornwell   Feb 13 1990
C	Added Local coordinates in the ANT directory
C				M.A.Holdaway	May 9 1991
C	Changed Local coordinates to REAL
C				M.A.Holdaway	May 10 1991
C	Added ability to read Mk II VLBI coordinates: put * as
C	first character on line.  Reads optional site longitude
C	after latitude in input file.  Added antenna based latitude
C	and longitude.  Bugfix in local coord storage.
C				D.S. Briggs	July 14 1992
C	Fixed internal read of SITELAT, SITELONG to use END=
C	rather than ERR=
C				T.J. Cornwell 	August 18 1992
C	Added comment lines.  Start a line with '#' and it will be
C	ignored in input
C				D.S.Briggs	Oct 31 1993
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ANT, ANTFILE
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FILGETAN')
C
      INTEGER		NANT, ICOMP, STRLEN,
     $   		I, NCHAR, LXADD, LYADD, LZADD, DMADD,
     $   		UIADD, VIADD, WIADD, SHADD,
     $   		RXADD, RYADD, RZADD, LATADD, LONGADD
      REAL              AC2M, AD2M
      DOUBLE PRECISION	LOCAL(3), EARTH(3), VLB(3),
     $   		SITELAT, SITELONG, LAT, LONG, RXY, PI
      LOGICAL		EOF
      CHARACTER*(SYSMXNAM)	STRM2
      CHARACTER*132	LINE
C=====================================================================
      IF (ERROR) GO TO 999
C
      PI = 4.D0 * ATAN(1.D0)
C
C Read number of antennas, site latitude and conversion factors
C
      CALL TXTOPEN (ROUTINE, ANTFILE, 'READ')
C
 1    CALL TXTREAD (ROUTINE, LINE, NCHAR, EOF)
      IF (ERROR) GOTO 990
      IF (EOF) GO TO 990
      IF (LINE(1:1).EQ.'#') GO TO 1
      READ (LINE(:STRLEN(LINE)), *, ERR = 200) NANT
C 
 2    CALL TXTREAD (ROUTINE, LINE, NCHAR, EOF)
      IF (ERROR) GOTO 990
      IF (EOF) GO TO 990
      IF (LINE(1:1).EQ.'#') GO TO 2
      READ (LINE(:STRLEN(LINE)), *, ERR = 200, END=10) SITELAT, SITELONG
      GO TO 20
 10   CONTINUE
      READ (LINE(:STRLEN(LINE)), *, ERR = 200) SITELAT
      SITELONG = 0.D0
 20   CONTINUE
      SITELAT = SITELAT * ATAN (1D0) / 45D0
      SITELONG = SITELONG * ATAN (1D0) / 45D0
C
 21   CALL TXTREAD (ROUTINE, LINE, NCHAR, EOF)
      IF (ERROR) GO TO 990
      IF (EOF) GO TO 990
      IF (LINE(1:1).EQ.'#') GO TO 21
      READ (LINE(:STRLEN(LINE)), *, ERR = 200) AC2M, AD2M
C
C Now make directory
C
      CALL DATCREAT (ANT)
      CALL DATPUTI (ANT, 'NANT', NANT, 1)
      CALL DATPUTD (ANT, 'SITELAT', SITELAT, 1)
      CALL DATPUTD (ANT, 'SITELONG', SITELONG, 1)
      CALL DATMAKAR (STRM2(ANT, 'LAT'), 1, NANT, 'D', LATADD)
      CALL DATMAKAR (STRM2(ANT, 'LONG'), 1, NANT, 'D', LONGADD)
      CALL DATMAKAR (STRM2(ANT, 'LX'), 1, NANT, 'D', LXADD)
      CALL DATMAKAR (STRM2(ANT, 'LY'), 1, NANT, 'D', LYADD)
      CALL DATMAKAR (STRM2(ANT, 'LZ'), 1, NANT, 'D', LZADD)
      CALL DATMAKAR (STRM2(ANT, 'RX'), 1, NANT, 'R', RXADD)
      CALL DATMAKAR (STRM2(ANT, 'RY'), 1, NANT, 'R', RYADD)
      CALL DATMAKAR (STRM2(ANT, 'RZ'), 1, NANT, 'R', RZADD)
      CALL DATMAKAR (STRM2(ANT, 'DIAM'), 1, NANT, 'D', DMADD)
      CALL DATMAKAR (STRM2(ANT, 'UANT'), 1, NANT, 'D', UIADD)
      CALL DATMAKAR (STRM2(ANT, 'VANT'), 1, NANT, 'D', VIADD)
      CALL DATMAKAR (STRM2(ANT, 'WANT'), 1, NANT, 'D', WIADD)
      CALL DATMAKAR (STRM2(ANT, 'SHANT'), 1, NANT, 'L', SHADD)
C
C Read antennas info
C
      IF (ERROR) GO TO 990
      DO 100 ICOMP = 1, NANT
 50      CALL TXTREAD (ROUTINE, LINE, NCHAR, EOF)
         IF(ERROR) GOTO 990
         IF (EOF) THEN
            CALL ERRREPOR (ERRINPUT, ROUTINE, 'Premature EOF')
            GO TO 999
         END IF
         IF (NCHAR.EQ.0) GO TO 100
         IF (LINE(1:1).EQ.'#') GO TO 50
         I = ICOMP - 1
         IF (LINE(1:1).EQ.'@') THEN
            READ (LINE(2:STRLEN(LINE)), *, ERR=200) LOCAL(1), 
     $         LOCAL(2), LOCAL(3), MEMD(DMADD+I)
            CALL UTLL2G(LOCAL, SITELONG, SITELAT, EARTH)
            CALL UTLG2V(EARTH, SITELONG, SITELAT, VLB)
         ELSE IF (LINE(1:1).EQ.'*') THEN
            READ (LINE(2:STRLEN(LINE)), *, ERR=200) VLB(1), 
     $         VLB(2), VLB(3), MEMD(DMADD+I)
            CALL UTLV2G(VLB, SITELONG, SITELAT, EARTH)
            CALL UTLG2L (EARTH, SITELONG, SITELAT, LOCAL)
         ELSE 
            READ (LINE(:STRLEN(LINE)), *, ERR=200) EARTH(1), 
     $         EARTH(2), EARTH(3), MEMD(DMADD+I)
            CALL UTLG2L (EARTH, SITELONG, SITELAT, LOCAL)
            CALL UTLG2V (EARTH, SITELONG, SITELAT, VLB)
         END IF
         RXY = SQRT(VLB(1)**2+VLB(2)**2)
         LAT = ATAN(VLB(3)/RXY)
         LONG = ATAN2(-VLB(2), VLB(1)) - SITELONG
         IF (LONG.GT.PI) LONG = LONG - 2.D0*PI
         IF (LONG.LT.-PI) LONG = LONG + 2.D0*PI
         MEMD(LATADD+I) = LAT
         MEMD(LONGADD+I) = LONG
         MEMD(LXADD+I) =  EARTH(1) * AC2M
         MEMD(LYADD+I) =  EARTH(2) * AC2M
         MEMD(LZADD+I) =  EARTH(3) * AC2M
         MEMR(RXADD+I) =  LOCAL(1) * AC2M
         MEMR(RYADD+I) =  LOCAL(2) * AC2M
         MEMR(RZADD+I) =  LOCAL(3) * AC2M
         MEMD(DMADD+I) =  MEMD(DMADD+I) * AD2M
  100 CONTINUE
      CALL TXTCLOSE(ROUTINE)
      GO TO 300
  200 CONTINUE
      CALL ERRREPOR (ERRINPUT, ROUTINE, 'Error in line '//LINE)
      GO TO 999
  300 CONTINUE
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
