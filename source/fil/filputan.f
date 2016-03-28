C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)filputan.f	1.5	 5/24/94
C
      SUBROUTINE FILPUTAN (ANT, ANTFILE, COORD)
C
CD Write an array specification ASCII file. This can convert to the
C local tangent plane if required. The coordinate system is
C chosen so that x points to the local meridian i.e. for ha = 0, the
C u component of the baseline is given by the difference in y components
C of the two antennas (see fred.f in the sim directory). The local tangent
C plane is thus obtained by rotating about the y axis by 90-sitelat
C degrees.
C
C      ANT/NANT        INT     Number of antennas
C      ANT/SITELAT     DBLE    Site latitude
C      ANT/LX          DBLE    Earth-based cartesian x coord. of ants.
C      ANT/LY          DBLE    Earth-based cartesian y coord. of ants.
C      ANT/LZ          DBLE    Earth-based cartesian z coord. of ants.
C      ANT/DIAM        DBLE    Diameter of ants.
C
C
C	ANT	CH*(*)	input	Name of directory entry
C	ANTFILE	CH*(*)	input	Name of input file
C	COORD	CH*(*)	input	COORD 'LOCAL' for local tangent plane
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Made some changes, can't remember exactly what.
C	Includes "centeronwrite option", and also something with local
C	coordinates.  ANTDEM now works with this.
C				M.A. Holdaway	May 24, 1994
C
C---------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ANT, ANTFILE, COORD
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FILPUTAN')
C
      LOGICAL		WCENTER
      DOUBLE PRECISION	AVE(3)
      INTEGER		NANT, ICOMP, NDUMMY, NAX,
     1			I, LXADD, LYADD, LZADD, DMADD
      DOUBLE PRECISION  SITELAT, LOCAL(3), EARTH(3), AC2M, AD2M, LONG
      CHARACTER*1	ATYPE, COORDTYP
      CHARACTER*132	LINE
C
      LOGICAL			DATEXIST
      CHARACTER*(SYSMXNAM)	STRM2
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
C Find coordinate system to use
C
      IF(COORD(1:1).EQ.'L') THEN
         COORDTYP = '@'
      ELSE
         COORDTYP = ' '
      END IF
C
      CALL DATGETI (ANT, 'NANT', NANT, 1, NDUMMY)
      CALL DATGETD (ANT, 'SITELAT', SITELAT, 1, NDUMMY)
      CALL DATGETAR (STRM2(ANT, 'LX'), NAX, NANT, ATYPE, LXADD)
      CALL DATGETAR (STRM2(ANT, 'LY'), NAX, NANT, ATYPE, LYADD)
      CALL DATGETAR (STRM2(ANT, 'LZ'), NAX, NANT, ATYPE, LZADD)
      CALL DATGETAR (STRM2(ANT, 'DIAM'), NAX, NANT, ATYPE, DMADD)
C
C
C Write number of antennas, site latitude and conversion factors
C
      CALL TXTOPEN (ROUTINE, ANTFILE, 'WRITE')
      WRITE (LINE, *) NANT
      CALL TXTWRITE (ROUTINE, LINE)
      WRITE (LINE, *) SNGL(45.0 * SITELAT / ATAN(1.0))
      CALL TXTWRITE (ROUTINE, LINE)
      AC2M = 1.0
      AD2M = 1.0
      WRITE (LINE, *) AC2M, AD2M
      CALL TXTWRITE (ROUTINE, LINE)
C
C Write antennas info
C
      IF (ERROR) GO TO 990
C
C Write in local tangent plane?. The local tangent plane is obtained
C by rotating about the Y axis since X points to the meridian. i.e.
C for ha = 0, the u component of the baseline is given by the difference
C in y coordinates of two antennas
C
      AVE(1) = 0.D0
      AVE(2) = 0.D0
      AVE(3) = 0.D0
      IF (DATEXIST(STRM2(ANT, 'CenterOnWrite'))) THEN
         CALL DATGETL (ANT, 'CenterOnWrite', WCENTER, 1, NDUMMY)
         IF (WCENTER) THEN
            DO 90 ICOMP = 1, NANT
               I = ICOMP - 1
               EARTH(1)=MEMD(LXADD+I)
               EARTH(2)=MEMD(LYADD+I)
               EARTH(3)=MEMD(LZADD+I)
               IF(COORDTYP.EQ.'@') THEN
                  CALL UTLG2L(EARTH, LONG, SITELAT, LOCAL)
                  AVE(1) = AVE(1) + LOCAL(1)
                  AVE(2) = AVE(2) + LOCAL(2)
                  AVE(3) = AVE(3) + LOCAL(3)
               ELSE
                  AVE(1) = AVE(1) + EARTH(1)
                  AVE(2) = AVE(2) + EARTH(2)
                  AVE(3) = AVE(3) + EARTH(3)
               END IF
 90         CONTINUE
            AVE(1) = AVE(1) / FLOAT(NANT)
            AVE(2) = AVE(2) / FLOAT(NANT)
            AVE(3) = AVE(3) / FLOAT(NANT)
         ENDIF
      ENDIF
C
      DO 100 ICOMP = 1, NANT
         I = ICOMP - 1
         EARTH(1)=MEMD(LXADD+I)
         EARTH(2)=MEMD(LYADD+I)
         EARTH(3)=MEMD(LZADD+I)
         IF(COORDTYP.EQ.'@') THEN
            CALL UTLG2L(EARTH, LONG, SITELAT, LOCAL)
            LOCAL(1) = LOCAL(1) - AVE(1)
            LOCAL(2) = LOCAL(2) - AVE(2)
            LOCAL(3) = LOCAL(3) - AVE(3)
            WRITE (LINE, 800) COORDTYP, LOCAL(1), LOCAL(2),
     $         LOCAL(3), MEMD(DMADD+I)
         ELSE
            EARTH(1) = EARTH(1) - AVE(1)
            EARTH(2) = EARTH(2) - AVE(2)
            EARTH(3) = EARTH(3) - AVE(3)
            WRITE (LINE, 800) COORDTYP, EARTH(1), EARTH(2),
     $         EARTH(3), MEMD(DMADD+I)
         END IF
         CALL TXTWRITE (ROUTINE, LINE)
  100 CONTINUE
 800  FORMAT( A1, 4F16.2 )
      CALL TXTCLOSE(ROUTINE)
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
