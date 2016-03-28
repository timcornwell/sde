C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)vismake.f	1.2    12/27/91
C
      SUBROUTINE VISMAKE (VIS, VISCLASS, NVIS)
C
C Make an empty, bare bones u,v database.  This is a much simpler
C version of SIMUV, in that there is no array or model information.
C Designed to provide a return directory for back transforming sky
C models.
C
C      VIS      CH*(*)  output  Name of directory for visibility file
C      VISCLASS CH*(*)  output  Class of visibiliy data.  E.g. 'MOD/I'
C      NVIS     I       input   Number of visibility points to allocate
C
C Audit trail:
C       Cloned from SIMUV
C                               D.S.Briggs      Oct 19 1990
C	Toplevel visibility directory being created incorrectly.
C				D.S.Briggs	Dec 19 1991
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	VIS, VISCLASS
      INTEGER           NVIS
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISMAKE')
C
      INTEGER		NAX, NAXIS(SYSMXDIM)
      CHARACTER*8	CTYPE(SYSMXDIM)
      REAL		CRPIX(SYSMXDIM),
     1                  CDELT(SYSMXDIM),
     2			CROTA(SYSMXDIM)
      DOUBLE PRECISION	CRVAL(SYSMXDIM)
C
      INTEGER           STRLEN
      CHARACTER*(SYSMXNAM)	STRM2, STRM3
C
      INTEGER           I, J, UADD, VADD, WADD, VSADD, WTADD
C
      DATA		NAXIS	/SYSMXDIM*1/
      DATA		CTYPE	/SYSMXDIM*' '/
      DATA		CRPIX	/SYSMXDIM*0.0/
      DATA		CDELT	/SYSMXDIM*0.0/
      DATA		CROTA	/SYSMXDIM*0.0/
      DATA		CRVAL	/SYSMXDIM*0.0D0/
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
C Create uv directory
C
      CALL DATCREAT (VIS)
      J = STRLEN(VISCLASS)
      DO 100 I = 2, J
         IF (VIS(I:I).EQ.'/') THEN
            CALL DATCREAT (STRM2(VIS,VISCLASS(1:I-1)))
         END IF
 100  CONTINUE
      IF (VISCLASS(J:J).NE.'/') THEN
         CALL DATCREAT (STRM2(VIS,VISCLASS(1:J)))
      END IF
      CALL DATSETTP (VIS, 'VIS')
C
C The coordinate system entries are left 0, in that if anything actually
C tries to use them, it's better that they error.  However, many things
C do call CRDGET, and will die if these items are not present.
C
      NAX = 3
      CTYPE(1) = 'RA'
      CTYPE(2) = 'DEC'
      CTYPE(3) = 'FREQ'
      CALL CRDPUT (STRM2(VIS, VISCLASS), NAX, CTYPE, NAXIS, CRVAL, 
     1   CRPIX, CDELT, CROTA)
C
C Now make the various arrays
C
      NAX = 1
      CALL DATMAKAR (STRM2(VIS, 'UU'), NAX, NVIS, 'R', UADD)
      CALL DATMAKAR (STRM2(VIS, 'VV'), NAX, NVIS, 'R', VADD)
      CALL DATMAKAR (STRM2(VIS, 'WW'), NAX, NVIS, 'R', WADD)
      CALL DATMAKAR (STRM3(VIS, VISCLASS, 'VIS'), NAX, NVIS, 'X', VSADD)
      CALL DATMAKAR (STRM3(VIS, VISCLASS, 'WT'), NAX, NVIS, 'R', WTADD)
      IF (ERROR) GO TO 990
C
C Initialize the array values
C
      DO 200 I = 1, NVIS
         MEMR(UADD+I-1) = 0.0
         MEMR(VADD+I-1) = 0.0
         MEMR(WADD+I-1) = 0.0
         MEMX(VSADD+I-1) = (0.0,0.0)
         MEMR(WTADD+I-1) = 1.0
 200  CONTINUE
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
