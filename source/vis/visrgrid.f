C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)visrgrid.f	1.1    3/15/93
C
      SUBROUTINE VISRGRID (VIS, SUB, GRID)
C
CD Grids real weights on a FULL PLANE grid
C
C	VIS	CH*(*)	inp	Vis set (only look at U,V,W, WTS)
C	SUB	CH*(*)	inp	Subclass for WTS ('OBS/I')
C	GRID	CH*(*)	inp	GRID image (2-D, Real, FULL PLANE)
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	March 2, 1993
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	VIS, SUB, GRID
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISRGRID')
C
      CHARACTER*1       VATYPE
      CHARACTER*8       VTYPE(SYSMXDIM)
      REAL              VRPIX(SYSMXDIM), VDELT(SYSMXDIM),
     1                  VROTA(SYSMXDIM)
      DOUBLE PRECISION  VRVAL(SYSMXDIM)
      INTEGER           VNAX, VNAXIS(SYSMXDIM)
      CHARACTER*8       ITYPE(SYSMXDIM)
      REAL              IRPIX(SYSMXDIM), IDELT(SYSMXDIM),
     1                  IROTA(SYSMXDIM)
      DOUBLE PRECISION  IRVAL(SYSMXDIM)
      INTEGER           INAX, IAX, INAXIS(SYSMXDIM), INREAL
      INTEGER           DATADD, WTADD, IADD, UADD, VADD
      REAL              USCALE, UOFFSET
      REAL              VSCALE, VOFFSET
      REAL              SUMWT
      INTEGER           UORIGIN, VORIGIN, NVIS2
      CHARACTER*(SYSMXNAM)      SVIS, STRM2
C
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
C Find coordinate information
C
      SVIS = STRM2 (VIS, SUB)
      CALL CRDGET (SVIS, VNAX, VTYPE, VNAXIS, VRVAL, VRPIX, VDELT, 
     1   VROTA)
      IF (ERROR) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1      'Cannot find coordinates for input data')
         GO TO 990
      END IF
      CALL CRDGET (GRID, INAX, ITYPE, INAXIS, IRVAL, IRPIX, IDELT,
     $   IROTA)
      IADD = DATADD (GRID)
      IF (ERROR) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1      'Cannot find coordinates for output image')
         GO TO 990
      END IF
      CALL DATGETAR (STRM2(SVIS, 'WT'), VNAX, VNAXIS, VATYPE,
     1   WTADD)
C
      INREAL = 0
      DO 5 IAX = 1, INAX
         IF (INAXIS(IAX).GT.1) INREAL = IAX
  5   CONTINUE
      IF (INREAL .NE. 2) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 
     $      'Can only deal with 2-D gridding')
         GOTO 990
      ENDIF
C
      UADD = DATADD (STRM2(VIS, 'UU'))
      VADD = DATADD (STRM2(VIS, 'VV'))

      USCALE = 1.0 / IDELT(1)
      UOFFSET = 0.0
      UORIGIN = NINT (IRPIX(1))
      VSCALE = 1.0 / IDELT(2)
      VOFFSET = 0.0
      VORIGIN = NINT (IRPIX(2))
C
      CALL GRDQNH2D (MEMR(WTADD), MEMR(UADD), 
     1   MEMR(VADD), VNAXIS(1), USCALE, UOFFSET, 
     2   VSCALE, VOFFSET, UORIGIN, VORIGIN, MEMR(IADD),  
     3   INAXIS(1), INAXIS(2), SUMWT)
C
      CALL DATPUTR (GRID, 'SUMWT', SUMWT, 1)
      NVIS2 = VNAXIS(1) * 2
      CALL DATPUTI (GRID, 'NVIS', NVIS2, 1)
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
