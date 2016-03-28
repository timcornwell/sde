C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)grduwt3d.f	1.3    11/7/90
C
      SUBROUTINE GRDUWT3D (WT, U, V, W, NVIS, USCALE, UOFFSET, 
     1   VSCALE, VOFFSET, WSCALE, WOFFSET, UORIGIN, VORIGIN, WORIGIN,
     2   NEWWT, GWT, NU, NV, NW)
C
CD 2-D uniform weighting. This version will properly reweight only
C Hermitean data. The current timing is about 70 microsec per point to
C be gridded onto a 1024**2 grid on the CONVEX C-1.
C The scaling factors should be set so that USCALE*U + UOFFSET converts
C to grid cells centered at 0. This is then shifted to UORIGIN. Thus,
C for example, UOFFSET, VOFFSET should nearly always be zero, while
C UORIGIN = 1, VORIGIN = NV/2.
C
C
C	WT	REAL(*)		input	Weights
C	U	REAL(*)		input	Coordinates of data
C	V	REAL(*)		input	Coordinates of data
C	W	REAL(*)		input	Coordinates of data
C	NVIS	INT		input	Number to be gridded
C	USCALE	REAL		input	Scaling factor to get to pixels
C	UOFFSET	REAL		input	Offset to get to pixels
C	VSCALE	REAL		input	Scaling factor to get to pixels
C	VOFFSET	REAL		input	Offset to get to pixels
C	WSCALE	REAL		input	Scaling factor to get to pixels
C	WOFFSET	REAL		input	Offset to get to pixels
C	UORIGIN	INT		input	Origin of u axis
C	VORIGIN	INT		input	Origin of v axis
C	WORIGIN	INT		input	Origin of v axis
C	NEWWT	REAL(*)		output	New weights
C	GWT	REAL(*)		output	Gridded weights
C	NU	INT		input	Size of gridded plane
C	NV	INT		input	Size of gridded plane
C	NW	INT		input	Size of gridded plane
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C------------------------------------------------------------------------
#include	"stdinc.h"
C
C
      INTEGER	NVIS, NU, SUPPU, OSAMPU, NV, SUPPV, OSAMPV, NW, SUPPW,
     1		OSAMPW
      INTEGER	UORIGIN, VORIGIN, WORIGIN
      REAL 	GWT(NU, NV, NW)
      REAL	WT(*), NEWWT(*)
      REAL	U(*), USCALE, UOFFSET
      REAL	V(*), VSCALE, VOFFSET
      REAL	W(*), WSCALE, WOFFSET
C
      CHARACTER*(*)	ROUTINE
      PARAMETER	(ROUTINE = 'GRDUWT3D')
C
      INTEGER 	IVIS, NREWT, USIGN
      INTEGER	UGRID, UGRIDI, UCEN
      INTEGER	VGRID, VGRIDI, VCEN
      INTEGER	WGRID, WGRIDI, WCEN
      REAL	UCELL, VCELL, WCELL, UVWT, WWT, UWT, REVIS, IMVIS
      COMPLEX	FVIS
C
C==========================================================================
      IF (ERROR) GO TO 999
C
C
      DO 7 WGRID = 1, NW
         DO 6 VGRID = 1, NV
            DO 5 UGRID = 1, NU
               GWT (UGRID,VGRID,WGRID) = 0.0
  5         CONTINUE
  6      CONTINUE
  7   CONTINUE
C
      NREWT = 0
C
C Start of loop elements to be gridded
C
      DO 10 IVIS = 1, NVIS
         IF (WT(IVIS).LE.0.0) GO TO 10
         UCELL = USCALE * U(IVIS) + UOFFSET 
         VCELL = VSCALE * V(IVIS) + VOFFSET 
         WCELL = WSCALE * W(IVIS) + WOFFSET 
         IF (UCELL.GT.0.0) THEN
            UCELL = UCELL + UORIGIN
            VCELL = VCELL + VORIGIN
	    WCELL = WCELL + WORIGIN
         ELSE
            UCELL = - UCELL + UORIGIN
            VCELL = - VCELL + VORIGIN
	    WCELL = - WCELL + WORIGIN
         END IF
         UCEN = NINT(UCELL) 
         VCEN = NINT(VCELL)
         WCEN = NINT(WCELL)
         GWT (UCEN,VCEN,WCEN) = GWT (UCEN,VCEN,WCEN) + WT(IVIS)
C
  10   CONTINUE
C
      DO 30 IVIS = 1, NVIS
         IF (WT(IVIS).LE.0.0) GO TO 30
         NREWT = NREWT + 1
         UCELL = USCALE * U(IVIS) + UOFFSET 
         VCELL = VSCALE * V(IVIS) + VOFFSET 
         WCELL = WSCALE * W(IVIS) + WOFFSET 
         IF (UCELL.GT.0.0) THEN
            UCELL = UCELL + UORIGIN
            VCELL = VCELL + VORIGIN
	    WCELL = WCELL + WORIGIN
         ELSE
            UCELL = - UCELL + UORIGIN
            VCELL = - VCELL + VORIGIN
	    WCELL = - WCELL + WORIGIN
         END IF
         UCEN = NINT(UCELL)
         VCEN = NINT(VCELL)
         WCEN = NINT(WCELL)
         IF (GWT(UCEN, VCEN, WCEN).NE.0.0) THEN
            IF (UCEN.EQ.UORIGIN) THEN
               NEWWT(IVIS) = 0.5 / GWT(UCEN, VCEN, WCEN)
            ELSE
               NEWWT(IVIS) = 1.0 / GWT(UCEN, VCEN, WCEN)
            END IF
         ELSE
            NEWWT(IVIS) = -1.0
         END IF
C
  30   CONTINUE
C
 999  CONTINUE
      END
