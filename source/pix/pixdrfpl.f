C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixdrfpl.f	1.3    11/16/92
C
      SUBROUTINE PIXDRFPL (F, NX, NY, X, Y, NPTS, VAL, WORK)
C
CD Draw a filled polygon in an array
C
C	F	R(NX,NY) output	Array (frame buffer)
C	NX	INT	input	Size of x-axis of A
C	NY	INT	input	Size of y-axis of A
C	X	R(NPTS)	input	Array of x-coordinates
C	Y	R(NPTS) input	Array of y-coordinates
C	NPTS	INT	input	Number of points in polygon
C	VAL	REAL	input	Cursor value to draw into array
C	WORK	REAL	output	Scratch area of size 4*NPTS or greater.
C
C Based on the routines in _Computer Graphics, A Programming Approach_,
C by Steven Harrington
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				D.S.Briggs	Nov 23 1991
C	Wasn't dealing with near-horizontal lines quite properly
C	in ADDEDGE
C				D.S.Briggs	Mar 20 1992
C	Ooops!  Didn't include edge clipping on vertical scan lines.
C				D.S.Briggs	Nov 14 1992
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		NX, NY, NPTS
      REAL		F(NX,NY), X(NPTS), Y(NPTS), WORK(NPTS, 4)
      REAL		VAL
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIXDRFPL')
C=====================================================================
      IF (ERROR) GO TO 999
C
      IF (NPTS.LE.2) THEN
         CALL MSGPUT ('Warning: Skipping polygon with too few sides',
     $      'W')
      END IF
C
C Just toss the whole mess to a subroutine, so that we can parcel up the
C work array into something sensible.
C
      CALL DRAWFLPL (F, NX, NY, X, Y, WORK(1,1), WORK(1,2), WORK(1,3),
     $   WORK(1,4), NPTS, VAL)
C
 999  CONTINUE
      END
C
      SUBROUTINE DRAWFLPL (F, NX, NY, X, Y, YMIN, YMAX, XA, DX,
     $   NPTS, VAL)
C
CD Draw a filled polygon in an array
C
C	F	R(NX,NY) output	Array (frame buffer)
C	NX	INT	input	Size of x-axis of A
C	NY	INT	input	Size of y-axis of A
C	X	R(NPTS)	input	Array of x-coordinates
C	Y	R(NPTS) input	Array of y-coordinates
C	YMIN	R(NPTS)	output	Scratch array of Min Y values
C	YMAX	R(NPTS)	output	Scratch array of Max Y values
C	XA	R(NPTS)	output	Scratch array of X-top
C	DX	R(NPTS)	output	Scratch array of DX / row
C	NPTS	INT	input	Number of points in polygon
C	VAL	REAL	input	Cursor value to draw into array
C
C See the comments in PIXDRFPL for version information
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		NX, NY, NPTS
      REAL		F(NX,NY), X(NPTS), Y(NPTS), YMIN(NPTS),
     $   		YMAX(NPTS), XA(NPTS), DX(NPTS)
      REAL		VAL
C
      INTEGER		I, J, K, L
      INTEGER		IX1, IX2, EDGES, IST, IEND, ISCAN
      REAL		X1, Y1, X2, Y2, T
      REAL		SCAN
C=====================================================================
      IF (ERROR) GO TO 999
C
C Build the edge tables we need
C
      X1 = X(1)
      Y1 = Y(1)
      EDGES = 1
      DO 100 I = 2, NPTS
         X2 = X(I)
         Y2 = Y(I)
         CALL ADDEDGE (X1, Y1, X2, Y2, YMIN, YMAX, XA, DX, EDGES)
 100  CONTINUE 
      X2 = X(1)
      Y2 = Y(1)
      CALL ADDEDGE (X1, Y1, X2, Y2, YMIN, YMAX, XA, DX, EDGES)
      EDGES = EDGES - 1
C
      ISCAN = YMAX(1)
      SCAN = ISCAN
      IST = 1
      IEND = 1
C
C Include any edges that we need consider
C
 200  CONTINUE
      IF ((IEND.LE.EDGES).AND.(YMAX(IEND).GE.SCAN)) THEN
         XA(IEND) = XA(IEND) + (YMAX(IEND)-SCAN)*DX(IEND)
         IEND = IEND + 1
         GO TO 200
      END IF
C
C Main loop over scan lines begins here
C
 300  CONTINUE
C
C Ensure that x-values are in sorted order within active region
C
      DO 450 K = IST, IEND-1
         L = K
 400     CONTINUE
         IF ((L.GT.IST).AND.(XA(L).LT.XA(L-1))) THEN
            T = YMIN(L)
            YMIN(L) = YMIN(L-1)
            YMIN(L-1) = T
            T = XA(L)
            XA(L) = XA(L-1)
            XA(L-1) = T
            T = DX(L)
            DX(L) = DX(L-1)
            DX(L-1) = T
            L = L - 1
            GO TO 400
         END IF
 450  CONTINUE
C
C Fill in the scan line
C
      IF ((ISCAN.GE.1).AND.(ISCAN.LE.NY)) THEN
         DO 550 K = IST, IEND-1, 2
            IX1 = MAX(1, MIN(NX, INT(XA(K))+1 ))
            IX2 = MAX(1, MIN(NX, INT(XA(K+1)) ))
            DO 500 J = IX1, IX2
               F(J, ISCAN) = VAL
 500        CONTINUE
 550     CONTINUE
      END IF
C
      ISCAN = ISCAN - 1
      SCAN = ISCAN
C
C Revise the active edges
C
      J = IEND-1
      DO 600 I = IEND-1, IST, -1
         IF (YMIN(I).LT.SCAN) THEN
            XA(J) = XA(I) + DX(I)
            IF (I.NE.J) THEN
               YMIN(J) = YMIN(I)
               DX(J) = DX(I)
            END IF
            J = J - 1
         END IF
 600     CONTINUE
      IST = J + 1
C
C Update the end of the edge list
C
 700  CONTINUE
      IF ((IEND.LE.EDGES).AND.(YMAX(IEND).GE.SCAN)) THEN
         XA(IEND) = XA(IEND) + (YMAX(IEND)-SCAN)*DX(IEND)
         IEND = IEND + 1
         GO TO 700
      END IF
C
C Main loopback over scan lines
C
      IF (IEND.NE.IST) GO TO 300
C
C All done
C
 999  CONTINUE
      END
C
      SUBROUTINE ADDEDGE (X1, Y1, X2, Y2, YMIN, YMAX, XA, DX, EDGES)
C
CD Draw a filled polygon in an array
C
C	X1	REAL	in/out	Defines edge to be added.  x, pnt 1
C	Y1	REAL	in/out	 same
C	X2	REAL	input	 same
C	Y2	REAL	input	 same
C	YMIN	R(NPTS)	output	array of Min Y values
C	YMAX	R(NPTS)	output	array of Max Y values
C	XA	R(NPTS)	output	array of X-top
C	DX	R(NPTS)	output	array of DX / row
C	EDGES	INT	in/out	size of table
C
C See the comments in PIXDRFPL for version information
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      REAL		X1, Y1, X2, Y2, YMIN(*), YMAX(*), XA(*), DX(*)
      INTEGER		EDGES
C
      INTEGER		J
      REAL		YM
C=====================================================================
      IF ((INT(Y1).EQ.INT(Y2)).AND.(Y2*Y2.GT.0.0)) THEN
         X1 = X2
      ELSE
C
C Insert edges into the work array
C
         J = EDGES
         YM = MAX(Y1, Y2)
C
C Find the correct insertion point, moving stuff as we go
C
 100     CONTINUE
         IF (J.NE.1) THEN
            IF (YMAX(J-1).LT.YM) THEN
               YMAX(J) = YMAX(J-1)
               YMIN(J) = YMIN(J-1)
               XA(J) = XA(J-1)
               DX(J) = DX(J-1)
               J = J - 1
               GO TO 100
            END IF
         END IF
C
C Now actually put the information into the array.  (there's an implicit
C  scan increment of 1 in the calculation for DX) 
C
         YMAX(J) = YM
         DX(J) = -(X2 - X1)/(Y2 - Y1)
         IF (Y1.GT.Y2) THEN
            YMIN(J) = Y2
            XA(J) = X1
         ELSE
            YMIN(J) = Y1
            XA(J) = X2
         END IF
C
         EDGES = EDGES + 1
         Y1 = Y2
         X1 = X2
      END IF
      END
C
