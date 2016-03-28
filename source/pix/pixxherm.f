C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixxherm.f	1.2	 24 Jul 1995
C
      SUBROUTINE PIXXHERM (HALF, HNX, HNY, FULL, FNX, FNY)
C
CD Make hermitian (FULL) out of HALF.
C
C	HALF	COMPLEX	input	Real array
C	HNX	INT(*)	input	Pixels in each axis, Half image
C	HNY	INT(*)	input	Pixels in each axis, Half image
C	FULL	COMPLEX	input	Real array
C	FNX	INT(*)	input	Pixels in each axis, Full image
C	FNY	INT(*)	input	Pixels in each axis, Full image
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A. Holdaway	June 30, 1994
C	Removed NAXIS from declarations
C				M.A. Holdaway	July 24 1995
C
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER	HNX, HNY, FNX, FNY
      COMPLEX	HALF(HNX, *), FULL(HNY, *)
C
      INTEGER		IX, IY, OFFSETX, OFFSETY
C=====================================================================
      OFFSETX = FNX/2
      OFFSETY = FNY/2
      DO 100 IY = 1, HNY
         DO 90 IX = 1, HNX
            FULL(OFFSETX-1+IX, IY) = HALF(IX, IY)
            IF ((2*OFFSETY-IY .GT. 0) .AND. (OFFSETX+1-IX .GT. 0)) THEN
               FULL(OFFSETX+1-IX, 2*OFFSETY-IY) = CONJG (HALF(IX, IY))
            ENDIF
 90      CONTINUE
 100  CONTINUE
C
      CONTINUE
      END
