C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgsym.f	1.1	 2/21/91
C
      SUBROUTINE SDEMAIN
C
C Program to get symmetric, antisymmetric parts of an image
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	June 17 1990
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGSYM')
C
      CHARACTER*(SYSMXNAM) 	INFILE, SYM, ANTI
      INTEGER			NDUMMY
C==================================================================
C
      CALL MSGWELCO ('I find symmetric and antisymmetric images')
      CALL USRCTL
C
C Get Image
C
      CALL USRGETC ('Image', INFILE, 1, NDUMMY)
      CALL USRGETC('Symmetric',SYM,1,NDUMMY)
      CALL USRGETC('AntiSymmetric',ANTI,1,NDUMMY)
      CALL FILIMGGE ('Image', INFILE, '*')
C      
      CALL IMGCLONE ('Image', 'Sym')
      CALL IMGCLONE ('Image', 'Anti')
      CALL ARRRSYM ('Image', 'Sym', 'Anti')
      IF (ERROR) GOTO 990
C
C Write result 
C
      CALL FILIMGPU('Sym', SYM,' ')
      CALL FILIMGPU('Anti', ANTI,' ')
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
 999  CONTINUE
      END
