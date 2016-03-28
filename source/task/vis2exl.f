C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)vis2exl.f	1.1    6/7/93
C
      SUBROUTINE SDEMAIN
C
CD Converts between SDE visibility and Excel formats
C
C This is a quickie hack.  This, and IMG2EXL should be integrated into
C imgcopy and/or viscat.
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				D.S.Briggs	17 May 1993
C	Added Variance option
C				D.S.Briggs	28 May 1993
C-----------------------------------------------------------------------
#include		"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VIS2EXL')
C
      CHARACTER*(SYSMXNAM)	VIS, EXCEL, CLASS, STOKES
      LOGICAL		DOALL, DOVAR
C
      INTEGER		NDUMMY
C
      INTEGER		STRLEN
C=======================================================================
      CALL MSGWELCO ('I convert between Image and Excel formats')
      CALL USRCTL
C
C Get input parameters
C
      CALL USRGETC ('Vis', VIS, 1, NDUMMY)
      CALL USRGETC ('Excel', EXCEL, 1, NDUMMY)
      CALL USRGETC ('Stokes', STOKES, 1, NDUMMY)
      CALL USRGETL ('DoAll', DOALL, 1, NDUMMY)
      CALL USRGETL ('DoVariance', DOVAR, 1, NDUMMY)
      CLASS = 'OBS/'//STOKES(1:1)
      STOKES(2:) = ' '
C
      CALL VISGET ('Vis', VIS, STOKES, '*', ' ')
      IF (EXCEL.EQ.'*') EXCEL = VIS(1:STRLEN(VIS)) // '.EXL'
      CALL VISEXLPU ('Vis', EXCEL, CLASS, DOALL, DOVAR)
C
C Can jump to here if an error found
C
  999 CONTINUE
      END
