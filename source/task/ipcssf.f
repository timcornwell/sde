C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)ipcssf.f	1.1    1/29/93
C
      SUBROUTINE SDEMAIN
C
C Program to straigthen Fringes in IPCS wavelenght dispersed data
C
C Arguments: CALL SDEMAIN
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				R.G. Marson	Jan 14 1991
C
C-----------------------------------------------------------------------
C
C The standard include must be present
C
#include	"stdinc.h"
C
C Declare name of routine
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IPCSSF')
C
C Declare Function References
C
      CHARACTER*(SYSMXNAM)      STRM2
      INTEGER                   DATFGETI
C
C Names of local variables
C
      CHARACTER*(SYSMXNAM)	INFILE
      CHARACTER*(SYSMXNAM)      OUTFILE, METHOD
      CHARACTER*1               ATYPE
      REAL                      LAMBDA(2)
      INTEGER		        NDUMMY, NAX, NAXIS(SYSMXDIM), LADD
      INTEGER                   SIZE(2), XADD, YADD, NPHOTONS
C==================================================================
C
C Write welcome message
C
      CALL MSGWELCO ('I straighten fringes')
C
C Call user interface routine, and set debug status
C
      CALL USRCTL
      CALL USRGETC ('Infile', INFILE, 1, NDUMMY)
      CALL USRGETC ('Outfile', OUTFILE, 1, NDUMMY)
      CALL USRGETR ('Range', LAMBDA, 2, NDUMMY)
      CALL USRGETC ('Method', METHOD, 1, NDUMMY)
      CALL USRGETL ('Debug', SYSDEBUG, 1, NDUMMY)
C
C Get Input data
C
      CALL IPCSGET ('Input', INFILE, '*', ' ')
C
C Make Lookup Table
C
      NAX = 2
      SIZE(1) = DATFGETI('Input', 'WINDOW_W')
      SIZE(2) = DATFGETI('Input', 'WINDOW_H')
      NAXIS(1) = SIZE(1)
      NAXIS(2) = SIZE(2)
      IF (ERROR) GO TO 990
      CALL DATMAKAR('Lookup', NAX, NAXIS, 'R', LADD)
      CALL IPCSFRLU(MEMR(LADD), NAXIS, LAMBDA)
C
C Find out what method we are using
C
      IF ((METHOD(1:1).EQ.'N').OR.(METHOD(1:1).EQ.'n')) THEN
         METHOD(1:1) = 'N'
         MESSAGE = 'Using Nearest neighbour interpolation'
      ELSE
         METHOD(1:1) = 'L'
         MESSAGE = 'Using Linear interpolation'
      END IF
      CALL MSGPUT(MESSAGE, 'I')
C
C Move all the photons
C
      NPHOTONS = DATFGETI('Input', 'GCOUNT')
      CALL DATGETAR(STRM2('Input', 'XPIXEL'), NAX, NAXIS, ATYPE, XADD)
      CALL DATGETAR(STRM2('Input', 'YPIXEL'), NAX, NAXIS, ATYPE, YADD)
      CALL IPCSFRMV(MEMR(XADD), MEMR(YADD), NPHOTONS, MEMR(LADD), 
     $              SIZE, METHOD)
C
C Write result 
C
      CALL DATRENAM('Input', 'Output')
      CALL DATPUTI('Output', 'WINDOW_W', SIZE(1), 1)
      CALL DATPUTI('Output', 'WINDOW_H', SIZE(2), 1)
      CALL HISINPUT('Output')
      CALL IPCSPUT('Output', OUTFILE)
C
 990  IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
 999  CONTINUE
      END



