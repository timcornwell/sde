C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)precess.f	1.1	 10/4/94
C
      SUBROUTINE SDEMAIN
C
CD Convert to galactic coordinates
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	May 3, 1994
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PRECESS')
C
      CHARACTER*(SYSMXNAM)	IN, OUT
C
      INTEGER			NAX1, NAXIS1(SYSMXDIM), I, NDUMMY
      INTEGER			IRA, IDEC, JRA, JDEC
      CHARACTER*1		T1
      INTEGER			MAXTHING
      PARAMETER			(MAXTHING = SYSMXDIM)
      CHARACTER*8		ARRNAMES(MAXTHING)
      DOUBLE PRECISION		RANEW, DECNEW
      DOUBLE PRECISION		EIN, EOUT
      INTEGER			NTHINGS, NOLD, ITHINGS
C
      CHARACTER*(SYSMXNAM)	NAME
C
      DATA			NAXIS1 /SYSMXDIM * 1/
      DATA			ARRNAMES/MAXTHING * ' '/
C==================================================================
C
      CALL MSGWELCO ('I find sources in the galactic plane')
      CALL USRCTL
C
C Get Inputs
C
      CALL USRGETC ('In', IN, 1, NDUMMY)
      CALL USRGETC ('Out', OUT, 1, NDUMMY)
      CALL USRGETD ('EpochIn', EIN, 1, NDUMMY)
      CALL USRGETD ('EpochOut', EOUT, 1, NDUMMY)
C
      CALL FILGETN ('Data', IN)
      CALL DATGETC ('Data', 'ARRAYNAMES', ARRNAMES, MAXTHING, NDUMMY)

      NTHINGS = MAXTHING
      DO 8 ITHINGS = MAXTHING, 1, -1
         IF (ARRNAMES(ITHINGS) .EQ. ' ') NTHINGS = ITHINGS - 1
 8    CONTINUE
C
      NOLD = NTHINGS
      IF (NTHINGS .GT. 5) THEN
         CALL ERRREPOR(ERRLOGIC, ROUTINE, 'No room for PRECESSED COs')
         GOTO 999
      ELSE
         I = INT(EOUT)
         WRITE ( ARRNAMES(NTHINGS+1), 1818) I
         WRITE ( ARRNAMES(NTHINGS+2), 1819) I
 1818    FORMAT ('RA',I4)
 1819    FORMAT ('DEC',I4)
         NTHINGS = NTHINGS + 2
      ENDIF
C
      CALL DATPUTC ('Data', 'ARRAYNAMES', ARRNAMES, MAXTHING)
C
      CALL DATGETAR ('Data/RA', NAX1, NAXIS1, T1, IRA)
      CALL DATGETAR ('Data/DEC', NAX1, NAXIS1, T1, IDEC)
      NAME = 'Data/'//ARRNAMES(NOLD+1)
      CALL DATMAKAR (NAME, NAX1, NAXIS1, T1, JRA)
      NAME = 'Data/'//ARRNAMES(NOLD+2)
      CALL DATMAKAR (NAME, NAX1, NAXIS1, T1, JDEC)
C
      DO 100 I = 0, NAXIS1(1)-1
         CALL CRDPREC( DBLE(MEMR(IRA+I)), DBLE(MEMR(IDEC+I)),
     $        EIN, EOUT, RANEW, DECNEW)
         MEMR(JRA+I)= RANEW
         MEMR(JDEC+I) = DECNEW
 100  CONTINUE
C
      CALL FILPUTN ('Data', OUT, NAXIS1(1))
C
 990  CONTINUE
C
 999  CONTINUE
      END
