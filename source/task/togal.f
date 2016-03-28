C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)togal.f	1.1	 10/4/94
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
      PARAMETER		(ROUTINE = 'TOGAL')
C
      CHARACTER*(SYSMXNAM)	IN, OUT
      REAL			LATRANGE(2)
C
      INTEGER			NAX1, NAXIS1(SYSMXDIM), I, NDUMMY
      INTEGER			IRA, IDEC, IGLAT, IGLONG
      INTEGER			MAXTHING, ISAVE, NOLD
      PARAMETER			(MAXTHING = SYSMXDIM)
      INTEGER			NTHINGS, ITHINGS
      INTEGER			INADD(MAXTHING), OUTADD(MAXTHING)
      CHARACTER*1		T1
      CHARACTER*8		ARRNAMES(SYSMXDIM)
      DOUBLE PRECISION		GLAT, GLONG
C
      CHARACTER*(SYSMXNAM)	STRM2
C
      DATA			NAXIS1 /MAXTHING * 1/
      DATA			ARRNAMES /MAXTHING * ' '/
C==================================================================
C
      CALL MSGWELCO ('I find sources in the galactic plane')
      CALL USRCTL
C
C Get Inputs
C
      CALL USRGETC ('In', IN, 1, NDUMMY)
      CALL USRGETC ('Out', OUT, 1, NDUMMY)
      CALL USRGETR ('LatRange', LATRANGE, 2, NDUMMY)
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
         CALL ERRREPOR(ERRLOGIC, ROUTINE, 'No room for LAT/LONG')
         GOTO 999
      ELSE
         ARRNAMES(NTHINGS+1) = 'GLONG'
         ARRNAMES(NTHINGS+2) = 'GLAT'
         NTHINGS = NTHINGS + 2
      ENDIF
      CALL DATCREAT ('Out')
      CALL DATPUTC ('Out', 'ARRAYNAMES', ARRNAMES, MAXTHING)
C
      CALL DATGETAR ('Data/RA', NAX1, NAXIS1, T1, IRA)
      CALL DATGETAR ('Data/DEC', NAX1, NAXIS1, T1, IDEC)
      DO 50 ITHINGS = 1, NOLD
         CALL DATGETAR (STRM2('Data', ARRNAMES(ITHINGS)),
     $        NAX1, NAXIS1, T1, INADD(ITHINGS))
         CALL DATMAKAR (STRM2('Out', ARRNAMES(ITHINGS)),
     $        NAX1, NAXIS1, T1, OUTADD(ITHINGS))
 50   CONTINUE
      CALL DATMAKAR ('Out/GLONG', NAX1, NAXIS1, T1, IGLONG)
      CALL DATMAKAR ('Out/GLAT', NAX1, NAXIS1, T1, IGLAT)
C
      ISAVE = 0
      DO 100 I = 0, NAXIS1(1)-1
         CALL CRDC2GAL( DBLE(MEMR(IRA+I)), DBLE(MEMR(IDEC+I)),
     $        GLONG, GLAT)
         IF (LATRANGE(1) .LE. GLAT .AND. GLAT .LE. LATRANGE(2)) THEN
            MEMR(IGLONG+ISAVE)= GLONG
            MEMR(IGLAT+ISAVE) = GLAT
            DO 90 ITHINGS = 1, NOLD
               MEMR(OUTADD(ITHINGS)+ISAVE) = MEMR(INADD(ITHINGS)+I)
 90         CONTINUE
            ISAVE = ISAVE + 1
         ENDIF            
 100  CONTINUE
C
      CALL FILPUTN ('Out', OUT, ISAVE)
C
 990  CONTINUE
C
 999  CONTINUE
      END
