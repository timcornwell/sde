C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)gal2c.f	1.1    10/2/91
C
      SUBROUTINE SDEMAIN
C
C Program to make a linear combination of two images. This routine is
C called by the main program /sde/source/tsk/tsk.f and must be
C called SDEMAIN.
C
C Arguments: CALL SDEMAIN
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	Oct 2 1991
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
      PARAMETER		(ROUTINE = 'GAL2C')
C
C Names of input variables
C
      DOUBLE PRECISION	GLAT, GLONG, CRA, CDEC, LAT, LONG, RA, DEC
      INTEGER	DIRECT, NDUMMY
      CHARACTER*(SYSMXNAM)	GC
C==================================================================
C
C Write welcome message
C
      CALL MSGWELCO ('I convert between GAL and CEL coords')
C
C Call user interface routine, and set debug status
C
 10   CONTINUE
      CALL USRCTL
C
      CALL USRGETD('GLong',GLONG,1,NDUMMY)
      CALL USRGETD('GLat',GLAT,1,NDUMMY)
      CALL USRGETD('CRA',CRA,1,NDUMMY)
      CALL USRGETD('CDEC',CDEC,1,NDUMMY)
      CALL USRGETI('Direct',DIRECT,1,NDUMMY)
C
      LAT = GLAT
      LONG = GLONG
      RA = CRA
      DEC = CDEC
C
      IF (DIRECT .EQ. 1) THEN
         CALL CRDGAL2C (LONG, LAT, RA, DEC)
         WRITE (MESSAGE, 100) RA, DEC
 100     FORMAT (' RA = ', F12.6, ' DEC = ',F12.6)
      ELSE
         CALL CRDC2GAL (RA, DEC, LONG, LAT)
         WRITE (MESSAGE, 200) LONG, LAT
 200     FORMAT (' LONG = ', F12.6, ' LAT = ',F12.6)
      ENDIF
C
      CALL MSGPUT (MESSAGE, 'I')
C
      GOTO 10
C
 999  CONTINUE
C
      END
