C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgclis.f	1.5    6/10/93
C
      SUBROUTINE IMGCLIS (CCLEAN, CCFILE, HEADER)
C
CD List the components of a Clark-Cleaned image. The first two lines are
C the HEADER and then the date and time when written.
C
C
C	CCLEAN	CH*(*)	input	Name of CCLEAN image
C	CCFILE	CH*(*)	input	Name of file to write to.
C	HEADER	CH*(*)	input	Header to write
C Audit trail:
C	New version
C				T.J.Cornwell	Jan 13 1989
C	Pass ANITER not NITER in the parameters to PIXCLISn
C				R.G. Marson	Nov 18 1990
C	Added 1-D code
C				D.S.Briggs	Oct 21 1992
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	CCLEAN, CCFILE, HEADER
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGCLIS')
C
      INTEGER		NITER, ANITER
      CHARACTER*1	CATYPE
      INTEGER		CNAX, CADD, CNAXIS(SYSMXDIM)
      INTEGER		NAXIS(SYSMXDIM)
      INTEGER		CLADD, CXLADD, CYLADD, CZLADD
      INTEGER		NREAL, NDUMMY, NAX, CRDRNAX
      REAL		DELT(SYSMXDIM)
      REAL		RPIX(SYSMXDIM)
      LOGICAL		DATEXIST
      CHARACTER*1	ATYPE
      CHARACTER*24	DTIME
      CHARACTER*(SYSMXNAM)	STRM2
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
C Get control parameters. ANITER is the actual number of iterations.
C NITER is the number specified.
C
      IF (DATEXIST(STRM2(CCLEAN, 'NITER'))) THEN
         CALL DATGETI(CCLEAN, 'NITER', ANITER, 1, NDUMMY)
      ELSE
         CALL ERRREPOR (ERRLOGIC, ROUTINE, 'No clean components')
         GO TO 999
      END IF
C
C Find lists
C
      CALL DATGETAR (CCLEAN, CNAX, CNAXIS, CATYPE, CADD)
      NREAL = CRDRNAX (CNAX, CNAXIS)
      IF (ERROR) GO TO 990
      IF (CATYPE.NE.'R') THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1      'Bad array type '//CATYPE//' for CLEAN Image')
         GO TO 990
      END IF
C
C Get locations for components list
C
      IF (.NOT.DATEXIST (STRM2(CCLEAN, 'PIXLIST'))) THEN
         CALL ERRREPOR (ERRLOGIC, ROUTINE, 'No clean components')
         GO TO 999
      ELSE
         CALL DATGETAR (STRM2(CCLEAN, 'PIXLIST'), NAX, NAXIS, ATYPE,
     1     CLADD)
         CALL DATGETAR (STRM2(CCLEAN, 'PIXLOC'), NAX, NAXIS, ATYPE,
     1     CXLADD)
      END IF
      NITER = NAXIS(1)
      CYLADD = CXLADD + ABS(NITER)
      CZLADD = CYLADD + ABS(NITER)
C
C Get coordinate increments
C
      CALL DATGETR (CCLEAN, 'CDELT', DELT, SYSMXDIM, NDUMMY)
      CALL DATGETR (CCLEAN, 'CRPIX', RPIX, SYSMXDIM, NDUMMY)
C
C Finally do something
C
      IF (NREAL.LE.3) THEN
         CALL TXTOPEN ('CClist', CCFILE, 'WRITE')
         CALL TXTWRITE ('CClist', HEADER)
         CALL SYSDATET (DTIME)
         WRITE (MESSAGE, 1000) DTIME
 1000    FORMAT ('/ Written ',A)
         CALL TXTWRITE ('CClist', MESSAGE)
         IF (NREAL.EQ.1) THEN
            WRITE (MESSAGE, 2000)
 2000       FORMAT ('/     Flux (Jy)        RA (asec)')
            CALL TXTWRITE ('CClist', MESSAGE)
            CALL PIXCLIS1 (MEMR(CLADD), MEMI(CXLADD), ANITER, RPIX(1),
     $         DELT(1), 'CClist')
         ELSE IF (NREAL.EQ.2) THEN
            WRITE (MESSAGE, 2050)
 2050       FORMAT ('/     Flux (Jy)        RA (asec)       Dec (asec)')
            CALL TXTWRITE ('CClist', MESSAGE)
            CALL PIXCLIS2 (MEMR(CLADD), MEMI(CXLADD), MEMI(CYLADD), 
     1         ANITER, RPIX(1), RPIX(2), DELT(1), DELT(2), 'CClist')
         ELSE
            WRITE (MESSAGE, 2100)
 2100       FORMAT ('/    ',
     1         'Flux (Jy)        RA (asec)        Dec (asec)',
     2         '        N(asec)')
            CALL TXTWRITE ('CClist', MESSAGE)
            CALL PIXCLIS3 (MEMR(CLADD), MEMI(CXLADD), MEMI(CYLADD), 
     1         MEMI(CZLADD), ANITER, RPIX(1), RPIX(2), RPIX(3), DELT(1), 
     2         DELT(2), DELT(3), 'CClist')
         END IF
         CALL TXTCLOSE ('CClist')
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Illegal dimension')
         GO TO 999
      END IF
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
