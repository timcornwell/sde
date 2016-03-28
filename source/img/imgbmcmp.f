C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgbmcmp.f	1.2	 9/23/92
C
      SUBROUTINE IMGBMCMP (IMAGE1, IMAGE2, OUT)
C
C Looks into image2 and uses that beam to smooth and
C rescale image1, returns in OUT.  Will not work correctly
C if IMAGE2 is already smoothed.
C
C	NAME	CH*(*)	input	Name of directory entry
C	IMAGE1	CH*(*)	input	Name of image to be smoothed
C	IMAGE2	CH*(*)	input	Name of image we must match it to
C	OUT	CH*(*)	input	Smoothed, scaled version of IMAGE1
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	March 12 1991
C	Get the beam parameters differently to avoid crash
C				M.A.Holdaway	Sept 23 1992
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	IMAGE1, IMAGE2, OUT
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGBMCMP')
C
      REAL		SBEAM(4), FACT, BFACT
      INTEGER		NAXISI(SYSMXDIM), NAXI, NDUMMY
      CHARACTER*(8)	TYPEI(SYSMXDIM)
      REAL		RPIXI(SYSMXDIM)
      REAL		DELTI(SYSMXDIM)
      REAL		ROTAI(SYSMXDIM)
      DOUBLE PRECISION	RVALI(SYSMXDIM)
C=======================================================================
      INTEGER		STRLEN
      REAL		DATFGETR
      LOGICAL		DATEXIST
      DATA		SBEAM/4*0.0/
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
C If IMAGE1 has been convolved already, die now!
C
      CALL DATGETR (IMAGE1, 'BMAJ', SBEAM(1), 1, NDUMMY)
      IF (ERROR .OR. SBEAM(1) .EQ. 0.0) THEN
         CALL ERRCANCE
      ELSE
         WRITE (MESSAGE, 100) IMAGE1(1:(STRLEN(IMAGE1)))
 100     FORMAT ('Image ',A, ' has already been smoothed')
         CALL ERRREPOR (ERRBDARG, ROUTINE, MESSAGE)
         GOTO 990
      ENDIF
C
      IF (DATEXIST (OUT)) THEN
         CALL CRDCHECK(IMAGE1, OUT)
         IF (ERROR) GOTO 990
         CALL ARRSCALE (IMAGE1, 1.0, 0.0, OUT)
      ELSE
         CALL IMGCLONE (IMAGE1, OUT)
      ENDIF
C
      CALL CRDGET (IMAGE1, NAXI, TYPEI, NAXISI, RVALI, RPIXI,
     $   DELTI, ROTAI)
      FACT = SQRT(ATAN(1.0)/LOG(2.0))
      SBEAM(1) = 0.
      IF (ERROR) GOTO 990
      CALL DATGETR (IMAGE2, 'BMAJ', SBEAM(1), 1, NDUMMY)
      CALL DATGETR (IMAGE2, 'BMIN', SBEAM(2), 1, NDUMMY)
      CALL DATGETR (IMAGE2, 'BPA', SBEAM(3), 1, NDUMMY)
      SBEAM(1) = 3600.0*SBEAM(1)
      SBEAM(2) = 3600.0*SBEAM(2)
      IF (NAXISI(3).NE.1) THEN
         SBEAM(4) =  DATFGETR( 'IMAGE2', 'BZ')*3600.0
      ELSE
         SBEAM(4) = 0.0
      END IF
      IF (ERROR) THEN
         CALL ERRCANCE
         SBEAM(1) = 0.0
         SBEAM(2) = 0.0
         SBEAM(3) = 0.0
         SBEAM(4) = 0.0
      ENDIF
      IF (SBEAM(1) .NE. 0.0) THEN
         WRITE (MESSAGE, 1200) IMAGE1(1:(STRLEN(IMAGE1))),SBEAM(1),
     $      SBEAM(2), SBEAM(3)
 1200    FORMAT ('Convolving  ',A,' with beam: ',2(F12.8,'" '), F7.2,
     $      'deg ')
         CALL MSGPUT (MESSAGE, 'I')

         CALL IMGSMOOT (IMAGE1, SBEAM, OUT, 'Vtemp')
         CALL IMGP2PB (OUT, SBEAM, OUT)
      ENDIF
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
