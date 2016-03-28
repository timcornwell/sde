C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C	(C)opyright 1989 by Associated Universities, Inc.; all rights reserved
C
      SUBROUTINE SDEMAIN
C
CD Program to put an image on the SUN. This writes a byte file to a 
CD standard place (e.g. /sde/scratch/tvfile). This byte file can then be
CD read using the tv.e routine found in /sde/tools/suntv.
C
CS Arguments: CALL SDEMAIN
CA Audit trail:
CA	Original version: Audit trail comments go on this line
CA	and successive lines
CA				T.J.Cornwell	Feb 9 1989
CA	Added Go commands load and show
CA				T.J.Cornwell	June 6 1989
CE
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGTVD')
C
      INTEGER 		IAX, NAX, NDUMMY, NREAL, BLC(SYSMXDIM), 
     1   		TRC(SYSMXDIM), NAXIS (SYSMXDIM), STRLEN
      CHARACTER*(SYSMXNAM)	INFILE, GOCOMM, IMAGE, SIMAGE, STRM2
      CHARACTER*6	STRINT
      INTEGER		ISHOW, ILOAD
      REAL		PIXR(2)
C
      DATA BLC /SYSMXDIM*1/
      DATA TRC /SYSMXDIM*1/
      DATA		ILOAD/0/
      DATA		ISHOW/0/
C==================================================================
C
      CALL MSGWELCO ('I put an image on the TV')
 100  CALL USRCTL
      CALL USRGETL ('Debug', SYSDEBUG, 1, NDUMMY)
C
C Get Go command
C
      CALL USRGETGO (GOCOMM)
C
C Get Image
C
      IF (GOCOMM.EQ.'load') THEN
         IF(ILOAD.GT.0) CALL DATDELET (IMAGE)
         ILOAD = ILOAD + 1
         IMAGE='Image'//STRINT(ILOAD)
         CALL USRGETC ('Image', INFILE, 1, NDUMMY)
         CALL FILIMGGE (IMAGE, INFILE, ' ')
C
C Dump coordinates
C
         CALL MSGPUT ('Coordinates:', 'I')
         CALL CRDLIST (IMAGE)
C
         CALL DATGETI (STRM2(IMAGE,'ARRAY'), 'NAXIS', NAXIS, SYSMXDIM,
     $      NAX)
C
C Find number of real axes
C
         NREAL = NAX
   1     IF (NAXIS(NREAL).EQ.1) THEN
            NREAL = NREAL - 1
            GO TO 1
         END IF
C
         IF (NREAL.LE.0) THEN
            CALL ERRREPOR (ERRBDARG, ROUTINE, 'No real axes!')
            GO TO 999
         END IF
C
C Do it
C
      ELSE IF (GOCOMM.EQ.'show') THEN
C
C Show image
C
         CALL USRGETR ('Pixrange', PIXR, 2, NDUMMY)
C
C Get window
C
         IF (ERROR) GO TO 999
         CALL USRGETI ('BLC', BLC, SYSMXDIM, NDUMMY)
         CALL ERRCANCE
         CALL USRGETI ('TRC', TRC, SYSMXDIM, NDUMMY)
         CALL ERRCANCE
C
C Validate BLC and TRC
C
         DO 10 IAX = 1, NREAL
            BLC (IAX) = MAX (1, MIN (NAXIS(IAX), BLC (IAX)))
            IF(TRC(IAX).EQ.1) THEN
               TRC(IAX) = NAXIS(IAX)
            END IF
   	    TRC (IAX) = MAX (1, MIN (NAXIS(IAX), TRC (IAX)))
  10     CONTINUE
         CALL MSGPUT ('Showing window: ', 'I')
         DO 20 IAX = 1, NREAL
            WRITE (MESSAGE, 1000) IAX, BLC (IAX), IAX, TRC (IAX)
 1000       FORMAT ('BLC(',I1,') = ',I4,', TRC(',I1,') = ',I4)
            CALL MSGPUT (MESSAGE, 'I')
  20     CONTINUE
C
C Make window
C
         CALL DATCREAT ('Window')
         CALL DATPUTI( 'Window', 'BLC', BLC, SYSMXDIM)
         CALL DATPUTI( 'Window', 'TRC', TRC, SYSMXDIM)
C
C Subsection image
C
         ISHOW = ISHOW + 1
         SIMAGE = 'SImage'//STRINT(ISHOW)
         CALL IMGSUBSE (IMAGE, SIMAGE, 'Window')
C
C Show image
C
         CALL IMGTVD (SIMAGE, PIXR(1), PIXR(2))
         CALL DATDELET (SIMAGE)
C
      ELSE
C
         CALL MSGPUT ('Unknown command '//GOCOMM(1:STRLEN(GOCOMM)), 'W')
      END IF
      IF(ERROR) GO TO 999
      GO TO 100
C
 999  CONTINUE
      END
