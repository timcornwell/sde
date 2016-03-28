C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgwin.f	1.1    1/29/93
C
      SUBROUTINE SDEMAIN
C
CD Program to apply a window to images
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				R.G Marson 5 Apr 1992
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C Parameter Definitions
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGWIN')
C
C Function Definitions
C
      INTEGER           CRDRNAXM
C
C Local Variables
C
      CHARACTER*(SYSMXNAM)	INFILE, OUTFILE, WINDOW, TEMPSTR
      INTEGER 		NDUMMY
      INTEGER           NAX, NAXIS(SYSMXDIM), I
      REAL              ALPHA
      INTEGER           DIM, AXIS, WNAXIS(SYSMXDIM), OFFSET
      INTEGER           WADD, DADD
      CHARACTER*1       ATYPE
C
C==================================================================
C
      CALL MSGWELCO ('I window images')
      CALL USRCTL
C
C                         Get inputs
C
      CALL USRGETC ('Image', INFILE, 1, NDUMMY)
      CALL USRGETC ('Output', OUTFILE, 1, NDUMMY)
      CALL USRGETC ('Window', WINDOW, 1, NDUMMY)
      CALL USRGETR ('Alpha', ALPHA, 1, NDUMMY)
      CALL USRGETI ('Dimension', DIM, 1, NDUMMY)
      CALL USRGETI ('Axis', AXIS, 1, NDUMMY)
      IF (ERROR) GO TO 999
C
C Convert Window type to Upper Case
C
      TEMPSTR = WINDOW
      CALL STRUC(TEMPSTR, WINDOW)
C
C                         Get input Image
C
      CALL FILIMGGE ('Image', INFILE, ' ')
      CALL DATGETAR('Image', NAX, NAXIS, ATYPE, DADD)
      IF (ERROR) GOTO 999
C
C Generate the window array (then just multiply them)
C
      DO I = 1, SYSMXDIM
         WNAXIS(I) = 1
      END DO
      DIM = MIN(2, MAX(1, DIM))
      IF (DIM.EQ.1) THEN
         AXIS = MIN(NAX, MAX(1, AXIS))
         WNAXIS(1) = NAXIS(AXIS)
         CALL DATMAKAR('Window', 1, WNAXIS, 'R', WADD)
         CALL IMGMKWI1(MEMR(WADD), WNAXIS, WINDOW, ALPHA)
      ELSE IF (NAX.GE.2) THEN
         WNAXIS(1) = NAXIS(1)
         WNAXIS(2) = NAXIS(2)
         IF (CRDRNAXM(NAX, NAXIS).GE.2) THEN
            WNAXIS(1) = NAXIS(1)
            WNAXIS(2) = NAXIS(2)
         END IF
         CALL DATMAKAR('Window', 2, WNAXIS, 'R', WADD)
         CALL IMGMKWI2(MEMR(WADD), WNAXIS, WINDOW, ALPHA)
      ELSE
         CALL ERRREPOR(ERRLOGIC, ROUTINE, 
     $        'Input Image is not 2 Dimensional')
         GOTO 999
      END IF
C
C Modify data by our array (do the multiplication)
C
      IF (DIM.EQ.2) THEN
         CALL PIXRMULT(MEMR(DADD), MEMR(WADD), MEMR(DADD),
     $        WNAXIS(1) * WNAXIS(2))
      ELSE IF(AXIS.EQ.1) THEN
         DO I = 0, NAXIS(2) - 1
            OFFSET = I * NAXIS(1)
            CALL PIXRMULT(MEMR(DADD + OFFSET), MEMR(WADD), 
     $           MEMR(DADD + OFFSET), NAXIS(1)) 
         END DO
      ELSE
         DO I = 0, NAXIS(2) - 1
            OFFSET = I * NAXIS(1)
            CALL PIXRSCAL(MEMR(DADD + OFFSET), MEMR(WADD + I), 0.,
     $           MEMR(DADD + OFFSET), NAXIS(1))
         END DO
      END IF
C
C Do something about the History
C
      CALL HISINPUT('Image')
C
C Put the data on Disk
C
      CALL FILIMGPU ('Image', OUTFILE, ' ')
C
 999  CONTINUE
      END

