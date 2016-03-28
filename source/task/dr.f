C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)dr.f	1.4    12/5/94
C
      SUBROUTINE SDEMAIN
C
CD Program to calculate statistics of an image
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Butchered IMGSTAT for our own porpoises
C				M.A.Holdaway	April 10 1990
C	Made PIXRSTAT call compatible with new subroutine
C				M.A.Holdaway	July 23 1992
C	Tracked the change in PIXRSTAT which returns MINLOC and MAXLOC
C				M.A. Holdaway	Dec 5 1994
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'DR')
C
      INTEGER 		IAX, NAX, NDUMMY, NREAL, BLC1(SYSMXDIM), 
     1   		TRC1(SYSMXDIM), NAXIS (SYSMXDIM),
     $			BLC2(SYSMXDIM), TRC2(SYSMXDIM), II,
     $			BLC(SYSMXDIM), TRC(SYSMXDIM), AADD,
     $			BBBLC(SYSMXDIM), BBTRC(SYSMXDIM), NLOC(3),
     $   		NTOT
      REAL		AVE(3), RMS(3), DMAX(3), DMIN(3), SUM(3)
      REAL		DISP(3), DRRMS, DRDISP
      CHARACTER*(SYSMXNAM) 	INFILE
      CHARACTER*1		TYPE
      INTEGER			MINLOC(SYSMXDIM), MAXLOC(SYSMXDIM)
C
      DATA BLC /SYSMXDIM*1/
      DATA TRC /SYSMXDIM*1/
C==================================================================
C
      CALL MSGWELCO ('I calculate dynamic range of images')
      CALL USRCTL
C
C Get Image
C
      CALL USRGETC ('Image', INFILE, 1, NDUMMY)
      CALL USRGETI ('BLC1', BLC1, SYSMXDIM, NDUMMY)
      CALL USRGETI ('TRC1', TRC1, SYSMXDIM, NDUMMY)
      CALL USRGETI ('BLC2', BLC2, SYSMXDIM, NDUMMY)
      CALL USRGETI ('TRC2', TRC2, SYSMXDIM, NDUMMY)
C
      CALL FILIMGGE ('Image', INFILE, ' ')
      IF (ERROR) GO TO 999
C      CALL MSGPUT ('Coordinates:', 'I')
C      CALL CRDLIST ('Image')
C
      CALL DATGETAR ('Image', NAX, NAXIS, TYPE, AADD)
C
C Find number of real axes
C
      NREAL = NAX
   1  IF (NAXIS(NREAL).EQ.1) THEN
         NREAL = NREAL - 1
         GO TO 1
      END IF
C
      IF (NREAL.LE.0) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'No real axes!')
         GO TO 999
      END IF
C
C Validate BLC and TRC
C
      DO 111 II = 1, 3
         IF (II .EQ. 1) THEN
            DO 5 IAX = 1, NREAL
               BLC(IAX) = 1
               TRC(IAX) = NAXIS(IAX)
 5          CONTINUE
         ELSE IF (II .EQ. 2) THEN
            DO 6 IAX = 1, NREAL
               BLC(IAX) = BLC1(IAX)
               TRC(IAX) = TRC1(IAX)
 6          CONTINUE
         ELSE IF (II .EQ. 3) THEN
            DO 7 IAX = 1, NREAL
               BLC(IAX) = BLC2(IAX)
               TRC(IAX) = TRC2(IAX)
 7          CONTINUE
         ENDIF
C
         DO 10 IAX = 1, NREAL
            IF(BLC(IAX).EQ.0) BLC(IAX) = 1
            IF(TRC(IAX).EQ.0) TRC(IAX) = NAXIS(IAX)
            BLC (IAX) = MAX (1, MIN (NAXIS(IAX), BLC (IAX)))
            TRC (IAX) = MAX (1, MIN (NAXIS(IAX), TRC (IAX)))
 10      CONTINUE
         DO 15 IAX = NREAL+1, SYSMXDIM
            BLC (IAX) = 1
            TRC (IAX) = 1
 15      CONTINUE
C         CALL MSGPUT ('Statistics for window: ', 'I')
C         DO 20 IAX = 1, NREAL
C            WRITE (MESSAGE, 1000) IAX, BLC (IAX), IAX, TRC (IAX)
C 1000       FORMAT ('BLC(',I1,') = ',I4,', TRC(',I1,') = ',I4)
C            CALL MSGPUT (MESSAGE, 'I')
C 20      CONTINUE
C
         CALL PIXRSTAT (MEMR(AADD), NAXIS(1), NAXIS(2), NAXIS(3), 
     $      NAXIS(4), NAXIS(5), NAXIS(6), NAXIS(7), 
     1      BLC, TRC, DMAX(II), DMIN(II), AVE(II), 
     $      RMS(II), SUM(II), DISP(II), NLOC(II), BBBLC, BBTRC,
     $      MINLOC, MAXLOC)
         IF (ERROR) THEN
            GO TO 999
         END IF
 111  CONTINUE
C
C calculate dynamic range
C
      NTOT = NLOC(2) + NLOC(3)
      DRRMS = 2. * DMAX(1) / ( RMS(2)*NLOC(2)/FLOAT(NTOT) 
     $   		     + RMS(3)*NLOC(3)/FLOAT(NTOT) )
      DRDISP = 2. * DMAX(1) / ( DISP(2)*NLOC(2)/FLOAT(NTOT) 
     $   		     + DISP(3)*NLOC(3)/FLOAT(NTOT) )
C
      WRITE (MESSAGE, 2000) DRRMS, DRDISP
 2000 FORMAT ('   DR_RMS = ', F10.3, '      DR_DISP = ', F10.3)
      CALL MSGPUT (MESSAGE, 'I')
C
 999  CONTINUE
      END



