C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)img2sun.f	1.2    11/7/90
C
      SUBROUTINE SDEMAIN
C
CD Program to put an image on the SUN. This writes a byte file to a 
CD standard place (e.g. /sde/scratch/tvfile). This byte file can then be
CD read using the tv.e routine found in /sde/tools/suntv.
C
CS Arguments: CALL SDEMAIN
CD Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Feb 9 1989
C	Added Go commands load and show
C				T.J.Cornwell	June 6 1989
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMG2SUN')
C
      INTEGER 		IAX, NAX, NDUMMY, NREAL, BLC(SYSMXDIM), 
     1   		TRC(SYSMXDIM), NAXIS (SYSMXDIM), STRLEN, ADD
      CHARACTER*(SYSMXNAM)	INFILE, GOCOMM, IMAGE, SIMAGE, STRM2,
     $   		FILENAME
      CHARACTER*6	STRINT
      CHARACTER*1	ATYPE
      INTEGER		ISHOW, ILOAD
      REAL		PIXR(2)
C
      DATA BLC /SYSMXDIM*1/
      DATA TRC /SYSMXDIM*1/
      DATA		ILOAD/0/
      DATA		ISHOW/0/
C==================================================================
C
      CALL MSGWELCO ('I put an image on the Sun TV')
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
         CALL DATGETAR (IMAGE, NAX, NAXIS, ATYPE, ADD)
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
         CALL SYSGTENV ('SDETV', FILENAME)
         CALL ARRBYTF (SIMAGE, FILENAME, PIXR(1), PIXR(2))
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
C++
C
      SUBROUTINE ARRBYTF (A, OUTFILE, PMIN, PMAX)
C
C Convert an array to a scaled byte file
C
C
C	A	CH*(*)	input	Name of array
C	OUTFILE	CH*(*)	input	Name of outfile
C	PMIN	REAL	input	Minimum value to be displayed
C	PMAX	REAL	input	Maximum value to be displayed

C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Feb 9 1989
C      3-D version
C                              T.J. Cornwell  May 25 1989
C	Cleaned up checking of dimensions
C				T.J. Cornwell	Sept 28 1989
C
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	A, OUTFILE
      REAL		PMIN, PMAX
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'ARRBYTF')
C
      REAL		BSCALE, BZERO
      CHARACTER*1	T
      INTEGER		I, N, NAXIS(SYSMXDIM)
      INTEGER		ADD, NT
C=====================================================================
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (A, N, NAXIS, T, ADD)
      NT = 1
      DO 10 I = 1, N
         NT = NT * MAX(1, NAXIS(I))
  10  CONTINUE
      IF (NT.EQ.0) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Zero pixels')
         GO TO 999
      END IF
C
C Check for dimensions
C
      IF (N.EQ.1) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Cannot display 1D images')
         GO TO 999
      ELSEIF ((N.GT.2).AND.(NAXIS(3).GT.1)) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Cannot display 3D images')
         GO TO 999
      END IF
C
C Call appropriate routine
C
      IF (T.EQ.'R') THEN
         CALL PIXBYTF2 (MEMR(ADD), NAXIS(1), NAXIS(2),
     1      OUTFILE, PMIN, PMAX, BSCALE, BZERO)
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Array type '//T//
     1     'Not supported')
      END IF
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
C++
C
      SUBROUTINE PIXBYTF2 (A, NX, NY, FILE, PMIN, PMAX, BSCALE,
     $   BZERO)
C
C Write an array to a file as scaled bytes in the Sun pixrect format.
C
C	A	CMPLX	input	Complex array
C	NX	INT	input	Number of elements on x -axis
C	FILE	CH*(*)	input	Name of file
C	PMIN	REAL	input	Minimum value to be displayed
C	PMAX	REAL	input	Maximum value to be displayed
C	BSCALE	REAL	output	Scale used
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C      Changed to 3-d version and new file format
C                              T.J. Cornwell    May 25 1989
C	Put true clipping in
C				T.J. Cornwell    August 18 1989
C	Initialize line to eliminate bright pixels at RHS
C				T.J. Cornwell	August 24 1989
C	Changed to use FTIDR for writing to buffer
C				T.J. Cornwell	September 7 1989
C	Changed to 2D and pixrect format
C				T.J. Cornwell	September 21 1989
C
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		NX, NY
      REAL 		A(NX, NY), PMIN, PMAX, BSCALE, BZERO
      CHARACTER*(*)	FILE
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE='PIXBYTF2')
C
      REAL		AMAX, AMIN
      INTEGER		IX, IY, NIO, ID, STRLEN, NNX
      INTEGER           BUFF(1024)
      REAL		LINE(1024)
      DOUBLE PRECISION	RSCALE, RZERO
C=====================================================================
      IF (ERROR) GO TO 999
C
C Must use even number for x axis
C
      IF(MOD(NX,2).EQ.1) THEN
        NNX = NX + 1
      ELSE
        NNX = NX
      END IF
C
C  Find Maximum and Minimum
C
      AMAX = A(1,1)
      AMIN = A(1,1)
      DO 11 IY = 1, NY
         DO 10 IX = 1, NX
            AMAX = MAX(A(IX,IY), AMAX)
            AMIN = MIN(A(IX,IY), AMIN)
 10      CONTINUE
 11   CONTINUE
C
       IF (PMAX.NE.0.0) THEN
          AMAX = PMAX
       END IF
       IF (PMIN.NE.0.0) THEN
          AMIN = PMIN
       END IF
       IF (AMAX.EQ.AMIN) THEN
          CALL ERRREPOR (ERRBDARG, ROUTINE, 'Zero pixrange')
          GO TO 990
       END IF
C
C Open file and perform write
C
      CALL FILCOPEN (FILE(1:STRLEN(FILE)), STRLEN(FILE), 'w', 1, ID)
      IF (ID.EQ.0) THEN
         CALL ERRREPOR (ERROPEN, ROUTINE, 'Cannot open byte file')
         GO TO 990
      END IF
C
C Write header info
C
      RSCALE = 1.0D0
      RZERO = 0.0D0
C
C Initialize header
C
      BUFF(1) = 1504078485
      BUFF(2) = NX
      BUFF(3) = NY
      BUFF(4) = 8
      BUFF(5) = NX * NY
      BUFF(6) = 1
      BUFF(7) = 1
      BUFF(8) = 3 * 256
      CALL FILCWRIT (BUFF, 8 * 4, ID, NIO)
      IF (NIO.NE.8*4) THEN
         CALL ERRREPOR (ERROUTPT, ROUTINE, 'Cannot write all bytes')
         GO TO 999
      END IF
C
C Write color maps
C
      DO 14 IX = 1, 256
         LINE(IX) = IX
 14   CONTINUE
      CALL FTIDR (1, RSCALE, RZERO, 256, BUFF, 0, 1, LINE)
      CALL FILCWRIT (BUFF, 256, ID, NIO)
      IF (NIO.NE.256) THEN
         CALL ERRREPOR (ERROUTPT, ROUTINE, 'Cannot write all bytes')
         GO TO 999
      END IF
      CALL FILCWRIT (BUFF, 256, ID, NIO)
      IF (NIO.NE.256) THEN
         CALL ERRREPOR (ERROUTPT, ROUTINE, 'Cannot write all bytes')
         GO TO 999
      END IF
      CALL FILCWRIT (BUFF, 256, ID, NIO)
      IF (NIO.NE.256) THEN
         CALL ERRREPOR (ERROUTPT, ROUTINE, 'Cannot write all bytes')
         GO TO 999
      END IF
C
C Initialize line
C
      DO 15 IX = 1, NNX
         LINE(IX) = 1.0
  15  CONTINUE
C
C Now write each line in turn, going down the image
C
      DO 21 IY = NY, 1, -1
         DO 20 IX = 1, NX
            LINE(IX) = 1 + 199 * ((A(IX,IY)-AMIN)/(AMAX-AMIN))
            LINE(IX) = MAX(1.0,MIN(200.0,LINE(IX)))
 20      CONTINUE
         CALL FTIDR (1, RSCALE, RZERO, NNX, BUFF, 0, 1, LINE)
         CALL FILCWRIT (BUFF, NNX, ID, NIO)
         IF (NIO.NE.NNX) THEN
            CALL ERRREPOR (ERROUTPT, ROUTINE,
     $         'Cannot write byte file')
            GO TO 990
         END IF
 21   CONTINUE
      BSCALE = 1/199.0
      BZERO = AMIN
C
C Now synchronize and close the file
C
      CALL FILCCLOS (ID)
C
 990  IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
 999  CONTINUE
      END

