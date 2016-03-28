C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)fftconjx.f	1.3	 5/19/92
C
      SUBROUTINE FFTCONJX (IN, OUT, ODIR, INAX)
C
CD Transform the axis information from IN to reflect a Fourier Transform
C Creat COMPLEX image if needed
C
C	IN	CH*(*)	input	Name of input image
C	OUT	CH*(*)	input	Name of output image
C	ODIR	INT	output	Direction of transform
C	INAX	INT	input	Number of axes to conjugate
C	IN/FFTSIZE	CH*(*)	input	'PWR2' or 'EXACT' : ' ' => 'PWR2'
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	Nov 30 1990
C	Support for non Power-of-2 FFTs added
C				D.S.Briggs	Dec 27 1991
C	FFTSIZE keyword now stored in output array
C				D.S.Briggs	Mar 30 1992
C=====================================================================
#include	"stdinc.h"
C
      CHARACTER*(*)	IN, OUT
      INTEGER		ODIR, INAX
C
      CHARACTER*(*)	ROUTINE
      PARAMETER 	(ROUTINE = 'FFTCONJX')
C
      CHARACTER*8	TYPE (SYSMXDIM), CTYPE (SYSMXDIM),
     1			CTEMP
      INTEGER		NAX, NAXIS(SYSMXDIM), OADD, DIR(SYSMXDIM)
      REAL		RPIX(SYSMXDIM), ROTA (SYSMXDIM),
     1			DELT(SYSMXDIM), CDELT(SYSMXDIM), RTEMP
      DOUBLE PRECISION	RVAL(SYSMXDIM), DTEMP
      CHARACTER*(SYSMXNAM)	FFTSIZE
C
      INTEGER		REALNAX, CRDRNAX, IAX, IAXMATCH, NDUMMY
C
      CHARACTER*(SYSMXNAM)	STRM2
      LOGICAL		DATEXIST
      INTEGER		FFTPWR2, STRLEN
C=====================================================================
      IF (ERROR) GO TO 999
C
C Dig out the optional argument
C
      FFTSIZE = ' '
      IF (DATEXIST(STRM2(IN,'FFTSIZE')))
     $   CALL DATGETC (IN, 'FFTSIZE', FFTSIZE, 1, NDUMMY)
      IF (FFTSIZE.EQ.' ') FFTSIZE = 'PWR2'
C
C Get the coordinate information for the input image
C
      CALL CRDGET (IN, NAX, TYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
      IF (ERROR) GO TO 990
C
C Find real number of axes
C
      IF (INAX.EQ.0) THEN
         REALNAX = CRDRNAX (NAX, NAXIS)
         IF (REALNAX.EQ.0) THEN
            CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1         'No dimensions for image')
            GO TO 999
         END IF
      ELSE
         REALNAX = INAX
      END IF
C
C Find the conjugate axis for each axis, also the axis increment
C
      CALL FFTCONJ (TYPE, CTYPE, REALNAX, NAXIS, DELT, FFTSIZE,
     $   CDELT, DIR)
      ODIR = DIR(1) 
C 
C Try to find conjugate pseudo-axes i.e.  with just one pixel.  If 
C successful, then swap axes 
C
      IAXMATCH = 0
      DO 40 IAX = REALNAX+1, NAX
         IF (TYPE(IAX).EQ.CTYPE(IAXMATCH+1)) THEN
            IAXMATCH = IAXMATCH + 1
            NAXIS (IAX) = 1
            CTEMP = TYPE(IAX)
            TYPE (IAX) = TYPE(IAXMATCH)
            TYPE (IAXMATCH) = CTEMP
	    DTEMP = RVAL(IAX)
            RVAL (IAX) = RVAL(IAXMATCH)
            RVAL (IAXMATCH) = DTEMP
            RTEMP = RPIX(IAX)
            RPIX (IAX) = RPIX(IAXMATCH)
            RPIX (IAXMATCH) = RTEMP
            RTEMP = DELT (IAX)
            DELT (IAX) = DELT(IAXMATCH)
            DELT(IAXMATCH) = RTEMP
            RTEMP = ROTA (IAX)
            ROTA (IAX) = ROTA(IAXMATCH)
            ROTA(IAXMATCH) = RTEMP
         END IF
 40   CONTINUE
C
C No matching axes: make new ones
C
      IF (IAXMATCH.EQ.0) THEN
         IF ((REALNAX+NAX).GT.SYSMXDIM) THEN
            WRITE (MESSAGE, 200) REALNAX, NAX
  200       FORMAT ('No room to expand header: NAX = ',I1,
     1         ' REALNAX = ',I1)
            CALL ERRREPOR (ERRBDARG, ROUTINE, MESSAGE)
            GO TO 999
         END IF
C
         DO 50 IAX = NAX+1, NAX+REALNAX
            TYPE (IAX) = TYPE (IAX-NAX)
            TYPE (IAX-NAX) = CTYPE (IAX-NAX)
            NAXIS (IAX) = 1
            RVAL (IAX) = RVAL (IAX-NAX)
            RVAL (IAX-NAX) = 0.0D0
            RPIX (IAX) = RPIX (IAX-NAX)
            RPIX (IAX-NAX) = 1.0
            DELT (IAX) = DELT (IAX-NAX)
            DELT (IAX-NAX) = CDELT (IAX-NAX)
            ROTA (IAX) = ROTA (IAX-NAX)
            ROTA (IAX-NAX) = 0.0
  50     CONTINUE
         NAX = NAX + REALNAX
      ELSE
         NAX = NAX - REALNAX
      END IF
C
C Change number of pixels on all axes to power of two
C
      IF (FFTSIZE.EQ.'PWR2') THEN
         DO 60 IAX = 1, REALNAX
            NAXIS(IAX) = FFTPWR2 (NAXIS(IAX))
 60      CONTINUE
      END IF
C
      DO 65 IAX = 1, REALNAX
         RPIX(IAX) = (NAXIS(IAX) + 1) / 2
 65   CONTINUE
C
C Make output array
C
      CALL DATCREAT (OUT)
C
C Make a new array with the correct axis information
C
      CALL DATMAKAR (OUT, NAX, NAXIS, 'X', OADD)
C
C Put coordinate information into the header
C
      CALL CRDPUT (OUT, NAX, TYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
      CALL DATPUTC (OUT, 'FFTSIZE', FFTSIZE(1:STRLEN(FFTSIZE)), 1)
C
  990 IF (ERROR) CALL ERRTRACE (ROUTINE)
C
  999 CONTINUE
      END
