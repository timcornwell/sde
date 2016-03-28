C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgstat.f	1.10	 7/18/97
C
      SUBROUTINE SDEMAIN
C
CD Program to calculate statistics of an image
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C       Added optional vector averaging when processing complex
C       image.
C				D.S.Briggs      Feb 14 1992
C	Added support for bounding box
C				D.S.Briggs	Feb 28 1992
C	More feeps.  Added mask file option.  It's a little kludgy,
C	since the calcs are done here, instead of doing it at the
C	pixel level.  Better would be to make a new version of ARRSTAT
C	and the PIX?STAT routines that know about boxes.
C				D.S.Briggs	March 18 1992
C	Use FILMASGE to get the mask, rather than FILIMGGE.  Allows
C	for recalculation of the mask on the fly.  Also include special
C	code to handle single pixel case.  Header of fitted image tweaked
C       tweaked up for the benefit of IMGFITTO.
C				D.S.Briggs	August 20, 1992
C	RA & DEC of reference position printed in conventional format
C				D.S.Briggs	May 11 1993
C	Converts JY/BEAM to JY if desired
C				M.A. Holdaway	June 6, 1994
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGSTAT')
C
      INTEGER 		IAX, NAX, BNAX, NDUMMY, NREAL, NLOC, NPIX,
     $   		ADDR, ADD, RNAX
      INTEGER		BLC(SYSMXDIM), TRC(SYSMXDIM), NAXIS (SYSMXDIM),
     $   		BNAXIS(SYSMXDIM),
     $			BBBLC(SYSMXDIM), BBTRC(SYSMXDIM),
     $   		MINLOC(SYSMXDIM), MAXLOC(SYSMXDIM)
      REAL		AVE, RMS, DMAX, DMIN, SUM, SUMJPB
      REAL		DISP, SX2, ERR, RRA(3), RDEC(3)
      REAL		BFACT, FACT, BEAM(4), DELT(SYSMXDIM)
      DOUBLE PRECISION	WORLD(SYSMXDIM), RVAL(SYSMXDIM)
      REAL		PIXEL(SYSMXDIM)
      INTEGER		IPIXEL(SYSMXDIM)
      CHARACTER*(SYSMXNAM) 	INFILE, BOXFILE, IMGUNITS,
     $   		AVGTYPE, TEMP
      CHARACTER		ATYPE*1, BATYPE*1, ARA*15, ADEC*15
      CHARACTER*8	TYPE(SYSMXDIM)
      CHARACTER*80	PIXBUF, STRPIX
      COMPLEX		XAVE, XDMAX, XDMIN, XRMS, XSUM, XSX2, XSUMJPB
      LOGICAL		TOJY
C
      REAL		DATFGETR
      INTEGER		DATFGETI, CRDRNAX, STRLEN
      COMPLEX		DATFGETX
      DATA BLC /SYSMXDIM*1/
      DATA TRC /SYSMXDIM*1/
C
C==================================================================
C
      CALL MSGWELCO ('I calculate statistics of images')
      CALL USRCTL
C
C Get Image
C
      CALL USRGETC ('Image', INFILE, 1, NDUMMY)
      CALL USRGETC ('Box', BOXFILE, 1, NDUMMY)
      CALL USRGETL ('Debug', SYSDEBUG, 1, NDUMMY)
      CALL USRGETC ('AvgType', TEMP, 1, NDUMMY)
      CALL STRUC (TEMP, AVGTYPE)
      IF (ERROR) GO TO 999
C
      CALL FILIMGGE ('Image', INFILE, ' ')
      CALL DATGETAR ('Image', NAX, NAXIS, ATYPE, ADDR)
      IF (ERROR) GO TO 999
C
C Get box, trim support, and check range
C
      IF (BOXFILE.NE.' ') THEN
         CALL FILMASGE ('Box', BOXFILE, 'Image')
         CALL IMGSTRIM('Box')
         IF (DATFGETR('Box', 'ARRMAX').GT.1.0) THEN
            MESSAGE = 'Clean Box has Maximum Value Greater Than 1.0'
            CALL MSGPUT(MESSAGE, 'W')
         END IF
         IF (DATFGETR('Box', 'ARRMIN').LT.0.0) THEN
            MESSAGE = 'Clean Box has Negative Minumum Value'
            CALL MSGPUT(MESSAGE, 'W')
         END IF
         CALL DATGETAR ('Box', BNAX, BNAXIS, BATYPE, NDUMMY)
         IF (CRDRNAX(BNAX,BNAXIS).EQ.0) THEN
            DO 5 IAX = 1, SYSMXDIM
               PIXEL(IAX) = 1.0
 5          CONTINUE
            CALL CRDDPTOW ('Box', PIXEL, WORLD)
            CALL CRDDWTOP ('Image', WORLD, PIXEL)
            DO 6 IAX = 1, NAX
               IPIXEL(IAX) = NINT(PIXEL(IAX))
 6          CONTINUE
            DO 7 IAX = NAX+1, SYSMXDIM
               IPIXEL(IAX) = 1
               NAXIS(IAX) = 1
 7          CONTINUE
            CALL GETPIXEL (ADDR, ATYPE, NAXIS, IPIXEL, AVE, XAVE)
            IF (ERROR) GO TO 999
            GO TO 1400
         END IF
      END IF
      IF (ERROR) GO TO 999
C
      IF (ATYPE.EQ.'X') THEN
         IF ((AVGTYPE.NE.'VECTOR').AND.(AVGTYPE.NE.'AMPSCALAR')) THEN
            AVGTYPE = 'AMPSCALAR'
            CALL MSGPUT ('Average type forced to AMPSCALAR','I')
         END IF
         CALL DATPUTC ('Image', 'AVGTYPE', AVGTYPE, 1)
      END IF
C
C Dump coordinates
C
      CALL MSGPUT ('Coordinates:', 'I')
      CALL CRDLIST ('Image')
C
      CALL DATGETD ('Image', 'CRVAL', RVAL, SYSMXDIM, NDUMMY)
      CALL DATGETC ('Image', 'CTYPE', TYPE, SYSMXDIM, NDUMMY)
      IF ((TYPE(1)(1:2).EQ.'RA').AND.(TYPE(2)(1:3).EQ.'DEC')) THEN
         CALL CRDD2RD (RVAL(1), RVAL(2), RRA, RDEC, ARA, ADEC)
         WRITE (MESSAGE,1008) ARA(1:STRLEN(ARA)), ADEC(1:STRLEN(ADEC))
 1008    FORMAT ('Reference RA = ',A,'  Dec = ',A)
         CALL MSGPUT (MESSAGE,'I')
      END IF
C
C Get window
C
      CALL USRGETI ('BLC', BLC, SYSMXDIM, NDUMMY)
      CALL ERRCANCE
      CALL USRGETI ('TRC', TRC, SYSMXDIM, NDUMMY)
      CALL ERRCANCE
C
C Find number of real axes
C
      NREAL = CRDRNAX (NAX, NAXIS)
C
C Special case for single pixel here
C
      IF (NREAL.LE.0) THEN
         DO 8 IAX = 1, SYSMXDIM
            IPIXEL(IAX) = 1
 8       CONTINUE
         DO 9 IAX = NAX+1, SYSMXDIM
            NAXIS(IAX) = 1
 9       CONTINUE
         CALL GETPIXEL (ADDR, ATYPE, NAXIS, IPIXEL, AVE, XAVE)
         IF (ERROR) GO TO 999
         GO TO 1400
      END IF
C
C Validate BLC and TRC
C
      DO 10 IAX = 1, NREAL
         IF(BLC(IAX).EQ.0) BLC(IAX) = 1
         IF(TRC(IAX).EQ.0) TRC(IAX) = NAXIS(IAX)
         BLC (IAX) = MAX (1, MIN (NAXIS(IAX), BLC (IAX)))
	 TRC (IAX) = MAX (1, MIN (NAXIS(IAX), TRC (IAX)))
  10  CONTINUE
      DO 15 IAX = NREAL+1, SYSMXDIM
         BLC (IAX) = 1
	 TRC (IAX) = 1
 15   CONTINUE
      IF (BOXFILE.EQ.' ') THEN
         CALL MSGPUT ('Statistics for window: ', 'I')
      ELSE
         CALL MSGPUT ('Statistics for (soft) window: ','I')
      END IF
      DO 20 IAX = 1, NREAL
         WRITE (MESSAGE, 1000) IAX, BLC (IAX), IAX, TRC (IAX)
 1000    FORMAT ('BLC(',I1,') =',I4,', TRC(',I1,') =',I4)
         CALL MSGPUT (MESSAGE, 'I')
 20   CONTINUE
C
C Multiply image by box, if present
C
      IF (BOXFILE.NE.' ') THEN
         CALL DATRENAM ('Image','RawImage')
         CALL IMGFITTO ('RawImage', 'Box', 'Image')
         CALL DATPUTC ('Image', 'AVGTYPE', AVGTYPE, 1)
         CALL DATDELET ('Image/ARRMAX')
         CALL DATDELET ('Image/ARRMIN')
         CALL DATDELET ('Image/ARRAVE')
         CALL DATDELET ('Image/ARRSUM')
         CALL ARRMULT ('Image', 'Box', 'Image')
         CALL CRDSHBOX ('RawImage', BLC, TRC, 'Image', BLC, TRC)
      END IF
C
C Make window
C
      CALL DATCREAT ('Window')
      CALL DATPUTI( 'Window', 'BLC', BLC, SYSMXDIM)
      CALL DATPUTI( 'Window', 'TRC', TRC, SYSMXDIM)
C
C Get Scaling factor into units of JY:
C if requested, and if units are JY/BEAM, SUM gets recomputed
C
C IMGUNITS may not exist
C
      IF (ERROR) GO TO 999
      IMGUNITS = ' '
      CALL DATGETC ('Image', 'BUNIT', IMGUNITS, 1, NDUMMY)
      CALL ERRCANCE
      TOJY = .FALSE.
      CALL STRUC (IMGUNITS, STRBUF)
      IF (STRBUF .EQ. 'JY/BEAM') THEN
         CALL DATGETR ('Image', 'BMAJ', BEAM(1), 1, NDUMMY)
         CALL DATGETR ('Image', 'BMIN', BEAM(2), 1, NDUMMY)
         CALL DATGETR ('Image', 'BZ', BEAM(4), 1, NDUMMY)
         TOJY = .TRUE.
         IF (ERROR) THEN
            CALL ERRCANCE
         ELSE
            TOJY = .TRUE.
         ENDIF
      ENDIF
      IF (TOJY) THEN
         BEAM(1) = BEAM(1) * 3600.
         BEAM(2) = BEAM(2) * 3600.
         BEAM(4) = BEAM(4) * 3600.
         CALL DATGETR ('Image', 'CDELT', DELT, SYSMXDIM, NDUMMY)
         CALL DATGETAR ('Image', NAX, NAXIS, ATYPE, ADD)
         RNAX = CRDRNAX(NAX, NAXIS)
C
         FACT = SQRT(ATAN(1.0)/LOG(2.0))/3600.0
         IF (RNAX.EQ.3) THEN
            BFACT = FACT**3 * BEAM(1) * BEAM(2) * BEAM(4)
     $           / ABS(DELT(1)**2 * DELT(3))
         ELSE IF (RNAX.EQ.2) THEN
            BFACT = FACT**2 * BEAM(1) * BEAM(2) / DELT(1)**2
         ELSE IF (RNAX.EQ.1) THEN
            BFACT = FACT * BEAM(1) / ABS(DELT(1))
         ELSE
            CALL ERRREPOR (ERRBDARG, ROUTINE, 'Bad number of axes')
            GO TO 999
         END IF
         IF (BFACT .EQ. 0.0) THEN
            CALL MSGPUT (
     $           'Conversion factor from JY/BEAM to JY is zero!', 'W')
            TOJY = .FALSE.
         ENDIF
      ENDIF
C      
C Call main routine to work on the ''Image'' array.
C
      CALL ARRSTAT ('Image', 'Window')
      IF (ERROR) GO TO 999
C
C Write results to the message file
C
C Get statistics from database
C
      RMS = DATFGETR('Image', 'ARRRMS')
      DISP = DATFGETR('Image', 'ARRDISP')
      NLOC = DATFGETI('Image','ARRNLOC')
      CALL DATGETI('Image', 'ARRBBBLC', BBBLC, SYSMXDIM, NDUMMY)
      CALL DATGETI('Image', 'ARRBBTRC', BBTRC, SYSMXDIM, NDUMMY)
      CALL DATGETI('Image', 'ARRMINLOC', MINLOC, SYSMXDIM, NDUMMY)
      CALL DATGETI('Image', 'ARRMAXLOC', MAXLOC, SYSMXDIM, NDUMMY)
      IF ((ATYPE.EQ.'X').AND.(AVGTYPE.NE.'AMPSCALAR')) THEN
         XAVE = DATFGETX('Image', 'ARRAVE')
         XDMAX = DATFGETX('Image', 'ARRMAX')
         XDMIN = DATFGETX('Image', 'ARRMIN')
         XSUM = DATFGETX('Image', 'ARRSUM')
         IF (TOJY) XSUMJPB = XSUM / BFACT
      ELSE
         AVE = DATFGETR('Image', 'ARRAVE')
         DMAX = DATFGETR('Image', 'ARRMAX')
         DMIN = DATFGETR('Image', 'ARRMIN')
         SUM = DATFGETR('Image', 'ARRSUM')
         IF (TOJY) SUMJPB = SUM / BFACT
      END IF
      
C
C If used a mask, figure out what they *should* have been.  Note that this
C  is a very ill conditioned calculation.  If it's ever really important,
C  go back and do it right at the pixel level calculations.  We calculate
C  a crude error estimate just for giggles.
C
      IF (BOXFILE.NE.' ') THEN
         IF ((ATYPE.EQ.'X').AND.(AVGTYPE.NE.'AMPSCALAR')) THEN
            NPIX = NINT(ABS(XSUM/XAVE))
            XSX2 = NPIX * XRMS
            XAVE = XSUM / NLOC
            XRMS = XSX2 / NLOC
            CALL MSGPUT ('Complex windowed dispersion not available',
     $         'W')
            DISP = -1.0
         ELSE
            NPIX = NINT(SUM/AVE)
            SX2 = NPIX * RMS**2
            AVE = SUM / NLOC
            RMS = SQRT (SX2 / NLOC)
            ERR = 3.0E-7 * (SX2 + SUM/NLOC) / (NLOC-1)
            DISP = (SX2 - SUM*SUM/NLOC) / (NLOC-1)
            ERR = ERR / DISP * 50.0
            IF (ERR.GT.0.1) THEN
               WRITE (MESSAGE,1030) ERR
 1030          FORMAT ('Error in dispersion at least',F5.2,' percent')
               CALL MSGPUT (MESSAGE,'W')
            END IF
            DISP = SQRT (DISP)
         END IF
C
C Present the bounding box relative to the raw image.
         CALL CRDSHBOX ('Image', BBBLC, BBTRC, 'RawImage',
     $      BBBLC, BBTRC)
      END IF
      IF (ERROR) GO TO 999
C
C Write all this stuff out
C
      IF ((ATYPE.EQ.'X').AND.(AVGTYPE.NE.'AMPSCALAR')) THEN
         WRITE (MESSAGE, 1100) XAVE, IMGUNITS(1:MAX(1,STRLEN(IMGUNITS)))
 1100    FORMAT ('   Average = ',1P,2E12.3,' ',A)
         CALL MSGPUT (MESSAGE, 'I')
         WRITE (MESSAGE, 1110) RMS, IMGUNITS(1:MAX(1,STRLEN(IMGUNITS)))
 1110    FORMAT ('   Rms     = ',1PE11.3,' ',A)
         CALL MSGPUT (MESSAGE, 'I')
         WRITE (MESSAGE, 1120) DISP, IMGUNITS(1:MAX(1,STRLEN(IMGUNITS)))
 1120    FORMAT ('   Dispers = ',1PE11.3,' ',A)
         CALL MSGPUT (MESSAGE, 'I')
         PIXBUF = STRPIX (MAXLOC, NREAL)
         WRITE (MESSAGE, 1130) XDMAX,
     $      IMGUNITS(1:MAX(1,STRLEN(IMGUNITS))),
     $      PIXBUF(1:STRLEN(PIXBUF))
 1130    FORMAT ('   Maximum = ',1P,2E12.3,' ',A,' at ',A)
         CALL MSGPUT (MESSAGE, 'I')
         PIXBUF = STRPIX (MINLOC, NREAL)
         WRITE (MESSAGE, 1140) XDMIN,
     $      IMGUNITS(1:MAX(1,STRLEN(IMGUNITS))),
     $      PIXBUF(1:STRLEN(PIXBUF))
 1140    FORMAT ('   Minimum = ',1P,2E12.3,' ',A,' at ',A)
         CALL MSGPUT (MESSAGE, 'I')
         WRITE (MESSAGE, 1150) XSUM, IMGUNITS(1:MAX(1,STRLEN(IMGUNITS)))
 1150    FORMAT ('   Sum     = ',1P,2E12.3,' ',A)
         CALL MSGPUT (MESSAGE, 'I')
         IF (TOJY) THEN
            WRITE (MESSAGE, 1160) XSUMJPB
 1160       FORMAT ('   Sum, JY = ',1P,2E12.3,' JY')
            CALL MSGPUT (MESSAGE, 'I')
         ENDIF
      ELSE
         WRITE (MESSAGE, 1200) AVE, IMGUNITS(1:MAX(1,STRLEN(IMGUNITS)))
 1200    FORMAT ('   Average = ',1PE11.3,' ',A)
         CALL MSGPUT (MESSAGE, 'I')
         WRITE (MESSAGE, 1210) RMS, IMGUNITS(1:MAX(1,STRLEN(IMGUNITS)))
 1210    FORMAT ('   Rms     = ',1PE11.3,' ',A)
         CALL MSGPUT (MESSAGE, 'I')
         WRITE (MESSAGE, 1220) DISP, IMGUNITS(1:MAX(1,STRLEN(IMGUNITS)))
 1220    FORMAT ('   Dispers = ',1PE11.3,' ',A)
         CALL MSGPUT (MESSAGE, 'I')
         PIXBUF = STRPIX (MAXLOC, NREAL)
         WRITE (MESSAGE, 1230) DMAX,
     $      IMGUNITS(1:MAX(1,STRLEN(IMGUNITS))),
     $      PIXBUF(1:STRLEN(PIXBUF))
 1230    FORMAT ('   Maximum = ',1PE11.3,' ',A,' at ',A)
         CALL MSGPUT (MESSAGE, 'I')
         PIXBUF = STRPIX (MINLOC, NREAL)
         WRITE (MESSAGE, 1240) DMIN,
     $      IMGUNITS(1:MAX(1,STRLEN(IMGUNITS))),
     $      PIXBUF(1:STRLEN(PIXBUF))
 1240    FORMAT ('   Minimum = ',1PE11.3,' ',A,' at ',A)
         CALL MSGPUT (MESSAGE, 'I')
         WRITE (MESSAGE, 1250) SUM, IMGUNITS(1:MAX(1,STRLEN(IMGUNITS)))
 1250    FORMAT ('   Sum     = ',1PE11.3,' ',A)
         CALL MSGPUT (MESSAGE, 'I')
         IF (TOJY) THEN
            WRITE (MESSAGE, 1260) SUMJPB
 1260       FORMAT ('   Sum, JY = ',1PE11.3,' JY')
            CALL MSGPUT (MESSAGE, 'I')
         ENDIF
      END IF
C
      DO 300 IAX = 1, NREAL 
         WRITE (MESSAGE, 1300) IAX, BBBLC(IAX), IAX, BBTRC(IAX)
 1300    FORMAT ('   BBBLC(',I1,') =',I4,', BBTRC(',I1,') =',I4)
         CALL MSGPUT(MESSAGE,'I')
 300  CONTINUE
      WRITE (MESSAGE, 1310) NLOC
 1310 FORMAT ('   ',I8,' non-zero pixels')
      CALL MSGPUT (MESSAGE,'I')
      GO TO 999
C
C Special case for single pixel
C
 1400 CONTINUE
      PIXBUF = STRPIX(IPIXEL, NREAL)
      WRITE (MESSAGE, 1410) PIXBUF(1:STRLEN(PIXBUF))
 1410 FORMAT ('  Single pixel at ',A)
      CALL MSGPUT (MESSAGE,'I')
      IF (ATYPE.EQ.'R') THEN
         WRITE (MESSAGE, 1420) AVE
 1420    FORMAT ('    =',1PE11.3)
      ELSE IF (ATYPE.EQ.'X') THEN
         WRITE (MESSAGE, 1430) XAVE
 1430    FORMAT ('    = (',1PE11.3,1PE11.3')')
      END IF
      CALL MSGPUT (MESSAGE,'I')
      GO TO 999
C
 999  CONTINUE
      END
C
      SUBROUTINE GETPIXEL (ADDR, ATYPE, N, PIX, AVE, XAVE)
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'GETPIXEL')
C
      INTEGER		ADDR, PIX(7), N(7)
      CHARACTER*1	ATYPE
      REAL		AVE
      COMPLEX		XAVE
      INTEGER 		I
C
      DO 10 I = 1, SYSMXDIM
         IF ((PIX(I).LT.1).OR.(PIX(I).GT.N(I))) THEN
            CALL ERRREPOR (ERRBDARG, ROUTINE,
     $         'subscript out of bounds')
            GO TO 999
         END IF
 10   CONTINUE
C
      IF (ATYPE.EQ.'R') THEN
         CALL GETRPIX (MEMR(ADDR), AVE, PIX,
     $      N(1), N(2), N(3), N(4), N(5), N(6), N(7))
      ELSE IF (ATYPE.EQ.'X') THEN
         CALL GETXPIX (MEMX(ADDR), XAVE, PIX,
     $      N(1), N(2), N(3), N(4), N(5), N(6), N(7))
      END IF
 999  CONTINUE
      END
C
      SUBROUTINE GETRPIX (A, AVE, P, N1, N2, N3, N4, N5, N6, N7)
C
      INTEGER	P(7), N1, N2, N3, N4, N5, N6, N7
      REAL	A(N1, N2, N3, N4, N5, N6, N7), AVE
C
      AVE = A(P(1),P(2),P(3),P(4),P(5),P(6),P(7))
      END
C
      SUBROUTINE GETXPIX (X, XAVE, P, N1, N2, N3, N4, N5, N6, N7)
C
      INTEGER	P(7), N1, N2, N3, N4, N5, N6, N7
      COMPLEX	X(N1, N2, N3, N4, N5, N6, N7), XAVE
C
      XAVE = X(P(1),P(2),P(3),P(4),P(5),P(6),P(7))
      END

