C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imxplot.f	1.7 29 Jun 1995
C
      SUBROUTINE SDEMAIN
C
C Program to plot an image
C
C Arguments: CALL SDEMAIN
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C      Increase MAXNC from 20 to 30
C      Add more label information
C                              M.A.Holdaway    Aug 18 1989
C	Added the option to display Clevs, Levs, rather than %peak labels
C				M.A.Holdaway	Sept 17 1989
C	Replaced call to CRDPTOW by in-line code to avoid rounding
C	errors when processing sub-milliarcsecond pixel images
C				T.J. Cornwell	Nov 3 1989
C	Added Quick contour routine invoked by QUICK
C				T.J. Cornwell	Nov 3 1989
C	Implemented Polarization 
C				M.A.Holdaway	Feb 18 1990
C	Added FILE0 to top label if non-zero
C				M.A.Holdaway	Feb 23 1993
C	Change "CALL PGADVANCE" -> "CALL PGPAGE" for pgplot.
C				M. Stupar	Jan 5 1995
C	Changed NAXIS to NAXIS(1), NAXIS(2) in call of PASTICKS
C	since one cannot use a passed array to dimension another
C	passed array
C				T.J. Cornwell	June 29 1995
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMXPLOT')
C
      INTEGER 		IAX, NAX, NDUMMY, NREAL, BLC(SYSMXDIM), 
     1   		TRC(SYSMXDIM), NAXIS (SYSMXDIM), ISTAT,
     2			PGBEGIN, AADD, CADD, CRDRNAX
      INTEGER		MAXNC, NC, ILEV, LINW, MXLABEL, IFONT
      INTEGER		IL4, NL4, STRLEN, NL4MX, I, SDX, SDY
      REAL		PSMIN, SLEN, ROTANG
      LOGICAL		EVEC, PLOTPA
      PARAMETER		(MAXNC = 30)
      PARAMETER		(MXLABEL= 80)
      PARAMETER		(NL4MX= 3)
      INTEGER		C(MAXNC)
      REAL		CACTUAL(MAXNC), TR(6)
      REAL		PIXR(2)
      REAL		PLEV, CLEV, DMAX, DMIN, DATFGETR
      REAL		WBLC(SYSMXDIM), WTRC(SYSMXDIM)
      REAL              BDISP, TDISP, RDISP, LDISP, CHEIGHT, PERCENT
      REAL		CELLX, CELLY, BMAJ1, BMIN1, BPA1, FREQ
      DOUBLE PRECISION	CRVAL(SYSMXDIM)
      REAL		CRPIX(SYSMXDIM), CDELT(SYSMXDIM)
      REAL		X1(4096), X2(4096), Y1(4096), Y2(4096)
      INTEGER		KPA
      DOUBLE PRECISION  BMAJ, BMIN, BPA
      LOGICAL           FLABEL, PLABEL
      CHARACTER*1	ATYPE
      CHARACTER*(SYSMXNAM) 	DEVICE,PLOT,FILE1,FILE2,BUNIT,OBJECT,
     $   			FILE0
      CHARACTER*(MXLABEL) 	SPERCENT
      CHARACTER*(MXLABEL)       BLABEL1, BLABEL2,BLABEL3,BLABEL4(NL4MX)
      CHARACTER*(MXLABEL)       RLABEL1, RLABEL2, RLABEL3, RLABEL4
      CHARACTER*(MXLABEL)       TLABEL1, LLABEL1 
      CHARACTER*(SYSMXNAM)      STRINT, STRREAL
C
      DATA BLC /SYSMXDIM*1/
      DATA TRC /SYSMXDIM*1/
      DATA TR	/0.0, 1.0, 0.0, 0.0, 0.0, 1.0/
      DATA C	/-9, -8, -7, -6, -5, -4, -3, -2, -1, 1, 2, 3, 4,
     1		5, 6, 7, 8, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/
      DATA IFONT /2/
      DATA CHEIGHT /.6/
C==================================================================
C
      CALL MSGWELCO ('I contour a complex image image')
      CALL USRCTL
C
C Get levels etc
C
      CALL USRGETI ('Levs', C, MAXNC, NC)
      DO 5 ILEV = 1, MAXNC-1
         IF (C(ILEV+1).EQ.0.0) THEN
            NC = ILEV
            GO TO 6
         END IF
  5   CONTINUE
      NC = MAXNC
  6   CONTINUE
C
      CALL USRGETR ('Plev', PLEV, 1, NDUMMY)
      CALL USRGETR ('Clev', CLEV, 1, NDUMMY)
      CALL USRGETR ('Pixrange', PIXR, 2, NDUMMY)
      CALL USRGETI ('Linwidth', LINW, 1, NDUMMY)
      CALL USRGETL ('Fullabel', FLABEL, 1, NDUMMY)
      CALL USRGETL ('%Label', PLABEL, 1, NDUMMY)
      CALL USRGETC ('Plot', PLOT, 1, NDUMMY)
C
C Polarization Stick features
C
      CALL USRGETL ('PlotPA', PLOTPA, 1, NDUMMY)
      CALL USRGETL ('EVEC', EVEC, 1, NDUMMY)
      CALL USRGETR ('PSMIN', PSMIN, 1, NDUMMY)
      CALL USRGETR ('Slength', SLEN, 1, NDUMMY)
      CALL USRGETI ('Sdelx', SDX, 1, NDUMMY)
      CALL USRGETI ('Sdely', SDY, 1, NDUMMY)
      CALL USRGETR ('Rotang', ROTANG, 1, NDUMMY)
C
C Get Image
C
      CALL USRGETC ('Xfile', FILE0, 1, NDUMMY)
      CALL USRGETC ('Qfile', FILE1, 1, NDUMMY)
      CALL USRGETC ('Ufile', FILE2, 1, NDUMMY)
      CALL USRGETL ('Debug', SYSDEBUG, 1, NDUMMY)
      IF (ERROR) GO TO 999
C
C Get images
C
      IF (FILE0 .NE. ' ') THEN
         CALL FILIMGGE ('PImage', FILE0, ' ')
      ELSE
         CALL FILIMGGE ('QImage', FILE1, ' ')
         CALL FILIMGGE ('UImage', FILE2, ' ')         
         CALL ARRQU2X  ('QImage', 'UImage', 'PImage')
         CALL HEDCOPY  ('QImage', 'PImage')
         CALL DATDELET ('QImage')
         CALL DATDELET ('UImage')
      ENDIF
C
C Dump coordinates
C
      CALL MSGPUT ('Coordinates:', 'I')
      CALL CRDLIST ('PImage')
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
      CALL ARRX2PC  ('PImage', 'Amp', 'Chi')
      CALL DATGETAR ('Amp', NAX, NAXIS, ATYPE, AADD)
      CALL DATGETAR ('Chi', NAX, NAXIS, ATYPE, CADD)
      NREAL = CRDRNAX(NAX, NAXIS)
      IF (NREAL.LE.0) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'No real axes!')
         GO TO 999
      END IF
      IF (ERROR) GOTO 999
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
         BLC(IAX) = 1
         TRC(IAX) = 1
 15   CONTINUE
      CALL MSGPUT ('Plotting window: ', 'I')
      DO 20 IAX = 1, NREAL
         WRITE (MESSAGE, 1000) IAX, BLC (IAX), IAX, TRC (IAX)
 1000    FORMAT ('BLC(',I1,') = ',I4,', TRC(',I1,') = ',I4)
         CALL MSGPUT (MESSAGE, 'I')
  20  CONTINUE
C
C Find contour levels
C
      CALL DATCREAT ('Window')
      CALL DATPUTI( 'Window', 'BLC', BLC, SYSMXDIM)
      CALL DATPUTI( 'Window', 'TRC', TRC, SYSMXDIM)
      CALL ARRSTAT ('Amp', 'Window')
      DMAX = DATFGETR('Amp', 'ARRMAX')
      DMIN = DATFGETR('Amp', 'ARRMIN')
      IF ((PIXR(1).EQ.0.0).AND.(PIXR(2).EQ.0.0)) THEN
         PIXR(1) = DMIN
         PIXR(2) = DMAX
      END IF
      IF (CLEV.NE.0.0) THEN
         DO 16 ILEV = 1, NC
             CACTUAL(ILEV) = FLOAT(C(ILEV)) * CLEV
 16       CONTINUE
      ELSE
         IF (PLEV.EQ.0.0) THEN
            PLEV = 10.0
         END IF
         DO 25 ILEV = 1, NC
             CACTUAL(ILEV) = FLOAT(C(ILEV)) * (PLEV/100.0) * DMAX
  25     CONTINUE
      END IF
C
C Now get scaling info: convert coordinates to relative with
C respect to the reference pixel. Do this by hand since CRDPTOW
C has insufficient precision for all cases
C
      CALL DATGETD ('PImage', 'CRVAL', CRVAL, SYSMXDIM, NDUMMY)
      CALL DATGETR ('PImage', 'CDELT', CDELT, SYSMXDIM, NDUMMY)
      CALL DATGETR ('PImage', 'CRPIX', CRPIX, SYSMXDIM, NDUMMY)
      IF (ERROR) GO TO 999
      DO 30 IAX = 1, NREAL
         WBLC(IAX) = 3600.0*CDELT(IAX)*(FLOAT(BLC(IAX))-CRPIX(IAX))
         WTRC(IAX) = 3600.0*CDELT(IAX)*(FLOAT(TRC(IAX))-CRPIX(IAX))
 30   CONTINUE
      TR(2) = 3600.0 * CDELT(1)
      TR(1) = - 3600.0 * CDELT(1) * CRPIX(1)
      TR(6) = 3600.0 * CDELT(2)
      TR(4) = - 3600.0 * CDELT(2) * CRPIX(2)
C
C Get Label info from image database
C   
      IF (FLABEL) THEN
         CALL DATGETD ('PImage', 'BMAJ', BMAJ, 1, NDUMMY)
         CALL DATGETD ('PImage', 'BMIN', BMIN, 1, NDUMMY)
         CALL DATGETD ('PImage', 'BPA', BPA, 1, NDUMMY)
         IF (ERROR) THEN
            BMAJ = 0.
            BMIN = 0.
            BPA = 0.
            CALL ERRCANCE
         END IF
         CALL DATGETC ('PImage', 'BUNIT', BUNIT, 1, NDUMMY)
         CALL ERRCANCE
         IF (BUNIT(1:1) .EQ. ' ') BUNIT = 'Jy/beam'
         CALL DATGETC ('PImage', 'OBJECT', OBJECT, 1, NDUMMY)
         CALL ERRCANCE
C
C Put Label info into character strings
C
         BLABEL1='RA'
         BLABEL2='Max('
         CALL STRAPNB3(BLABEL2, BUNIT, ') =', STRREAL(DMAX, 3) )
         BLABEL3='Min('
         CALL STRAPNB3(BLABEL3, BUNIT, ') =', STRREAL(DMIN, 3) )
         IL4 = 1
         IF (PLABEL) THEN
            BLABEL4(IL4)='CONTOURS(%) ='
            DO 35 ILEV = 1, NC
               PERCENT=CACTUAL(ILEV)/DMAX * 100.
               IF (PERCENT .LT. 100.) THEN
                  SPERCENT = STRREAL(PERCENT,3)
                  IF (STRLEN(BLABEL4(IL4)) .GT. 
     $               (MXLABEL-STRLEN(SPERCENT))) THEN
                     IL4=IL4+1
                     BLABEL4(IL4) = ' '
                  ENDIF
                  IF (IL4 .LE. NL4MX) CALL STRAPNB2(BLABEL4(IL4), 
     $               SPERCENT, ',')
               ENDIF
 35         CONTINUE
         ELSE 
C	 contour information in form of CLEV, LEVS inputs
            IF (CLEV .NE. 0) THEN
               BLABEL4(IL4)= 'CLEV ='
               CALL STRAPNB2(BLABEL4(IL4), STRREAL(CLEV,4),' (')
               CALL STRAPNB2(BLABEL4(IL4), BUNIT, ')')
            ELSE
               BLABEL4(IL4) = 'PLEV ='
               CALL STRAPNB2(BLABEL4(IL4), STRREAL(PLEV,4),'%')
            ENDIF
            CALL STRAPPEN (BLABEL4(IL4), '   Levs = ' )
            DO 40 ILEV = 1, NC
               IF (CACTUAL(ILEV) .LT. DMAX) THEN
                  SPERCENT = STRINT( C(ILEV) )
                  IF (STRLEN(BLABEL4(IL4)) .GT. 
     $               (MXLABEL-STRLEN(SPERCENT))) THEN
                     IL4=IL4+1
                     BLABEL4(IL4) = ' '
                  ENDIF
                  IF (IL4 .LE. NL4MX) CALL STRAPNB2(BLABEL4(IL4), 
     $               SPERCENT, ', ')
               ENDIF
 40         CONTINUE
         ENDIF
         NL4 = MIN(IL4, NL4MX)
         I = STRLEN(BLABEL4(NL4))
         BLABEL4(NL4) = BLABEL4(NL4)(1:(I-1))
C
         IF (FILE0 .NE. ' ') THEN
            TLABEL1 = FILE0
         ELSE
            TLABEL1=FILE1(1:32)//FILE2(1:32)
         ENDIF
C
         RLABEL1=OBJECT
         IF(CRVAL(3).GT.1E12) THEN
            FREQ = CRVAL(3)/1.E12
            CALL STRAPNB2(RLABEL1, '      Frequency ='//STRREAL(FREQ,4),
     $        ' THz')
         ELSE
            FREQ = CRVAL(3)/1.E6
            CALL STRAPNB2(RLABEL1, '      Frequency ='//STRREAL(FREQ,4),
     $         ' MHz')
         END IF
         RLABEL2=STRINT(NAXIS(1))
         CALL STRAPNB2(RLABEL2, ' x', ' '//STRINT(NAXIS(2)) )
         CELLX = CDELT(1)*3600.
         CALL STRAPNB2(RLABEL2, ' cells of ', STRREAL(CELLX,3))
         CELLY = CDELT(2)*3600.
         CALL STRAPNB3(RLABEL2, ' x ', STRREAL(CELLY,3), ' arcsec')
C
         RLABEL3='BLC = ('
         CALL STRAPNB3( RLABEL3, STRINT(BLC(1)), ',', STRINT(BLC(2)) )
         CALL STRAPNB3( RLABEL3, ')   TRC = (', STRINT(TRC(1)), ',')
         CALL STRAPNB2( RLABEL3, STRINT(TRC(1)), ')' )
C
         RLABEL4='Beam(arcsec) =' 
         BMAJ1 = BMAJ * 3600.
         CALL STRAPNB2( RLABEL4, STRREAL(BMAJ1,3),' x ')
         BMIN1 = BMIN * 3600.
         CALL STRAPNB2( RLABEL4, STRREAL(BMIN1,3), ' @ ')
         BPA1 = BPA
         CALL STRAPNB2( RLABEL4, STRREAL(BPA1,3), ' degrees')
C
         LLABEL1='DEC'
      END IF
C
C Call PGPLOT routines: Get device, etc
C
      CALL USRGETC ('Device', DEVICE, 1, NDUMMY)
      ISTAT = PGBEGIN(0, DEVICE, 1, 1)
      CALL PGSCH(CHEIGHT)
      CALL PGSCF(IFONT)
      IF (ISTAT.NE.1) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1      'Cannot open graphics device '//DEVICE)
         GO TO 999
      END IF
      CALL PGSLW (LINW)
C     In order to print extensive labels, we are doing PGENV by hand
C      CALL PGENV (WBLC(1), WTRC(1), WBLC(2), WTRC(2), 1, 0)
      CALL PGPAGE
C     sneaky...PGVSTAND opens the largest viewport available, leaving
C     4 character heights for margins.  So, we increase character ht.
      CALL PGSCH(3.0*CHEIGHT)
      CALL PGVSTAND
      CALL PGSCH(1.5*CHEIGHT)
      CALL PGWNAD(WBLC(1), WTRC(1), WBLC(2), WTRC(2))
      CALL PGBOX('BCNST', 0.0, 0, 'BCNST', 0.0, 0)
      CALL PGSCH(CHEIGHT)
C
      CALL PGIDEN
      IF (FLABEL ) THEN
         BDISP = 4.0
         TDISP = 2.0
         RDISP = 3.0
         LDISP = 4.0
C        Bottom Label
         CALL PGMTEXT ('B', BDISP, .5,  .5, BLABEL1) 
         BDISP = BDISP + 2.0
         CALL PGMTEXT ('B', BDISP, .5,  .5, BLABEL2) 
         BDISP = BDISP + 1.4
         CALL PGMTEXT ('B', BDISP, .5,  .5, BLABEL3) 
         IF (PLOT(1:2) .NE. 'GR') THEN
            DO 60 IL4 = 1, NL4
               BDISP = BDISP + 1.4
               CALL PGMTEXT('B', BDISP, .5, .5, BLABEL4(IL4))
 60         CONTINUE
         ENDIF
C        Top Labels
         CALL PGSCH (CHEIGHT*1.5)
         CALL PGMTEXT ('T', TDISP, .5, .5, TLABEL1)
         CALL PGSCH (CHEIGHT)
C        Right Labels
         CALL PGMTEXT ('R', RDISP, 0., 0., RLABEL1)
         RDISP = RDISP + 1.4
         CALL PGMTEXT ('R', RDISP, 0., 0., RLABEL2)
         RDISP = RDISP + 1.4
         CALL PGMTEXT ('R', RDISP, 0., 0., RLABEL3)
         RDISP = RDISP + 1.4
         CALL PGMTEXT ('R', RDISP, 0., 0., RLABEL4)
C        Left Labels
         CALL PGMTEXT ('L', LDISP, 0.5, 0.5, LLABEL1)
      ELSE
         TLABEL1=FILE1(1:32)//FILE2(1:32)
         CALL PGLABEL ('RA', 'DEC', TLABEL1)
      END IF
      IF (PLOT(1:2).EQ.'GR') THEN
         CALL PGGRAY (MEMR(AADD), NAXIS(1), NAXIS(2), BLC(1), TRC(1), 
     1      BLC(2), TRC(2), PIXR(1), PIXR(2), TR)
      ELSE IF(PLOT(1:2).EQ.'QU') THEN
         CALL PGCONS (MEMR(AADD), NAXIS(1), NAXIS(2), BLC(1), TRC(1), 
     1      BLC(2), TRC(2), CACTUAL, NC, TR)
      ELSE
         CALL PGCONT (MEMR(AADD), NAXIS(1), NAXIS(2), BLC(1), TRC(1), 
     1      BLC(2), TRC(2), CACTUAL, NC, TR)
      END IF
      CALL PGSLW (LINW+1)
      IF (PLOTPA) THEN
         PSMIN = PSMIN * DMAX/100.
         CALL MSGPUT ('Plotting polarization PA sticks','I')
         CALL PASTICKS (MEMR(AADD), MEMR(CADD), NAXIS(1), NAXIS(2), 
     $      PSMIN, ROTANG, EVEC, TR,  SDX, SDY, SLEN, KPA, X1, X2,
     $        Y1, Y2)
         DO 200 I = 1, KPA
            CALL PGMOVE(X1(I), Y1(I))
            CALL PGDRAW(X2(I), Y2(I))
 200     CONTINUE
      ENDIF
      CALL PGSLW (LINW)
      CALL PGEND
      CALL MSGPUT ('Plot File is in '//DEVICE, 'I')
C
 999  CONTINUE
      END
C
C
      SUBROUTINE PASTICKS (PAMP, CHI, NX, NY, PSMIN, ROTANG, EVEC,
     $      TR,  SDX, SDY, SLEN, KPA, X1, X2, Y1, Y2)
C
CD This routine generates the endpoints of the PA sticks
CD to be plotted on a polarization image
C
CS Arguments: CALL PASTICKS (PAMP, CHI, NX, NY, PSMIN, ROTANG, EVEC,
CS     $      TR,  SDX, SDY, SLEN, KPA, X1, X2, Y1, Y2)
CS
CA
CA Audit trail:
CA	Original version: Audit trail comments go on this line
CA	and successive lines
CA				M.A.Holdaway	Feb 19 1990
CE
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      INTEGER		NX, NY, KPA, SDX, SDY
      REAL		PAMP(NX,NY),CHI(NX,NY)
      REAL		PSMIN, ROTANG, SLEN, TR(6)
      LOGICAL		EVEC
      REAL		X1(*), X2(*), Y1(*), Y2(*)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PASTICKS')
C
      INTEGER		IX, IY
      REAL		XC, YC, CT, ST, S2, THETA
C
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      S2 = SLEN / 2.
      KPA = 0
      IF (.NOT. EVEC) ROTANG = ROTANG + 90.
      DO 240 IY = 1, NY, SDY
         DO 230 IX = 1, NX, SDX
            IF (PAMP(IX,IY).GT.PSMIN) THEN               
               KPA=KPA+1
               THETA=90.-CHI(IX, IY)-ROTANG
               CT=COSD(THETA)
               ST=SIND(THETA)
               XC=TR(1)+TR(2)*IX+TR(3)*IY
               YC=TR(4)+TR(5)*IX+TR(6)*IY
               X1(KPA)=XC+S2*CT
               Y1(KPA)=YC+S2*ST
               X2(KPA)=XC-S2*CT
               Y2(KPA)=YC-S2*ST
            ENDIF
 230     CONTINUE
 240  CONTINUE
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
