C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)ftiiniti.f	1.7    11/17/90
C
       SUBROUTINE FTIINITI(TYPE)
C
CD Initialize commons for writing a header. The type can be SIMPLE or XTENSION
C
C	TYPE	CHAR	input	Type of header
C
C Audit trail:
C	Added TYPE argument to allow writing of XTENSION files
C				T.J.Cornwell	Jan 8 1989
C       Added keywords for writing of GROUPS data files
C                              R.G. Marson     Oct 2 1989
C       Added ISCALE Keyword
C                              R.G. Marson     Nov 17 1990
C
C---------------------------------------------------------------------
#include	"stdinc.h"
C
#include	"ftsinc.h"
C
      CHARACTER*(*)	TYPE
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FTIINITI')
C
      INTEGER		IKEY
C====================================================================
      IF (ERROR) GO TO 999
C
      DO 10 IKEY = 1, FTSNKEYS
         FTSKEYS(1,IKEY) = ' '
         FTSKEYS(2,IKEY) = ' '
  10  CONTINUE
C
C These are the standard keywords which FTSWRITH will write.
C
      IKEY = 1
      IF (TYPE.EQ.'SIMPLE') THEN
         FTSKEYS(1,IKEY) =  'SIMPLE  '
         FTSKEYS(2,IKEY) =  'L'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'BITPIX  '
         FTSKEYS(2,IKEY) =  'I'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'NAXIS   '
         FTSKEYS(2,IKEY) =  'I'
C
C These previous cards must be present: the others are optional
C
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'NAXIS1  '
         FTSKEYS(2,IKEY) =  'I'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'NAXIS2  '
         FTSKEYS(2,IKEY) =  'I'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'NAXIS3  '
         FTSKEYS(2,IKEY) =  'I'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'NAXIS4  '
         FTSKEYS(2,IKEY) =  'I'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'NAXIS5  '
         FTSKEYS(2,IKEY) =  'I'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'NAXIS6  '
         FTSKEYS(2,IKEY) =  'I'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'NAXIS7  '
         FTSKEYS(2,IKEY) =  'I'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'EXTEND  '
         FTSKEYS(2,IKEY) =  'L'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'BLOCKED '
         FTSKEYS(2,IKEY) =  'L'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'OBJECT  '
         FTSKEYS(2,IKEY) =  'C'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'TELESCOP'
         FTSKEYS(2,IKEY) =  'C'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'INSTRUME'
         FTSKEYS(2,IKEY) =  'C'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'POLTYPE'
         FTSKEYS(2,IKEY) =  'C'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'POLPAIR'
         FTSKEYS(2,IKEY) =  'C'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'OBSERVER'
         FTSKEYS(2,IKEY) =  'C'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'DATE-OBS'
         FTSKEYS(2,IKEY) =  'C'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'DATE-MAP'
         FTSKEYS(2,IKEY) =  'C'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'BSCALE  '
         FTSKEYS(2,IKEY) =  'D'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'BZERO   '
         FTSKEYS(2,IKEY) =  'D'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'BUNIT   '
         FTSKEYS(2,IKEY) =  'C'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'EPOCH   '
         FTSKEYS(2,IKEY) =  'R'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'OBSRA   '
         FTSKEYS(2,IKEY) =  'D'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'OBSDEC  '
         FTSKEYS(2,IKEY) =  'D'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'XSHIFT  '
         FTSKEYS(2,IKEY) =  'R'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'YSHIFT  '
         FTSKEYS(2,IKEY) =  'R'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'NITER   '
         FTSKEYS(2,IKEY) =  'I'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'FLUX    '
         FTSKEYS(2,IKEY) =  'R'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'TFLUX   '
         FTSKEYS(2,IKEY) =  'R'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'ALPHA   '
         FTSKEYS(2,IKEY) =  'R'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'BETA    '
         FTSKEYS(2,IKEY) =  'R'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'DATAMAX '
         FTSKEYS(2,IKEY) =  'R'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'DATAMIN '
         FTSKEYS(2,IKEY) =  'R'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'CTYPE1  '
         FTSKEYS(2,IKEY) =  'C'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'CRVAL1  '
         FTSKEYS(2,IKEY) =  'D'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'CDELT1  '
         FTSKEYS(2,IKEY) =  'R'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'CRPIX1  '
         FTSKEYS(2,IKEY) =  'R'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'CROTA1  '
         FTSKEYS(2,IKEY) =  'R'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'CTYPE2  '
         FTSKEYS(2,IKEY) =  'C'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'CRVAL2  '
         FTSKEYS(2,IKEY) =  'D'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'CDELT2  '
         FTSKEYS(2,IKEY) =  'R'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'CRPIX2  '
         FTSKEYS(2,IKEY) =  'R'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'CROTA2  '
         FTSKEYS(2,IKEY) =  'R'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'CTYPE3  '
         FTSKEYS(2,IKEY) =  'C'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'CRVAL3  '
         FTSKEYS(2,IKEY) =  'D'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'CDELT3  '
         FTSKEYS(2,IKEY) =  'R'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'CRPIX3  '
         FTSKEYS(2,IKEY) =  'R'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'CROTA3  '
         FTSKEYS(2,IKEY) =  'R'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'CTYPE4  '
         FTSKEYS(2,IKEY) =  'C'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'CRVAL4  '
         FTSKEYS(2,IKEY) =  'D'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'CDELT4  '
         FTSKEYS(2,IKEY) =  'R'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'CRPIX4  '
         FTSKEYS(2,IKEY) =  'R'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'CROTA4  '
         FTSKEYS(2,IKEY) =  'R'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'CTYPE5  '
         FTSKEYS(2,IKEY) =  'C'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'CRVAL5  '
         FTSKEYS(2,IKEY) =  'D'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'CDELT5  '
         FTSKEYS(2,IKEY) =  'R'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'CRPIX5  '
         FTSKEYS(2,IKEY) =  'R'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'CROTA5  '
         FTSKEYS(2,IKEY) =  'R'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'CTYPE6  '
         FTSKEYS(2,IKEY) =  'C'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'CRVAL6  '
         FTSKEYS(2,IKEY) =  'D'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'CDELT6  '
         FTSKEYS(2,IKEY) =  'R'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'CRPIX6  '
         FTSKEYS(2,IKEY) =  'R'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'CROTA6  '
         FTSKEYS(2,IKEY) =  'R'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'CTYPE7  '
         FTSKEYS(2,IKEY) =  'C'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'CRVAL7  '
         FTSKEYS(2,IKEY) =  'D'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'CDELT7  '
         FTSKEYS(2,IKEY) =  'R'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'CRPIX7  '
         FTSKEYS(2,IKEY) =  'R'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'CROTA7  '
         FTSKEYS(2,IKEY) =  'R'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'BMAJ    '
         FTSKEYS(2,IKEY) =  'R'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'BMIN    '
         FTSKEYS(2,IKEY) =  'R'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'BPA     '
         FTSKEYS(2,IKEY) =  'R'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'BZ      '
         FTSKEYS(2,IKEY) =  'R'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'ORIGIN  '
         FTSKEYS(2,IKEY) =  'C'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'SUMWT   '
         FTSKEYS(2,IKEY) =  'R'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'ISCALE  '
         FTSKEYS(2,IKEY) =  'R'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'DATE    '
         FTSKEYS(2,IKEY) =  'C'
         IKEY = IKEY + 1
C
C Some keys for group files
C
         FTSKEYS(1,IKEY) =  'GROUPS  '
         FTSKEYS(2,IKEY) =  'L'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'PCOUNT  '
         FTSKEYS(2,IKEY) =  'I'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'GCOUNT  '
         FTSKEYS(2,IKEY) =  'I'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'PTYPE1  '
         FTSKEYS(2,IKEY) =  'C'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'PSCAL1  '
         FTSKEYS(2,IKEY) =  'D'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'PZERO1  '
         FTSKEYS(2,IKEY) =  'D'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'PTYPE2  '
         FTSKEYS(2,IKEY) =  'C'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'PSCAL2  '
         FTSKEYS(2,IKEY) =  'D'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'PZERO2  '
         FTSKEYS(2,IKEY) =  'D'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'PTYPE3  '
         FTSKEYS(2,IKEY) =  'C'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'PSCAL3 '
         FTSKEYS(2,IKEY) =  'D'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'PZERO3  '
         FTSKEYS(2,IKEY) =  'D'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'PTYPE4  '
         FTSKEYS(2,IKEY) =  'C'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'PSCAL4  '
         FTSKEYS(2,IKEY) =  'D'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'PZERO4  '
         FTSKEYS(2,IKEY) =  'D'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'PTYPE5  '
         FTSKEYS(2,IKEY) =  'C'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'PSCAL5  '
         FTSKEYS(2,IKEY) =  'D'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'PZERO5  '
         FTSKEYS(2,IKEY) =  'D'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'PTYPE6  '
         FTSKEYS(2,IKEY) =  'C'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'PSCAL6  '
         FTSKEYS(2,IKEY) =  'D'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'PZERO6  '
         FTSKEYS(2,IKEY) =  'D'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'PTYPE7  '
         FTSKEYS(2,IKEY) =  'C'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'PSCAL7  '
         FTSKEYS(2,IKEY) =  'D'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'PZERO7  '
         FTSKEYS(2,IKEY) =  'D'
         IKEY = IKEY + 1
C
C Some keys associated with IPCS data files
C
         FTSKEYS(1,IKEY) =  'MODE    '
         FTSKEYS(2,IKEY) =  'C'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'UT      '
         FTSKEYS(2,IKEY) =  'C'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'FRAME_H '
         FTSKEYS(2,IKEY) =  'I'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'FRAME_W '
         FTSKEYS(2,IKEY) =  'I'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'WINDOW_H'
         FTSKEYS(2,IKEY) =  'I'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'WINDOW_W'
         FTSKEYS(2,IKEY) =  'I'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'FR_START'
         FTSKEYS(2,IKEY) =  'I'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'FR_END  '
         FTSKEYS(2,IKEY) =  'I'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'RA      '
         FTSKEYS(2,IKEY) =  'D'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'DEC     '
         FTSKEYS(2,IKEY) =  'D'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'EQNX    '
         FTSKEYS(2,IKEY) =  'D'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'ARA     '
         FTSKEYS(2,IKEY) =  'D'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'ADEC    '
         FTSKEYS(2,IKEY) =  'D'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'HA      '
         FTSKEYS(2,IKEY) =  'D'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'ZD      '
         FTSKEYS(2,IKEY) =  'D'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'SORT    '
         FTSKEYS(2,IKEY) =  'C'
         IKEY = IKEY + 1
C
C Keys that are normally at the end of the header
C
         FTSKEYS(1,IKEY) =  'HISTORY '
         FTSKEYS(2,IKEY) =  'C'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'END     '
         FTSKEYS(2,IKEY) =  ' '
      ELSE IF (TYPE.EQ.'XTENSION') THEN
         FTSKEYS(1,IKEY) =  'XTENSION'
         FTSKEYS(2,IKEY) =  'C'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'BITPIX  '
         FTSKEYS(2,IKEY) =  'I'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'NAXIS   '
         FTSKEYS(2,IKEY) =  'I'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'NAXIS1  '
         FTSKEYS(2,IKEY) =  'I'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'NAXIS2  '
         FTSKEYS(2,IKEY) =  'I'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'PCOUNT  '
         FTSKEYS(2,IKEY) =  'I'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'GCOUNT  '
         FTSKEYS(2,IKEY) =  'I'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'TFIELDS '
         FTSKEYS(2,IKEY) =  'I'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'EXTNAME '
         FTSKEYS(2,IKEY) =  'C'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'EXTVER  '
         FTSKEYS(2,IKEY) =  'I'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'EXTLEVEL'
         FTSKEYS(2,IKEY) =  'I'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'TBCOL1  '
         FTSKEYS(2,IKEY) =  'I'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'TBCOL2  '
         FTSKEYS(2,IKEY) =  'I'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'TBCOL3  '
         FTSKEYS(2,IKEY) =  'I'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'TBCOL4  '
         FTSKEYS(2,IKEY) =  'I'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'TBCOL5  '
         FTSKEYS(2,IKEY) =  'I'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'TBCOL8  '
         FTSKEYS(2,IKEY) =  'I'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'TFORM1  '
         FTSKEYS(2,IKEY) =  'C'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'TFORM2  '
         FTSKEYS(2,IKEY) =  'C'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'TFORM3  '
         FTSKEYS(2,IKEY) =  'C'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'TFORM4  '
         FTSKEYS(2,IKEY) =  'C'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'TFORM5  '
         FTSKEYS(2,IKEY) =  'C'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'TFORM6  '
         FTSKEYS(2,IKEY) =  'C'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'TTYPE1  '
         FTSKEYS(2,IKEY) =  'C'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'TTYPE2  '
         FTSKEYS(2,IKEY) =  'C'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'TTYPE3  '
         FTSKEYS(2,IKEY) =  'C'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'TTYPE4  '
         FTSKEYS(2,IKEY) =  'C'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'TTYPE5  '
         FTSKEYS(2,IKEY) =  'C'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'TTYPE6  '
         FTSKEYS(2,IKEY) =  'C'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'TUNIT1  '
         FTSKEYS(2,IKEY) =  'C'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'TUNIT2  '
         FTSKEYS(2,IKEY) =  'C'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'TUNIT3  '
         FTSKEYS(2,IKEY) =  'C'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'TUNIT4  '
         FTSKEYS(2,IKEY) =  'C'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'TUNIT5  '
         FTSKEYS(2,IKEY) =  'C'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'TUNIT6  '
         FTSKEYS(2,IKEY) =  'C'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'TNULL1  '
         FTSKEYS(2,IKEY) =  'C'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'TNULL2  '
         FTSKEYS(2,IKEY) =  'C'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'TNULL3  '
         FTSKEYS(2,IKEY) =  'C'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'TNULL4  '
         FTSKEYS(2,IKEY) =  'C'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'TNULL5  '
         FTSKEYS(2,IKEY) =  'C'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'TNULL6  '
         FTSKEYS(2,IKEY) =  'C'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'TSCAL1  '
         FTSKEYS(2,IKEY) =  'R'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'TSCAL2  '
         FTSKEYS(2,IKEY) =  'R'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'TSCAL3  '
         FTSKEYS(2,IKEY) =  'R'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'TSCAL4  '
         FTSKEYS(2,IKEY) =  'R'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'TSCAL5  '
         FTSKEYS(2,IKEY) =  'R'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'TSCAL6  '
         FTSKEYS(2,IKEY) =  'R'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'TZERO1  '
         FTSKEYS(2,IKEY) =  'R'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'TZERO2  '
         FTSKEYS(2,IKEY) =  'R'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'TZERO3  '
         FTSKEYS(2,IKEY) =  'R'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'TZERO4  '
         FTSKEYS(2,IKEY) =  'R'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'TZERO5  '
         FTSKEYS(2,IKEY) =  'R'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'TZERO6  '
         FTSKEYS(2,IKEY) =  'R'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'AUTHOR  '
         FTSKEYS(2,IKEY) =  'C'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'REFERENC'
         FTSKEYS(2,IKEY) =  'C'
         IKEY = IKEY + 1
         FTSKEYS(1,IKEY) =  'END     '
         FTSKEYS(2,IKEY) =  'C'
         IKEY = IKEY + 1
      END IF
C
      IF (IKEY.GT.FTSNKEYS) THEN
         CALL ERRREPOR (ERRLOGIC, ROUTINE, 'Exceeded FTSNKEYS')
      END IF
C
 999  CONTINUE
      END
