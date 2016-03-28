C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)filccpu.f	1.2    7/13/94
C
C Audit trail:
      SUBROUTINE FILCCPU (IMAGE, CCFILE, MASK)
C
CD Write masked image to a FITS CC table
C
C	IMAGE	CH*(*)	input	Name of image
C	CCFILE	CH*(*)	input	Name of output file
C	MASK	CH*(*)	input	Name of image mask
C
C	This format can be read into AIPS via the task TBIN.  The CC
C	positions are relative to the reference position of the image,
C	so you must be sure that the original image, and the destination
C	image within AIPS have the same reference position.  The safest
C	course is probably to read the original image in with IMLOD and
C	use it as the target for TBIN.
C
C	Note that it will not deal with non-zero rotions correctly.
C
C Audit trail:
C	Split out from FILEXLPU
C				D.S.Briggs	June 21 1994
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FILCCPU')
C
      CHARACTER*(*)	IMAGE, CCFILE, MASK
C
      INTEGER		NAX, NAXIS(SYSMXDIM), IADD, MADD, NCC
      CHARACTER		ERRLEV*5
      CHARACTER*8	TYPE(SYSMXDIM)
      REAL		RPIX(SYSMXDIM), DELT(SYSMXDIM), ROTA(SYSMXDIM)
      DOUBLE PRECISION	RVAL(SYSMXDIM)
C
      INTEGER		DATADD, STRLEN, DATFGETI
C==================================================================
      IF (ERROR) GO TO 999
C
      CALL IMGMSCHK (IMAGE, MASK, 'EXACT', ERRLEV)
      IF (ERROR) GO TO 990
C
      IADD = DATADD (IMAGE)
      MADD = DATADD (MASK)
      CALL CRDGET (IMAGE, NAX, TYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
C
      CALL ARRMULT (IMAGE, MASK, 'TempCCs')
      CALL ARRSTAT ('TempCCs', ' ')
      NCC = DATFGETI ('TempCCs', 'ARRNLOC')
      CALL DATDELET ('TempCCs')
C
      CALL FILDEL (CCFILE)
      MESSAGE = 'Opening ' // CCFILE(1:STRLEN(CCFILE)) //
     $   ' as AIPS ASCII CC table file'
      CALL MSGPUT (MESSAGE, 'I')
      CALL TXTOPEN ('CComp', CCFILE, 'WRITE')
      IF (ERROR) GO TO 990
      CALL TXTWRITE ('CComp','XTENSION= ''TABLE   ''           /'
     $   // ' extension type')
      CALL TXTWRITE ('CComp','BITPIX  =                    8 /'
     $   //' printable ASCII codes')
      CALL TXTWRITE ('CComp','NAXIS   =                    2 /'
     $   //' Table is a matrix')
      CALL TXTWRITE ('CComp','NAXIS1  =                   80 /'
     $   //' Max. no. of characters/pass')
C
C Number of entries in table should be correct
C
 1000 FORMAT ('NAXIS2  =           ',I10,
     $   ' / Number of entries in table')
      WRITE (MESSAGE, 1000) NCC
      CALL TXTWRITE ('CComp',MESSAGE)
C
      CALL TXTWRITE ('CComp','PCOUNT  =                    0 /'
     $   //' Random parameter count')
      CALL TXTWRITE ('CComp','GCOUNT  =                    1 /'
     $   //' Group count')
      CALL TXTWRITE ('CComp','NOPASS  =                    1 /'
     $   //' Number of passes thru table')
      CALL TXTWRITE ('CComp','TFIELDS =                    3 /'
     $   //' Number of fields in each row')
      CALL TXTWRITE ('CComp','EXTNAME = ''AIPS CC ''           /'
     $   //' AIPS table file')
      CALL TXTWRITE ('CComp','EXTVER  =                    1 /'
     $   //' Version Number of table')
      CALL TXTWRITE ('CComp','TBCOL1  =                    9 /'
     $   //' Starting char. pos. of field')
      CALL TXTWRITE ('CComp','TFORM1  = ''E15.6   ''           /'
     $   //' Fortran format of field  1')
      CALL TXTWRITE ('CComp','TFDIM1  =                    1 /'
     $   //' Dimension of field  1')
      CALL TXTWRITE ('CComp','TTYPE1  = ''FLUX                    '''
     $   //'/ type (heading) of field  1')
      CALL TXTWRITE ('CComp','TUNIT1  = ''JY      ''           /'
     $   //' physical units of field  1')
      CALL TXTWRITE ('CComp','TBCOL2  =                   24 /'
     $   //' Starting char. pos. of field')
      CALL TXTWRITE ('CComp','TFORM2  = ''E15.6   ''           /'
     $   //' Fortran format of field  2')
      CALL TXTWRITE ('CComp','TFDIM2  =                    1 /'
     $   //' Dimension of field  2')
      CALL TXTWRITE ('CComp','TTYPE2  = ''DELTAX                  '''
     $   //'/ type (heading) of field  2')
      CALL TXTWRITE ('CComp','TUNIT2  = ''DEGREES ''           /'
     $   //' physical units of field  2')
      CALL TXTWRITE ('CComp','TBCOL3  =                   39 /'
     $   //' Starting char. pos. of field')
      CALL TXTWRITE ('CComp','TFORM3  = ''E15.6   ''           /'
     $   //' Fortran format of field  3')
      CALL TXTWRITE ('CComp','TFDIM3  =                    1 /'
     $   //' Dimension of field  3')
      CALL TXTWRITE ('CComp','TTYPE3  = ''DELTAY                  '''
     $   //'/ type (heading) of field  3')
      CALL TXTWRITE ('CComp','TUNIT3  = ''DEGREES ''           /'
     $   //' physical units of field  3')
      CALL TXTWRITE ('CComp','ISORTORD =                -257')
C
C History
C
      CALL TXTWRITE ('CComp','HISTORY SDE img2list')
C
      CALL TXTWRITE ('CComp','END')
      CALL TXTWRITE ('CComp',
     $   'COL. NO.      1              2              3')
      CALL TXTWRITE ('CComp',
     $   '     ROW   FLUX           DELTAX         DELTAY')
      CALL TXTWRITE ('CComp',
     $   '  NUMBER   JY             DEGREES        DEGREES')
      CALL TXTWRITE ('CComp', '***BEGIN*PASS***')
C
C Write the pixel values
C
      CALL PIX2DRTB (MEMR(IADD), MEMR(MADD), NAXIS(1), NAXIS(2),
     $   RPIX(1), RPIX(2), DELT(1), DELT(2), 'CComp')
C
      CALL TXTWRITE ('CComp', '***END*PASS***')
      CALL TXTCLOSE ('CComp')
C
C All done
C
 990  CONTINUE
      IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
 999  CONTINUE
      END
