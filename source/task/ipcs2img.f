C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)ipcs2img.f	1.7    5/12/93
C
      SUBROUTINE SDEMAIN
C     
CD    This routine converts an IPCS groups file into an image
C     
C    Arguments: CALL sdemain
C    Audit trail:
C             Cloned from ipcsgtv
C                                         R.G. Marson     Sep 11 1990
C    
C-----------------------------------------------------------------------
#include	"stdinc.h"
C     
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IPCS2IMG')
C     
      INTEGER 		NAX, NDUMMY, DATFGETI
      INTEGER           OUTADD
      INTEGER           NAXIS (SYSMXDIM)
      CHARACTER*(SYSMXNAM)	INFILE, STRM2, XTYPE, YTYPE, OUTFILE
      CHARACTER*(SYSMXNAM)	XARRAY, YARRAY
      CHARACTER*8       CTYPE(SYSMXDIM)
      DOUBLE PRECISION  RVAL(SYSMXDIM)
      REAL              RPIX(SYSMXDIM), DELT(SYSMXDIM), ROTA(SYSMXDIM)

      INTEGER XADD, YADD, XMIN, XMAX, YMIN, YMAX, I
      INTEGER NPHOTONS, DATADD
C     
C==================================================================
C     
      CALL MSGWELCO ('I convert an IPCS groups file to an image')
      CALL USRCTL
C     
      CALL USRGETL ('Debug', SYSDEBUG, 1, NDUMMY)
C     
C     Get IPCS file
C     
      CALL USRGETC ('Infile', INFILE, 1, NDUMMY)
      CALL IPCSGET('IPCS', INFILE, '*', ' ')
      IF (ERROR) GOTO 999
C     
C     First create the array
C     
      NAX = 2
      CALL USRGETC ('Xaxis', XTYPE, 1, NDUMMY)
      IF (XTYPE.EQ.'XPIXEL') THEN
         NAXIS(1) = DATFGETI('IPCS', 'WINDOW_W')
      ELSE IF (XTYPE.EQ.'YPIXEL') THEN
         NAXIS(1) = DATFGETI('IPCS', 'WINDOW_H')
      ELSE IF (XTYPE.EQ.'FRAME') THEN
         NAXIS(1) = DATFGETI('IPCS', 'FR_END') - 
     $        DATFGETI('IPCS', 'FR_START') + 1
      ELSE
         CALL ERRREPOR(ERRNTFND, ROUTINE, 
     $        'Xaxis must be either XPIXEL, YPIXEL or FRAME')
         GOTO 999
      END IF
C
      IF (ERROR) GOTO 999
      CALL USRGETC ('Yaxis', YTYPE, 1, NDUMMY)
      IF (YTYPE.EQ.'XPIXEL') THEN
         NAXIS(2) = DATFGETI('IPCS', 'WINDOW_W')
      ELSE IF (YTYPE.EQ.'YPIXEL') THEN
         NAXIS(2) = DATFGETI('IPCS', 'WINDOW_H')
      ELSE IF (YTYPE.EQ.'FRAME') THEN
         NAXIS(2) = DATFGETI('IPCS', 'FR_END') - 
     $        DATFGETI('IPCS', 'FR_START') + 1
      ELSE
         CALL ERRREPOR(ERRNTFND, ROUTINE, 
     $        'Yaxis must be either XPIXEL, YPIXEL or FRAME')
         GOTO 999
      END IF
C
      CALL DATMAKAR ('IPCSarray', NAX, NAXIS, 'R', OUTADD)
C     
C     Now ensure it is blank
C     
      CALL ARRSETCO('IPCSarray', 0, 0)
C
C Add 1 to X,Y Coords to make them go from 1->DETECTORsize
C
      XARRAY = STRM2('IPCS', XTYPE)
      YARRAY = STRM2('IPCS', YTYPE)
      CALL ARRSCALE(XARRAY, 1.0, 1.0, XARRAY)
      CALL ARRSCALE(YARRAY, 1.0, 1.0, YARRAY)
C     
C     Populate array with photons
C     
      NPHOTONS = DATFGETI('IPCS', 'GCOUNT')
      XADD = DATADD('IPCS/XPIXEL')
      YADD = DATADD('IPCS/YPIXEL')
      XMAX = MEMR(XADD)
      XMIN = XMAX
      YMAX = MEMR(YADD)
      YMIN = YMAX
      DO I = 1, NPHOTONS - 1
         XMAX = MAX(NINT(MEMR(XADD + I)), XMAX)
         XMIN = MIN(NINT(MEMR(XADD + I)), XMIN)
         YMAX = MAX(NINT(MEMR(YADD + I)), YMAX)
         YMIN = MIN(NINT(MEMR(YADD + I)), YMIN)
      END DO
      IF ((XMIN.NE.1).OR.(XMAX.NE.NAXIS(1)).OR.
     $     (YMIN.NE.1).OR.(YMAX.NE.NAXIS(2))) THEN
         WRITE(MESSAGE, '(A, I3,A,I3,A,I3,A,I3)')
     $        'Photon Range X:',XMIN,'-',XMAX,
     $                    ' Y:',YMIN,'-',YMAX
         CALL MSGPUT(MESSAGE, 'W')
      END IF
      CALL ARRSCAT(XARRAY, YARRAY, 'IPCS', 'IPCSarray', ' ') 
C
C Fix up the header
C
      CALL CRDGET('IPCSarray', NAX, CTYPE, NAXIS, RVAL, 
     $     RPIX, DELT, ROTA)
      CTYPE(1) =  XTYPE
      CTYPE(2) =  YTYPE
      RVAL(1) = 1
      RVAL(2) = 1
      RPIX(1) = 1
      RPIX(2) = 1
      DELT(1) = 1
      DELT(2) = 1
      CALL CRDPUT('IPCSarray', NAX, CTYPE, NAXIS, RVAL, 
     $     RPIX, DELT, ROTA)
      CALL HISCOPY ('IPCS', 'IPCSarray')
C     
C     Save the image
C     
      CALL USRGETC ('Outfile', OUTFILE, 1, NDUMMY)
      CALL FILIMGPU ('IPCSarray', OUTFILE, ' ')
C     
 999  CONTINUE
      END

