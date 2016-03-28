C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)ipcscorr.f	1.5    5/28/93
C
      SUBROUTINE SDEMAIN
CD     
C    Program to auto-correlate ipcs data
C
C    Arguments: CALL sdemain
C    Audit trail:
C             Cloned from ipcsdp
C                                         R.G. Marson     Sep 14 1990
C    
C-----------------------------------------------------------------------
#include	"stdinc.h"
C     
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'ICORR')
C     
      INTEGER DATFGETI
      CHARACTER*(SYSMXNAM) STRM2
      INTEGER DATADD
C
      CHARACTER*(SYSMXNAM) INFILE, OUTFILE, SORT
      CHARACTER*8 CTYPE(SYSMXDIM)
      DOUBLE PRECISION RVAL(SYSMXDIM)
      REAL RPIX(SYSMXDIM), DELT(SYSMXDIM), ROTA(SYSMXDIM)
      REAL NPHOTONS
      INTEGER NDUMMY
      INTEGER YSIZE, XSIZE
      INTEGER NAX, NAXIS(SYSMXDIM)
      INTEGER CADD, PADD, FADD
      INTEGER XADD, YADD
C     
C==================================================================
C     
      CALL MSGWELCO ('I auto-correlate IPCS data')
      CALL USRCTL
C     
      CALL USRGETL ('Debug', SYSDEBUG, 1, NDUMMY)
C     
C     Get IPCS file
C    
      CALL USRGETC ('Infile', INFILE, 1, NDUMMY)
      CALL USRGETC ('Outfile', OUTFILE, 1, NDUMMY)
C
      CALL IPCSGET('IPCS', INFILE, '*', ' ')
      IF (ERROR) GOTO 999
C
C     Find the size of the output array
C
      XSIZE = DATFGETI('IPCS', 'WINDOW_W')
      YSIZE = DATFGETI('IPCS', 'WINDOW_H')
C
C Create the output array
C
      NAX = 2
      NAXIS(1) = XSIZE * 2 - 1
      NAXIS(2) = YSIZE * 2 - 1
      CALL DATMAKAR('CORR', NAX, NAXIS, 'R', CADD)
      CTYPE(1) = 'XPIXEL'
      CTYPE(2) = 'YPIXEL'
      RVAL(1) = 0.0
      RVAL(2) = 0.0
      RPIX(1) = XSIZE
      RPIX(2) = YSIZE
      DELT(1) = 1.0
      DELT(2) = 1.0
      ROTA(1) = 0.0
      ROTA(2) = 0.0
      CALL CRDPUT('CORR', NAX, CTYPE, NAXIS, RVAL, RPIX, DELT, ROTA) 
C
C Sort the data
C
      SORT = 'IF'
      CALL IPCSSORT('IPCS', SORT)
C
C Get the size of the photon arays
C
      NPHOTONS = DATFGETI('IPCS', 'GCOUNT')
C
C Get the address of all the photon arrays
C
C#      CALL DATGETAR(STRM2('IPCS','XPIXEL'), NAX, NAXIS, CTYPE(1), XADD)
C#      PRINT *, 'NPHOTONS = ', NPHOTONS, NAXIS(1), NAXIS(2), NAX
      XADD = DATADD(STRM2('IPCS','XPIXEL'))
      YADD = DATADD(STRM2('IPCS','YPIXEL'))
      FADD = DATADD(STRM2('IPCS','FRAME'))
      PADD = DATADD('IPCS')
C
C Now do the autocorrelation
C
      CALL ARRSETCO('CORR', 0.0, 0.0)
      CALL IPCACORR(MEMR(CADD), XSIZE, YSIZE, MEMR(XADD), MEMR(YADD), 
     $     MEMR(FADD), MEMR(PADD), NINT(NPHOTONS))
C
C Fix up the header
C
      CALL HISCOPY('IPCS', 'CORR')
      CALL HISINPUT('CORR')
C
C And write out the file
C
      CALL FILIMGPU ('CORR', OUTFILE, ' ')
C
 999  CONTINUE
      END
