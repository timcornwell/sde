C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)mosbima.f	1.2    11/7/90
C
      SUBROUTINE SDEMAIN
C
CD Program to make mosaic data from BIMA data sets as written by
C J. Uson
C
C Audit trail:
C	Change to VISMOSPU
C				T.J.Cornwell	Feb 3 1989
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'MOSBIMA')
C
      CHARACTER*(SYSMXNAM)	MOSFILE, INFILE, SOURCE, SB
      INTEGER		        NDUMMY, IF
C==================================================================
      CALL MSGWELCO ('I make mosaic data from BIMA formats')
      CALL USRCTL
C
C Get input parameters
C
      CALL USRGETC('Infile', INFILE, 1, NDUMMY)
      CALL USRGETC ('Mos',MOSFILE, 1, NDUMMY)
      CALL USRGETC ('Source', SOURCE, 1, NDUMMY)
      CALL USRGETC ('Sideband', SB, 1, NDUMMY)
      CALL USRGETL('Debug', SYSDEBUG, 1, NDUMMY) 
      IF(SB.EQ.'LSB') THEN
         IF = 1
      ELSE
         IF = 2
      END IF
C
C Now get data from file
C
      CALL VISSFGET ('Mos', INFILE, SOURCE, IF)
C
C Print out some info
C
      CALL MSGPUT ('Coordinates for visibility data', 'I')
      CALL CRDLIST ('Mos/PC1/OBS/I')
      IF (ERROR) GO TO 999
C
      CALL HISOPEN ('Mos')
      WRITE (MESSAGE, 1000) INFILE
 1000 FORMAT ('Assembled from file ',A)
      CALL HISPUT ('Mos', MESSAGE)
      WRITE (MESSAGE, 1100) SB
 1100 FORMAT ('Sideband : ',A)
      CALL HISPUT ('Mos', MESSAGE)
      CALL HISLIST ('Mos')
C
      CALL DATSETTP ('Mos','VISMOSAIC')
      CALL VISMOSPU ('Mos', MOSFILE)
C
 999  CONTINUE
      END
      SUBROUTINE VISSFGET (MOS, MOSFILE, SOURCE, IF)
C
C Get visibility data from a strange format file. 
C
C
C	MOS		CH*(*)	input	NAME of directory entry
C	MOSFILE		CH*(*)	input	Name of file
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 21 1989
C
C-------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*) 	MOS, MOSFILE, SOURCE
      INTEGER		IF
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISSFGET')
C
      DOUBLE PRECISION  RA, DEC, RAT, DECT
C
      INTEGER		NAX, NAXIS(SYSMXDIM)
      CHARACTER*8	CTYPE(SYSMXDIM)
      REAL		CRPIX(SYSMXDIM), CDELT(SYSMXDIM),
     1			CROTA(SYSMXDIM)
      DOUBLE PRECISION	CRVAL(SYSMXDIM)
C
      INTEGER		NCHAR, NPC
      CHARACTER*6	STRINT
      CHARACTER*(SYSMXNAM)	STRM2, MOSS
      CHARACTER*80	LINE
      LOGICAL 		EOF
C
      INTEGER		IND, IPC, IBASE
      REAL		OFFRAT, OFFDECT, TIME, BASE(3), TIMEMIN
      DOUBLE PRECISION	OBSRA, OBSDEC
      REAL		UNANO, VNANO, AMP(2), PHASE(2)
      DOUBLE PRECISION	F(2)
C
      REAL		PI
      INTEGER		MAXNPC
      PARAMETER		(MAXNPC=100)
      REAL		OFFRA(MAXNPC), OFFDEC(MAXNPC)
      INTEGER		NVIS(MAXNPC)
      INTEGER		UADD(MAXNPC), VADD(MAXNPC), WADD(MAXNPC)
      INTEGER		TADD(MAXNPC), BADD(MAXNPC), VSADD(MAXNPC)
      INTEGER		WTADD(MAXNPC)
C
      DATA		NAXIS	/SYSMXDIM*1/
      DATA		CTYPE	/SYSMXDIM*' '/
      DATA		CRPIX	/SYSMXDIM*0.0/
      DATA		CDELT	/SYSMXDIM*0.0/
      DATA		CROTA	/SYSMXDIM*0.0/
      DATA		CRVAL	/SYSMXDIM*0.0D0/
      DATA		OFFRA	/100*0.0/
      DATA		OFFDEC	/100*0.0/
C=======================================================================
C
      IF (ERROR) GO TO 999
C
      PI = 4*ATAN(1.0)
      BASE(1) = 256.0 * 1 + 2
      BASE(2) = 256.0 * 2 + 3
      BASE(3) = 256.0 * 3 + 1
C
      NPC = 0
      TIMEMIN = 1E20
      CALL DATCREAT (MOS)
C
C Need to set NVIS, etc. => read data base first to get these numbers
C Define RA, DEC to be the first found since an object may move on the
C sky e.g. Jupiter
C
      CALL MSGPUT (' ', 'I')
      CALL MSGPUT ('Reading file to get pointing centers', 'I')
      CALL MSGPUT (' ', 'I')
      CALL TXTOPEN ('MOSFILE', MOSFILE, 'READ')
  1   CONTINUE
      CALL TXTREAD ('MOSFILE', LINE, NCHAR, EOF)
      IF (EOF) GO TO 10
      READ (LINE, 1100) IND, TIME, RAT, DECT, OFFRAT, OFFDECT, 
     1   F(1), F(2)
 1100 FORMAT (1X,I4,3(1X,F8.4),2(F5.0),2(1X,F9.5))
      TIMEMIN = MIN(TIME, TIMEMIN)
      DO 8 IPC = 1, NPC
         IF((OFFRAT.EQ.OFFRA(IPC)).AND.(OFFDECT.EQ.OFFDEC(IPC)))
     1      THEN
            GO TO 9
         END IF
  8   CONTINUE
      NPC = NPC + 1
      IPC = NPC
      IF (IPC.EQ.1) THEN
         RA = RAT
         DEC = DECT
      END IF
      WRITE (MESSAGE, 1000) IPC, OFFRAT, OFFDECT, INT(TIME)
 1000 FORMAT ('Found pointing ',I3,' at (',F5.0,',',F5.0,')',
     1   ' on day ',I3)
      CALL MSGPUT (MESSAGE, 'I')
      OFFRA(IPC) = OFFRAT
      OFFDEC(IPC) = OFFDECT
  9   CONTINUE
      NVIS(IPC) = NVIS(IPC) + 3
      CALL TXTREAD ('MOSFILE', LINE, NCHAR, EOF)
      CALL TXTREAD ('MOSFILE', LINE, NCHAR, EOF)
      CALL TXTREAD ('MOSFILE', LINE, NCHAR, EOF)
      GO TO 1
  10  CONTINUE
      CALL TXTCLOSE ('MOSFILE')
      CALL DATPUTI (MOS, 'NPC', NPC, 1)
C
      CALL DATPUTC (MOS, 'OBJECT', SOURCE, 1)
      CALL DATPUTC (MOS, 'DATE-OBS', '?', 1)
      CALL DATPUTC (MOS, 'OBSERVER', 'USON', 1)
      CALL DATPUTC (MOS, 'INSTRUME', 'BIMA', 1)
C
C Set up some parameters
C
      DO 100 IPC = 1, NPC
         MOSS = STRM2(MOS,'PC'//STRINT(IPC))
         CALL DATCREAT (MOSS)
         CALL DATCREAT (STRM2(MOSS, 'OBS'))
         CALL DATCREAT (STRM2(MOSS, 'OBS/I'))
         CALL DATPUTC (STRM2(MOSS, 'OBS/I'), 'TELESCOP', 'BIMA', 1)
         CALL DATPUTC (STRM2(MOSS, 'OBS/I'), 'INSTRUME', 'BIMA', 1)
         NAX = 3
C
C Pointing center and phase center coincide
C
         OBSRA = RA + OFFRA(IPC)/(COS(DEC*PI/180.0)*3600.0)
         OBSDEC = DEC + OFFDEC(IPC)/3600.0
         CRVAL(1) = OBSRA
         CRVAL(2) = OBSDEC
         CRVAL(3) = F(IF) * 1E9
         CTYPE(1) = 'RA'
         CTYPE(2) = 'DEC'
         CTYPE(3) = 'FREQ'
         CDELT(3) = 0.01 * CRVAL(3)
         CALL CRDPUT (STRM2(MOSS, 'OBS/I'), NAX, CTYPE, NAXIS, CRVAL, 
     1      CRPIX, CDELT, CROTA)
         CALL DATPUTD (MOSS, 'OBSRA', OBSRA, 1)
         CALL DATPUTD (MOSS, 'OBSDEC', OBSDEC, 1)
         CALL DATPUTC (MOSS, 'TELESCOP', 'BIMA', 1)
 100  CONTINUE
C
C Make the arrays to store everything in
C
      DO 110 IPC = 1, NPC
         MOSS = STRM2(MOS,'PC'//STRINT(IPC))
         NAX = 1
         NAXIS(1) = NVIS(IPC)
         NVIS(IPC) = 0
         CALL DATMAKAR (STRM2(MOSS, 'UU'), NAX, NAXIS, 'R', UADD(IPC))
         CALL DATMAKAR (STRM2(MOSS, 'VV'), NAX, NAXIS, 'R', VADD(IPC))
         CALL DATMAKAR (STRM2(MOSS, 'WW'), NAX, NAXIS, 'R', WADD(IPC))
         CALL DATMAKAR (STRM2(MOSS, 'BASELINE'), NAX, NAXIS, 'R', 
     1      BADD(IPC))
         CALL DATMAKAR (STRM2(MOSS, 'TIME'), NAX, NAXIS, 'R', 
     1      TADD(IPC))
         CALL DATMAKAR (STRM2(MOSS, 'OBS/I/VIS'), NAX, NAXIS, 'X', 
     1      VSADD(IPC))
         CALL DATMAKAR (STRM2(MOSS, 'OBS/I/WT'), NAX, NAXIS, 'R', 
     1      WTADD(IPC))
 110  CONTINUE
C
C Now read in earnest
C
      CALL MSGPUT (' ', 'I')
      CALL MSGPUT ('Reading file to get data', 'I')
      CALL MSGPUT (' ', 'I')
      CALL TXTOPEN ('MOSFILE', MOSFILE, 'READ')
 101  CONTINUE
      CALL TXTREAD ('MOSFILE', LINE, NCHAR, EOF)
      IF (EOF) GO TO 200
      READ (LINE, 1100) IND, TIME, RA, DEC, OFFRAT, OFFDECT, 
     1   F(1), F(2)
      DO 108 IPC = 1, NPC
         IF((OFFRAT.EQ.OFFRA(IPC)).AND.(OFFDECT.EQ.OFFDEC(IPC)))
     1      THEN
            GO TO 109
         END IF
 108  CONTINUE
      CALL ERRREPOR (ERRLOGIC, ROUTINE, 'Unknown pointing')
      GO TO 990
 109  CONTINUE
      DO 150 IBASE = 1, 3
         CALL TXTREAD ('MOSFILE', LINE, NCHAR, EOF)
         READ (LINE, 1200) UNANO, VNANO, AMP(1), PHASE(1), AMP(2), 
     1      PHASE(2)
 1200    FORMAT (F9.4,1X,F9.4, 2(1X,F8.3,1X,F6.3))
         NVIS(IPC) = NVIS(IPC) + 1
         MEMR(UADD(IPC)+(NVIS(IPC)-1)) = F(IF) * UNANO
         MEMR(VADD(IPC)+(NVIS(IPC)-1)) = F(IF) * VNANO
         MEMR(WADD(IPC)+(NVIS(IPC)-1)) = 0.0
         MEMR(TADD(IPC)+(NVIS(IPC)-1)) = TIME - INT(TIMEMIN)
         MEMR(WTADD(IPC)+(NVIS(IPC)-1)) = SIGN(1.0, AMP(IF))
         MEMR(BADD(IPC)+(NVIS(IPC)-1)) = BASE(IBASE)
         MEMX(VSADD(IPC)+(NVIS(IPC)-1)) = ABS(AMP(IF)) * 
     1      CMPLX (COS(PHASE(IF)), SIN(PHASE(IF)))
 150  CONTINUE
      GO TO 101
 200  CONTINUE
      CALL TXTCLOSE ('MOSFILE')
C
 990  CONTINUE
      IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
 999  CONTINUE
      END

