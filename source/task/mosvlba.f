C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)mosvlba.f	1.2    11/7/90
C
      SUBROUTINE SDEMAIN
C
CD Program to make mosaic data from VLBA data sets
C
C Audit trail:
C				R. T. Duquet	May 15 1990
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'MOSVLBA')
C
      CHARACTER*(SYSMXNAM)	MOSFILE, INFILE, SOURCE, STATN, EPOCH
      REAL			SCALE(2), PE(2), BW
      INTEGER		        NDUMMY, IF
C==================================================================
      CALL MSGWELCO ('I make mosaic data from VLBA files')
      CALL USRCTL
C
C Get input parameters
C
      CALL USRGETC ('Infile', INFILE, 1, NDUMMY)
      CALL USRGETC ('Mosaic', MOSFILE, 1, NDUMMY)
      CALL USRGETC ('Source', SOURCE, 1, NDUMMY)
      CALL USRGETC ('Station', STATN, 1, NDUMMY)
      CALL USRGETC ('Epoch', EPOCH, 1, NDUMMY)
      CALL USRGETR ('BW', BW, 1, NDUMMY)
      CALL USRGETI ('IF', IF, 1, NDUMMY)
      CALL USRGETR ('Scale', SCALE, 2, NDUMMY)
      CALL USRGETR ('Pointing', PE, 2, NDUMMY)
      CALL USRGETL ('Debug', SYSDEBUG, 1, NDUMMY)
C
C Now get data from file
C
      CALL VIS2GET ('Mos', INFILE, SOURCE, STATN, IF, SCALE, PE(1),
     $   PE(2), BW, EPOCH)
C
      CALL VISMOSPU ('Mos', MOSFILE)
C
 999  CONTINUE
      END
C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C
      SUBROUTINE VIS2GET (MOS, MOSFILE, RSOURCE, STATN, IF, SCALE,
     $   AZOFF, ELOFF, BW, EPOCH)
C
C Get total power data from a VLBA monitor file
C
C	MOS		CH*(*)	input	NAME of directory entry
C	MOSFILE		CH*(*)	input	Name of file
C       RSOURCE         CH*(*)  input   Requested Source name
C       STATN           CH*(*)  input   Station name
C       IF              INT     input   IF number
C	SCALE		REAL	input	Scaling number to get Jy
C	AZOFF		REAL	input	Azimuth pointing correction
C	ELOFF		REAL	input	Elevation pointing correction
C	BW		REAL	input	Bandwidth required 16|500
C
C Audit trail:
C				R. T. Duquet	May 15 1990
C
C-------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*) 	MOS, MOSFILE, RSOURCE, STATN, EPOCH
      REAL		SCALE(*), AZOFF, ELOFF, BW
      INTEGER		IF
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VIS2GET')
C
      INTEGER		NAX, NAXIS(SYSMXDIM)
      CHARACTER*8	CTYPE(SYSMXDIM)
      REAL		CRPIX(SYSMXDIM), CDELT(SYSMXDIM),
     1			CROTA(SYSMXDIM)
      DOUBLE PRECISION	CRVAL(SYSMXDIM)
C
      INTEGER		NCHAR, NPC, STNUM, IBW, INDEX
      CHARACTER*(SYSMXNAM)	STRM2, MOSS
      CHARACTER*300	LINE
      LOGICAL 		EOF, BADSRC
C
      INTEGER		IPC, IDUMM(3), QUAL, IND, I, PEAKQUAL
      INTEGER           DEG, HR, MIN, SEC, FRAC, NDUMMY, NBLANK
      REAL		RTEMP, BLANK(2), D2R, TPDATA(32), PEAKTP, TP
      DOUBLE PRECISION  AZ, EL, AZC, ELC, AZCNOM, ELCNOM, LAT, LON
      DOUBLE PRECISION	OBSRA, OBSDEC, MJD, TIME, DATE, TSTART, TEND

      DOUBLE PRECISION	F(2)
      CHARACTER*8	FSOURCE
C
      INTEGER		MAXNPC
      PARAMETER		(MAXNPC=1000)
      DOUBLE PRECISION	RA(MAXNPC), DEC(MAXNPC), RANOM, DECNOM, RAT,
     $			DECT, RADATE, DECDATE, SLAEPB
      REAL		PTIME(MAXNPC)
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
      DATA		PTIME	/1000*0.0/
      DATA		RA	/1000*0.0D0/
      DATA		DEC	/1000*0.0D0/
C
      REAL              LOCATN(6,11)
C                          Latitude              Longitude
      DATA  LOCATN   / 34.0, 18.0, 03.61,    108.0, 07.0, 07.24,
     1                 31.0, 57.0, 22.39,    111.0, 36.0, 42.26,          
     1                 35.0, 46.0, 30.33,    106.0, 14.0, 42.01,   
     1                 30.0, 38.0, 05.63,    103.0, 56.0, 39.13,        
     1                 41.0, 46.0, 17.03,    091.0, 34.0, 26.35,        
     1                 48.0, 07.0, 52.80,    119.0, 40.0, 55.34,        
     1                 17.0, 45.0, 30.57,    064.0, 35.0, 02.61,        
     1                 37.0, 13.0, 54.19,    118.0, 16.0, 33.98,        
     1                 19.0, 48.0, 00.00,    155.0, 28.0, 00.00,        
     1                 42.0, 56.0, 00.96,    071.0, 59.0, 11.69,         
     1                 34.0, 18.0, 03.61,    108.0, 07.0, 07.24/
C        
      CHARACTER*6	STRINT
      CHARACTER*(SYSMXNAM)	STRRMBL
      LOGICAL		STRMATCH
C
      CHARACTER*1               INITIAL(11)
      DATA INITIAL/'P','K','L','F','N','B','S','O','M','H','A'/
C
C=======================================================================
C
      IF (ERROR) GO TO 999
C
      D2R = ATAN(1.0)/45.0
C
      IF(BW.GT.20.0) THEN
         IBW = 1
      ELSE
         IBW = 2
      END IF
C
      CALL MSGPUT ('Required source is '//RSOURCE, 'I')
      CALL MSGPUT ('Required IF is '//STRINT(IF), 'I')
      CALL MSGPUT ('Required station is '//STATN, 'I')
      CALL MSGPUT ('Required epoch is '//EPOCH, 'I')
      WRITE(MESSAGE, 4000) BW
 4000 FORMAT ('Required bandwidth is ',F7.1,' MHz')
      CALL MSGPUT (MESSAGE, 'I')
      WRITE(MESSAGE, 4010) 3600.0*AZOFF, 3600.0*ELOFF
 4010 FORMAT('Input Collimation errors = ',F7.2,', ',F7.2)
      CALL MSGPUT (MESSAGE, 'I')
C
      DO 201 I = 1,11
         IF (STATN(1:1) .EQ. INITIAL(I)) THEN
            STNUM = I
            GO TO 202
         END IF
 201  CONTINUE
      CALL ERRREPOR(ERRBDARG,ROUTINE,'Unrecognizable Station Name')
      GO TO 999
 202  CONTINUE
C
      LAT = LOCATN(1,STNUM) + LOCATN(2,STNUM) / 60.0 +
     1      LOCATN(3,STNUM) / 3600.0
      LON = LOCATN(4,STNUM) + LOCATN(5,STNUM) / 60.0 +
     1      LOCATN(6,STNUM) / 3600.0
C
C Need to set NPC, etc. => read data base first to get these numbers
C
      CALL MSGPUT (' ', 'I')
      CALL MSGPUT ('Initial reading of file', 'I')
      CALL MSGPUT (' ', 'I')
      NPC = 0
      MJD = 0.0D0
      BLANK(1) = 0.0
      BLANK(2) = 0.0
      NBLANK = 0
      BADSRC = .TRUE.
      CALL TXTOPEN ('MOSFILE', MOSFILE, 'READ')
C
  1   CONTINUE
      CALL TXTREAD ('MOSFILE', LINE, NCHAR, EOF)
      IF (EOF) GO TO 10
      IF (LINE.EQ.' ') THEN
         GO TO 1
      ELSEIF (LINE(3:9).EQ.'version') THEN
         CALL TXTREAD ('MOSFILE', LINE, NCHAR, EOF)
         READ(LINE(29:36), '(A)',ERR=900) FSOURCE
         FSOURCE = STRRMBL(FSOURCE)
         IF(STRMATCH(RSOURCE,FSOURCE)) THEN
            BADSRC = .FALSE.
         ELSE
            BADSRC = .TRUE.
            CALL MSGPUT ('Found unwanted source '//FSOURCE, 'I')
         END IF
      ELSEIF (BADSRC) THEN
         GO TO 1
      ELSEIF ((LINE(1:5).EQ.'CTRL1').AND.(MJD.EQ.0.0D0)) THEN
         CALL UTLD2MJD(LINE(10:16), MJD)
         CALL MSGPUT ('Start date is '//LINE(10:16), 'I')
         DATE = SLAEPB(MJD)
         WRITE(MESSAGE,3000) DATE
 3000    FORMAT ('Bessellian date is ',F10.3)
         CALL MSGPUT (MESSAGE, 'I')
         WRITE(MESSAGE,3010) MJD
 3010    FORMAT ('Modified Julian date is ',F10.2)
         CALL MSGPUT (MESSAGE, 'I')
      ELSEIF (LINE(1:4).EQ.'MON1') THEN
         READ (LINE(25:),*,ERR=900) IDUMM(1), IDUMM(2), QUAL, AZ, EL,
     $      IDUMM(3), (TPDATA(I),I=1,16)
         IF(QUAL.EQ.0) THEN
            GO TO 1
         END IF
         IF(QUAL.EQ.10) THEN
            IND = 2 + 7*(IBW-1)
            TP = TPDATA(IND) / TPDATA(IND+4)
            BLANK(1) = BLANK(1) + TP
            TP = TPDATA(IND+2) / TPDATA(IND+6)
            BLANK(2) = BLANK(2) + TP
            NBLANK = NBLANK + 1
            GO TO 1
         END IF
         NPC = NPC + 1
         PTIME(NPC) = TIME
         CALL CRDPREC (RANOM, DECNOM, 2000.0D0, DATE+TIME, RADATE,
     $      DECDATE)
         CALL CRDRD2AE(RADATE,DECDATE,LAT,LON,MJD+TIME,AZ,EL)
         AZ = AZ + (AZC - AZCNOM + AZOFF)/COS(D2R*EL)
         EL = EL + (ELC - ELCNOM + ELOFF)
         CALL CRDAE2RD(AZ,EL,LAT,LON,MJD+TIME,RA(NPC),DEC(NPC))
         CALL CRDSIN (RADATE, DECDATE, RA(NPC), DEC(NPC), RAT, DECT)
         WRITE(MESSAGE, *) QUAL, 3600.0*SNGL(RAT-RADATE),
     $      3600.0*SNGL(DECT-DECDATE)
         CALL MSGPUT (MESSAGE, 'D')
         IF(EPOCH(1:4).EQ.'1950') THEN
            CALL CRDPREC (RA(NPC), DEC(NPC), DATE+TIME, 1950.0D0, 
     $         RA(NPC), DEC(NPC))
         ELSE
            CALL CRDPREC (RA(NPC), DEC(NPC), DATE+TIME, 2000.0D0, 
     $         RA(NPC), DEC(NPC))
         END IF
      ELSEIF(INDEX(LINE,'thistime,date').NE.0) THEN
         CALL TXTREAD ('MOSFILE', LINE, NCHAR, EOF)
         READ(LINE(6:13),1105,ERR=900) HR,MIN,SEC
 1105    FORMAT (I2,1X,I2,1X,I2)
         IF(SYSDEBUG) THEN
            WRITE (MESSAGE, 1106) HR, MIN, SEC
 1106       FORMAT ('Start of integration is ',I2,':',I2,':',I2)
            CALL MSGPUT (MESSAGE, 'I')
         END IF
         TSTART = FLOAT(HR)/24.0 + FLOAT(MIN)/(24.0*60.0) + 
     $      FLOAT(SEC) /(24.0*3600.0)
         READ(LINE(31:38),1105,ERR=900) HR,MIN,SEC
         IF(SYSDEBUG) THEN
            WRITE (MESSAGE, 1206) HR, MIN, SEC
 1206       FORMAT ('End of integration is   ',I2,':',I2,':',I2)
            CALL MSGPUT (MESSAGE, 'I')
         END IF
         TEND = FLOAT(HR)/24.0 + FLOAT(MIN)/(24.0*60.0) + 
     $      FLOAT(SEC) /(24.0*3600.0)
         TIME = 0.5*(TSTART+TEND)
      ELSEIF(INDEX(LINE,'ra,dec').NE.0) THEN
         CALL TXTREAD ('MOSFILE', LINE, NCHAR, EOF)
         READ(LINE(3:), 1107) HR, MIN, SEC, FRAC
 1107    FORMAT (I2,1X,I2,1X,I2,1X,I1)
         CALL SLACTF2D  (HR, MIN, FLOAT(SEC)+FLOAT(FRAC)/10.0, RTEMP,
     $      NDUMMY)
         RANOM = RTEMP * 360.0D0
         READ(LINE(16:), 1107) DEG, MIN, SEC, FRAC
         DECNOM = FLOAT(DEG) + FLOAT(MIN)/60.0 + FLOAT(SEC)/3600.0
     $      + FLOAT(FRAC)/36000.0
      ELSEIF(INDEX(LINE,'center frequencies').NE.0) THEN
         CALL TXTREAD ('MOSFILE', LINE, NCHAR, EOF)
         READ(LINE(1:19),1101,ERR=900) F(1), F(2)
 1101    FORMAT (F9.2,3X,F9.2)
         IF(SYSDEBUG) THEN
            WRITE(MESSAGE, 1103) F(1), F(2)
 1103       FORMAT ('Frequencies = ',F9.2,', ',F9.2,' MHz')
            CALL MSGPUT (MESSAGE, 'I')
         END IF
         F(1) = F(1) * 1E6
         F(2) = F(2) * 1E6
      ELSEIF(INDEX(LINE,'azc,elc').NE.0) THEN
         CALL TXTREAD ('MOSFILE', LINE, NCHAR, EOF)
         READ (LINE, *, ERR=900) AZ, EL, AZC, ELC
         IF((AZCNOM.EQ.0.0D0).AND.(ELCNOM.EQ.0.0D0)) THEN
            AZCNOM = AZC
            ELCNOM = ELC
            WRITE(MESSAGE, 1104) 3600.0*AZCNOM, 3600.0*ELCNOM
 1104       FORMAT('Nominal Collimation errors = ',F7.2,', ',F7.2)
            CALL MSGPUT (MESSAGE, 'I')
         ENDIF
      END IF 
      GO TO 1
C
  10  CONTINUE
      CALL TXTCLOSE ('MOSFILE')
C
      WRITE (MESSAGE, 1000) NPC
 1000 FORMAT ('Found ',I6,' distinct pointings')
      CALL MSGPUT (MESSAGE, 'I')
C
C Find blank signal
C
      IF(NBLANK.EQ.0) THEN
         CALL ERRREPOR (ERRLOGIC,  ROUTINE, 'No Off source fields')
         GO TO 999
      ENDIF
      BLANK(1) = BLANK(1) / NBLANK
      WRITE(MESSAGE,1010) BLANK(1)
 1010 FORMAT ('IF 1: Off-source = ',1PE12.3,' units')
      CALL MSGPUT (MESSAGE, 'I')
      BLANK(2) = BLANK(2) / NBLANK
      WRITE(MESSAGE,1011) BLANK(2)
 1011 FORMAT ('IF 2: Off-source = ',1PE12.3,' units')
      CALL MSGPUT (MESSAGE, 'I')
C
C Now create the output file
C
      CALL DATCREAT (MOS)
      WRITE(MESSAGE, *) 'RA, Dec (date)  = ', RADATE, DECDATE
      CALL MSGPUT (MESSAGE, 'I')
      IF(EPOCH(1:4).EQ.'1950') THEN
         CALL MSGPUT ('Putting J1950 coordinates in header', 'I')  
         WRITE(MESSAGE, *) 'RA, Dec (J2000) = ', RANOM, DECNOM
         CALL MSGPUT (MESSAGE, 'I')
         CALL CRDPREC (RANOM, DECNOM, 2000.0D0, 1950D0, RANOM,
     $      DECNOM)
         WRITE(MESSAGE, *) 'RA, Dec (J1950) = ', RANOM, DECNOM
         CALL MSGPUT (MESSAGE, 'I')
         CALL DATPUTR (MOS, 'EPOCH', 1950.0, 1)
      ELSE
         CALL MSGPUT ('Putting J2000 coordinates in header', 'I')
         CALL DATPUTR (MOS, 'EPOCH', 2000.0, 1)
      END IF
      CALL DATPUTI (MOS, 'NPC', NPC, 1)
      CALL DATPUTC (MOS, 'OBJECT', RSOURCE, 1)
      CALL DATPUTC (MOS, 'DATE-OBS', '?', 1)
      CALL DATPUTC (MOS, 'OBSERVER', 'MMA', 1)
      DO 100 IPC = 1, NPC
         MOSS = STRM2(MOS,'PC'//STRINT(IPC))
         CALL DATCREAT (MOSS)
         CALL DATPUTC (MOSS, 'TELESCOP', 'VLBA', 1)
         CALL DATPUTC (MOSS, 'INSTRUME', 'VLBA', 1)
         CALL DATCREAT (STRM2(MOSS, 'OBS'))  
         CALL DATCREAT (STRM2(MOSS, 'OBS/I'))  
         CALL DATPUTC (STRM2(MOSS, 'OBS/I'), 'TELESCOP', 'VLBA', 1)
         CALL DATPUTC (STRM2(MOSS, 'OBS/I'), 'INSTRUME', 'VLBA', 1)
         NAX = 3
         OBSRA = RA(IPC)
         OBSDEC = DEC(IPC)
         CRVAL(1) = RANOM
         CRVAL(2) = DECNOM
         CRVAL(3) = F(IF) 
         CTYPE(1) = 'RA'
         CTYPE(2) = 'DEC'
         CTYPE(3) = 'FREQ'
         CDELT(3) = 1E6 * BW
         CALL CRDPUT (STRM2(MOSS, 'OBS/I'), NAX, CTYPE, NAXIS, CRVAL,
     1                 CRPIX, CDELT, CROTA)
         CALL DATPUTD(MOSS, 'OBSRA' , OBSRA, 1)
         CALL DATPUTD(MOSS, 'OBSDEC' , OBSDEC, 1)
 100  CONTINUE
C
      DO 110 IPC = 1, NPC
         MOSS = STRM2(MOS,'PC'//STRINT(IPC))
         NAX = 1
         NAXIS(1) = 1
         CALL DATMAKAR (STRM2(MOSS,'UU'),NAX,NAXIS,'R',UADD(IPC))
         CALL DATMAKAR (STRM2(MOSS,'VV'),NAX,NAXIS,'R',VADD(IPC))
         CALL DATMAKAR (STRM2(MOSS,'WW'),NAX,NAXIS,'R',WADD(IPC))
         CALL ARRSETCO (STRM2(MOSS,'UU'),0.0, 0.0)
         CALL ARRSETCO (STRM2(MOSS,'VV'),0.0, 0.0)
         CALL ARRSETCO (STRM2(MOSS,'WW'),0.0, 0.0)
         CALL DATMAKAR (STRM2(MOSS,'BASELINE'),NAX,NAXIS,'R',BADD(IPC))
         CALL ARRSETCO (STRM2(MOSS,'BASELINE'),0.0, 257.0)
         CALL DATMAKAR (STRM2(MOSS,'TIME'),NAX,NAXIS,'R',TADD(IPC))
         CALL DATMAKAR (STRM2(MOSS,'OBS/I/VIS'),NAX,NAXIS,'X',
     $      VSADD(IPC))
         CALL DATMAKAR (STRM2(MOSS,'OBS/I/WT'),NAX,NAXIS,'R',WTADD(IPC))
         CALL ARRSETCO (STRM2(MOSS,'OBS/I/WT'),0.0, 1.0)
 110  CONTINUE
C
C Now read in earnest
C
      NPC = 0
      PEAKTP = 0.0
      CALL MSGPUT (' ', 'I')
      CALL MSGPUT ('Reading file to get data', 'I')
      CALL MSGPUT (' ', 'I')
      CALL TXTOPEN ('MOSFILE', MOSFILE, 'READ')
C
 101  CONTINUE
      CALL TXTREAD ('MOSFILE', LINE, NCHAR, EOF)
      IF (EOF) GO TO 200
      IF(LINE(1:4).EQ.'MON1') THEN
         READ (LINE(25:),*,ERR=900) IDUMM(1), IDUMM(2), QUAL, AZ, EL,
     $      IDUMM(3), (TPDATA(I),I=1,16)
         IF(QUAL.EQ.0) THEN
            GO TO 101
         END IF
         IF(QUAL.EQ.10) THEN
            GO TO 101
         END IF
         NPC = NPC + 1
         IF(IF.NE.0) THEN
            IND = 2 + 7*(IBW-1) + 2*(IF-1)
            TP = SCALE(IF)*((TPDATA(IND) / TPDATA(IND+4)) - BLANK(IF))
            MEMX(VSADD(NPC)) = CMPLX(TP, 0.0)
         ELSE
            IND = 2 + 7*(IBW-1) 
            TP = (SCALE(1)*((TPDATA(IND) / TPDATA(IND+4)) - BLANK(1))
     $         + SCALE(2)*((TPDATA(IND+2) / TPDATA(IND+6)) - BLANK(2)))
     $         / 2.0
            MEMX(VSADD(NPC)) = CMPLX(TP, 0.0)
         END IF
         IF(TP.GT.PEAKTP) THEN
            PEAKTP = TP
            PEAKQUAL = QUAL
         END IF
         MEMR(TADD(NPC)) = PTIME(NPC)
      END IF
      GO TO 101
 200  CONTINUE
      CALL TXTCLOSE ('MOSFILE')
C
      WRITE(MESSAGE,1030) PEAKTP, PEAKQUAL
 1030 FORMAT ('Peak total power (ON-OFF) = ',1PE12.3,' Jy at qual = ',
     $   I3)
      CALL MSGPUT (MESSAGE, 'I')
C
      GO TO 990
C
 900  CONTINUE
      CALL ERRREPOR(ERRBDARG, ROUTINE, 'Reading line '//LINE)
      GO TO 999
C
 990  CONTINUE
      IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
 999  CONTINUE
      END
