C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)tipsfav.f	1.1    3/26/93
C
      SUBROUTINE TIPSFAV (DB, STIME)
C
CD Calculate Structure Function and Allan Variance from Raw Data
C
C	DB	CH*(*)	inp	Tipper DataBase
C	STIME	INT(2)	inp	Start and Stop Times
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	23 Dec 1992
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	DB
      INTEGER		STIME(2)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'TIPSFAV')
C
      CHARACTER*(SYSMXNAM)	ANAME
      INTEGER	NAX, NAXIS(SYSMXDIM), LADD, VADD, SCADD,
     $   	SADD, SSADD, RADD, TADD, NAVE
      CHARACTER*1	TYPE
      REAL		SUM, AMIN, AMAX
      INTEGER		I1, I2, NRAW, IDEL, I, J, K, JUNK(9)
C
      CHARACTER*(SYSMXNAM)	STRM2, STRINT
      LOGICAL		DATEXIST
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      CALL TIPRANGE (DB, STIME, I1, I2)
      CALL DATGETAR (STRM2(DB, 'LIST'), NAX, NAXIS, TYPE, LADD)
      IF (ERROR) GOTO 990
C
      DO 500 I = I1, I2
         ANAME = STRM2(DB, STRINT(MEMI(LADD+I)))
         CALL TIPREAD (ANAME)
         CALL DATGETAR (STRM2(ANAME, 'RAWDATA'), NAX, NAXIS, TYPE, RADD)
         NRAW = NAXIS(1)
         IF (ERROR) THEN
            CALL MSGPUT ('RAWDATA missing from '//ANAME, 'E')
            GOTO 990
         ENDIF
         CALL MSGPUT ('Calculating Uncalibrated AV and SF for '//
     $      ANAME, 'I')
C
C Find extrema of raw data
C
         AMAX = MEMR (RADD)
         AMIN = MEMR (RADD)
         DO 20 K = 1, 1023
            IF (MEMR(RADD+K) .GT. AMAX) AMAX = MEMR(RADD+K)
            IF (MEMR(RADD+K) .LT. AMIN) AMIN = MEMR(RADD+K)
 20      CONTINUE
         CALL DATPUTR (ANAME, 'MIN', AMIN, 1)
         CALL DATPUTR (ANAME, 'MAX', AMAX, 1)
C
C Make Structure Function
C
         IF (DATEXIST(STRM2(ANAME, 'SF'))) THEN
            CALL DATDELET(STRM2(ANAME, 'SF'))
         ENDIF
         CALL DATCREAT (STRM2(ANAME, 'SF'))
         CALL DATMAKAR (STRM2(ANAME, 'SF/TIME'), 1, 9, 'R', TADD)
         CALL DATMAKAR (STRM2(ANAME, 'SF/SF'), 1, 9, 'R', SADD)
         CALL DATMAKAR (STRM2(ANAME, 'SF/SSF'), 1, 9, 'R', SSADD)
         DO 50 J = 0, 8
            MEMR(TADD + J) = 3.5 * 2.0**(FLOAT(J))
 50      CONTINUE
         DO 200 J = 0, 8
            NAVE = 0
            SUM = 0
            IDEL = 2.0**FLOAT(J)
            DO 100 K = 0, NRAW-IDEL-1
               NAVE = NAVE + 1
               SUM = SUM + (MEMR(RADD+K) - MEMR(RADD+K+IDEL))**2
 100        CONTINUE
            IF (NAVE .LE. 0) THEN
               CALL ERRREPOR (ERRLOGIC, ROUTINE, 'Nave = 0!?!!')
               GOTO 999
            ENDIF
            MEMR(SADD+J) = SUM / FLOAT(NAVE)
            MEMR(SSADD+J) = SQRT( SUM / FLOAT(NAVE) )
 200     CONTINUE
C
C Make Allan Variance
C
         CALL DATMAKAR (STRM2(ANAME, 'SCRATCH'), 1, NRAW, 'R', SCADD)
C
         IF (DATEXIST(STRM2(ANAME, 'AV'))) THEN
            CALL DATDELET(STRM2(ANAME, 'AV'))
         ENDIF
         CALL DATCREAT (STRM2(ANAME, 'AV'))
         CALL DATMAKAR (STRM2(ANAME, 'AV/TIME'), 1, 9, 'R', TADD)
         CALL DATMAKAR (STRM2(ANAME, 'AV/AV'), 1, 9, 'R', VADD)
         CALL DATMAKAR (STRM2(ANAME, 'AV/ASD'), 1, 9, 'R', SADD)
         DO 250 J = 0, 8
            MEMR(TADD + J) = 3.5 * 2.0**(FLOAT(J))
 250     CONTINUE
C
         CALL PIXALVAR (MEMR(RADD), MEMR(SCADD), NRAW, 3.5, MEMR(SADD),
     $      MEMR(VADD), JUNK, 9)
C
         CALL DATDELET (STRM2(ANAME, 'SCRATCH'))
         CALL DATDELET (STRM2(ANAME, 'RAWDATA'))
 500  CONTINUE
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
