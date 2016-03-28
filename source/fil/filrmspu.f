C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)filrmspu.f	1.2	 11/15/91
C
      SUBROUTINE FILRMSPU (INDIR, OUTFILE, COMMENT, BLINTER)
C
CD Writes out PHASE RMS as a function of UVdist, AVE TIME
C
C	INDIR	CHAR*(*)	in	Directory with RMS info
C	OUTFILE	CHAR*(*)	in	Output File
C	COMMENT	CHAR*(*)	in	Comment to be written      
C	BLINTER	REAL		in	BL Averaging interval
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	July 8 1991
C	Added BL averaging option, header comment
C				M.A.Holdaway	Nov 13 1991
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	INDIR, OUTFILE, COMMENT
      REAL		BLINTER
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FILRMSPU')
C
      INTEGER		NAX, NAXIS(SYSMXDIM)

      CHARACTER*1	ATYPE
      CHARACTER*(132)	LINE
      INTEGER		MADD, UVADD, TADD, NT, NUV, NTT, IT, IUV, IND,
     $   		NBL
      REAL		PHASE(30), BLAVE, BLHIGH, BL
C
      CHARACTER*(SYSMXNAM)	STRM2
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (STRM2 (INDIR, 'MAT'), NAX, NAXIS, ATYPE, MADD)
      CALL DATGETAR (STRM2 (INDIR, 'TIME'), NAX, NT, ATYPE, TADD)
      CALL DATGETAR (STRM2 (INDIR, 'UV'), NAX, NUV, ATYPE, UVADD)
      IF (ERROR) GOTO 990
      IF (NT .NE. NAXIS(1) .OR. NUV .NE. NAXIS(2)) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $      'Inconsistent array sizes for phase RMS')
         GOTO 990
      ENDIF
      CALL MSGPUT('Writing phase RMS file '//OUTFILE, 'I')
C
      CALL TXTOPEN (ROUTINE, OUTFILE, 'WRITE')
      CALL TXTWRITE (ROUTINE, COMMENT)
      LINE = 'Baseline, m                       Averaging Time, s'
      CALL TXTWRITE (ROUTINE, LINE)
      IF (NT .GT. 14) THEN
         NTT = 14
         CALL MSGPUT ('Only writing 14 averaging times', 'W')
      ELSE
         NTT = NT
      ENDIF
C
      WRITE (LINE, 1000) (MEMR(TADD+IT-1), IT=1, NTT)
 1000 FORMAT (16X, 14F8.0)
      CALL TXTWRITE (ROUTINE, LINE)
      CALL TXTWRITE (ROUTINE, ' ')
C
C  Do we need to average baseline results?
C
      IF (BLINTER .LE. 0.0) THEN
         DO 200 IUV = 1, NUV
            IND = MADD + (IUV - 1) * NT 
            WRITE (LINE, 2000) MEMR(UVADD + IUV - 1), 
     $         (MEMR(IND + IT - 1), IT=1, NTT)
            CALL TXTWRITE (ROUTINE, LINE)
 200     CONTINUE
      ELSE
         DO 250 IT = 1, NTT
            PHASE(IT) = 0.0
 250     CONTINUE
         NBL = 0
         BL = MEMR(UVADD)
         IF (BL .LT. BLINTER/2.0) THEN
            BLAVE = 0.0
         ELSE
            BLAVE = BLINTER * NINT (BL/BLINTER) 
         ENDIF
         BLHIGH = BLAVE + BLINTER / 2.0
C
         DO 500 IUV = 1, NUV
            BL = MEMR(UVADD + IUV - 1)
            IF (BL .LE. BLHIGH) THEN
               NBL = NBL + 1
               IND = MADD + (IUV - 1) * NT
               DO 300 IT = 1, NTT
                  PHASE(IT) = PHASE(IT) + MEMR(IND + IT - 1)
 300           CONTINUE
            ELSE
               IF (NBL .GT. 0) THEN
                  DO 350 IT = 1, NTT
                     PHASE(IT) = PHASE(IT) / FLOAT(NBL)
 350              CONTINUE
               ENDIF
C
               WRITE (LINE, 2000) BLAVE, (PHASE(IT), IT=1, NTT)
               CALL TXTWRITE (ROUTINE, LINE)
C
               BLAVE  = BLINTER * NINT (BL/BLINTER) 
               BLHIGH = BLAVE + BLINTER / 2.0
               IND = MADD + (IUV - 1) * NT               
               NBL = 1
               DO 450 IT = 1, NTT
                 PHASE(IT) = MEMR(IND + IT - 1)
 450          CONTINUE
           ENDIF

 500     CONTINUE
      ENDIF
C
 2000 FORMAT (F14.1, 2X, 14F8.1)
      CALL TXTCLOSE (ROUTINE)
C      
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
