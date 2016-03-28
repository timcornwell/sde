C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)amerge.f	1.1    15 Aug 1995
C
      SUBROUTINE SDEMAIN
C
CD Program to merge two ASCII data files
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A. Holdaway	14 Aug 1995
C
C
C-----------------------------------------------------------------------
C
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'AMERGE')
C
      CHARACTER*(SYSMXNAM) 	INFILE1, INFILE2, OUTFILE
      INTEGER		ISF1, ISF2, IOUT(20)
      REAL		DMAX, D1, D2
C
      INTEGER		NDUMMY, N2, I2
      INTEGER		I, NCHAR, NOUT, NMAX, ICLOSE
      LOGICAL		EOF
      PARAMETER		(NMAX = 5000)
      CHARACTER*(132)	LINE, LINE2(NMAX), CTEMP
      CHARACTER*(256)	OUTLINE
      REAL		SF2(NMAX), SF1
C==================================================================
C
C Write welcome message
C
      CALL MSGWELCO ('I merge two ASCII data files')
C
C Call user interface routine, and set debug status
C
      CALL USRCTL
      CALL USRGETC ('In1', INFILE1, 1, NDUMMY)
      CALL USRGETC ('In2', INFILE2, 1, NDUMMY)
      CALL USRGETC ('Outfile', OUTFILE, 1, NDUMMY)
      CALL USRGETI ('i1', ISF1, 1, NDUMMY)
      CALL USRGETI ('i2', ISF2, 1, NDUMMY)
      CALL USRGETI ('outfields', IOUT, 20, NDUMMY)
      CALL USRGETR ('dmax', DMAX, 1, NDUMMY)
      CALL USRGETR ('d1', D1, 1, NDUMMY)
      CALL USRGETR ('d2', D2, 1, NDUMMY)
C
      NOUT = 20
      DO 37 I = 20, 1, -1
         IF (IOUT(I) .EQ. 0)   NOUT = I-1
 37   CONTINUE
C
C Read in second file first
C
      CALL TXTOPEN ('In2', INFILE2, 'READ')
      IF (ERROR) GOTO 999
      I2 = 0
 1    CONTINUE
         CALL TXTREAD('In2', LINE, NCHAR, EOF)
         IF (EOF) GOTO 10
         IF (LINE(1:1) .EQ. '#' .OR.
     $        LINE(1:1) .EQ. '*' .OR.
     $        LINE(1:1) .EQ. '!' ) GOTO 1
         I2 = I2 + 1
         IF (I2 .GT. NMAX) THEN
            CALL ERRREPOR (ERRBDARG, ROUTINE, 'Increase NMAX')
            GOTO 999
         ENDIF
         LINE2(I2) = LINE
         CALL GETITH (LINE2(I2), ISF2, CTEMP)
         READ (CTEMP, *) SF2(I2)
         GOTO 1
 10   CONTINUE
      CALL TXTCLOSE('In2')
      N2 = I2
C
C Read in first file, traverse the second file looking
C for appropriate output
C
      CALL TXTOPEN ('In1', INFILE1, 'READ')
      CALL TXTOPEN ('Out', OUTFILE, 'WRITE')
 20   CONTINUE
         CALL TXTREAD('In1', LINE, NCHAR, EOF)
         IF (EOF) GOTO 40
C  Just output commented lines, don't try to process.
         IF (LINE(1:1) .EQ. '#' .OR.
     $        LINE(1:1) .EQ. '*' .OR.
     $        LINE(1:1) .EQ. '!' ) THEN
            CALL TXTWRITE ('Out', LINE)
            GOTO 20
         ENDIF
C
         CALL GETITH (LINE, ISF1, CTEMP)
         READ (CTEMP, *) SF1
         CALL GETCLOSE (SF2, N2, SF1, DMAX, D1, D2, ICLOSE)
         CALL MAKEOUT (LINE, LINE2(ICLOSE), IOUT, NOUT, ICLOSE, OUTLINE)
         CALL TXTWRITE ('Out', OUTLINE)
         GOTO 20
 40   CONTINUE
      CALL TXTCLOSE ('Out')
      CALL TXTCLOSE ('In1')
      CALL MSGPUT ('Output merged file is in '//OUTFILE, 'I')
C
 999  CONTINUE
C     
      END
C
      SUBROUTINE GETITH (LINE, IT, COUT)
C     	LINE	CH*(*)	input	String of items separated by white space
C	IT	INT	input	We want to get just the Ith item out
C	COUT	CH*(*)	output	Output String
C
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'GETITH')
      CHARACTER*(*)	LINE, COUT
      INTEGER		IT, I, STRLEN, N, JT, I1, I2
      LOGICAL		INBLANK
C==========================================================================
      N = STRLEN ( LINE )
      INBLANK = .TRUE.
      JT = 0
      I1 = 0
      I2 = 0
C
      DO 10 I = 1, N
         IF (LINE(I:I) .NE. ' ' .AND. INBLANK) THEN
            INBLANK = .FALSE.
            JT = JT + 1
            IF (JT .EQ. IT) THEN
               I1 = I
               I2 = N
            ENDIF
         ELSEIF (LINE(I:I) .EQ. ' ' .AND. .NOT. INBLANK) THEN
            INBLANK = .TRUE.
            IF (JT .EQ. IT) THEN
               I2 = I
               GOTO 200
            ENDIF
         ENDIF
 10   CONTINUE
 200  CONTINUE
      COUT = LINE(I1:I2)
      END


      SUBROUTINE GETCLOSE (SF2, N2, SF1, DMAX, D1, D2, ICLOSE)
C
C	Find the index ICLOSE of SF2 such that 
C	    |(SF2(ICLOSE)+D2) - (SF1+D1)| is minimized.
C	    If this difference is greater than DMAX, return ICLOSE = -1
C	ICLOSE initial value is used as a guess, assuming SF2 is increasing
C	
C	
C
C	SF2	R(*)	in	Array of [times]
C	N2	INT	in	Dim of SF2
C	SF1	R	in	A single time to be compared against SF2
C	DMAX	R	in	Maximum allowed difference
C	D1	R	in	Delta added to SF1
C	D2	R	in	Delta added to SF2
C	ICLOSE	INT	in/out	Index of SF2 such that SF2(ICLOSE) is closest to SF1
C
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'GETCLOSE')
      REAL	SF2(*), SF1, DMAX, D1, D2
      INTEGER	ICLOSE, I, N2, IEND, IDIR, ISTART
      REAL	DELTCLOS, DELTA
C==========================================================================
      IF (ICLOSE .EQ. 0) ICLOSE = 1
      ISTART = ICLOSE
      DELTA = (SF2(ICLOSE) + D2) - (SF1 + D1)
      DELTCLOS = ABS(DELTA)
C
      IF (DELTA .EQ. 0) THEN
         GOTO 200
      ELSEIF (DELTA .LT. 0) THEN
         IDIR = 1
         IEND = N2
      ELSE IF (DELTA .GT. 0) THEN
         IDIR = -1
         IEND = 1
      ENDIF
C
C Iterate until DELTA starts to increase
C
      DO 10 I = ISTART, IEND
         DELTA = ABS((SF2(I) + D2) - (SF1 + D1))
         IF (DELTA .LT. DELTCLOS) THEN
            DELTCLOS = DELTA
            ICLOSE = I
         ELSE IF (DELTA .GT. DELTCLOS) THEN
            GOTO 200
         ENDIF
 10   CONTINUE
C
 200  CONTINUE
      IF (DELTCLOS .GT. DMAX) THEN
         ICLOSE = -1
      ENDIF
C
      END       
      SUBROUTINE MAKEOUT (LINE1, LINE2, IOUT, NOUT, ICLOSE, OUTLINE)
C
C     Make output line from input line.
C     If (ICLOSE == -1), fill the items with -1.0
C
C
C     	LINE1	CH*(*)	input	First part of OUTLINE
C	LINE2	CH*(*)	input	Second part of OUTLINE, consisting of
C				blank separated items, some of which we take
C	IOUT	INT(*)	input	Which items of LINE2 go out?
C	NOUT	INT	input	number of items
C	OUTLINE	CH*(*)	out	Output Line = LINE1 + LINE2_items separated by '  '
C
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'MAKEOUT')
      CHARACTER*(*)	LINE1, LINE2, OUTLINE
      CHARACTER*132	TEMP1, TEMP2, ITEM
      INTEGER		IOUT(*), NOUT, I, STRLEN, ICLOSE
C==========================================================================
C
      TEMP1 = ' '
      TEMP2 = ' '
      IF (ICLOSE .LE. 0) THEN
         DO 10 I = 1, NOUT
            TEMP2 = TEMP1(1:STRLEN(TEMP1))//'  -1.0'
            TEMP1 = TEMP2
 10      CONTINUE
      ELSE
         DO 20 I = 1, NOUT
            CALL GETITH (LINE2, IOUT(I), ITEM)
            TEMP2 = TEMP1(1:STRLEN(TEMP1))//'  '//ITEM(1:STRLEN(ITEM))
            TEMP1 = TEMP2
 20      CONTINUE
      ENDIF
      OUTLINE = LINE1(1:STRLEN(LINE1))//'  '//TEMP1(1:STRLEN(TEMP1))
C
      END

