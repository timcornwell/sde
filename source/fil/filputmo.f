C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)filputmo.f	1.4    11/21/94
C
      SUBROUTINE FILPUTMO (MODEL, FILENAME)
C
CD Write a model from a directory entry to a text file
C
C	MODE	CH*(*)	input	Directory entry of model
C       FILENAME CH*(*) input   Filename to create
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Added time variable models
C				D.S.Briggs	Nov 19 1994
C-----------------------------------------------------------------------
#include		"stdinc.h"
C
      CHARACTER*(*)	MODEL, FILENAME
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FILPUTMO')
C
      CHARACTER*(SYSMXNAM) TYPE(SYSMXDIM)
      INTEGER FLADD, RAADD, DECADD, BMAJADD, BMINADD, BPAADD, TADD,
     $   VADD, VPADD, NAX, NAXIS(SYSMXDIM), I, NCOMP
      LOGICAL DOTIMEV
C
      CHARACTER*(SYSMXNAM) STRM2
      INTEGER DATADD, STRLEN
      LOGICAL DATEXIST
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
C A message for the user
C
      MESSAGE = 'Writing model file: '//FILENAME(1:STRLEN(FILENAME))
      CALL MSGPUT(MESSAGE, 'I')
C
C Open the output file
C
      CALL TXTOPEN('OUTPUT', FILENAME, 'WRITE')
C
C Find out how many components in the model file
C
      CALL DATGETAR(STRM2(MODEL, 'FLUX'), NAX, NAXIS, TYPE, FLADD)
      NCOMP = NAXIS(1)
C
C Now get the addresses for all the arrays
C
      RAADD = DATADD(STRM2(MODEL, 'RA'))
      DECADD = DATADD(STRM2(MODEL, 'DEC'))
      BMAJADD = DATADD(STRM2(MODEL, 'BMAJ'))
      BMINADD = DATADD(STRM2(MODEL, 'BMIN'))
      BPAADD = DATADD(STRM2(MODEL, 'BPA'))
      TADD = DATADD(STRM2(MODEL, 'TYPE'))
      DOTIMEV = .FALSE.
      IF (DATEXIST(STRM2(MODEL,'VEL'))) THEN
         DOTIMEV = .TRUE.
         VADD = DATADD(STRM2(MODEL, 'VEL'))
         VPADD = DATADD(STRM2(MODEL, 'VPA'))
      END IF
C
C Assemble a string for each component and write it
C
      DO I = 0, NCOMP - 1
         IF (DOTIMEV) THEN
            WRITE(STRBUF, 100) MEMR(FLADD +I), MEMR(RAADD + I), 
     $         MEMR(DECADD + I), MEMR(BMAJADD + I), MEMR(BMINADD + I),
     $         MEMR(BPAADD + I), MEMC(TADD + I),
     $         MEMR(VPADD + I), MEMR(VPADD + i)
 100        FORMAT(6(G11.5, ','), '''', A4,'''',',',G11.5,',',G11.5)
         ELSE
            WRITE(STRBUF, 110) MEMR(FLADD +I), MEMR(RAADD + I), 
     $         MEMR(DECADD + I), MEMR(BMAJADD + I), MEMR(BMINADD + I),
     $         MEMR(BPAADD + I), MEMC(TADD + I)
 110        FORMAT(6(G11.5, ','), '''', A4,'''')
         END IF
         CALL TXTWRITE('OUTPUT', STRBUF)
      END DO
C
C Close the file
C
      CALL TXTCLOSE('OUTPUT')
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
