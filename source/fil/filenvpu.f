C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)filenvpu.f	1.1    6/9/93
C
C Audit trail:
      SUBROUTINE FILENVPU (ENV, ENVFILE)
C
CD Write parameters to Gaussfit style Environment file.
C
C	ENV	CH*(*)	input	Name of ENV database entry
C	ENVFILE	CH*(*)	input	Name of environment file.
C
C	ENV/DATAn	CH*(*)	input	Name of data file(s) n = 1 .. 99
C	ENV/PARAMn	CH*(*)	input	Name of parm file n = 'S' or 1 .. 10
C	ENV/RESULTS	CH*(*)	input	Name of results file
C	ENV/FAIR	REAL	input	Fair reduction parm
C	ENV/HUBER	REAL	input	Huber reduction parm
C	ENV/TUKEY	REAL	input	Tukey reduction parm
C	ENV/MINSUM	LOG	input	Minsum method?
C	ENV/IRLS	LOG	input	Iteratively reweighted LS?
C	ENV/ORM		LOG	input	Orthogonal reduction method?
C	ENV/DOUBLE	LOG	input	Double iter. of eqns of condition?
C	ENV/TRIANG	LOG	input	Orthogonal reduction method?
C	ENV/LAMBDA	REAL	input	Marq-Levenberg parm
C	ENV/FACTOR	REAL	input	Marq-Levenberg parm
C	ENV/ITERS	INT	input	Iteration limit
C	ENV/TOL		REAL	input	Tolerance for stopping
C	ENV/PRMAT	LOG	input	Print condition matrix?
C	ENV/PRVAR	LOG	input	Print covariance mat?
C	ENV/SCALE	REAL	input	Current scale factor
C	ENV/SIGMA1	REAL	input	Current sigma factor
C
C	This version of Gauusfit, 2.56, uses a brain damaged subset of
C	a FITS header to control its execution.  Here, we just read
C	the value into such a file.  I rather suspect that keeping up with
C	format of this file might entail shooting at a moving target.
C
C Audit trail:
C	Original version:
C				D.S.Briggs	April 15 1992
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FILENVPU')
C
      CHARACTER*(*)	ENV, ENVFILE
C
      CHARACTER*10	NAME
      INTEGER		I
C
      CHARACTER		STRINT*5
      LOGICAL		DATEXIST
C==================================================================
      IF (ERROR) GO TO 999
C
      IF (.NOT.DATEXIST(ENV)) THEN
         CALL ERRREPOR(ERRFATAL, ROUTINE,
     $      'Environment does not exist !')
         GO TO 999
      END IF
C
C In all of these subroutine calls, the last parameter governs the behavior
C when the name is not found in the database.  -1 = error.  0 = take the
C passed default instead.  1 = don't write anything to the file.
C
      CALL FILDEL (ENVFILE)
      CALL TXTOPEN ('EnvFile', ENVFILE, 'WRITE')
C         
      CALL WENVSTR ('EnvFile', ENV, 'RESULTS', ' ', -1)
      CALL WENVSTR ('EnvFile', ENV, 'PARAMS', ' ', 1)
      DO 100 I = 1, 10
         NAME = 'PARAM' // STRINT(I)
         CALL WENVSTR ('EnvFile', ENV, NAME, ' ', 1)
 100  CONTINUE
      DO 110 I = 1, 99
         NAME = 'DATA' // STRINT(I)
         CALL WENVSTR ('EnvFile', ENV, NAME, ' ', 1)
 110  CONTINUE
      CALL WENVREAL ('EnvFile', ENV, 'FAIR', 0.0, 1)
      CALL WENVREAL ('EnvFile', ENV, 'HUBER', 0.0, 1)
      CALL WENVREAL ('EnvFile', ENV, 'TUKEY', 0.0, 1)
      CALL WENVINT ('EnvFile', ENV, 'MINSUM', 0, 1)
      CALL WENVINT ('EnvFile', ENV, 'IRLS', 0, 0)
      CALL WENVINT ('EnvFile', ENV, 'ORM', 1, 0)
      CALL WENVINT ('EnvFile', ENV, 'DOUBLE', 0, 1)
      CALL WENVINT ('EnvFile', ENV, 'TRIANG', 1, 0)
      CALL WENVREAL ('EnvFile', ENV, 'LAMBDA', 0.0, 1)
      CALL WENVREAL ('EnvFile', ENV, 'FACTOR', 0.0, 1)
      CALL WENVINT ('EnvFile', ENV, 'ITERS', 10, 0)
      CALL WENVREAL ('EnvFile', ENV, 'TOL', 1.e-5, 0)
      CALL WENVINT ('EnvFile', ENV, 'PRMAT', 0, 0)
      CALL WENVINT ('EnvFile', ENV, 'PRVAR', 0, 0)
      CALL WENVREAL ('EnvFile', ENV, 'SCALE', 0.0, 1)
      CALL WENVREAL ('EnvFile', ENV, 'SIGMA1', 0.0, 1)
C
      CALL TXTWRITE ('EnvFile', 'END')
      CALL TXTCLOSE ('EnvFile')
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
C
C Some evil local subroutines
C
      SUBROUTINE WENVSTR (HANDLE, ENV, NAME, DEF, FLAG)
#include	"stdinc.h"
      CHARACTER*(*) HANDLE, ENV, NAME, DEF
      INTEGER FLAG
C
      INTEGER L, DUMMY, STRLEN
      CHARACTER*(SYSMXNAM) STRM2, VALUE
      LOGICAL DATEXIST
C
      IF (ERROR) GO TO 999
      IF (DATEXIST(STRM2(ENV,NAME))) THEN
         CALL DATGETC(ENV, NAME, VALUE, 1, DUMMY)
      ELSE
         IF (FLAG.GT.0) THEN
            GO TO 999
         ELSE IF (FLAG.EQ.0) THEN
            VALUE = DEF
         ELSE
            MESSAGE = 'Cannot find environment entry ' // NAME
            CALL ERRREPOR (ERRNTFND, 'WENVSTR', MESSAGE)
            GO TO 999
         END IF
      END IF
C
      L = STRLEN(VALUE)
      IF (L.GT.20) THEN
         CALL ERRREPOR (ERRBDARG, 'WENVSTR', 'String ''' //
     $      VALUE(1:L) // ''' is too long !')
      ELSE
         MESSAGE = ' '
         CALL STRLC (NAME, MESSAGE)
         WRITE (MESSAGE(9:), 1000) VALUE(1:L)
         CALL TXTWRITE (HANDLE, MESSAGE)
      END IF
 1000 FORMAT (' =  ''',A,''' ')
 999  CONTINUE
      END
C
      SUBROUTINE WENVREAL (HANDLE, ENV, NAME, DEF, FLAG)
#include	"stdinc.h"
      CHARACTER*(*) HANDLE, ENV, NAME
      REAL DEF
      INTEGER FLAG
C
      REAL R, DATFGETR
      CHARACTER*(SYSMXNAM) STRM2
      LOGICAL DATEXIST
C
      IF (ERROR) GO TO 999
      IF (DATEXIST(STRM2(ENV,NAME))) THEN
         R = DATFGETR (ENV, NAME)
      ELSE
         IF (FLAG.GT.0) GO TO 999
         R = DEF
      END IF
C
      MESSAGE = ' '
      CALL STRLC (NAME, MESSAGE)
      IF ((ABS(R).LT.1000).AND.(ABS(R).GT.1E-5)) THEN
         WRITE (MESSAGE(9:), 1000) R
 1000    FORMAT (' =  ',F10.6)
      ELSE
         WRITE (MESSAGE(9:), 1010) R
 1010    FORMAT (' =  ',1PE14.6)
      END IF
      CALL TXTWRITE (HANDLE, MESSAGE)
 999  CONTINUE
      END
C
      SUBROUTINE WENVINT (HANDLE, ENV, NAME, DEF, FLAG)
#include	"stdinc.h"
      CHARACTER*(*) HANDLE, ENV, NAME
      INTEGER DEF, FLAG
C
      INTEGER I, DATFGETI
      CHARACTER*(SYSMXNAM) STRM2
      LOGICAL DATEXIST
C
      IF (ERROR) GO TO 999
      IF (DATEXIST(STRM2(ENV,NAME))) THEN
         I = DATFGETI (ENV, NAME)
      ELSE
         IF (FLAG.GT.0) GO TO 999
         I = DEF
      END IF
C
      MESSAGE = ' '
      CALL STRLC (NAME, MESSAGE)
      WRITE (MESSAGE(9:), 1000) I
 1000 FORMAT (' =  ',I4)
      CALL TXTWRITE (HANDLE, MESSAGE)
 999  CONTINUE
      END
