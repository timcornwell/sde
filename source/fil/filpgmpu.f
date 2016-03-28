C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)filpgmpu.f	1.1    12/11/92
C
      SUBROUTINE FILPGMPU (NAME, FILENAME)
C
CD Put image file in psuedo "PGM" format.
C
C	NAME	 CH*(*)	input	NAME of file as specified to user
C	FILENAME CH*(*)	input	File name
C
C This is a very stupid format, and isn't completely faithful to the
C original definition.  (Real PGM files are integer pixel values.  We
C extend it here to include real values as well.)  All coordinate
C information is lost, so keep a copy of the original image around so
C so that any later modified pixels values can be read back in via the
C task 'imgmake'
C
C If you wish to output a PGM file for the purposes of display, rather
C than editing, look into the pbm+ program fitstogif.  (There may be an
C SDE local version of this, as well.)
C
C Audit trail:
C	Original version
C				D.S.Briggs	Oct 29 1992
C-------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*) 	NAME, FILENAME
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FILPGMPU')
C
      CHARACTER		ATYPE*1, TFORM*(SYSMXNAM)
      INTEGER		NAX, NAXIS(SYSMXDIM), ADD, NDUMMY, RNAX
      REAL		ARRMAX
C
      INTEGER		CRDRNAX, STRLEN
      LOGICAL		DATEXIST
      CHARACTER*(SYSMXNAM)	STRM2, STRINT
C==================================================================
      IF (ERROR) GO TO 999
C
      TFORM = '(1PE11.4)'
      IF (DATEXIST(STRM2(NAME,'TFORM'))) THEN
         CALL DATGETC (NAME, 'TFORM', STRBUF, 1, NDUMMY)
         TFORM = '(1P' // STRBUF(1:STRLEN(STRBUF)) // ')'
      END IF
C
      CALL DATGETAR (NAME, NAX, NAXIS, ATYPE, ADD)
      RNAX = CRDRNAX(NAX, NAXIS)
C
      IF (RNAX.GT.2) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $      'Only 1D or 2D images supported')
         GO TO 999
      END IF
C
      IF (ATYPE.NE.'R') THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $      'Only real images supported')
         GO TO 999
      END IF
C
      MESSAGE = 'Opening PGM file ' // FILENAME(1:STRLEN(FILENAME)) //
     $   ' for WRITE as ' // NAME
      CALL MSGPUT (MESSAGE, 'I')
      CALL FILDEL (FILENAME)
      CALL TXTOPEN ('PGM', FILENAME, 'WRITE')
      CALL TXTWRITE ('PGM', 'P2')
      IF (RNAX.EQ.1) THEN
         STRBUF = STRINT(NAXIS(1))
         MESSAGE = STRBUF(1:STRLEN(STRBUF)) // ' 1'
      ELSE
         STRBUF = STRINT(NAXIS(1))
         MESSAGE = STRBUF(1:STRLEN(STRBUF)) // ' ' //
     $      STRINT(NAXIS(2))
      END IF
      CALL TXTWRITE ('PGM', MESSAGE)
C
      CALL ARRSTAT (NAME, ' ')
      CALL DATGETR (NAME, 'ARRMAX', ARRMAX, 1, NDUMMY)
      IF (INDEX(TFORM,'I').GT.0) THEN
         WRITE (MESSAGE, TFORM, ERR=100) NINT(ARRMAX)
      ELSE
         WRITE (MESSAGE, TFORM, ERR=100) ARRMAX
      END IF
      CALL TXTWRITE ('PGM', MESSAGE)
      GO TO 200
C
 100  CONTINUE
      CALL ERRREPOR (ERRFATAL, ROUTINE, 'Error in write. TFORM = '
     $   // TFORM)
      GO TO 999
C
 200  CONTINUE
      IF (RNAX.EQ.1) THEN
         CALL FILPPG1D ('PGM', MEMR(ADD), NAXIS(1), TFORM)
      ELSE
         CALL FILPPG2D ('PGM', MEMR(ADD), NAXIS(1), NAXIS(2), TFORM)
      END IF
C
      CALL TXTCLOSE('PGM')
C
      IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
 999  CONTINUE
      END
C
      SUBROUTINE FILPPG1D (HANDLE, A, N1, TFORM)
C
C	HANDLE	 CH*(*)	input	Handle for PGM
C	A	 R(*)	input	array 
C	N1	 INT	input	Size of array
C	TFORM	 CH*(*)	input	format defintion for text output
C
C Audit trail:
C	Original version
C				D.S.Briggs	Oct 29 1992
C-------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*) 	HANDLE, TFORM
      INTEGER		N1
      REAL		A(N1)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FILPPGPU')
C
      CHARACTER		BUFFER*30
      INTEGER		I, L, S, I1
      LOGICAL		DOINT
C
      INTEGER		STRLEN
C==================================================================
      IF (ERROR) GO TO 999
C
      DOINT = (INDEX(TFORM,'I').GT.0)
      STRBUF = ' '
      S = 0
C
      DO 500 I = 1, N1
         IF (DOINT) THEN
            WRITE (BUFFER, TFORM) NINT(A(I))
         ELSE
            WRITE (BUFFER, TFORM) A(I)
         END IF
         L = STRLEN (BUFFER)
         
         IF ((S+L).GT.72) THEN
            WRITE (STRBUF(74:),1000) I1
 1000       FORMAT('# ',I4)
            CALL TXTWRITE(HANDLE, STRBUF)
            STRBUF = BUFFER
            S = L + 1
            I1 = I
         ELSE
            IF (S.EQ.0) I1 = I
            MESSAGE = STRBUF
            STRBUF = MESSAGE(1:S) // BUFFER
            S = S + L + 1
         END IF
 500  CONTINUE
      IF (S.GT.0) THEN
         WRITE (STRBUF(74:),1000) I1
         CALL TXTWRITE(HANDLE, STRBUF)
      END IF
C
      IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
 999  CONTINUE
      END
C
      SUBROUTINE FILPPG2D (HANDLE, A, N1, N2, TFORM)
C
C	HANDLE	 CH*(*)	input	Handle for PGM
C	A	 R(*)	input	array value
C	N1,N2	 INT	input	Size of array A
C	TFORM	 CH*(*)	input	format defintion for text output
C
C Audit trail:
C	Original version
C				D.S.Briggs	Oct 29 1992
C-------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*) 	HANDLE, TFORM
      INTEGER		N1, N2
      REAL		A(N1,N2)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FILPPG2D')
C
      CHARACTER		BUFFER*30
      INTEGER		I, J, L, S, I1, J1
      LOGICAL		DOINT
C
      INTEGER		STRLEN
C==================================================================
      IF (ERROR) GO TO 999
C
      DOINT = (INDEX(TFORM,'I').GT.0)
      STRBUF = ' '
      S = 0
C
      DO 510 J = N2, 1, -1
         DO 500 I = 1, N1
            IF (DOINT) THEN
               WRITE (BUFFER, TFORM) NINT(A(I,J))
            ELSE
               WRITE (BUFFER, TFORM) A(I,J)
            END IF
            L = STRLEN (BUFFER)
            
            IF ((S+L).GT.67) THEN
               WRITE (STRBUF(69:),1000) I1,J1
 1000          FORMAT('# ',I4,',',I4)
               CALL TXTWRITE(HANDLE, STRBUF)
               STRBUF = BUFFER
               S = L + 1
               I1 = I
               J1 = J
            ELSE
               IF (S.EQ.0) THEN
                  I1 = I
                  J1 = J
               END IF
               MESSAGE = STRBUF
               STRBUF = MESSAGE(1:S) // BUFFER
               S = S + L + 1
            END IF
 500     CONTINUE
         IF (S.GT.0) THEN
            WRITE (STRBUF(69:),1000) I1,J1
            CALL TXTWRITE(HANDLE, STRBUF)
            STRBUF = ' '
            S = 0
         END IF
 510  CONTINUE
      IF (S.GT.0) THEN
         WRITE (STRBUF(69:),1000) I1,J1
         CALL TXTWRITE(HANDLE, STRBUF)
      END IF
C
      IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
 999  CONTINUE
      END
