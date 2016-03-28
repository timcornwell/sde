C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)tskintha.f	1.5	 8/17/92
C
      INTEGER FUNCTION TSKINT10 (SIG, CODE, SCP)
C
C Interrupt Handler
C
C
C	SIG	INT	input	Signal number
C	CODE	INT	input	Signal sub-code
C	SCP	INT(5)	input	Signal Context
C Audit trail:
C-------------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		SIG, CODE, SCP(5)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'TSKINTHA')
C
      INTEGER		EXITSTAT
      DATA		EXITSTAT	/1/
C=========================================================================
      TSKINT10 = 0
C
C SIGSEGV
C
      CALL MSGPUT ('Segmentation error: Emergency stop', 'E')
      CALL MSGPUT ('This usually means a Bad software problem', 'E')
      CALL DAIEXIT
      CALL EXIT (EXITSTAT)
C
      END
C
      INTEGER FUNCTION TSKINT3 (SIG, CODE, SCP)
C
C Interrupt Handler
C
C
C	SIG	INT	input	Signal number
C	CODE	INT	input	Signal sub-code
C	SCP	INT(5)	input	Signal Context
C Audit trail:
C-------------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		SIG, CODE, SCP(5)
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'TSKINTHA')
C
C=========================================================================
      TSKINT3 = 0
C
C SIGQUIT
C
      SYSINTRP = .TRUE.
      SYSINTAC = 'QUIT'
      CALL MSGPUT ('Will quit as soon as possible', 'I')
C
      END
C
      INTEGER FUNCTION TSKINT2 (SIG, CODE, SCP)
C
C Interrupt Handler
C
C
C	SIG	INT	input	Signal number
C	CODE	INT	input	Signal sub-code
C	SCP	INT(5)	input	Signal Context
C Audit trail:
C-------------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		SIG, CODE, SCP(5)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'TSKINTHA')
C
      CHARACTER*8	ACTION
      INTEGER		EXITSTAT
      DATA		EXITSTAT	/1/
C=========================================================================
      TSKINT2 = 0
C
C SIGINT
C
      CALL MSGPUT (
     1   'Interrupt Handler: Abort, Quit, Inputs, '//
     2   'Continue, Debug, Trace?', 'I')
      READ (5,'(A)') ACTION
C
      IF (ACTION(1:1).EQ.'A') THEN
         CALL MSGPUT ('Emergency Stop', 'I')
         CALL DAIEXIT
         CALL EXIT (EXITSTAT)
      ELSE IF (ACTION(1:1).EQ.'Q') THEN
         SYSINTRP = .TRUE.
         SYSINTAC = 'QUIT'
         CALL MSGPUT ('Will quit as soon as possible', 'I')
      ELSE IF (ACTION(1:1).EQ.'I') THEN
         SYSINTRP = .TRUE.
         SYSINTAC = 'INPUTS'
         CALL MSGPUT ('Revise inputs:', 'I')
         CALL USRCTL
      ELSE IF (ACTION(1:1).EQ.'D') THEN
         SYSDEBUG = .NOT.SYSDEBUG
         IF (SYSDEBUG) THEN
            CALL MSGPUT ('Debug ON', 'I')
         ELSE
            CALL MSGPUT ('Debug OFF', 'I')
         END IF
      ELSE IF (ACTION(1:1).EQ.'C') THEN
         CALL MSGPUT ('Continuing', 'I')
      ELSE IF (ACTION(1:1).EQ.'T') THEN
         CALL MSGPUT ('Will cause deliberate abort', 'I')
         CALL ERRREPOR (ERRLOGIC, ROUTINE, 'Deliberate abort')
      ELSE 
         CALL MSGPUT ('Continuing', 'I')
      END IF
C
      END

