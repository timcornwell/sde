C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C	(C)opyright 1989 by Associated Universities, Inc.; all rights reserved
C
C @(#)stdinc.h	1.13 24 Feb 1995
C
C Standard include file for all SDE subroutines. This contains the
C few global variables used in SDE routines.
C
C Audit trail:
C	Cleaned up by removing numeric variables
C				T.J. Cornwell	Feb 26 1989
C	Added IMPLICIT UNDEFINED switch for IBM
C				D.S.Briggs	Mar 20 1992
C	Added horrible ugly hack to get the SGI character alignment
C	working.  This should really be done better, but it is not
C	really clear how.
C				D.S.Briggs	May 26 1994
C	Continuing the recent tradition of ugly hacks, we have to
C	deal with the namespace collision between STRLEN and the C library
C	function of the same name.  We redefine it here on the fortran
C	side for the systems where C & FORTRAN share the same namespace.
C	(Since this will break lowercase strlen, we would rather not do it
C	universally.)
C				D.S.Briggs	June 1 1994
C	It was a bad move to make the replacement string longer than
C	the original for strlen.  Causes a few >72 character problems.
C	So we try again with the tortured replacement name SDESTL
C				D.S.Briggs	June 30 1994
C	SDELOG now is 64 characters long instead of 32.
C				D.S.Briggs	Sept 3 1994
C	Added "IMPLICIT UNDEFINED (A-Z)" for the sgi (COMP_SGI).
C				M. Stupar	Dec 28, 1994
C	Changed Dec 28 mod to be "IMPLICIT NONE" due to sgi/R8000; this
C	is ok on any of the sgis, so leave it that way for all of them.
C				M. Stupar	Mar 13, 1995
C       Added LINUX
C                               T. Cornwell     May 22, 1997
C-----------------------------------------------------------------------
C
C All variables must be declared:
C -------------------------------
C
C Prefer to put this here so that it can be switched off if required.
C
#if COMP_CONVEX || COMP_ALPHA || COMP_SGI || COMP_GNU
      IMPLICIT 		NONE
#endif
#if COMP_SUN || COMP_IBM
      IMPLICIT 		UNDEFINED (A-Z)
#endif
C
C Global logical variables:
C -------------------------
C
C ERROR: 		Error status T if an error has occured
C SYSDEBUG: 		Do we want to debug?
C SYSMON: 		Not used
C SYSINTRP:		Has an interrupt from the user occurred?
C SYSMSG: 		Is the message system on?
C SYSPAR: 		Go parallel?
C
      LOGICAL ERROR, SYSDEBUG, SYSMON, SYSINTRP, SYSMSG, SYSPAR
      INTEGER MSGWIDTH
      COMMON /SYSI/ ERROR, MSGWIDTH, SYSDEBUG, SYSMON,
     1   SYSINTRP, SYSMSG, SYSPAR
C
C Global Strings:
C ---------------
C
C Program name, version string, interrupt action, log name,
C and name of system e.g. NRAO yucca SDE. This latter is set
C by the startup routine to be NRAO hostname SDE. SYSPROG is
C derived from the tail of the invoking string e.g. if you type
C ~tcornwel/sde/mapper then SYSPROG = mapper
C
      CHARACTER SYSPROG*16, SYSVERS*40, SYSGVER*40, SYSINTAC*10,
     1   SDELOG*64, SYSORIG*64
      COMMON /SYSC/ SYSPROG, SYSVERS, SYSGVER, SYSINTAC, SDELOG,
     1   SYSORIG
C
C Useful buffers:
C ---------------
C
C These are for immediate use only: do not expect them to remain
C unchanged over a subroutine call (except for MSGPUT).
C
      CHARACTER*256 STRBUF
      CHARACTER*256  MESSAGE
      COMMON /STRC/ STRBUF, MESSAGE
C
C System parameters:
C ------------------
C
C Max number of array dimensions
C
      INTEGER SYSMXDIM		
      PARAMETER (SYSMXDIM = 7)
C
C Max length of names
C
      INTEGER SYSMXNAM		
      PARAMETER (SYSMXNAM = 64)
C
C Max number of images treated as a unit
C
      INTEGER SYSMXIMG
      PARAMETER (SYSMXIMG = 32)
C
C Characters per INT
C
      INTEGER SYSCHINT			
#if MACH_CRAY2
      PARAMETER (SYSCHINT = 8)
#else
      PARAMETER (SYSCHINT = 4)
#endif
C
C Memory reference common:
C ------------------------
C
C All application arrays are accessed by a reference to the appropriate
C common block. The address with respect to the common block is handled
C by the dai.c routines e.g. MEMR(DATADD("Vis/UU")) contains the first
C element of the u coordinates of the visibility data base "Vis".
C
      INTEGER MEMI(2)
      LOGICAL MEML(2)
      REAL MEMR(2)
      DOUBLE PRECISION MEMD(1)
      COMPLEX MEMX(1)
      CHARACTER*(SYSMXNAM) MEMC(1)
      EQUIVALENCE (MEMI(1), MEMR(1), MEMD(1), MEMX(1), MEML(1))
      COMMON /SYSMEMI/ MEMI
#if COMP_SGI
      CHARACTER*(SYSMXNAM-8) MEMCPAD
      COMMON /SYSMEMC/ MEMCPAD, MEMC
#else
      COMMON /SYSMEMC/ MEMC
#endif
C
C Error reason messages:
C ----------------------
C
C These are the approved strings for use in ERRREPOR. You can use something
C else but other routines may not understand what you mean.
C
      CHARACTER*(*) ERRFATAL
      PARAMETER (ERRFATAL = 'Fatal error')
      CHARACTER*(*) ERRBDARG
      PARAMETER (ERRBDARG = 'Bad subroutine argument')
      CHARACTER*(*) ERRNOMEM
      PARAMETER (ERRNOMEM = 'No memory available')
      CHARACTER*(*) ERRNTFND
      PARAMETER (ERRNTFND = 'Requested item not found')
      CHARACTER*(*) ERRLOGIC
      PARAMETER (ERRLOGIC = 'Program logic error')
      CHARACTER*(*) ERRNOSLT
      PARAMETER (ERRNOSLT = 'No slot available')
      CHARACTER*(*) ERRWRGTP
      PARAMETER (ERRWRGTP = 'Item is of wrong type')
      CHARACTER*(*) ERRTRUNC
      PARAMETER (ERRTRUNC = 'Result truncated')
      CHARACTER*(*) ERRBADID
      PARAMETER (ERRBADID = 'Bad identifier supplied')
      CHARACTER*(*) ERRDB
      PARAMETER (ERRDB = 'Internal database error')
      CHARACTER*(*) ERRINPUT
      PARAMETER (ERRINPUT = 'Input read error')
      CHARACTER*(*) ERROUTPT
      PARAMETER (ERROUTPT = 'Output write error')
      CHARACTER*(*) ERROPEN
      PARAMETER (ERROPEN = 'File open error')
      CHARACTER*(*) ERRCLOSE
      PARAMETER (ERRCLOSE = 'File close error')
C
C Namespace collision redefinitions:
C ----------------------------------
C
#if COMP_IBM || COMP_HP
#define STRLEN SDESTL
#endif
