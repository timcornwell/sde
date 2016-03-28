C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C
      SUBROUTINE SDEMAIN
C
C Program to plot UV plane residuals from a Clean data base against CC
C   iteration number.
C
C Audit trail:
C	Original version: Cloned loosely from VISRES.F
C				D.S.Briggs	Sep 27 90
C	Max number of traces increased to 20
C				D.S.Briggs	May 11 1994
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISCCPLT')
C
      INTEGER		MAXTRACE
      PARAMETER		(MAXTRACE=20)
C
      CHARACTER*(SYSMXNAM)      CLNFILE, PLOTFILE, LISTFILE
      REAL              CCINC
      INTEGER           CCBEGIN, CCEND
      REAL	        U(MAXTRACE), V(MAXTRACE)
      INTEGER		NPTS
C
      CHARACTER  MSGTXT*80
      CHARACTER  LINEBUFF*1024
      REAL       RPART, IPART, AMPL, PHASE, RLOOP
      INTEGER    ILOOP, NDUMMY, NU, NV, CCMAX, I, NITER, NCC
      INTEGER    ADDUU, ADDVV, ADDVIS
      LOGICAL    DOLIST, DOPLOT
C
      INTEGER    DATADD
      LOGICAL    DATEXIST
C==================================================================
      CALL MSGWELCO ('I plot UV plane models')
      CALL USRCTL
C
C Get input images
C
      CALL USRGETC('CompFile', CLNFILE, 1, NDUMMY)
      CALL USRGETI('Npts', NPTS, 1, NDUMMY)
      CALL USRGETR('U', U, MAXTRACE, NU)
      CALL USRGETR('V', V, MAXTRACE, NV)
      CALL USRGETI('CCbegin', CCBEGIN, 1, NDUMMY)
      CALL USRGETI('CCend', CCEND, 1, NDUMMY)
      CALL USRGETR('CCinc', CCINC, 1, NDUMMY)
      CALL USRGETC('ListFile', LISTFILE, 1, NDUMMY)
      CALL USRGETC('PlotFile', PLOTFILE, 1, NDUMMY)
      IF (ERROR) GOTO 999
C
      DOLIST = LISTFILE.NE.' '
      DOPLOT = PLOTFILE.NE.' '
C
      IF (DOPLOT) CALL MSGPUT ('Plotting NYI','W')
C
C Read in the CC file
C
      CALL FILIMGGE ('comps', CLNFILE, '*')
C
C Not much in the way of sanity checking, I'm afraid.
C
C Get true max number of components
C
      IF (DATEXIST('comps/NITER')) THEN
         CALL DATGETI('comps', 'NITER', NITER, 1, NDUMMY)
      ELSE
         CALL MSGPUT ('Careful!  Assuming NITER = CCend', 'W')
         NITER = CCEND
      END IF
C
      IF (CCEND.GT.0) THEN
         CCMAX = MIN(CCEND,NITER)
      ELSE
         CCMAX = NITER
      END IF
C
      ILOOP = CCBEGIN
      RLOOP = ILOOP
C
C Set up bare bones output UV dataset.
C
      CALL VISMAKE ('VisOut','MOD/I',NPTS)
      ADDUU = DATADD ('VisOut/UU')
      ADDVV = DATADD ('VisOut/VV')
      ADDVIS = DATADD ('VisOut/MOD/I/VIS')
      DO 10 I = 1, NPTS
         MEMR(ADDUU+I-1) = U(I) * 1000.0
         MEMR(ADDVV+I-1) = V(I) * 1000.0
 10   CONTINUE
C
      IF (DOLIST) THEN
         CALL FILDEL (LISTFILE)
         CALL TXTOPEN ('ListFile', LISTFILE, 'WRITE')
         WRITE (LINEBUFF, 1010) LISTFILE
 1010    FORMAT ('Visibility File = ',A)
         CALL TXTWRITE ('ListFile', LINEBUFF)
         WRITE (LINEBUFF, 1020) CLNFILE
 1020    FORMAT ('Clean Component File = ',A)
         CALL TXTWRITE ('ListFile', LINEBUFF)
         WRITE (LINEBUFF, 1030) (U(I), I = 1,NPTS)
 1030    FORMAT ('U =',100F10.1)
         CALL TXTWRITE ('ListFile', LINEBUFF)
         WRITE (LINEBUFF, 1040) (V(I), I = 1,NPTS)
 1040    FORMAT ('V =',100F10.1)
         CALL TXTWRITE ('ListFile', LINEBUFF)
      END IF
C
C Main loop starts here.  Loop over Max CC number.
C
 100  CONTINUE
C
C Lie to IMGCCIMG.  Do not let ILOOP exceed MAXCC!
C
      CALL DATPUTI ('comps', 'NITER', ILOOP, 1)
      CALL IMGCCIMG ('comps')
      IF (ERROR) GOTO 999
C
C Find the true number of CCs
C
      CALL ARRNSPRT('comps', NCC)
C
C Back transform the model
C
      CALL IMGDFT ('comps','VisOut','MOD/I')
      IF (ERROR) GOTO 999
C
C Dump it out to the list file
C
      IF (DOLIST) THEN
 1100    FORMAT (2I6)
         WRITE (LINEBUFF, 1100) ILOOP, NCC
         DO I = 1, NPTS
            RPART = REAL(MEMX(ADDVIS+I-1))
            IPART = IMAG(MEMX(ADDVIS+I-1))
            AMPL = SQRT(RPART*RPART + IPART*IPART)
            PHASE = ATAN2(IPART,RPART)
            WRITE (MSGTXT, 1110) RPART, IPART, AMPL, PHASE
 1110       FORMAT (G15.3,G12.4,G12.4,F12.4)
            CALL STRAPPEN (LINEBUFF, MSGTXT)
         END DO
         CALL TXTWRITE ('ListFile', LINEBUFF)
      END IF

      WRITE (MSGTXT,1120) ILOOP, NCC
 1120 FORMAT('Done with iteration',i6,'  Ncc =',i4)
      CALL MSGPUT (MSGTXT, 'I')
C
      IF (CCINC .LT. 0.0) THEN
 150     RLOOP = RLOOP * ABS(CCINC)
         IF (NINT(RLOOP).EQ.ILOOP) GO TO 150
         ILOOP = NINT(RLOOP)
      ELSE
         ILOOP = ILOOP + CCINC
      END IF

      IF (ILOOP .LE. CCMAX) GO TO 100
C
      CALL TXTCLOSE ('ListFile')
C
 999  CONTINUE
      END

