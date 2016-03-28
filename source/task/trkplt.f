C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)trkplt.f	1.1    2/10/93
C
      SUBROUTINE SDEMAIN
C
CD One line description of this routine for extraction into doc/oneline
C
C More extensive description with details etc.
C
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				R.G. Marson	Apr 14 1992
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
C Parameter Definitions
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'TRKPLT')

C
C Function definitions
C
      REAL DATFGETR
      INTEGER DATFGETI, STRLEN
C
C Local Variables
C
      CHARACTER*(SYSMXNAM) INFILE, DEVICE, FILESYS
      CHARACTER*(SYSMXNAM) XLABEL, YLABEL, PLTLABEL, REFLBL, HOLELBL
      CHARACTER*1 ATYPE
      REAL MINVAL, MAXVAL, XMIN, XMAX, MINDLY, MAXDLY, DLYSCAL
      REAL TRANGE(2)
      INTEGER NDUMMY, NUMDLY, FRANGE(2)
      INTEGER NHOLES, NFRAMES, REF, HOLES(5), I, J, NPLOT, NPTS
      INTEGER ADD, PADD, XADD
      CHARACTER*8      CTYPE(SYSMXDIM)
      DOUBLE PRECISION RVAL(SYSMXDIM)
      REAL             RPIX(SYSMXDIM), DELT(SYSMXDIM), ROTA(SYSMXDIM)
      INTEGER          ANAX, NAXIS(SYSMXDIM)
C#      LOGICAL
C
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
C Get Usr parameters
C
      CALL MSGWELCO ('I Plot MAPPIT Track files')
      CALL USRCTL
C
C Get the data array
C
      CALL USRGETC('Infile', INFILE, 1, NDUMMY)
      CALL FILTRKGE('TRACK', INFILE, ' ')
      IF (ERROR) GOTO 990
      CALL DATCHKTP('TRACK', 'TRACK')
      CALL DATGETAR('TRACK', ANAX, NAXIS, ATYPE, ADD)
      NHOLES = NAXIS(1)
      NFRAMES = NAXIS(2)
      WRITE(MESSAGE, '(A, I2, A, I5, A)') 'Track file has ', NHOLES,
     $     ' Holes and ', NFRAMES, ' Frames'
      CALL MSGPUT(MESSAGE, 'I')
C
C Get the reference hole
C
      CALL USRGETI('Reference', REF, 1, NDUMMY)
      REF = MAX(1, MIN(REF, NHOLES))
      WRITE(MESSAGE, '(A, I2)') 'Reference Hole is ', REF
      CALL MSGPUT(MESSAGE, 'I')
C
C Get the holes to plot
C
      CALL USRGETI('Holes', HOLES, 5, NDUMMY)
      NPLOT = 0
      DO I = 1, NDUMMY
         IF ((HOLES(I).GE.1).AND.(HOLES(I).LE.NHOLES)) THEN
            NPLOT = NPLOT + 1
            HOLES(NPLOT) = HOLES(I)
            WRITE(MESSAGE, '(A, I2)') 'Plotting Hole ', HOLES(NPLOT)
            CALL MSGPUT(MESSAGE, 'I')
         END IF
      END DO
C
C Check if this is an SDE file (so we can get the delay scaling also)
C
      CALL FILSYSEX (INFILE, FILESYS)
      IF (FILESYS.EQ.'SDE') THEN
         NUMDLY = DATFGETI('TRACK', 'NUMDLY')
         MINDLY = DATFGETR('TRACK', 'MINDLY')
         MAXDLY = DATFGETR('TRACK', 'MAXDLY')
         DLYSCAL = (MAXDLY - MINDLY) / FLOAT(NUMDLY) * 1.E15
         YLABEL = 'Delay (femto-secs)'
      ELSE
         DLYSCAL = 1.
         YLABEL = 'Delay Steps'
      END IF
C
C Generate the Delays
C
      NAXIS(1) = NFRAMES
      NAXIS(2) = NPLOT
      MINVAL = (MEMR(ADD + HOLES(1)) - MEMR(ADD + REF)) * DLYSCAL
      MAXVAL = MINVAL
      CALL DATMAKAR('PLOT', 2, NAXIS, 'R', PADD)
      DO I = 0, NPLOT - 1
         DO J = 0, NFRAMES - 1
            MEMR(PADD + I*NFRAMES + J) = 
     $           (MEMR(ADD + HOLES(I+1) - 1 + NHOLES * J) - 
     $            MEMR(ADD + REF        - 1 + NHOLES * J)) * DLYSCAL
            MINVAL = MIN(MEMR(PADD + I*NFRAMES + J), MINVAL)
            MAXVAL = MAX(MEMR(PADD + I*NFRAMES + J), MAXVAL)
         END DO
      END DO
C
C Do the verticle scaling
C
      CALL USRGETR('Trange', TRANGE, 2, NDUMMY)
      IF ((TRANGE(2)-TRANGE(1)).LT.1.E-28) THEN
         IF (MINVAL.EQ.MAXVAL) THEN
            MINVAL = MINVAL - DLYSCAL
            MAXVAL = MAXVAL + DLYSCAL
         END IF
      ELSE
         MINVAL = TRANGE(1)
         MAXVAL = TRANGE(2)
      END IF
C
C Get the Horizontal Scales and axis labels
C
      CALL CRDGET('TRACK', ANAX, CTYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
      XMIN = (1. - RPIX(2)) * DELT(2) + RVAL(2)
      XMAX = (NAXIS(2) - RPIX(2)) * DELT(2) + RVAL(2)
      CALL USRGETI('Frange', FRANGE, 2, NDUMMY)
      IF ((FRANGE(1).GT.XMIN).AND.(FRANGE(1).LT.XMAX)) 
     $     XMIN = FRANGE(1)
      IF ((FRANGE(2).LT.XMAX).AND.(FRANGE(2).GT.XMIN))
     $     XMAX = FRANGE(2)
      NPTS = NINT((XMAX-XMIN)/ DELT(2)) + 1
      NAXIS(1) = NPTS
      CALL DATMAKAR('XAXIS', 1, NAXIS, 'R', XADD)
      DO I = 0, NPTS - 1 
         MEMR(XADD + I) =  XMIN +  I * DELT(2)
      END DO
      XLABEL = 'Frame Number'
      WRITE(MESSAGE, '(A, I5, A, I5)') 
     $     'Plotting from Frame ', NINT(XMIN),  
     $     ' to Frame ', NINT(XMAX)
      CALL MSGPUT(MESSAGE, 'I')
C
C Do the plot label
C
      WRITE(REFLBL,'(A,I3)')'     Ref Hole:', REF
      PLTLABEL = '     File: '// INFILE(1:STRLEN(INFILE))
C
C Get the Plotting Device and open it
C
      CALL USRGETC('Device', DEVICE, 1, NDUMMY)
      CALL PGBEGIN(0, DEVICE, 1, NPLOT)
      DO I = 0, NPLOT - 1
         CALL PGSCI(5)
         CALL PGENV(XMIN, XMAX, MINVAL, MAXVAL, 0, 1)
         WRITE(HOLELBL, '(A, I3)') 'Hole:', HOLES(I+1)
         CALL PGLABEL(XLABEL, YLABEL, 
     $        HOLELBL(1:STRLEN(HOLELBL))//
     $        REFLBL(1:STRLEN(REFLBL))//
     $        PLTLABEL(1:STRLEN(PLTLABEL)))
         CALL PGSCI(2)
         CALL PGLINE(NPTS, MEMR(XADD), 
     $        MEMR(PADD + I * NFRAMES + 
     $        (XMIN - RVAL(2))/DELT(2) + RPIX(2) - 1))
      END DO
      CALL PGEND
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
