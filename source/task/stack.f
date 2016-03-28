C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)stack.f	1.4    12/14/94
C
      SUBROUTINE SDEMAIN
C
CD Program to stack images together
C
C Audit trail:
C	Original version:
C				D.S.Briggs	Oct 16 1992
C
C	BLC, TRC, & Ascend?  inputs added
C				D.S.Briggs	Feb 1993
C	Added Delete option
C				D.S.Briggs	March 11 1994
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'STACK')
C
      CHARACTER*(SYSMXNAM) 	INFILE, OUTFILE
      INTEGER		NIBEGIN, NIEND
      REAL		NIINC
      LOGICAL		DODELET
C
      INTEGER		NDUMMY, I, NITER, NAX, NAXIS(SYSMXDIM), SSIZ,
     $   		ISLOT, ISTART, IINC, RNAX, BLC(SYSMXDIM),
     $     		TRC(SYSMXDIM), INC
      REAL		RNITER, VALUE
      CHARACTER		ATYPE*1, TYPE(SYSMXDIM)*8
      DOUBLE PRECISION	RVAL(SYSMXDIM)
      REAL		RPIX(SYSMXDIM), DELT(SYSMXDIM), ROTA(SYSMXDIM)
      CHARACTER*(SYSMXNAM)	TEMPFILE, LASTFILE, FILL
      LOGICAL		DOVFILL, DOASCEND
C
      INTEGER		CRDRNAX, STRLEN
      LOGICAL		FILEXIST
C==================================================================
C
      CALL MSGWELCO ('I stack images together')
      CALL USRCTL
      CALL USRGETC ('Image', INFILE, 1, NDUMMY)
      CALL USRGETC ('Output', OUTFILE, 1, NDUMMY)
      CALL USRGETI ('Begin', NIBEGIN, 1, NDUMMY)
      CALL USRGETI ('End', NIEND, 1, NDUMMY)
      CALL USRGETR ('Inc', NIINC, 1, NDUMMY)
      CALL USRGETC ('Fill', STRBUF, 1, NDUMMY)
      CALL STRUC (STRBUF, FILL)
      CALL USRGETR ('Value', VALUE, 1, NDUMMY)
      CALL USRGETI ('BLC', BLC, SYSMXDIM, NDUMMY)
      CALL ERRCANCE
      CALL USRGETI ('TRC', TRC, SYSMXDIM, NDUMMY)
      CALL ERRCANCE
      CALL USRGETL ('Ascend', DOASCEND, 1, NDUMMY)
      CALL USRGETL ('Delete', DODELET, 1, NDUMMY)
      CALL USRGETL ('Debug', SYSDEBUG, 1, NDUMMY)
C
      IF (FILL(1:1).EQ.'V') THEN
         DOVFILL = .TRUE.
         CALL MSGPUT ('Will fill unused rows with VALUE','I')
      ELSE
         DOVFILL = .FALSE.
         CALL MSGPUT ('Will fill unused rows by copying previous row',
     $      'I')
      END IF
C
      IF (DOASCEND) THEN
         CALL MSGPUT ('Lowest index is FIRST pixel','I')
      ELSE
         CALL MSGPUT ('Lowest index is LAST pixel','I')
      END IF
C
C Find any example of the input files
C
      INC = 1
      IF (NIEND.LT.NIBEGIN) INC = -1
      DO 5 I = NIBEGIN, NIEND, INC
         CALL STRNUMFL (INFILE, I, TEMPFILE)
         IF (FILEXIST(TEMPFILE)) GO TO 6
 5    CONTINUE
 6    CONTINUE
      CALL FILIMGGE ('Img', TEMPFILE, ' ')
      CALL DATGETAR ('Img', NAX, NAXIS, ATYPE, NDUMMY)
      CALL CRDGET ('Img', NAX, TYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
      RNAX = CRDRNAX(NAX, NAXIS)
C
      DO 10 I = 1, SYSMXDIM
         IF (BLC(I).LE.0) BLC(I) = 1
         BLC(I) = MIN(BLC(I),NAXIS(I))
         IF (TRC(I).LE.0) TRC(I) = NAXIS(I)
         TRC(I) = MIN(TRC(I),NAXIS(I))
         NAXIS(I) = TRC(I) - BLC(I) + 1
 10   CONTINUE
      WRITE (MESSAGE, 1010) BLC
 1010 FORMAT (' BLC =',6(I4,','),I4)
      CALL MSGPUT (MESSAGE, 'I')
      WRITE (MESSAGE, 1020) TRC
 1020 FORMAT (' TRC =',6(I4,','),I4)
      CALL MSGPUT (MESSAGE, 'I')
C
      CALL DATCREAT ('Window')
      CALL DATPUTI ('Window', 'TRC', TRC, SYSMXDIM)
      CALL DATPUTI ('Window', 'BLC', BLC, SYSMXDIM)
      CALL DATRENAM ('Img', 'TempImg')
      CALL IMGSUBSE ('TempImg', 'Img', 'Window')
      CALL DATDELET ('TempImg')
C
C Figure out how many slots in the stack we'll need.  It's a little
C convoluted, so we just do it with a loop.
C
      RNITER = NIBEGIN
      NITER = NINT(RNITER)
      SSIZ = 0
 100  CONTINUE
      SSIZ = SSIZ + 1
      IF (NIINC .LT. 0.0) THEN
         RNITER = RNITER * ABS(NIINC)
         NITER = NINT(RNITER)
      ELSE IF (NIINC .GT. 0.0) THEN
         RNITER = RNITER + NIINC
         NITER = NINT(RNITER)
      ELSE
         NITER = NIEND + 1
      END IF
      IF (NITER .LE. NIEND) GO TO 100
C
C Make the output array, by messing with input image
C
      IF (NAX.EQ.SYSMXDIM) THEN
         CALL ERRREPOR(ERRNOSLT, ROUTINE, 'Can''t expand image header')
         GO TO 999
      END IF
      DO 200 I = NAX, RNAX+1, -1
         TYPE(I+1) = TYPE(I)
         NAXIS(I+1) = NAXIS(I)
         RVAL(I+1) = RVAL(I)
         RPIX(I+1) = RPIX(I)
         DELT(I+1) = DELT(I)
         ROTA(I+1) = ROTA(I)
 200  CONTINUE
      NAX = NAX + 1
      RNAX = RNAX + 1
      NAXIS(RNAX) = SSIZ
      IF (NIINC.LT.0.0) THEN
         TYPE(RNAX) = 'LOG-INDX'
         RVAL(RNAX) = LOG10(REAL(NIBEGIN))
         DELT(RNAX) = -LOG10(ABS(NIINC))
      ELSE IF (NIINC.GT.0.0) THEN
         TYPE(RNAX) = 'INDEX'
         RVAL(RNAX) = NIBEGIN
         DELT(RNAX) = -NIINC
      ELSE
         TYPE(RNAX) = 'INDEX'
         RVAL(RNAX) = NIBEGIN
         DELT(RNAX) = -1.0
      END IF
      IF (DOASCEND) THEN
         RPIX(RNAX) = 1
         DELT(RNAX) = -DELT(RNAX)
      ELSE
         RPIX(RNAX) = SSIZ
      END IF
      CALL DATMAKAR ('Stack', NAX, NAXIS, ATYPE, NDUMMY)
      CALL HEDCOPY ('Img', 'Stack')
      CALL CRDPUT ('Stack', NAX, TYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
      IF (ERROR) GO TO 999
C
C Now the main loop to fill it with stuff
C
      IF (DOASCEND) THEN
         ISTART = 1
         IINC = 1
      ELSE
         ISTART = SSIZ
         IINC = -1
      END IF
      RNITER = NIBEGIN
      NITER = NINT(RNITER)
      LASTFILE = TEMPFILE
      ISLOT = ISTART
C
 300  CONTINUE
C
      CALL STRNUMFL (INFILE, NITER, TEMPFILE)
      IF (TEMPFILE.NE.LASTFILE) THEN
         IF (FILEXIST(TEMPFILE)) THEN
            CALL DATDELET ('Img')
            CALL FILIMGGE ('Img', TEMPFILE, ' ')
            CALL DATRENAM ('Img', 'TempImg')
            CALL IMGSUBSE ('TempImg', 'Img', 'Window')
            CALL DATDELET ('TempImg')
         ELSE
            WRITE (MESSAGE,1300) TEMPFILE(1:STRLEN(TEMPFILE))
 1300       FORMAT ('File ',A,' not found')
            CALL MSGPUT (MESSAGE,'W')
            IF (DOVFILL) CALL ARRSETCO ('Img', 0.0, VALUE)
         END IF
      END IF
C
      IF ((TEMPFILE.EQ.LASTFILE).AND.(ISLOT.NE.ISTART)) THEN
         CALL MSGPUT ('Duplicate row', 'I')
         IF (DOVFILL)
     $      CALL ARRSETCO ('Img', 0.0, VALUE)
      END IF
      LASTFILE = TEMPFILE
C
      CALL ARRCPPL ('Img', 'Stack', RNAX, ISLOT)
C
      IF (NIINC .LT. 0.0) THEN
         RNITER = RNITER * ABS(NIINC)
         NITER = NINT(RNITER)
      ELSE IF (NIINC .GT. 0.0) THEN
         RNITER = RNITER + NIINC
         NITER = NINT(RNITER)
      ELSE
         NITER = NIEND + 1
      END IF
C
      ISLOT = ISLOT + IINC
      IF (NITER .LE. NIEND) GO TO 300
C
C Write result 
C
      CALL FILIMGPU ('Stack', OUTFILE, ' ')
C
C Delete input files if requested
C
      IF (ERROR) GO TO 999
      IF (DODELET) THEN
         IF (DOASCEND) THEN
            ISTART = 1
            IINC = 1
         ELSE
            ISTART = SSIZ
            IINC = -1
         END IF
         RNITER = NIBEGIN
         NITER = NINT(RNITER)
         LASTFILE = TEMPFILE
         ISLOT = ISTART
C
 500     CONTINUE
C
         CALL STRNUMFL (INFILE, NITER, TEMPFILE)
         IF (FILEXIST(TEMPFILE)) THEN
            MESSAGE = 'Deleting file ' // TEMPFILE(1:STRLEN(TEMPFILE))
            CALL MSGPUT (MESSAGE, 'I')
            CALL FILDEL (TEMPFILE)
         END IF
C
         IF (NIINC .LT. 0.0) THEN
            RNITER = RNITER * ABS(NIINC)
            NITER = NINT(RNITER)
         ELSE IF (NIINC .GT. 0.0) THEN
            RNITER = RNITER + NIINC
            NITER = NINT(RNITER)
         ELSE
            NITER = NIEND + 1
         END IF
C
         ISLOT = ISLOT + IINC
         IF (NITER .LE. NIEND) GO TO 500
      END IF
C
  999 CONTINUE
      END






