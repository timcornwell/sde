C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)annotate.f	1.3 1/12/95
C
      SUBROUTINE SDEMAIN
C
CD Program to annotate a blank page.  Similar to a *very* stripped version
C  of imgplot.
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				D.S.Briggs	Aug 30 1994
C	Force an page initialization by printing as innocuous a character
C	as we can.  (Unfortunately, a space doesn't cut it.)
C				D.S.Briggs	Sept 19 1994
C	Change "CALL PGADVANCE" -> "CALL PGPAGE" for pgplot
C				M. Stupar	Jan 5, 1995
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'ANNOTATE')
C
      INTEGER		NDUMMY, ISTAT
      REAL		CHEIGHT, NTOP, NBOT, NRIGHT, NLEFT
      CHARACTER*(SYSMXNAM)	DEVICE, AFILE
      LOGICAL		DOBOX
C
      INTEGER		PGBEGIN
C==================================================================
C
      CALL MSGWELCO ('I annotate a blank image')
      CALL USRCTL
C
      CALL USRGETC ('Annotate', AFILE, 1, NDUMMY)
      CALL USRGETR ('CHeight', CHEIGHT, 1, NDUMMY)
      CALL USRGETR ('NBotLab', NBOT, 1, NDUMMY)
      CALL USRGETR ('NTopLab', NTOP, 1, NDUMMY)
      CALL USRGETR ('NRightLab', NRIGHT, 1, NDUMMY)
      CALL USRGETR ('NLeftLab', NLEFT, 1, NDUMMY)
      CALL USRGETL ('Box', DOBOX, 1, NDUMMY)
      CALL USRGETC ('Device', DEVICE, 1, NDUMMY)
C
      MESSAGE = 'Device = ' // DEVICE
      CALL MSGPUT (MESSAGE, 'I')
      ISTAT = PGBEGIN(0, DEVICE, 1, 1)
      CALL PGSCH(CHEIGHT)
      CALL PGSCF(2)
      IF (ISTAT.NE.1) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1      'Cannot open graphics device '//DEVICE)
         GO TO 999
      END IF
      CALL PGSLW (1)
C
      CALL PGPAGE
C
C This is a non-standard PGPLOT routine!  Seek it in the SDE source area.
C Similar to PGVSTAND, but takes margin arguments in character heights:
C L, R, B, T
C
      CALL PGVCPORT (NLEFT*1.4+4., NRIGHT*1.4+2.,
     $   NBOT*1.4+4., NTOP*2.+4.)
C
      CALL PGSCH(1.25*CHEIGHT)
      CALL PGWNAD(0.0, 1.0, 0.0, 1.0)
      CALL PGSCH(CHEIGHT)
C
C Kludge!  We have to cause it to emit *something* to start a new page
C
      CALL PGSCI(0)
      CALL PGMTEXT ('T',.1,-.01,0.5,'.')
      CALL PGSCI(1)
C
C Box
C
      IF (DOBOX) THEN
         CALL PGBOX('BC', 0.0, 0, 'BC', 0.0, 0)
      END IF
C
C Annotation
C
      IF (AFILE.NE.' ')  CALL IMGANNOT (AFILE)
C
      CALL PGEND
C
 999  CONTINUE
      END
