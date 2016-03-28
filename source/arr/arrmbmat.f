C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrmbmat.f	1.2    2/8/95
C
      SUBROUTINE ARRMBMAT (BMAT, INWIN, OUTWIN, PSF, DODBLE)
C
CD Make a beam matrix
C
C	BMAT	CH*(*)	input	Name of beam matrix
C	INWIN	CH*(*)	input	Name of input window
C	OUTWIN	CH*(*)	input	Name of output window
C	PSF	CH*(*)	input	Name of PSF
C	DODBLE	LOG	input	Do double precision?
C	INWIN/ARRSUM	input	Assumed to be number of pixels in INWIN
C	OUTWIN/ARRSUM	input	Assumed to be number of pixels in OUTWIN
C
C INWIN and OUTWIN are assumed to have been normalized with ARRDIV and
C stated with ARRSTAT
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				D.S. Briggs	Nov 25 1992
C	Remove assumption that the window images are the same size
C	as the PSF.  This can save a lot of memory when the image
C	size grows large.
C				D.S.Briggs	Jan 22 1995
C-----------------------------------------------------------------------
#include		"stdinc.h"
C
      CHARACTER*(*)	BMAT, PSF, INWIN, OUTWIN
      LOGICAL		DODBLE
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'ARRMBMAT')
C
      INTEGER		PNAX, PNAXIS(SYSMXDIM), PADD,
     $   		BNAX, BNAXIS(SYSMXDIM), BADD,
     $   		IWNAX, IWNAXIS(SYSMXDIM), IWADD,
     $   		OWNAX, OWNAXIS(SYSMXDIM), OWADD,
     $			IBBLO(SYSMXDIM), IBBHI(SYSMXDIM),
     $			OBBLO(SYSMXDIM), OBBHI(SYSMXDIM),
     $   		IDELT(SYSMXDIM), NIWIN, NOWIN, NDUMMY
      CHARACTER*1	PTYPE, BTYPE, IWTYPE, OWTYPE
C
      REAL		DATFGETR
      INTEGER		CRDRNAX
      LOGICAL		DATEXIST
C=======================================================================
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (PSF, PNAX, PNAXIS, PTYPE, PADD)
      PNAX = CRDRNAX (PNAX, PNAXIS)
      IF (((.NOT.DODBLE).AND.(PTYPE.NE.'R')).OR.
     $     DODBLE.AND.(PTYPE.NE.'D'))
     $   CALL ERRREPOR (ERRBDARG, ROUTINE, 'PSF is bad type')
      IF ((PNAX.GT.2).OR.(PNAX.LT.1))
     $   CALL ERRREPOR (ERRBDARG, ROUTINE, 'PSF has bad dimension')
      IF (ERROR) GOTO 999
C
      CALL DATGETAR (INWIN, IWNAX, IWNAXIS, IWTYPE, IWADD)
      CALL DATGETAR (OUTWIN, OWNAX, OWNAXIS, OWTYPE, OWADD)
      IWNAX = CRDRNAX (IWNAX, IWNAXIS)
      OWNAX = CRDRNAX (OWNAX, OWNAXIS)
C
      IF (IWNAX.GT.PNAX)
     $   CALL ERRREPOR (ERRBDARG, ROUTINE, 'INWIN has bad dimension')
      IF (OWNAX.GT.PNAX)
     $   CALL ERRREPOR (ERRBDARG, ROUTINE, 'OUTWIN has bad dimension')
      IF (ERROR) GO TO 999
C
C Find alignment between windows
C
      CALL IMGIDELT (INWIN, OUTWIN, IDELT)
      IF (ERROR) GO TO 999
C
      NIWIN = NINT(DATFGETR(INWIN,'ARRSUM'))
      NOWIN = NINT(DATFGETR(OUTWIN,'ARRSUM'))
      CALL DATGETI (INWIN, 'ARRBBBLC', IBBLO, SYSMXDIM, NDUMMY)
      CALL DATGETI (INWIN, 'ARRBBTRC', IBBHI, SYSMXDIM, NDUMMY)
      CALL DATGETI (OUTWIN, 'ARRBBBLC', OBBLO, SYSMXDIM, NDUMMY)
      CALL DATGETI (OUTWIN, 'ARRBBTRC', OBBHI, SYSMXDIM, NDUMMY)
      IF (ERROR) GO TO 999
C
      IF (DATEXIST(BMAT)) THEN
         CALL DATGETAR (BMAT, BNAX, BNAXIS, BTYPE, BADD)
         BNAX = CRDRNAX (BNAX, BNAXIS)
         IF (((.NOT.DODBLE).AND.(BTYPE.NE.'R')) .OR.
     $       (DODBLE.AND.(BTYPE.NE.'D'))) THEN
            CALL ERRREPOR (ERRBDARG, ROUTINE, 'BMAT is wrong type')
            GO TO 999
         END IF
         IF ((BNAX.GT.2).OR.
     $       (BNAXIS(1).NE.NIWIN).OR.
     $       (BNAXIS(2).NE.NOWIN)) THEN
            CALL ERRREPOR (ERRBDARG, ROUTINE, 'BMAT has bad dims')
            GO TO 999
         END IF
      ELSE
         BTYPE = 'R'
         IF (DODBLE) BTYPE = 'D'
         BNAX = 2
         BNAXIS(1) = NIWIN
         BNAXIS(2) = NOWIN
         CALL DATMAKAR (BMAT, BNAX, BNAXIS, BTYPE, BADD)
      END IF
C
      IF (DODBLE) THEN
         IF (PNAX.EQ.1) THEN
            CALL PIXDMB1D (MEMD(BADD), MEMR(IWADD), MEMR(OWADD),
     $         MEMD(PADD), IDELT, NIWIN, NOWIN, PNAXIS(1),
     $	       IWNAXIS(1), OWNAXIS(1), IBBLO, IBBHI, OBBLO, OBBHI)
         ELSE
            CALL PIXDMB2D (MEMD(BADD), MEMR(IWADD), MEMR(OWADD),
     $         MEMD(PADD), IDELT, NIWIN, NOWIN, PNAXIS(1), PNAXIS(2),
     $	       IWNAXIS(1), IWNAXIS(2), OWNAXIS(1), OWNAXIS(2),
     $         IBBLO, IBBHI, OBBLO, OBBHI)
         END IF
      ELSE
         IF (PNAX.EQ.1) THEN
            CALL PIXRMB1D (MEMR(BADD), MEMR(IWADD), MEMR(OWADD),
     $         MEMR(PADD), IDELT, NIWIN, NOWIN, PNAXIS(1),
     $	       IWNAXIS(1), OWNAXIS(1), IBBLO, IBBHI, OBBLO, OBBHI)
         ELSE
            CALL PIXRMB2D (MEMR(BADD), MEMR(IWADD), MEMR(OWADD),
     $         MEMR(PADD), IDELT, NIWIN, NOWIN, PNAXIS(1), PNAXIS(2),
     $	       IWNAXIS(1), IWNAXIS(2), OWNAXIS(1), OWNAXIS(2),
     $         IBBLO, IBBHI, OBBLO, OBBHI)
         END IF
      END IF
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END




