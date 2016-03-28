C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrstat.f	1.6    10/21/94
C
      SUBROUTINE ARRSTAT (ANAME, WINDOW)
C
CD Calculate array statistics in window
C
C
C	ANAME	CH*(*)	input	Directory name of array
C	WINDOW	CH*(*)	input	Directory name of window
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Added Double precision support
C				T.J.Cornwell	Feb 6 1991
C	Added support for AVGTYPE
C				D.S.Briggs	Feb 14 1992
C	Added NLOC, BBBLC & BBTRC outputs.  These are the number of,
C	and the bounding box of the non-zero pixels in the array
C				D.S.Briggs	Feb 27 1992
C	Added location of minimum and maximum, MINLOC & MAXLOC
C				D.S.Briggs	Oct 21 1994
C----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*) 	ANAME, WINDOW
C
      CHARACTER*(*) 	ROUTINE
      PARAMETER 	(ROUTINE='ARRSTAT')
C
C Local variables
C
      CHARACTER 	ATYPE*1
      CHARACTER*(SYSMXNAM)  STRM2, AVGTYPE
      INTEGER 		ANAX, ANAXIS(SYSMXDIM), 
     $			BLC (SYSMXDIM), TRC(SYSMXDIM),
     $   		BBBLC(SYSMXDIM), BBTRC(SYSMXDIM),
     $   		MINLOC(SYSMXDIM), MAXLOC(SYSMXDIM)
      INTEGER 		AADD, IAX, NDUMMY, NLOC
      REAL 		AVE, RMS, SUM, DISP, RDMAX, RDMIN
      COMPLEX		XAVE, XSUM, XDMAX, XDMIN
      LOGICAL		DATEXIST
      DATA		BLC	/SYSMXDIM*1/
      DATA		TRC	/SYSMXDIM*1/
C=====================================================================
C
C If there is an error on entry then return immediately
C
      IF (ERROR) GO TO 999
C
      DO 2 IAX = 1, SYSMXDIM
         ANAXIS(IAX) = 1
         BLC(IAX) = 1
         TRC(IAX) = 1
 2    CONTINUE
C
C Get array attributes
C
      CALL DATGETAR (ANAME, ANAX, ANAXIS, ATYPE, AADD)
      DO 1 IAX = 1, ANAX
         ANAXIS (IAX) = MAX(1, ANAXIS(IAX))
  1   CONTINUE
C
      AVGTYPE = 'AMPSCALAR'
      IF (ATYPE.EQ.'X') THEN
         IF (DATEXIST(STRM2(ANAME,'AVGTYPE')))
     $      CALL DATGETC (ANAME, 'AVGTYPE', AVGTYPE, 1, NDUMMY)
      END IF
C
C Get window parameters
C
      IF (DATEXIST(STRM2(WINDOW, 'BLC'))) THEN
         CALL DATGETI (WINDOW, 'BLC', BLC, SYSMXDIM, NDUMMY)
      ELSE
         DO 5 IAX = 1, ANAX
           BLC(IAX) = 1
  5      CONTINUE
      END IF
      IF (DATEXIST(STRM2(WINDOW, 'TRC'))) THEN
         CALL DATGETI (WINDOW, 'TRC', TRC, SYSMXDIM, NDUMMY)
      ELSE
         DO 10 IAX = 1, ANAX
            TRC (IAX) = ANAXIS(IAX)
  10     CONTINUE
      END IF
      DO 20 IAX = 1, ANAX
         BLC(IAX) = MIN(MAX(1, BLC(IAX)), ANAXIS(IAX))
         TRC(IAX) = MAX(BLC(IAX), MIN(TRC(IAX), ANAXIS(IAX)))
  20  CONTINUE
C
C Now actually call routine which does the work on the pixels. Branch
C here on data type of the array.
C
      IF ((ATYPE.EQ.'X').AND.(AVGTYPE.NE.'AMPSCALAR')) THEN
         CALL PIXXSTA2 (MEMX(AADD), ANAXIS(1), ANAXIS(2), ANAXIS(3),
     $      ANAXIS(4), ANAXIS(5), ANAXIS(6), ANAXIS(7),
     $      BLC, TRC, XDMAX, XDMIN, XAVE, RMS, XSUM, DISP,
     $      NLOC, BBBLC, BBTRC, MINLOC, MAXLOC)
         CALL DATPUTX(ANAME, 'ARRMAX', XDMAX, 1)
         CALL DATPUTX(ANAME, 'ARRMIN', XDMIN, 1)
         CALL DATPUTX(ANAME, 'ARRAVE', XAVE, 1)
         CALL DATPUTX(ANAME, 'ARRSUM', XSUM, 1)
      ELSE
         IF (ATYPE.EQ.'R') THEN
            CALL PIXRSTAT (MEMR(AADD), ANAXIS(1), ANAXIS(2), ANAXIS(3),
     $         ANAXIS(4), ANAXIS(5), ANAXIS(6), ANAXIS(7),
     $         BLC, TRC, RDMAX, RDMIN, AVE, RMS, SUM, DISP,
     $         NLOC, BBBLC, BBTRC, MINLOC, MAXLOC)
         ELSE IF (ATYPE.EQ.'D') THEN
            CALL PIXDSTAT (MEMD(AADD), ANAXIS(1), ANAXIS(2), ANAXIS(3),
     $         ANAXIS(4), ANAXIS(5), ANAXIS(6), ANAXIS(7),
     $         BLC, TRC, RDMAX, RDMIN, AVE, RMS, SUM, DISP,
     $         NLOC, BBBLC, BBTRC, MINLOC, MAXLOC)
         ELSE IF (ATYPE.EQ.'X') THEN
            CALL PIXXSTAT (MEMX(AADD), ANAXIS(1), ANAXIS(2), ANAXIS(3),
     $         ANAXIS(4), ANAXIS(5), ANAXIS(6), ANAXIS(7),
     $         BLC, TRC, RDMAX, RDMIN, AVE, RMS, SUM, DISP,
     $         NLOC, BBBLC, BBTRC, MINLOC, MAXLOC)
         ELSE 
            CALL ERRREPOR (ERRLOGIC, ROUTINE, 
     $         'CANNOT SENSIBLY FIND STATISTICS FOR ARRAY ')
            GO TO 999
         END IF
         CALL DATPUTR(ANAME, 'ARRMAX', RDMAX, 1)
         CALL DATPUTR(ANAME, 'ARRMIN', RDMIN, 1)
         CALL DATPUTR(ANAME, 'ARRAVE', AVE, 1)
         CALL DATPUTR(ANAME, 'ARRSUM', SUM, 1)
      END IF
      CALL DATPUTR(ANAME, 'ARRDISP', DISP, 1)
      CALL DATPUTR(ANAME, 'ARRRMS', RMS, 1)
      CALL DATPUTI(ANAME, 'ARRNLOC', NLOC, 1)
      CALL DATPUTI(ANAME, 'ARRBBBLC', BBBLC, SYSMXDIM)
      CALL DATPUTI(ANAME, 'ARRBBTRC', BBTRC, SYSMXDIM)
      CALL DATPUTI(ANAME, 'ARRMINLOC', MINLOC, SYSMXDIM)
      CALL DATPUTI(ANAME, 'ARRMAXLOC', MAXLOC, SYSMXDIM)
C
C Trace errors
C
      IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
 999  CONTINUE
      END
