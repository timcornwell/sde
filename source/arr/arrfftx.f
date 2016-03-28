C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrfftx.f	1.3    7/14/97
C
      SUBROUTINE ARRFFTX (IARRAY, VARRAY, DIR)
C
CD Complex to complex Fourier transform. 
C
C	IARRAY	CH*(*)	input	Name of image array
C	VARRAY	CH*(*)	input	Name of vis array
C	DIR	INT	input	Direction of transform FPS convention
C
C As VIS(u,v) .NE. VIS(-u,-v)*, need full size VIS.  Transform of an IMAGE
C array of size NX, NY is a VISIBILITY array of size NX, NY.  FFTIMGX calls
C this routine to do the work, and FFTCONJX to calculate new headers.
C
C This version will expand and contract arrays as required, zero-padding and
C avoiding extra transforms. The phase center of the transforms is always at
C an integer pixel, and is (NX+1)/2, (NY+1)/2, (NZ+1)/2, etc. in both planes.
C (note the integer arithmetic.)
C
C This version will automatically detect equivalent third axes and not
C transform. We should generalize this one day...
C
C Audit trail:
C	Now can do COMPLEX-to-COMPLEX FFT
C				M.A.Holdaway	Nov 30 1990
C	FUDGE: X->X FFT errors appeared when transforming between arrays of
C	different sizes.  We now PAD and SUBSECTION such arrays here to get
C	the X->X FFT to work.  This should probably be fixed in the future,
C	as things would speed up somewhat.
C				M.A.Holdaway	Nov 30 1990
C	Basically just a documentation cleanup.  The new fft routines I
C	just installed use their own buffers, so a buffer increase
C	isn't needed.  Note that while there are passed buffers allocated
C	here, about half the pixel level ffts don't use them.  When
C	increasing buffer sizes, be sure to check (at least) arrfftx.f,
C	arrfftr.f, fftx.f, fftr.f, and fftxnp2.f.  Currently, they are
C	all allow transforms up to 8192.
C				D.S.Briggs	Dec 27 1991
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	IARRAY, VARRAY
      INTEGER		DIR
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'ARRFFTX')
C
      INTEGER		MAXGAMMA, MAXN
      PARAMETER		(MAXGAMMA = 13)
      PARAMETER		(MAXN = 2**MAXGAMMA+1)
C
      CHARACTER		ITYPE*1, VTYPE*1
      INTEGER		INAX, VNAX, IAX, VNAXIS(SYSMXDIM),
     1			INAXIS(SYSMXDIM), XNAXIS(SYSMXDIM),
     $   		BLC(SYSMXDIM), TRC(SYSMXDIM)
      REAL   		IRPIX(SYSMXDIM), VRPIX(SYSMXDIM),
     $   		XRPIX(SYSMXDIM)
      REAL		DELT(SYSMXDIM), ROTA(SYSMXDIM)
      DOUBLE PRECISION	RVAL(SYSMXDIM)
      INTEGER		NDUMMY, IADD, VADD, VWORKADD, IWORKADD, OFFI,
     1			OFFV, IZ, RVNAX, NAX
      CHARACTER*8	ICTYPE(SYSMXDIM), VCTYPE(SYSMXDIM),
     $   		XCTYPE(SYSMXDIM)
      LOGICAL		SLOWZ, ICHANGE, VCHANGE
C
      LOGICAL		DATEXIST
      INTEGER		CRDRNAX
      DATA		XNAXIS /SYSMXDIM*1/
      DATA		BLC  /SYSMXDIM*1/
      DATA		TRC  /SYSMXDIM*1/
C=======================================================================
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (IARRAY, INAX, INAXIS, ITYPE, IADD)
C
      CALL DATGETAR (VARRAY, VNAX, VNAXIS, VTYPE, VADD)
      IF (ITYPE.NE.'X') THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Bad array type '//ITYPE)
      END IF
      IF (VTYPE.NE.'X') THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Bad array type '//VTYPE)
      END IF
      IF (VTYPE.NE.'X'  .OR.  ITYPE.NE.'X') GOTO 990
C
      RVNAX = CRDRNAX(VNAX, VNAXIS)
      IF (RVNAX.LE.0) THEN
         CALL ERRREPOR (ERRLOGIC, ROUTINE, 
     1      'No real axes for visibility image')
         GO TO 990
      END IF
      DO 10 IAX = 1, RVNAX
         IF(VNAXIS(IAX).LE.1) THEN
            CALL ERRREPOR (ERRLOGIC, ROUTINE, 
     1         'Trying to transform vis axis with only one pixel')
            GO TO 999
         END IF
         IF(INAXIS(IAX).LE.1) THEN
            CALL ERRREPOR (ERRLOGIC, ROUTINE, 
     1         'Trying to transform image axis with only one pixel')
            GO TO 999
         END IF
  10  CONTINUE
C
C Fudge the problem with VIS and IMAGE sizes differing
C>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      ICHANGE = .FALSE.
      VCHANGE = .FALSE.
      DO 20 IAX = 1, RVNAX
         XNAXIS(IAX) = MAX( INAXIS(IAX), VNAXIS(IAX) )
         IF (XNAXIS(IAX) .NE. INAXIS(IAX)) ICHANGE = .TRUE.
         IF (XNAXIS(IAX) .NE. VNAXIS(IAX)) VCHANGE = .TRUE.
C         WRITE (MESSAGE, 754) INAXIS(IAX), VNAXIS(IAX)
C 754     FORMAT(' Image: ',I6, '   Vis: ', I6)
C         CALL MSGPUT (MESSAGE, 'I')
 20   CONTINUE
      IF (ICHANGE) THEN
         CALL IMGPAD (IARRAY, 'IFFTtemp', XNAXIS, 0.0)
         CALL DATGETAR ('IFFTtemp', INAX, XNAXIS, ITYPE, IADD)
      ENDIF
      IF (VCHANGE) THEN
         CALL IMGPAD (VARRAY, 'VFFTtemp', XNAXIS, 0.0)
         CALL DATGETAR ('VFFTtemp', VNAX, XNAXIS, VTYPE, VADD)
      ENDIF
C<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
C End of Fudge............see below for undoing of fudge
C
C Create work arrays
C
      IF (.NOT.DATEXIST('SCRATCH/FFTV')) THEN
         CALL DATCREAT ('SCRATCH')
         CALL DATMAKAR ('SCRATCH/FFTV', 1, 2*MAXN, 'X', VWORKADD)
         CALL DATMAKAR ('SCRATCH/FFTI', 1, 2*MAXN, 'X', IWORKADD)
      END IF
C
C Find out if the third axis should be transformed
C
      CALL DATGETC (IARRAY, 'CTYPE', ICTYPE, SYSMXDIM, NDUMMY)
      CALL DATGETC (VARRAY, 'CTYPE', VCTYPE, SYSMXDIM, NDUMMY)
      SLOWZ = ICTYPE(3).EQ.VCTYPE(3)
C
C Now call pixel level routine
C
      IF (RVNAX.EQ.1) THEN
         CALL FFTX1D2 (MEMX(IADD), MEMX(VADD), MEMX(VWORKADD), 
     $      MEMX(VWORKADD+MAXN), MEMX(IWORKADD), INAXIS(1), 
     $      VNAXIS(1), DIR)
      ELSEIF (RVNAX.EQ.2) THEN
         CALL FFTX2D (MEMX(IADD), MEMX(VADD), MEMX(VWORKADD), 
     $      MEMX(VWORKADD+MAXN), MEMX(IWORKADD), XNAXIS(1), 
     $      XNAXIS(2), XNAXIS(1), XNAXIS(2), DIR)
      ELSEIF (RVNAX.EQ.3) THEN
         IF(SLOWZ) THEN
            DO 100 IZ = 1, XNAXIS(3)
               OFFI = XNAXIS(1) * XNAXIS(2) * (IZ - 1)
               OFFV = XNAXIS(1) * XNAXIS(2) * (IZ - 1)
               CALL FFTX2D (MEMX(IADD + OFFI), MEMX(VADD+OFFV), 
     $            MEMX(VWORKADD), MEMX(VWORKADD+MAXN), 
     $            MEMX(IWORKADD), XNAXIS(1), 
     $            XNAXIS(2), XNAXIS(1), XNAXIS(2), DIR)
 100        CONTINUE
         ELSE
            CALL ERRREPOR (ERRBDARG, ROUTINE, 
     $         'Only SLOW Z X-to-X FFT available in 3-D')
            GO TO 999
C            CALL FFTR3D (MEMR(RADD), MEMX(XADD), MEMX(XWORKADD), 
C     $         MEMX(XWORKADD+MAXN), MEMR(RWORKADD), XNAXIS(1), 
C     $         XNAXIS(2), XNAXIS(3), XNAXIS(1), XNAXIS(2), XNAXIS(3),
C     $         DIR)
         END IF
      ELSE 
         CALL ERRREPOR (ERRBDARG, ROUTINE, 
     $      'Illegal dimension for X-to-X FFT')
         GO TO 999
      END IF
C
C Cover up for the fudge (see above)
C>>>>>>>>>>>>>>>>>>>>>>>
      IF (DIR .GT. 0) THEN
         IF (ICHANGE) THEN
            CALL DATDELET ('IFFTtemp')
         ENDIF
         IF (VCHANGE) THEN
            CALL DATCREAT ('Fwindow')
            CALL CRDGET ('VFFTtemp', NAX, XCTYPE, XNAXIS, RVAL, 
     $         XRPIX, DELT, ROTA)
            CALL CRDGET (VARRAY, NAX, VCTYPE, VNAXIS, RVAL, 
     $         VRPIX, DELT, ROTA)
            DO 880 IAX = 1, RVNAX
               BLC(IAX) = XRPIX(IAX) - VRPIX(IAX) + 1
               TRC(IAX) = XRPIX(IAX) - VRPIX(IAX) + VNAXIS(IAX)
 880        CONTINUE
            CALL DATPUTI  ('Fwindow', 'BLC', BLC ,SYSMXDIM)
            CALL DATPUTI  ('Fwindow', 'TRC', TRC ,SYSMXDIM)
            CALL IMGSUBSE ('VFFTtemp', VARRAY, 'Fwindow')
            CALL DATDELET ('VFFTtemp')
            CALL DATDELET ('Fwindow')
         ENDIF
      ELSE IF (DIR .LT. 0) THEN
         IF (ICHANGE) THEN
            CALL DATCREAT ('Fwindow')
            CALL CRDGET ('IFFTtemp', NAX, XCTYPE, XNAXIS, RVAL, 
     $         XRPIX, DELT, ROTA)
            CALL CRDGET (IARRAY, NAX, ICTYPE, INAXIS, RVAL, 
     $         IRPIX, DELT, ROTA)
            DO 890 IAX = 1, RVNAX
               BLC(IAX) = XRPIX(IAX) - IRPIX(IAX) + 1
               TRC(IAX) = XRPIX(IAX) - IRPIX(IAX) + INAXIS(IAX)
 890        CONTINUE
            CALL DATPUTI  ('Fwindow', 'BLC', BLC ,SYSMXDIM)
            CALL DATPUTI  ('Fwindow', 'TRC', TRC ,SYSMXDIM)
            CALL IMGSUBSE ('IFFTtemp', IARRAY, 'Fwindow')
            CALL DATDELET ('IFFTtemp')
            CALL DATDELET ('Fwindow')
         ENDIF
         IF (VCHANGE) THEN
            CALL DATDELET ('VFFTtemp')
         ENDIF
      ENDIF
C<<<<<<<<<<<<<<<<<<<<<<<<<<
C End of cover up for fudge
C
  990 IF (ERROR) CALL ERRTRACE (ROUTINE)
C
  999 CONTINUE
      END
