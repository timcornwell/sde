
C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgdftpb.f	1.5	 5/16/91
C
      SUBROUTINE IMGDFTPB (VIS, CLASS, IMAGE, TIMAGE)
C
CD Direct Fourier transform to Image to Vis. data after applying
C the appropriate primary beam.
C
C
C	VIS		CH*(*)	input	Name of visibility data
C	CLASS		CH*(*)	input	Class of visibility data
C	IMAGE		CH*(*)	input	Name of Image
C	TIMAGE		CH*(*)	input	Name of Image after tapering
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Altered to deal with Beam Switching
C				M.A.Holdaway	Jan 9 1990
C	Added call to HEDGETPB
C				M.A.Holdaway	May 2 1991
C
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	IMAGE, VIS, CLASS, TIMAGE
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGDFTPB')
C
      CHARACTER*1	VATYPE
      CHARACTER*8	VTYPE(SYSMXDIM)
      REAL		VRPIX(SYSMXDIM), VDELT(SYSMXDIM),
     1			VROTA(SYSMXDIM)
      DOUBLE PRECISION	VRVAL(SYSMXDIM)
      INTEGER		INAX, INAXIS(SYSMXDIM)
      CHARACTER*8	ITYPE(SYSMXDIM)
      REAL		IRPIX(SYSMXDIM), IDELT(SYSMXDIM),
     1			IROTA(SYSMXDIM)
      DOUBLE PRECISION	IRVAL(SYSMXDIM)
      INTEGER		VNAX, VNAXIS(SYSMXDIM)
      LOGICAL		BEAMTHRO
C
      INTEGER		VSADD, IADD, WTADD, UADD, VADD, DATADD,
     1			PBADD, TIADD, NPB
      CHARACTER*(SYSMXNAM)	SVIS, STRM2, TELESCOP
      CHARACTER*(SYSMXNAM)	AROUTINE, PROUTINE
C
      REAL		RADMAX, RCONST, BMX, BMY, TELDIAM, PBLEVEL
C
      REAL	PI
      PARAMETER	(PI=3.14159274101257)
C
      REAL		C(SYSMXDIM), PC(SYSMXDIM), CBT(SYSMXDIM)
      REAL		GAIN1, GAIN2
      DATA	C	/SYSMXDIM*0.0/
      DATA	CBT	/SYSMXDIM*0.0/
      DATA	PC	/SYSMXDIM*0.0/
C==================================================================
      IF (ERROR) GO TO 999
C
      CALL CRDGET (IMAGE, INAX, ITYPE, INAXIS, IRVAL, IRPIX, IDELT, 
     1   IROTA)
C
C Get all relevant PB information
C
      CALL HEDGETPB (IMAGE, VIS, VIS, .TRUE., TELESCOP, TELDIAM, 
     $   PBLEVEL, PROUTINE, AROUTINE, RADMAX, NPB, PBADD, RCONST, C, 
     $   BEAMTHRO, CBT, BMX, BMY)
C
C Get Images
C
      IADD = DATADD (IMAGE)
      TIADD = DATADD (TIMAGE)
      IF (INAXIS(1).EQ.1) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Can only DFTPB 2 or 3-D')
         GO TO 999
      ELSEIF (INAXIS(2).EQ.1) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Can only DFTPB 2 or 3-D')
         GO TO 999
      ELSEIF (INAXIS(3).EQ.1) THEN
C
C Get UV data
C
      SVIS = STRM2 (VIS, CLASS)
      CALL CRDGET (SVIS, VNAX, VTYPE, VNAXIS, VRVAL, VRPIX, VDELT, 
     1   VROTA)
C
      CALL DATGETAR (STRM2(SVIS, 'VIS'), VNAX, VNAXIS, VATYPE,
     1   VSADD)
      WTADD =  DATADD (STRM2(SVIS, 'WT'))
      UADD = DATADD (STRM2(VIS, 'UU'))
      VADD = DATADD (STRM2(VIS, 'VV'))
C
C Do we need to do beam switching?
C
         IF (BEAMTHRO) THEN
            GAIN1 = 1.0
            GAIN2 = -1.0
            IF (SYSDEBUG) THEN
               WRITE (MESSAGE, 9374) C(1), C(2), CBT(1), CBT(2)
 9374          FORMAT ('IMGDFTPB: X1,Y1,  X2,Y2 ', 4F10.2)
               CALL MSGPUT (MESSAGE, 'D')
            ENDIF
C
            CALL IMGDFTS2 (MEMX(VSADD), VNAXIS(1), MEMR(WTADD), 
     1         MEMR(UADD), MEMR(VADD), MEMR(IADD), MEMR(TIADD),
     2         INAXIS(1), INAXIS(2), IRPIX(1), IDELT(1), IRPIX(2), 
     3         IDELT(2), C(1), C(2), CBT(1), CBT(2), BMX, BMY, 
     $         GAIN1, NPB, MEMR(PBADD), RADMAX,
     $         GAIN2, NPB, MEMR(PBADD), RADMAX)
         ELSE
            CALL IMGDFTP2 (MEMX(VSADD), VNAXIS(1), MEMR(WTADD), 
     1         MEMR(UADD), MEMR(VADD), MEMR(IADD), MEMR(TIADD),
     2         INAXIS(1), INAXIS(2), IRPIX(1), IDELT(1), IRPIX(2), 
     3         IDELT(2), C(1), C(2), BMX, BMY, NPB, MEMR(PBADD),
     4         RADMAX)
         ENDIF
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Can only DFTPB 2 or 3-D')
         GO TO 999
      END IF
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
