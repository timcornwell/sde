C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)visdftpb.f	1.4    5/16/91
C
      SUBROUTINE VISDFTPB (VIS, CLASS, IMAGE)
C
CD Direct Fourier transform to Image from Vis. data after applying
C the appropriate primary beam.
C
C
C	VIS		CH*(*)	input	Name of visibility data
C	CLASS		CH*(*)	input	Class of visibility data
C	IMAGE		CH*(*)	input	Name of Image
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Beam Switches Between Arbitrary Beams
C				M.A.Holdaway	Jan 10 1991
C	Utilizes HEDGETPB to get PB parameters
C				M.A.Holdaway	May 2 1991
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	IMAGE, VIS, CLASS
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISDFTPB')
C
      REAL		SUMWT
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
      INTEGER		NPB
      INTEGER		VSADD, IADD, WTADD, UADD, VADD, DATADD, PBADD
      CHARACTER*(SYSMXNAM)	SVIS, STRM2, TELESCOP, AROUTINE, 
     $   			PROUTINE
C
      REAL		RADMAX, RCONST, BMX, BMY, TELDIAM, PBLEVEL
C
      REAL	PI
      PARAMETER	(PI=3.14159274101257)
C
      REAL		C(SYSMXDIM), PC(SYSMXDIM), CBT(SYSMXDIM),
     $   		GAIN1, GAIN2
      DATA	C	/SYSMXDIM*0.0/
      DATA	CBT	/SYSMXDIM*0.0/
      DATA	PC	/SYSMXDIM*0.0/
C==================================================================
      IF (ERROR) GO TO 999
C
      CALL CRDGET (IMAGE, INAX, ITYPE, INAXIS, IRVAL, IRPIX, IDELT, 
     1   IROTA)
C
C Get all relevent PB information
C
      CALL HEDGETPB (IMAGE, VIS, IMAGE, .TRUE., TELESCOP, TELDIAM, 
     $   PBLEVEL, PROUTINE, AROUTINE, RADMAX, NPB, PBADD, RCONST, C, 
     $   BEAMTHRO, CBT, BMX, BMY)
C
C Now get UV data
C
      SVIS = STRM2 (VIS, CLASS)
      CALL CRDGET (SVIS, VNAX, VTYPE, VNAXIS, VRVAL, VRPIX, VDELT, 
     1   VROTA)
C
      CALL DATGETAR (STRM2(SVIS, 'VIS'), VNAX, VNAXIS, VATYPE,
     1   VSADD)
      WTADD =  DATADD (STRM2(SVIS, 'WT'))
      IADD = DATADD (IMAGE)
C
      IF (INAXIS(1).EQ.1) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Can only DFTPB 2 or 3-D')
         GO TO 999
      ELSEIF (INAXIS(2).EQ.1) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Can only DFTPB 2 or 3-D')
         GO TO 999
      ELSEIF (INAXIS(3).EQ.1) THEN
C
C ***********************  Two dimensions *************************
C
         UADD = DATADD (STRM2(VIS, 'UU'))
         VADD = DATADD (STRM2(VIS, 'VV'))
C
C Do we need to beam switch?
C
         IF (BEAMTHRO) THEN
            GAIN1 = 1.0
            GAIN2 = -1.0
C
            CALL VISDFTS2 (MEMX(VSADD), VNAXIS(1), MEMR(WTADD), 
     1         MEMR(UADD), MEMR(VADD), MEMR(IADD), 
     2         INAXIS(1), INAXIS(2), IRPIX(1), IDELT(1), IRPIX(2), 
     3         IDELT(2), C(1), C(2), CBT(1), CBT(2), BMX, BMY, 
     $         GAIN1, NPB, MEMR(PBADD), RADMAX,
     $         GAIN2, NPB, MEMR(PBADD), RADMAX, SUMWT)
         ELSE
            CALL VISDFTP2 (MEMX(VSADD), VNAXIS(1), MEMR(WTADD), 
     1         MEMR(UADD), MEMR(VADD), MEMR(IADD), 
     2         INAXIS(1), INAXIS(2), IRPIX(1), IDELT(1), IRPIX(2), 
     3         IDELT(2), C(1), C(2), BMX, BMY, NPB, MEMR(PBADD),
     4         RADMAX, SUMWT)
            CALL DATPUTR (IMAGE, 'SUMWT', SUMWT, 1)
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
