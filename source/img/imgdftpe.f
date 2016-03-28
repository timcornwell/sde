C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgdftpe.f	1.4    5/16/91
C
      SUBROUTINE IMGDFTPE (VIS, CLASS, IMAGE, TELESCOP)
C
CD Direct Fourier transform to Image to Vis. data--pointing erros
C are taken into account via the symmetric voltage patterns
C
C
C	VIS		CH*(*)	input	Name of visibility data
C	CLASS		CH*(*)	input	Class of visibility data
C	IMAGE		CH*(*)	input	Name of Image
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Added individual antenna pointing errors, took away
C	the tapered image, used voltage patterns instead of
C	probary beam; hacked from IMGDFTPB
C				M.A.Holdaway	Sep 15 1989
C	Implemented HEDGETPB to get all primary beam parameters
C				M.A.Holdaway	May 2 1991
C
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	IMAGE, VIS, CLASS
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGDFTPE')
C
      CHARACTER*1	VATYPE
      CHARACTER*8	VTYPE(SYSMXDIM)
      REAL		VRPIX(SYSMXDIM), VDELT(SYSMXDIM),
     1			VROTA(SYSMXDIM)
      DOUBLE PRECISION	VRVAL(SYSMXDIM)
      INTEGER		INAX, INAXIS(SYSMXDIM), ONAX, ONAXIS(SYSMXDIM)
      CHARACTER*1	OTYPE
      CHARACTER*8	ITYPE(SYSMXDIM)
      REAL		IRPIX(SYSMXDIM), IDELT(SYSMXDIM),
     1			IROTA(SYSMXDIM)
      DOUBLE PRECISION	IRVAL(SYSMXDIM)
      INTEGER		VNAX, VNAXIS(SYSMXDIM)
      LOGICAL		DATEXIST, BEAMTHRO
      REAL		C(SYSMXDIM), CBT(SYSMXDIM), PBLEVEL
      INTEGER		NVP
      INTEGER 		CRDRNAX, RNAX, NDUMMY
      INTEGER		VSADD, IADD, WTADD, UADD, VADD, DATADD,
     1			VPADD, TADD, BADD, ROADD, DOADD,
     $   		VP1ADD, VP2ADD
      CHARACTER*(SYSMXNAM)	SVIS, STRM2, TELESCOP,
     $   			AROUTINE, PROUTINE
C
      REAL		RADMAX, RCONST, BMX, BMY, TELDIAM
C
      REAL	PI
      PARAMETER	(PI=3.14159274101257)
C
      DOUBLE PRECISION	OBSRA, OBSDEC
C==================================================================
      IF (ERROR) GO TO 999
C
      CALL HEDGETPB (IMAGE, VIS, VIS, .FALSE., TELESCOP, TELDIAM, 
     $   PBLEVEL, PROUTINE, AROUTINE, RADMAX, NVP, VPADD, RCONST, C, 
     $   BEAMTHRO, CBT, BMX, BMY)
C
      IF (BEAMTHRO) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $     'Software cannot treat Beam Switching AND Pointing Errors')
         GOTO 990
      ENDIF
C
      CALL CRDGET (IMAGE, INAX, ITYPE, INAXIS, IRVAL, IRPIX, IDELT, 
     1   IROTA)
      RNAX = CRDRNAX(INAX, INAXIS)
      IF (ERROR) GO TO 990
      CALL DATGETD (IMAGE, 'OBSRA', OBSRA, 1, NDUMMY)
      CALL DATGETD (IMAGE, 'OBSDEC', OBSDEC, 1, NDUMMY)
      IF (ERROR) GOTO 990
      IADD = DATADD (IMAGE)
C
      IF (RNAX .EQ. 1) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Can only DFTPE 2-D')
         GO TO 999
      ELSEIF (RNAX .EQ. 3) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Can only DFTPE 2-D')
         GO TO 999
      ELSEIF (RNAX .EQ. 2) THEN
C
C ***********************  Two dimensions *************************
C
C Now get UV data
C
         SVIS = STRM2 (VIS, CLASS)
         CALL CRDGET (SVIS, VNAX, VTYPE, VNAXIS, VRVAL, VRPIX, VDELT, 
     1      VROTA)
C
         CALL DATGETAR (STRM2(SVIS, 'VIS'), VNAX, VNAXIS, VATYPE,
     1      VSADD)
         WTADD =  DATADD (STRM2(SVIS, 'WT'))
         UADD = DATADD (STRM2(VIS, 'UU'))
         VADD = DATADD (STRM2(VIS, 'VV'))
         BADD = DATADD (STRM2(VIS, 'BASELINE'))
         TADD = DATADD (STRM2(VIS, 'TIME'))
         CALL DATGETAR (STRM2(VIS, 'RAOFF'), ONAX, ONAXIS, OTYPE, 
     $      ROADD)
         CALL DATGETAR (STRM2(VIS, 'DECOFF'), ONAX, ONAXIS, OTYPE,
     $      DOADD)
         WRITE(MESSAGE, '('' IMGDFTPE found '',I5,
     $      '' integration intervals '')') ONAXIS(2)
         CALL MSGPUT (MESSAGE, 'I')
         WRITE(MESSAGE, '('' IMGDFTPE found '',I5,
     $      '' ANTENNAS '')') ONAXIS(1)
         CALL MSGPUT (MESSAGE, 'I')
         IF (ERROR) GOTO 990
C
C        create work arrays for IMGDFTE2.F voltage patterns
C
         IF (.NOT. DATEXIST('VP1')) THEN
            CALL DATMAKAR ('VP1', INAX, INAXIS, 'X', VP1ADD)
            CALL DATMAKAR ('VP2', INAX, INAXIS, 'X', VP2ADD)
         ENDIF
C
         CALL IMGDFTE2 (MEMX(VSADD), VNAXIS(1), MEMR(WTADD), 
     1      MEMR(UADD), MEMR(VADD), MEMR(TADD), MEMR(BADD),
     $      MEMR(ROADD), MEMR(DOADD), ONAXIS(1), ONAXIS(2), MEMR(IADD),
     2      INAXIS(1), INAXIS(2), IRPIX(1), IDELT(1), IRPIX(2), 
     3      IDELT(2), OBSRA, OBSDEC, BMX, BMY, NVP, MEMR(VPADD),
     4      RADMAX, MEMX(VP1ADD), MEMX(VP2ADD), IMAGE )
C
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Can only DFTPE 2-D')
         GO TO 999
      END IF
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
