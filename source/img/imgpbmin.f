C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgpbmin.f	1.3	 5/17/91
C
      SUBROUTINE IMGPBMIN (IN, OUT, VNAXIS, VVBLC)
C
CD From IN, generate the smallest image OUT which includes the entire PB
C
C IN needs to have OBSRA, OBSDEC, and TELESCOP set correctly
C
C	IN	CH*(*)	input	Name of IN directory 
C	OUT	CH*(*)	input	Name of minimal image directory
C	VNAXIS	INT(*)	output	Virtual OUT size:
C				maximum size needed for PB
C	VVBLC	INT(*)	output	BLC of OUT in the Virtual Image
C
C	A tricky point (which means I must be doing things the wrong way):
C	a) No Pixels in 
C	 
C 
C Audit trail: 
C	Original version: Audit trail comments go on this line 
C	and successive lines
C				M.A.Holdaway	Dec 17 1990
C	Implemented HEDGETPB to get all PB info and make PB array
C				M.A.Holdaway	March 28 1991
C	Added BEAMTHROW parameters to HEDGETPB
C				M.A.Holdaway	May 2 1991
C	Fixed bug: NAXIS was undefined
C				M.A. Holdaway	May 17 1991
C-----------------------------------------------------------------------
#include		"stdinc.h"
      CHARACTER*(*)	IN, OUT
      INTEGER		VNAXIS(*), VVBLC(*)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGPBMIN')
C
      INTEGER		TRC(SYSMXDIM), BLC(SYSMXDIM)
      INTEGER		VTRC(SYSMXDIM), VBLC(SYSMXDIM)
      INTEGER		VBTTRC(SYSMXDIM), VBTBLC(SYSMXDIM)
      CHARACTER*(SYSMXNAM)	PROUTINE, AROUTINE, TELESCOP
      CHARACTER*1	ATYPE
      REAL		TELDIAM, PBLEVEL, RCONST, RADMAX, BMX, BMY
      INTEGER		NAXIS(SYSMXDIM), NAX
      INTEGER		NPB, PBADD, ADD
C
      REAL		C(SYSMXDIM), CBT(SYSMXDIM)
C
      LOGICAL		BEAMTHRO
C
      DATA		TRC /SYSMXDIM * 1/
      DATA		BLC /SYSMXDIM * 1/
      DATA		VTRC /SYSMXDIM * 1/
      DATA		VBLC /SYSMXDIM * 1/
      DATA		VBTTRC /SYSMXDIM * 1/
      DATA		VBTBLC /SYSMXDIM * 1/
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
C Get all relevent PB information
C
      CALL HEDGETPB (IN, IN, IN, .TRUE., TELESCOP, TELDIAM, PBLEVEL, 
     $   PROUTINE, AROUTINE, RADMAX, NPB, PBADD, RCONST, C, 
     $   BEAMTHRO, CBT, BMX, BMY)
C
      CALL DATGETAR (IN, NAX, NAXIS, ATYPE, ADD)
C
C Determine the support required for the PB:
C a) The Virtual Image (can go outside of IN image)
C
      VTRC(1) = NINT(C(1) + RADMAX/SQRT(BMX)) + 1
      VTRC(2) = NINT(C(2) + RADMAX/SQRT(BMY)) + 1
      VBLC(1) = NINT(C(1) - RADMAX/SQRT(BMX)) - 1
      VBLC(2) = NINT(C(2) - RADMAX/SQRT(BMY)) - 1
C
C Do we need to consider beam switching?
C
      IF (BEAMTHRO) THEN
         VBTTRC(1) = NINT(CBT(1) + RADMAX/SQRT(BMX)) + 1
         VBTTRC(2) = NINT(CBT(2) + RADMAX/SQRT(BMY)) + 1
         VBTBLC(1) = NINT(CBT(1) - RADMAX/SQRT(BMX)) - 1
         VBTBLC(2) = NINT(CBT(2) - RADMAX/SQRT(BMY)) - 1
C
         VTRC(1) = MAX ( VTRC(1), VBTTRC(1) )
         VTRC(2) = MAX ( VTRC(2), VBTTRC(2) )
         VBLC(1) = MIN ( VBLC(1), VBTBLC(1) )
         VBLC(2) = MIN ( VBLC(2), VBTBLC(2) )
      ENDIF

      If (MOD(VTRC(1) - VBLC(1)+1, 2).NE. 0) THEN
         IF (VTRC(1) .LT. NAXIS(1) ) THEN
            VTRC(1) = VTRC(1) + 1
         ELSE
            VBLC(1) = VBLC(1) - 1
         ENDIF
      ENDIF
      VNAXIS(1) = VTRC(1) - VBLC(1) + 1
      IF (MOD(VTRC(2) - VBLC(2)+1, 2).NE. 0) THEN
         IF (VTRC(2) .LT. NAXIS(2) ) THEN
            VTRC(2) = VTRC(2) + 1
         ELSE
            VBLC(2) = VBLC(2) - 1
         ENDIF
      ENDIF
      VNAXIS(2) = VTRC(2) - VBLC(2) + 1
C
C b) Determine the corners of the REAL subimage of IN to use
C
      BLC(1) = MAX (VBLC(1), 1)
      BLC(2) = MAX (VBLC(2), 1)
      TRC(1) = MIN (VTRC(1), NAXIS(1))
      TRC(2) = MIN (VTRC(2), NAXIS(2))
      IF (MOD(TRC(1) - BLC(1)+1, 2).NE. 0) THEN
         IF (TRC(1) .LT. NAXIS(1) ) THEN
            TRC(1) = TRC(1) - 1
         ELSE
            BLC(1) = BLC(1) + 1
         ENDIF
      ENDIF
      IF (MOD(TRC(2) - BLC(2)+1, 2).NE. 0) THEN
         IF (TRC(2) .LT. NAXIS(2) ) THEN
            TRC(2) = TRC(2) - 1
         ELSE
            BLC(2) = BLC(2) + 1
         ENDIF
      ENDIF
C
C c) Determine relationship between the Virtual Image and OUT
C
      VVBLC(1) = BLC(1) - VBLC(1) + 1
      VVBLC(2) = BLC(2) - VBLC(2) + 1
C
C Finally make the OUT image with BLC, TRC
C
      CALL DATCREAT ('PBwindow')
      CALL DATPUTI('PBwindow', 'TRC', TRC, SYSMXDIM)
      CALL DATPUTI('PBwindow', 'BLC', BLC, SYSMXDIM)
C
      CALL IMGSUBSE (IN, OUT, 'PBwindow')
      CALL DATDELET ('PBwindow')
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
