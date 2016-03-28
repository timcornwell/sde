C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgpbave.f	1.4	 7/16/92
C
      SUBROUTINE IMGPBAVE (VISROOT, IMAGE, VP, TELDIAM)
C
CD From a VP for each antenna, we make an average PB
C
C	VISROOT	CH*(*)	input	Name of Mosaic Root Dir (add PB here)
C	IMAGE	CH*(*)	in/out	Template for PB Image,
C				returned as the average PB
C	VP	CH*(*)	input	Directory for Voltage Patterns
C	TELDIAM	REAL	input	Telescope Diameter
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A. Holdaway	Feb 28 1991
C	Average VP is now output in IMAGE
C				M.A.Holdaway	May 2 1991	
C	Ooops!  HGEOM now keeps its old output images around,
C	set them to zero if they exist
C				M.A.Holdaway	Dec 2 1991
C	Track change to PIXPGGRF
C				D.S.Briggs	July 15 1992
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	IMAGE, VISROOT, VP
      REAL		TELDIAM
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGPBAVE')
C
      INTEGER		INAX, INAXIS(SYSMXDIM)
      INTEGER		VPNAX, VPNAXIS(SYSMXDIM)
      INTEGER		NAXIS(SYSMXDIM), NAX
      CHARACTER*8	ITYPE(SYSMXDIM)
      REAL		IRPIX(SYSMXDIM), IDELT(SYSMXDIM),
     1			IROTA(SYSMXDIM)
      DOUBLE PRECISION	IRVAL(SYSMXDIM)
      CHARACTER*8	TYPE(SYSMXDIM)
      REAL		RPIX(SYSMXDIM), DELT(SYSMXDIM),
     1			ROTA(SYSMXDIM)
      DOUBLE PRECISION	RVAL(SYSMXDIM)
      LOGICAL		DATEXIST
      INTEGER 		NDUMMY, CRDRNAX, RNAX
      INTEGER		IADD, DATADD, VPADD, XADD,
     $   		PB1ADD, NPIX, IANT, NANT, R2ADD, I2ADD,
     $   		PB2ADD, NPB, I
      REAL		RADMAX, BMX, BMY, RCONST, FREQ
      CHARACTER*(SYSMXNAM)	STRM2
C
      REAL		RTOA
C
      REAL	PI
      PARAMETER	(PI=3.14159274101257)
C
      REAL		DATFGETR      
      CHARACTER*6	STRINT
      
      DOUBLE PRECISION	OBSRA, OBSDEC
      DATA		RVAL /SYSMXDIM * 0.D0/
      DATA		RPIX /SYSMXDIM * 1.0/
C==================================================================
      IF (ERROR) GO TO 999
      RTOA = 180.0 * 3600.0 / PI
C
C Get image characteristics
C
      CALL CRDGET (IMAGE, INAX, ITYPE, INAXIS, IRVAL, IRPIX, IDELT, 
     1   IROTA)
      CALL CRDGET (IMAGE, NAX, TYPE, NAXIS, RVAL, RPIX, DELT, 
     1   ROTA)
      CALL DATPUTD (IMAGE, 'OBSRA', IRVAL(1), 1)
      CALL DATPUTD (IMAGE, 'OBSDEC', IRVAL(2), 1)
      IADD = DATADD (IMAGE)
      RNAX = CRDRNAX(INAX, INAXIS)
      IF (RNAX .NE. 2) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Can only DFTVP 2-D')
         GO TO 990
      ENDIF
C
C Deal with voltage patterns
C We need to HGEOM the VP's to the proper cellsize
C
      IF (DATEXIST('VPSCRATCH'))  CALL DATDELET ('VPSCRATCH')
      CALL DATCREAT ('VPSCRATCH')
      CALL DATGETI (VP, 'NANT', NANT, 1, NDUMMY)
      CALL DATMAKAR ('VPSCRATCH/AVEPB', INAX, INAXIS, 'R', PB1ADD)
      VPNAX = 3
      VPNAXIS(1) = INAXIS(1)
      VPNAXIS(2) = INAXIS(2)
      VPNAXIS(3) = NANT
      CALL DATMAKAR ('VPSCRATCH/VPARR', VPNAX, VPNAXIS, 'X', VPADD)
      NPIX = VPNAXIS(1) * VPNAXIS(2)
      DO 200 IANT = 1, NANT
         CALL IMGREAL (STRM2(VP, 'ANT'//STRINT(IANT)), 
     $      'VPSCRATCH/R1')
         CALL IMGIMAG (STRM2(VP, 'ANT'//STRINT(IANT)), 
     $      'VPSCRATCH/I1')
         CALL DATPUTL ('VPSCRATCH/R1', 'ROTATABLE', .FALSE., 1)
         CALL DATPUTL ('VPSCRATCH/I1', 'ROTATABLE', .FALSE., 1)
         RPIX(1) = INAXIS(1)/2
         RPIX(2) = INAXIS(2)/2
         CALL DATPUTR (IMAGE, 'CRPIX', RPIX, SYSMXDIM)
         CALL DATPUTD ('VPSCRATCH/R1', 'CRVAL', RVAL, SYSMXDIM)
         CALL DATPUTD ('VPSCRATCH/I1', 'CRVAL', RVAL, SYSMXDIM)
         IF (DATEXIST('VPSCRATCH/R2')) THEN
            CALL ARRSETCO ('VPSCRATCH/R2', 0.0, 0.0)
            CALL ARRSETCO ('VPSCRATCH/I2', 0.0, 0.0)
         ENDIF
         CALL IMGHGEOM ('VPSCRATCH/R1', IMAGE, 'VPSCRATCH/R2', 'S')
         CALL IMGHGEOM ('VPSCRATCH/I1', IMAGE, 'VPSCRATCH/I2', 'S')
         R2ADD = DATADD( 'VPSCRATCH/R2' )
         I2ADD = DATADD( 'VPSCRATCH/I2' )
         CALL PIXQU2X (NPIX, MEMX(VPADD + (IANT-1)*NPIX), 
     $      MEMR(R2ADD), MEMR(I2ADD) )
 200  CONTINUE
C
C Set up PB array:  we may want to go further out than 2nd null
C
      NPB = 8192
      FREQ = RVAL(3)
      RCONST = RTOA * (3E8/FREQ)/(PI * TELDIAM)
      BMX = (3600.0*DELT(1)/RCONST)**2
      BMY = (3600.0*DELT(2)/RCONST)**2
      IF (DATEXIST(STRM2(VISROOT, 'PB'))) CALL 
     $   DATDELET (STRM2(VISROOT, 'PB'))
      CALL DATMAKAR (STRM2(VISROOT, 'PB'), 1, NPB, 'R', PB2ADD)
      CALL DATPUTR  (STRM2(VISROOT, 'PB'), 'TELDIAM', TELDIAM, 1)
C
C Make all possible multiplications V(i) . V(i)*, average, take absolute value
C
      CALL IMGPBAV2 ( MEMX(VPADD), NANT, MEMR(PB1ADD), INAXIS(1), 
     $   INAXIS(2), IRPIX(1), IRPIX(2), IDELT(1), IDELT(2), 
     $   RADMAX, RCONST, MEMR(PB2ADD),  NPB)
      CALL DATPUTR (STRM2(VISROOT, 'PB'), 'RADMAX', RADMAX, 1)
C
      CALL ARRCOPY  ('VPSCRATCH/AVEPB', IMAGE)
      CALL DATDELET ('VPSCRATCH')
Cvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
      CALL DATMAKAR (STRM2(VISROOT, 'X'), 1, NPB, 'R', XADD)
      DO 400 I = 0, NPB-1
         MEMR( XADD + I ) = I
 400  CONTINUE
      CALL PIXPGGRF (NPB, MEMR(XADD), MEMR(PB2ADD), 0.0, 8192., 
     $   0.0, 1.0, '/xw', 'X', 'PB', 'PB TEST', 0, 0)
C^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
