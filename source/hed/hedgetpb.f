C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)hedgetpb.f	1.15	 7/18/97
C
      SUBROUTINE HEDGETPB (IMAGE, IMGTEL, IMGOBS, DOPB, TELESCOP, 
     $   TELDIAM, PBLEVEL, PROUTINE, AROUTINE, RADMAX, NPB, PBADD,
     $   RCONST, C, BEAMTHRO, CBT, BMX, BMY)
C
CD Given IN, Gets ALL PB or VP atributes. Even makes the PB and passes ADD
C
C	IMAGE		CH*(*)	in	Name of Image to apply PB to
C	IMGTEL		CH*(*)	in	Name of Image to pull Telescope Name
C	IMGOBS		CH*(*)	in	Name of Image to pull OBSRA, OBSDEC
C	DOPB		LOGICAL	in	Make PB?  Else make VP
C	TELESCOP	CH*(*)	out	Name of telescop
C	TELDIAM		REAL	out	Diameter of telescope
C	PBLEVEL		REAL	out	Level of error in PB
C	PROUTINE	CH*(*)	out	Name of PIX routine to apply PB
C	AROUTINE	CH*(*)	out	ARR routine which generates PB
C	RADMAX		REAL	out	maximum radius in normalized units
C	NPB		INT	out	number of elements in PB array
C	RCONST		REAL	out	scaling factor
C	C		REAL	out	PIXEL at which PB should be applied
C	BEAMTHRO	LOG	out	Do a BEAMTHROW?
C	CBT		REAL	out	PIXEL at which Beam Throw PB 
C					should be applied
C	BMX		REAL	out	scaling factor
C Audit trail:
C
C	New Feature: "CANNEDPB".  If TELESCOP is a directory and
C	STRM2(TELESCOP, 'PB') exists, get the PB from that array.
C				M.A.Holdaway	April 2 1991
C	Added Beam Switching, CBT
C				M.A.Holdaway	May 2 1991
C	Understands ATMOSPHERE (RX, RY, RZ)
C				M.A.Holdaway	May 13 1991
C	Three Directories for header items:
C		IMAGE	basic image for CRDWTOP, etc
C		IMGTEL	get the TELESCOP, TELDIAM from this
C		IMGOBS	get OBSDEC, OBSRA from this
C		Any/All can be the same or different.  For example,
C		VISDFTPB has IMAGE='IMAGE', IMGTEL='VIS', IMGOBS='IMAGE'
C				M.A.Holdaway	May 16 1991
C	Added telescop type AIRY1, an airy disk out to the first null
C				M.A.Holdaway	May 17 1991
C	Added NMA type telescope
C				M.A.Holdaway	June 13 1991
C	Changed VLA: now the VLA PB is dealt with in the same manner
C	as the other telescopes
C	NOTE THAT THIS CHANGE IS NOT RECORDED IN SCCS:
C	checked out by B. Payne 91/06/22
C				M.A.Holdaway	Aug 12 1991
C	Now understands AIRYB1
C				M.A.Holdaway	Sep 10 1991
C	B. PAYNE CHECKED HEDGETPB IN WITH NO CHANGES
C       Now changes are recorded by SCCS
C				M.A.Holdaway	Jan 1 1992
C	Added GAUS Telescop type.  Actually, just a GAUS PB.
C				M.A.Holdaway	Sept 16 1992
C	Added OVRO Telescope type
C				M.A.Holdaway	March 9 1993
C	Added CSO		M.A.Holdaway	March 30 1993
C---------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	IMAGE, IMGTEL, IMGOBS, AROUTINE, PROUTINE
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'HEDGETPB')
C
      CHARACTER*(*)	TELESCOP
      INTEGER		NDUMMY, I, STRSEARC
      INTEGER		NPB, PBADD, PBNAX, PBNAXIS(SYSMXDIM)
      LOGICAL		DATEXIST, DOPB
      REAL		BMX, BMY, RTOA, RCONST, TELDIAM
      CHARACTER*8	CTYPE(SYSMXDIM), ATYPE(SYSMXDIM)
      REAL		CDELT(SYSMXDIM)
      DOUBLE PRECISION	FREQ, CRVAL(SYSMXDIM), DTMP, DTMP2
      CHARACTER*1	PBTYPE
      INTEGER		ANAX, ANAXIS(SYSMXDIM), ATMX, ATMY      
C
      REAL		RADMAX
C
      REAL	PI
      PARAMETER	(PI=3.14159274101257)
C
      REAL		C(*), CBT(*), PC(SYSMXDIM), HEIGHT, RTOD
C
      LOGICAL		NEW, BEAMTHRO, ATMOSPHE
      REAL		PBLEVEL
      CHARACTER*(SYSMXNAM)	STRM2
      DATA	PC	/SYSMXDIM*0.0/
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      PROUTINE = "PIXPB2D"
      NEW = .FALSE.
      TELDIAM = 0.0
      PBLEVEL = 0.
      RTOA = 180.0 * 3600.0 / PI
      RTOD = 180.0 / PI
C
      IF (DATEXIST (STRM2(IMGTEL, 'TELESCOP'))) THEN
         CALL DATGETC (IMAGE, 'TELESCOP', TELESCOP, 1, NDUMMY)
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Telescope not known')
         GO TO 999
      END IF
C
      CALL DATGETD (IMAGE, 'CRVAL', CRVAL, SYSMXDIM, NDUMMY)
      CALL DATGETC (IMAGE, 'CTYPE', CTYPE, SYSMXDIM, NDUMMY)
      IF (ERROR) GO TO 990
      I = STRSEARC ('FREQ', CTYPE, NDUMMY)
      IF (I.EQ.0) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'No frequency axis')
         GO TO 999
      ELSE
         FREQ = CRVAL(I)
      END IF
C
C Get the target pixel
C Two ways: one for IMAGE, one for ATMOSPHERE
C
      CALL DATGETC (IMAGE, 'CTYPE', CTYPE, SYSMXDIM, NDUMMY)
      IF (ERROR) GO TO 990
      I = STRSEARC ('RX------', CTYPE, NDUMMY)
      IF (I.EQ.0) THEN
         ATMOSPHE = .FALSE.
         IF (ERROR) GO TO 990
         CALL DATGETD (IMGOBS, 'OBSRA', DTMP, 1, NDUMMY)
         IF (ERROR) THEN
            CALL ERRCANCE
            PC(1) = CRVAL(1)
            CALL DATPUTD (IMGOBS, 'OBSRA', CRVAL(1), 1)
         ELSE
            PC(1) = DTMP
         END IF
         CALL DATGETD (IMGOBS, 'OBSDEC', DTMP, 1, NDUMMY)
         IF (ERROR) THEN
            CALL ERRCANCE
            PC(2) = CRVAL(2)
            CALL DATPUTD (IMGOBS, 'OBSDEC', CRVAL(2), 1)
         ELSE
            PC(2) = DTMP
         END IF
         CALL CRDWTOP (IMAGE, PC, C)
      ELSE
         ATMOSPHE = .TRUE.
         CALL DATGETAR (STRM2(IMAGE, 'AtmoX'), ANAX, ANAXIS, ATYPE, 
     $      ATMX)
         CALL DATGETAR (STRM2(IMAGE, 'AtmoY'), ANAX, ANAXIS, ATYPE,
     $      ATMY)
         C(1) = MEMR (ATMX)
         C(2) = MEMR (ATMY)
      ENDIF
C
C Check for beam switching
C
      IF (DATEXIST (STRM2(IMAGE, 'THROWRA'))) THEN
         BEAMTHRO =  .TRUE.
         CALL DATGETD (IMAGE, 'THROWRA', DTMP, 1, NDUMMY)
         PC(1) = DTMP
         CALL DATGETD (IMAGE, 'THROWDEC', DTMP2, 1, NDUMMY)
         PC(2) = DTMP2
         CALL CRDWTOP (IMAGE, PC, CBT)
      ELSE
         BEAMTHRO  = .FALSE.
      ENDIF
C
C If "ATMOSPHERE", then change the scale to angular size
C
      CALL DATGETR (IMAGE, 'CDELT', CDELT, SYSMXDIM, NDUMMY)
      IF (ATMOSPHE)  THEN
         CALL DATGETC (IMAGE, 'CTYPE', CTYPE, SYSMXDIM, NDUMMY)
         I = STRSEARC ('RZ------', CTYPE, NDUMMY)
         CALL DATGETD (IMAGE, 'CRVAL', CRVAL, SYSMXDIM, NDUMMY)
         HEIGHT = CRVAL(I)
         CDELT(1) = CDELT(1) / HEIGHT * RTOD
         CDELT(2) = CDELT(2) / HEIGHT * RTOD
      ENDIF
      IF (ERROR) GO TO 990
C
C Figure out what to do for our telescope; add new telescopes here
C
      IF (TELESCOP(1:3).EQ.'VLA') THEN
         TELDIAM = 25.0
         IF (DOPB) THEN
            AROUTINE = 'ARRVLAPB'
         ELSE
            CALL ERRREPOR (ERRBDARG, ROUTINE,
     $         'No VP information on VLA')
            GOTO 990
         ENDIF
C
      ELSE IF (TELESCOP(1:4) .EQ. 'VLBA') THEN
         TELDIAM = 25.0
         IF (DOPB) THEN
            AROUTINE = 'ARRVLBAP'
         ELSE
            CALL ERRREPOR (ERRBDARG, ROUTINE,
     $         'No VP information on VLA')
            GOTO 990
         ENDIF
C
      ELSE IF (TELESCOP(1:4) .EQ. 'GAUS') THEN
         CALL DATGETR(IMGTEL, 'TELDIAM', TELDIAM, 1, NDUMMY)
         TELDIAM = TELDIAM * .5300
         IF (DOPB) THEN
            AROUTINE = 'ARRGPB'
         ELSE
            AROUTINE = 'ARRGVP'
         ENDIF
      ELSE IF (TELESCOP(1:3) .EQ. 'CSO') THEN
         TELDIAM = 8.84 * .5300
         IF (DOPB) THEN
            AROUTINE = 'ARRGPB'
         ELSE
            AROUTINE = 'ARRGVP'
         ENDIF
      ELSE IF (TELESCOP(1:4) .EQ. 'BIMA') THEN
         TELDIAM = 6.1/2.438
         IF (DOPB) THEN
            AROUTINE = 'ARRGPB'
         ELSE
            AROUTINE = 'ARRGVP'
         ENDIF
      ELSE IF (TELESCOP(1:3) .EQ. 'NMA') THEN
         TELDIAM = 10.0
         IF (DOPB) THEN
            AROUTINE = 'ARRAIRYB'
         ELSE
            AROUTINE = 'ARRAIRBV'
         ENDIF
      ELSE IF (TELESCOP(1:4) .EQ. 'OVRO') THEN
         TELDIAM = 10.4
         IF (DOPB) THEN
            AROUTINE = 'ARRAIRYB'
         ELSE
            AROUTINE = 'ARRAIRBV'
         ENDIF
C
      ELSE IF (TELESCOP(1:3) .EQ. '12M') THEN
         TELDIAM = 12.0
         IF (DOPB) THEN
            AROUTINE = 'ARRAIRYB'
         ELSE
            AROUTINE = 'ARRAIRBV'
         ENDIF
      ELSE IF (TELESCOP(1:2) .EQ. 'RS') THEN
         TELDIAM = 12.0
         IF (DOPB) THEN
            AROUTINE = 'ARRAIRYB'
         ELSE
            AROUTINE = 'ARRAIRBV'
         ENDIF
      ELSE IF (TELESCOP(1:4) .EQ. 'MMA8') THEN
         TELDIAM = 8.0
         IF (DOPB) THEN
            AROUTINE = 'ARRAIRYB'
         ELSE
            AROUTINE = 'ARRAIRBV'
         ENDIF
      ELSE IF (TELESCOP(1:3) .EQ. 'MMA') THEN
         TELDIAM = 7.5
         IF (DOPB) THEN
            AROUTINE = 'ARRAIRYB'
         ELSE
            AROUTINE = 'ARRAIRBV'
         ENDIF
      ELSE IF (TELESCOP(1:2) .EQ. 'SI') THEN
	 TELDIAM = .20
         IF (DOPB) THEN
            AROUTINE = 'ARRAIRY'
         ELSE
            AROUTINE = 'ARRAIRV'
         ENDIF
C
      ELSE IF (TELESCOP(1:6) .EQ. 'AIRYB1') THEN
         CALL DATGETR(IMGTEL, 'TELDIAM', TELDIAM, 1, NDUMMY)
         IF (DOPB) THEN
            AROUTINE = 'ARRAIRB1'
         ELSE
            AROUTINE = 'NONE'
         ENDIF
      ELSE IF (TELESCOP(1:5) .EQ. 'AIRY1') THEN
         CALL DATGETR(IMGTEL, 'TELDIAM', TELDIAM, 1, NDUMMY)
         IF (DOPB) THEN
            AROUTINE = 'ARRAIRY1'
         ELSE
            AROUTINE = 'ARRAIRV1'
         ENDIF
      ELSE IF (TELESCOP(1:5) .EQ. 'AIRYB') THEN
         CALL DATGETR(IMGTEL, 'TELDIAM', TELDIAM, 1, NDUMMY)
         IF (DOPB) THEN
            AROUTINE = 'ARRAIRYB'
         ELSE
            AROUTINE = 'ARRAIRBV'
         ENDIF
      ELSE IF (TELESCOP(1:4) .EQ. 'AIRY') THEN
         CALL DATGETR(IMGTEL, 'TELDIAM', TELDIAM, 1, NDUMMY)
         IF (DOPB) THEN
            AROUTINE = 'ARRAIRY'
         ELSE
            AROUTINE = 'ARRAIRV'
         ENDIF
      ELSE IF (DATEXIST (STRM2(TELESCOP, 'PB'))) THEN
         CALL DATGETR(STRM2(TELESCOP, 'PB'), 'TELDIAM', TELDIAM, 1, 
     $      NDUMMY)
         AROUTINE = 'CANNEDPB'
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $      'Cannot treat Telescope')
         GOTO 990
      ENDIF
C
C Make or Get the Primary Beam or the Voltage Pattern;
C Add new PB or VP types here
C
      IF (AROUTINE .EQ. 'ARRVLAPB') THEN
         CALL ARRVLAPB (NPB, RADMAX, PBADD, NEW)
      ELSE IF (AROUTINE .EQ. 'ARRVLBAP') THEN
         CALL ARRVLBAP (NPB, RADMAX, PBADD, NEW)
      ELSE IF (AROUTINE .EQ. 'ARRGPB') THEN
         CALL ARRGPB (NPB, RADMAX, PBADD, NEW)
      ELSE IF (AROUTINE .EQ. 'ARRAIRYB') THEN
         CALL ARRAIRYB (NPB, RADMAX, PBADD, NEW)
      ELSE IF (AROUTINE .EQ. 'ARRAIRB1') THEN
         CALL ARRAIRB1 (NPB, RADMAX, PBADD, NEW)
      ELSE IF (AROUTINE .EQ. 'ARRAIRY') THEN
         CALL ARRAIRY (NPB, RADMAX, PBADD, NEW)
      ELSE IF (AROUTINE .EQ. 'ARRAIRY1') THEN
         CALL ARRAIRY1 (NPB, RADMAX, PBADD, NEW)
C
      ELSE IF (AROUTINE .EQ. 'ARRGVP') THEN
         CALL ARRGVP (NPB, RADMAX, PBADD)
      ELSE IF (AROUTINE .EQ. 'ARRAIRBV') THEN
         CALL ARRAIRBV (NPB, RADMAX, PBADD)
      ELSE IF (AROUTINE .EQ. 'ARRAIRV') THEN
         CALL ARRAIRV (NPB, RADMAX, PBADD)
      ELSE IF (AROUTINE .EQ. 'ARRAIRV1') THEN
         CALL ARRAIRV1 (NPB, RADMAX, PBADD)
C
      ELSE IF (AROUTINE .EQ. 'CANNEDPB')  THEN
         CALL DATGETAR (STRM2(TELESCOP, 'PB'), PBNAX, PBNAXIS, PBTYPE,
     $      PBADD)
         IF (PBTYPE .NE. 'R') THEN
            CALL ERRREPOR (ERRBDARG, ROUTINE, 'Canned PB is not REAL')
            GOTO 990
         ENDIF
         IF (PBNAXIS(2) .GT. 1) THEN
            CALL ERRREPOR (ERRBDARG, ROUTINE, 'Canned PB is not 1-D')
            GOTO 990
         ENDIF            
         NPB = PBNAXIS(1)
         CALL DATGETR (STRM2(TELESCOP, 'PB'), 'RADMAX', RADMAX, 1, 
     $      NDUMMY)
C
      ELSE IF (AROUTINE .NE. 'NONE')   THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $      'Unknown PB ARRAY ROUTINE ')
         GOTO 990
      ENDIF
C
      IF (NEW .AND. DOPB) THEN
         IF (DATEXIST (STRM2(IMGTEL, 'PBLEVEL'))) THEN
            CALL DATGETR (IMAGE, 'PBLEVEL', PBLEVEL, 1, NDUMMY)
            CALL PIXPBCOR (NPB, MEMR(PBADD), RADMAX, PBLEVEL)
         ENDIF
      ENDIF
C
      IF (TELDIAM .LE. 0.0) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $      'Non-positive telescope diameter !')
         GOTO 990
      ENDIF
      RCONST = RTOA * (3E8/FREQ)/(PI * TELDIAM)
      BMX = (3600.0*CDELT(1)/RCONST)**2
      BMY = (3600.0*CDELT(2)/RCONST)**2
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END



