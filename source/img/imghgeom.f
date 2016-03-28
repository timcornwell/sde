C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imghgeom.f	1.9	 9/17/92
C
       SUBROUTINE IMGHGEOM (IN, MODEL, OUT, ORDER)
C++
CD Geometric transform of one image to geometry of the header of another.
C
C
C
C  IN        CH*(*)  input    Name of array to modify
C  MODEL     CH*(*)  input    Name of array that has model parameters
C  OUT       CH*(*)  input    Name of array that will hold the output
C  ORDER     CH*(*)  input    The interpolation order
C Audit trail:
C       Original code modeled after AIPS routine HGEOM.
C                               R.T.Duquet   March 15 1990
C	Don't delete output if it already exists
C                               T.J. Cornwell	March 13 1991
C       -Looks for PARANGLE in MODEL header.  If NOT ZERO, looks
C        for ROTATABLE in IN header and will rotate IN by PARANGLE
C	-Also changed a bit of the array checking logic
C				M.A. Holdaway	March 22 1991
C	Used DATEXIST to check for existence of PARANGLE
C				T.J. Cornwell   March 24 1991
C	HGEOM works correctly for images whose pixel values correspond to
C	surface brightness (JY/BEAM).  However, the image needs to be
C	rescaled if the units are JY/PIXEL
C
C-- ---------------------------------------------------------------------------
#include     "stdinc.h"
#include     "locat1.h"
#include     "locat2.h"
C
      CHARACTER*(*)     IN, MODEL, OUT, ORDER
C
      CHARACTER*(*)     ROUTINE
      PARAMETER         (ROUTINE = 'IMGHGEOM')
C
      CHARACTER*1       TYPE1, TYPE2, TYPE3
      CHARACTER         TYPE(SYSMXDIM)*8
      INTEGER           I, NAX, NAXIS(SYSMXDIM)
      INTEGER           IORDER, NDUMMY
      INTEGER           RNAX1, NAX1, ADD1, NAXIS1(SYSMXDIM)
      INTEGER           RNAX2, NAX2, ADD2, NAXIS2(SYSMXDIM)
      INTEGER           NAX3, ADD3, NAXIS3(SYSMXDIM)
      INTEGER           NBX4, NBY4, XADD, YADD, XI, YI
      REAL              EPOCH
      REAL              RPIX(SYSMXDIM), DELT(SYSMXDIM), ROTA(SYSMXDIM)
      REAL		PARANGLE, ODELT(SYSMXDIM), SCALE
      LOGICAL		ROTATABL
      DOUBLE PRECISION  RVAL(SYSMXDIM)
      CHARACTER*(SYSMXNAM)	STRM2, BUNIT
C
      LOGICAL           DATEXIST, DATFGETL
      REAL		DATFGETR
      INTEGER		CRDRNAX
C
      CHARACTER*4       ORDERSET
      DATA              ORDERSET/'LCQS'/ 
      DATA              ROTA /SYSMXDIM * 0.0/ 
C=============================================================================
      IF (ERROR) GO TO 999
C
C Check the arguments - first the order of interpolation
C
      DO 100 I = 1,4
         IF (ORDER(1:1) .EQ. ORDERSET(I:I)) GO TO 10
 100  CONTINUE
      CALL ERRREPOR(ERRBDARG, ROUTINE, 'Interpolation Order')
      GO TO 999
 10   IORDER = I - 1  
C
C Check the arguments - next the files
C
      CALL DATGETAR(IN, NAX1, NAXIS1, TYPE1, ADD1)
      CALL DATGETAR(MODEL, NAX2, NAXIS2, TYPE2, ADD2)
      RNAX1 = CRDRNAX (NAX1, NAXIS1)
      RNAX2 = CRDRNAX (NAX2, NAXIS2)
      IF (ERROR) GO TO 990
      IF (RNAX1 .NE. RNAX2) THEN
         CALL ERRREPOR(ERRBDARG, ROUTINE,
     $      'Different number of usable axes')
         GO TO 999
      END IF
      IF (RNAX1 .LE. 1) THEN
         CALL ERRREPOR(ERRBDARG, ROUTINE,
     1                 'Cant do Geometry on 1-dim array')
         GO TO 999
      END IF
      IF (TYPE1 .NE. TYPE2) THEN
         WRITE (MESSAGE,1000) TYPE1, TYPE2
 1000    FORMAT('Array types for input and sample images disagree:',
     1           A1,1X,A1)
         CALL ERRREPOR(ERRBDARG, ROUTINE, MESSAGE)
         GO TO 999
      END IF
C
C Check for ROTATION required by PARANGLE in MODEL
C
      PARANGLE = 0.0
      ROTATABL = .FALSE.
      IF (ERROR) GOTO 990
      IF(DATEXIST(STRM2(MODEL, 'PARANGLE' ))) THEN
         PARANGLE = DATFGETR (MODEL, 'PARANGLE' )
         ROTATABL = DATFGETL (IN, 'ROTATABLE')
      ENDIF
      IF (PARANGLE .NE. 0.0 .AND. ROTATABL) THEN
         IF (SYSDEBUG) THEN
            WRITE (MESSAGE, 1111) PARANGLE
 1111       FORMAT ('IMGHGEOM Rotating image by ', F10.2)
            CALL MSGPUT (MESSAGE, 'D')
         ENDIF
         CALL DATGETR (MODEL, 'CROTA', ROTA, SYSMXDIM, NDUMMY)
         ROTA(2) = ROTA(2) + PARANGLE
         CALL DATPUTR (MODEL, 'CROTA', ROTA, NDUMMY)
      ENDIF
C
C Create the output file
C
      IF (.NOT.DATEXIST(OUT)) THEN
         CALL IMGCLONE(MODEL, OUT)
         CALL ARRSETCO (OUT, 0.0, 0.0)
      ENDIF
C
C Copy pertinent header values from the MODEL and the IN file
C
      CALL CRDGET(MODEL, NAX, TYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
      CALL CRDPUT(OUT, NAX, TYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
      CALL COMHDR(IN, OUT)
      CALL DATGETAR(OUT, NAX3, NAXIS3, TYPE3, ADD3)
      EPOCH = DATFGETR(OUT,'EPOCH')
C
C Make two arrays to hold the X and Y coordinates of the input map. These
C arrays will be sized to hold only every 4th point in X and Y dimension
C (1/16 of total points) of the output map.
C
      NBX4 = (NAXIS3(1) + 2) / 4 + 1
      NBY4 = (NAXIS3(2) + 2) / 4 + 1
      CALL DATMAKAR('XADDR', 1, NBX4 * NBY4, 'R', XADD)
      CALL DATMAKAR('YADDR', 1, NBX4 * NBY4, 'R', YADD)
C
C Make two scratch arrays
C
      CALL DATMAKAR('XI', 1, NAXIS3(1), 'R', XI)
      CALL DATMAKAR('YI', 1, NAXIS3(2), 'R', YI)

C
C Initialize the commons LOCATION1 and LOCATION2
C
      CALL SETLOC1(IN)
      CALL SETLOC2(OUT)
C
C Do the work
C
      IF (RNAX1 .EQ. 2) THEN
         CALL PIX2HGEO(NAXIS1(1),NAXIS1(2),
     1                  NAXIS3(1),NAXIS3(2),
     2                  NBX4, NBY4, IORDER, EPOCH,     
     3                  MEMR(ADD1),MEMR(ADD3),
     4                  MEMR(XI), MEMR(YI),
     5                  MEMR(XADD), MEMR(YADD))    
      ELSE IF (RNAX1 .EQ. 3) THEN
         CALL PIX3HGEO(NAXIS1(1),NAXIS1(2),NAXIS1(3),
     1                  NAXIS3(1),NAXIS3(2),NAXIS3(3),
     2                  NBX4, NBY4, IORDER, EPOCH,
     3                  MEMR(ADD1),MEMR(ADD3),
     4                  MEMR(XI), MEMR(YI),
     5                  MEMR(XADD), MEMR(YADD))    
      END IF
C
C Delete dynamically allocated arrays
C
      CALL DATDELAR('XADDR')
      CALL DATDELAR('YADDR')
      CALL DATDELAR('XI')
      CALL DATDELAR('YI')
C
C Undo the artificial change in ROTA required for AZ-EL transformations 
C in the MODEL and the OUT images
C
      IF (PARANGLE .NE. 0.0) THEN
         CALL DATGETR (MODEL, 'CROTA', ROTA, SYSMXDIM, NDUMMY)
         ROTA(2) = ROTA(2) - PARANGLE
         CALL DATPUTR (MODEL, 'CROTA', ROTA, NDUMMY)
         CALL DATPUTR (OUT, 'CROTA', ROTA, NDUMMY)
      ENDIF
C
C If needed, scale output image to conserve flux
C
      IF (DATEXIST(STRM2(IN, 'BUNIT'))) THEN
         CALL DATGETC (IN, 'BUNIT', BUNIT, 1, NDUMMY)
         IF (INDEX (BUNIT, '/PIXEL') .NE. 0) THEN
            CALL DATGETR (IN, 'CDELT', DELT, SYSMXDIM, NDUMMY)
            CALL DATGETR (OUT, 'CDELT', ODELT, SYSMXDIM, NDUMMY)
            SCALE = 1.0
            DO 800 I = 1, RNAX1
               SCALE = SCALE * ABS (ODELT(I)/DELT(I))
 800        CONTINUE
            IF (SCALE .EQ. 0.0) THEN
               CALL ERRREPOR (ERRLOGIC, ROUTINE, 
     $            'Scale factor is 0.0')
               GOTO 990
            ENDIF
            CALL ARRSCALE (OUT, SCALE, 0.0, OUT)
            CALL DATPUTC  (OUT, 'BUNIT', BUNIT, 1)
         ENDIF
      ENDIF
C
 990  IF (ERROR) CALL ERRTRACE(ROUTINE)
C
 999  CONTINUE
      END
