C
C       National Radio Astronomy Observatory, Socorro, NM 87801
C       Software Development Environment (SDE)
C++
C @(#)grduruw2.f	1.5 24 Feb 1995
C
      SUBROUTINE GRDURUW2 (U, V, W, NVIS, URADIUS, VRADIUS,
     $   WT, NEWWT, HEAD, NU, NV, LINKS, BSUMWT,
     $   SHIFT, RMODE, ROBUST, DS, SVWTS, RDWTS, WTNAM)
C
CD 2-D robust uniform weighting.  Ungridded version!
C
C       U       REAL(NVIS)      input   Coordinates of data
C       V       REAL(NVIS)      input   Coordinates of data
C       W       REAL(NVIS)      input   Coordinates of data
C       NVIS    INT             input   Number to be weighted
C       URADIUS REAL            input   Weighting radius in u
C       VRADIUS REAL            input   Weighting radius in v
C       WT      REAL(NVIS)      input   Input weights
C       NEWWT   REAL(NVIS)      output  New weights
C       HEAD    INT(NU,NV)      scratch Binned plane of head pointers
C       NU      INT             input   Size of binned plane
C       NV      INT             input   Size of binned plane
C       LINKS   INT(NVIS)       scratch Linked list storage
C       BSUMWT  REAL(NVIS)      scratch Binned summed weights
C       SHIFT   DBLE(3,3)       input   Rotation matrix
C       RMODE   CH*(*)          input   Robustness mode
C       ROBUST  REAL            input   Robust flux threshold
C       DS      REAL            input   Delta S for unit weight
C       SVWTS   CH*(*)          input   Save summed weights to this file
C       RDTWS   CH*(*)          input   Read summed weights from this file
C       WTNAM   CH*(*)          input   Name of summed weight array
C
C The routine does use a grid for speed purposes, but only to reduce the
C number of visibilities which must be checked.  The actual weighting
C is done with the local density of ungridded visibilities.  A robustness
C parameter may also be included.  It's fairly slow, but not completely
C unreasonable.
C
C Since we are dealing with ungridded coordinates, the pixel conventions
C are slightly different than in the other gridding routines.  The point
C (u,v)=(0,0) is mapped to the V pixel coordinates between NV/2 &
C NV/2+1.  The origin is mapped to the pixel coordinate UORIGIN & UORIGIN+1,
C which is calculted internally to allow for just enough bins on the
C u<0 side to cover a point with u=0.  This is an extended half plane
C version, and allows for nearly the same logic/speed savings as a full
C plane, but uses less memory.  A real 'speed at all costs' version of the
C routine would have to double the size of the visibility data base and
C separate out the conjugations into individual visibilities.  But that's
C too extreme for now, and it probably wouldn't buy all that much.
C
C The basic strategy is to sort all the visibilities into a large number of
C buckets, indexed by HEAD.  Each bucket contains an unsorted linked list
C with a hopefully modest number of elements.  A rectangular region of
C buckets including all buckets in the neighborhood of u0,v0 is searched.  If
C HEAD is zero for a given bucket, there are no VIS samples there, and it is
C skipped.  if the extreme point away from u0,v0 (for each bucket) is within
C the neighborhood, the entire list is added via the accumulated sum from the
C binning stage.  If the extreme point closest to u0,v0 is outside the
C neighborhood, the bucket is skipped.  If neither case is true, then the
C list is scenned, and each individual point added or not depending on its
C individual uv coordinates.
C
C To keep the size of the head pointer array down, we scale and shift the
C coordinate system of this array to just enclose the visibilities found.
C The U axis will run from -ur to +umax, given that we force all visibilities
C to the positive conjugation.  The origin must be between pixels for the
C algorithm to work, so we keep the origin as an integer.  We force
C symmetry in V, since in most practical cases with a track crossing
C U=0 it will be needed anyway and it makes the housekeeping much simpler!
C
C Note the warnings in grdurwt.f about SVWTS and RDWTS
C
C Audit trail:
C       Cloned from GRDRUW2D, and modified for ungridded operation.
C       There's not much left of the original.
C                               D.S.Briggs      Sept 13 1993
C       Normalized robustness mode added.  WTSAV & RDSAV added.
C                               D.S.Briggs      Sept 25 1993
C       Minor bugfixs.  Don't assume that NU = NV/2, even though it will
C       almost always be called that way.  Don't force NV to be even --
C       force a copy instead.
C                               D.S.Briggs      Jan 18 1994
C       Cloned from GRDURUW2.  In fact since I want it to be the new
C       default version, I've now called it GRDURUW2.  The old routine
C       becomes GRDURUW2A.  What a version control nightmare, eh?
C                               D.S.Briggs      Jan 21 1994
C       Bugfix when dealing with extremely small grids
C                               D.S.Briggs      Jan 3 1995
C	Elim double-declared vars to elim compiler warnings; rearrange
C	PIXISAMA declaration (c$$$) for same reason.
C				M. Stupar	Jan 6 1995
C------------------------------------------------------------------------
#include        "stdinc.h"
C
      INTEGER   NVIS, NU, NV
      INTEGER   HEAD(NU, NV), LINKS(NVIS)
      REAL      WT(NVIS), NEWWT(NVIS), BSUMWT(NVIS)
      REAL      U(NVIS), URADIUS
      REAL      V(NVIS), VRADIUS
      REAL      W(NVIS)
      REAL      ROBUST, DS
      DOUBLE  PRECISION SHIFT (3,3)
      CHARACTER*(*)     SVWTS, RDWTS, WTNAM, RMODE
C
      CHARACTER*(*)     ROUTINE
      PARAMETER (ROUTINE = 'GRDURUW2')
C
      REAL      BINEPS
      PARAMETER (BINEPS = 0.1)
C
      INTEGER   IVIS, NDROP, I, J, L,  IFILL, NSEL
      INTEGER   UBIN, UBIN1, VBIN, VBIN1, UORIGIN, VORIGIN
      REAL      UB, VB, DU, DV, UMAX, VMAX
      REAL      UCELL, VCELL, UR2, VR2
      REAL      U0, V0, UTEST, VTEST, F2, D2, WTSUM
      REAL      USCALE, VSCALE, FILL
      DOUBLE PRECISION  SUMLOCWT
      CHARACTER*(SYSMXNAM)      CTIME, WTFILE
      LOGICAL   POSBIN
C
      LOGICAL   DOSHIFT
      INTEGER   UTLGINT
c$$$  INTEGER   PIXISAMA
C==========================================================================
      IF (ERROR) GO TO 999
C
      SUMLOCWT = 0.D0
      NSEL = 0
C
C If old summed weights are being passed in, skip most of the routine
C
      IF (RDWTS.NE.' ') THEN
         DO 3 IVIS = 1, NVIS
            IF (WT(IVIS).GT.0.0) THEN
               SUMLOCWT = SUMLOCWT + NEWWT(IVIS)
               NSEL = NSEL + 1
            END IF
 3       CONTINUE
         GO TO 700
      END IF
C
C Sanity check
C
      IF ((NU.LT.2).OR.(NV.LT.2)) THEN
         CALL ERRREPOR (ERRFATAL, ROUTINE, 'Bad size for binning array')
         GO TO 999
      END IF
C
      UR2 = URADIUS**2
      VR2 = VRADIUS**2
      DO 6 VBIN = 1, NV
         DO 5 UBIN = 1, NU
            HEAD (UBIN,VBIN) = 0
 5       CONTINUE
 6    CONTINUE
      DO 7 IVIS = 1, NVIS
         LINKS(IVIS) = 0
         BSUMWT(IVIS) = 0.0
 7    CONTINUE
C
C Is there a shift?
C
      DOSHIFT = (SHIFT(1,1).NE.1.0D0).OR.(SHIFT(2,2).NE.1.0D0).OR.
     1   (SHIFT(3,3).NE.1.0D0)
C
C Find min & max of possibly shifted coordinates
C
      UMAX = -1.0
      VMAX = -1.0
      DO 50 IVIS = 1, NVIS
         IF (WT(IVIS).LE.0.0) GO TO 50
         IF (DOSHIFT) THEN
            U0 = SHIFT(1,1) * U(IVIS) + SHIFT(2,1) * V(IVIS) +
     $         SHIFT(3,1) * W(IVIS)
            V0 = SHIFT(1,2) * U(IVIS) + SHIFT(2,2) * V(IVIS) +
     $         SHIFT(3,2) * W(IVIS)
         ELSE
            U0 = U(IVIS)
            V0 = V(IVIS)
         END IF
         IF (ABS(U0).GT.UMAX) UMAX = ABS(U0)
         IF (ABS(V0).GT.VMAX) VMAX = ABS(V0)
 50   CONTINUE
C
      IF (SYSDEBUG) THEN
         WRITE (MESSAGE, 1050) UMAX, VMAX
 1050    FORMAT ('Umax =',1PE12.5,'  Vmax =',1PE12.5)
         CALL MSGPUT (MESSAGE, 'I')
      END IF
C
C Calculate origin and scaling factors to make best use of binning grid.
C There are various definitions of "best".  This one will make DU & DV
C as similar as possible in the limit of a really small array.
C
      DO 60 UORIGIN = 1, NU-1
         DU = UMAX / (NU - UORIGIN - BINEPS)
         IF ((UORIGIN-BINEPS)*DU .GT. URADIUS) GO TO 65
 60   CONTINUE
      UORIGIN = NU-1
 65   CONTINUE
C
      IF ((URADIUS/DU+BINEPS .GT. UORIGIN).AND.
     $    ((NU-UORIGIN).GT.UORIGIN)) THEN
         CALL ERRREPOR (ERRFATAL, ROUTINE,
     $      'Problems with binsize calcs')
         GO TO 999
      END IF
C
      VORIGIN = NV / 2
      DV = VMAX / (VORIGIN - BINEPS)
C
      USCALE = 1.0 / DU
      VSCALE = 1.0 / DV
C
      IF (SYSDEBUG) THEN
         WRITE (MESSAGE, 1060) DU, DV
 1060    FORMAT ('delta U =',1PE12.5,'  delta V =',1PE12.5)
         CALL MSGPUT (MESSAGE, 'I')
      END IF
C
C Start of loop elements to be binned
C
      NDROP = 0
      IFILL = 0
      DO 100 IVIS = 1, NVIS
         NEWWT(IVIS) = WT(IVIS)
         IF (WT(IVIS).LE.0.0) GO TO 100
         IF (DOSHIFT) THEN
            U0 = SHIFT(1,1) * U(IVIS) + SHIFT(2,1) * V(IVIS) +
     $         SHIFT(3,1) * W(IVIS)
            V0 = SHIFT(1,2) * U(IVIS) + SHIFT(2,2) * V(IVIS) +
     $         SHIFT(3,2) * W(IVIS)
            UCELL = USCALE * U0
            VCELL = VSCALE * V0
         ELSE
            UCELL = USCALE * U(IVIS)
            VCELL = VSCALE * V(IVIS)
         END IF
         IF (UCELL.LT.0.0) THEN
            UCELL = -UCELL
            VCELL = -VCELL
         END IF
         UBIN = UTLGINT(UCELL) + UORIGIN + 1
         VBIN = UTLGINT(VCELL) + VORIGIN + 1
         UBIN1= UTLGINT(-UCELL) + UORIGIN + 1
         VBIN1= UTLGINT(-VCELL) + VORIGIN + 1
         IF ((UBIN.LT.1).OR.(UBIN.GT.NU).OR.(VBIN.LT.1).OR.
     $       (VBIN.GT.NV)) THEN
            NDROP = NDROP + 1
            WT(IVIS) = -1.0
         ELSE
C                                       Insert a new element at the head
            L = HEAD(UBIN,VBIN)
            HEAD(UBIN,VBIN) = IVIS
            LINKS(IVIS) = L
            IF (L.EQ.0) IFILL = IFILL + 1
C                                       Set a link to the same list for the
C                                       other conjugation
            IF ((UBIN1.GE.1).AND.(UBIN1.LE.NU).AND.
     $          (VBIN1.GE.1).AND.(VBIN1.LE.NV))
     $              HEAD(UBIN1,VBIN1) = IVIS
C                                       Accumulate sum of weights in list
            IF (L.EQ.0) THEN
               BSUMWT(IVIS) = WT(IVIS)
            ELSE
               BSUMWT(IVIS) = WT(IVIS) + BSUMWT(L)
            END IF
         END IF
 100  CONTINUE
C
      FILL = REAL(IFILL) / (NU*NV)
      IF (SYSDEBUG) THEN
         WRITE (MESSAGE, 1100) FILL
 1100    FORMAT ('Filling factor = ', F9.6)
         CALL MSGPUT (MESSAGE, 'I')
      END IF
C
      IF (NDROP.GT.0) THEN
         WRITE (MESSAGE, 1110) NDROP
 1110    FORMAT ('Warning:',I7,' points dropped off the binning grid')
         CALL MSGPUT (MESSAGE, 'W')
      END IF
C
c      call arrcvtr ('GRD-HEADLINKS', 'HEAD')
c      call filimgpu ('HEAD', 'HEAD', ' ')
C
C Main reweighting loop
C
      DO 600 IVIS = 1, NVIS
         IF (WT(IVIS).LE.0.0) GO TO 600
C
C Progress messages
C
         IF (MOD(IVIS, MAX(1,(ABS(NVIS))/10)).EQ.1) THEN
            WRITE (MESSAGE, 1101) IVIS
 1101       FORMAT (I8,' visibilities reweighted')
            CALL MSGPUT (MESSAGE, 'I')
            IF (SYSDEBUG) THEN
               CALL SYSETIME (CTIME)
               CALL MSGPUT (CTIME, 'I')
            END IF
         END IF
C
C The coordinates of the point under consideration
C
         IF (DOSHIFT) THEN
            U0 = SHIFT(1,1) * U(IVIS) + SHIFT(2,1) * V(IVIS) +
     $         SHIFT(3,1) * W(IVIS)
            V0 = SHIFT(1,2) * U(IVIS) + SHIFT(2,2) * V(IVIS) +
     $         SHIFT(3,2) * W(IVIS)
         ELSE
            U0 = U(IVIS)
            V0 = V(IVIS)
         END IF
         IF (U0.LT.0.0) THEN
            U0 = -U0
            V0 = -V0
         END IF
C
         UBIN = UTLGINT((U0-URADIUS)*USCALE) + UORIGIN + 1
         UBIN1 = UTLGINT((U0+URADIUS)*USCALE) + UORIGIN + 1
         VBIN = UTLGINT((V0-VRADIUS)*VSCALE) + VORIGIN + 1 
         VBIN1 = UTLGINT((V0+VRADIUS)*VSCALE) + VORIGIN + 1
C
         UBIN = MAX(1,MIN(NU,UBIN))
         UBIN1 = MAX(1,MIN(NU,UBIN1))
         VBIN = MAX(1,MIN(NV,VBIN))
         VBIN1 = MAX(1,MIN(NV,VBIN1))
C
C                               The goal of the whole mess
         WTSUM = 0.0
C
C Loop over headlinks that might contain a pointer to an interesting vis
C
            DO 220 J = VBIN, VBIN1
               DO 210 I = UBIN, UBIN1
C                                       Any visibilities there at all?
                  IF (HEAD(I,J).EQ.0) GO TO 210
C
C The fiducial point is upper right corner
C
                  UB = (I-UORIGIN-1)*DU
                  VB = (J-VORIGIN)*DV
C
                  IF (UB+DU.LT.U0) THEN
                     IF (VB-DV.GT.V0) THEN
                        IF (((UB-U0)**2/UR2 +
     $                       (VB-V0)**2/VR2) .LE. 1.0) GO TO 205
                        IF (((UB+DU-U0)**2/UR2 +
     $                       (VB-DV-V0)**2/VR2) .GE. 1.0) GO TO 210
                     ELSE IF (VB.LT.V0) THEN
                        IF (((UB-U0)**2/UR2 +
     $                       (VB-DV-V0)**2/VR2) .LE. 1.0) GO TO 205
                        IF (((UB+DU-U0)**2/UR2 +
     $                       (VB-V0)**2/VR2) .GE. 1.0) GO TO 210
                     ELSE
                        IF ((((UB-U0)**2/UR2 +
     $                        (VB-V0)**2/VR2) .LE. 1.0) .AND.
     $                      (((UB-U0)**2/UR2 +
     $                        (VB-DV-V0)**2/VR2) .LE. 1.0)) GO TO 205
                     END IF
                  ELSE IF (UB.GT.U0) THEN
                     IF (VB-DV.GT.V0) THEN
                        IF (((UB+DU-U0)**2/UR2 +
     $                       (VB-V0)**2/VR2) .LE. 1.0) GO TO 205
                        IF (((UB-U0)**2/UR2 +
     $                       (VB-DV-V0)**2/VR2) .GE. 1.0) GO TO 210
                     ELSE IF (VB.LT.V0) THEN
                        IF (((UB+DU-U0)**2/UR2 +
     $                       (VB-DV-V0)**2/VR2) .LE. 1.0) GO TO 205
                        IF (((UB-U0)**2/UR2 +
     $                       (VB-V0)**2/VR2) .GE. 1.0) GO TO 210
                     ELSE
                        IF ((((UB+DU-U0)**2/UR2 +
     $                        (VB-V0)**2/VR2) .LE. 1.0) .AND.
     $                      (((UB+DU-U0)**2/UR2 +
     $                        (VB-DV-V0)**2/VR2) .LE. 1.0)) GO TO 205
                     END IF
                  ELSE
                     IF (VB-DV.GT.V0) THEN
                        IF ((((UB-U0)**2/UR2 +
     $                        (VB-V0)**2/VR2) .LE. 1.0) .AND.
     $                      (((UB+DU-U0)**2/UR2 +
     $                        (VB-V0)**2/VR2) .LE. 1.0)) GO TO 205
                     ELSE IF (VB.LT.V0) THEN
                        IF ((((UB-U0)**2/UR2 +
     $                        (VB-DV-V0)**2/VR2) .LE. 1.0) .AND.
     $                      (((UB+DU-U0)**2/UR2 +
     $                        (VB-DV-V0)**2/VR2) .LE. 1.0)) GO TO 205
                     ELSE
                        IF ((((UB-U0)**2/UR2 +
     $                        (VB-V0)**2/VR2) .LE. 1.0) .AND.
     $                      (((UB+DU-U0)**2/UR2 +
     $                        (VB-V0)**2/VR2) .LE. 1.0) .AND.
     $                      (((UB+DU-U0)**2/UR2 +
     $                        (VB-DV-V0)**2/VR2) .LE. 1.0) .AND.
     $                      (((UB-U0)**2/UR2 +
     $                        (VB-DV-V0)**2/VR2) .LE. 1.0)) GO TO 205
                     END IF
                  END IF
C                                       No shortcut applies.  Fall though &
C                                       traverse the list checking each vis
 199              continue

                  L = HEAD(I,J)
                  POSBIN = (I.GT.UORIGIN)
 200              CONTINUE
                  IF (L.NE.0) THEN
                     IF (DOSHIFT) THEN
                        UTEST = SHIFT(1,1) * U(L) + SHIFT(2,1)
     $                     * V(L) + SHIFT(3,1) * W(L)
                        VTEST = SHIFT(1,2) * U(L) + SHIFT(2,2)
     $                     * V(L) + SHIFT(3,2) * W(L)
                     ELSE
                        UTEST = U(L)
                        VTEST = V(L)
                     END IF
C
C                                       Get the right conjugation
                     IF (POSBIN) THEN
                        IF (UTEST.LT.0.0) THEN
                           UTEST = -UTEST
                           VTEST = -VTEST
                        END IF
                     ELSE
                        IF (UTEST.GT.0.0) THEN
                           UTEST = -UTEST
                           VTEST = -VTEST
                        END IF
                     END IF
C
C                                       Test distance
                     IF (((UTEST-U0)**2/UR2 +
     $                   (VTEST-V0)**2/VR2) .LT. 1.0) THEN
                        WTSUM = WTSUM + WT(L)

c                        if (ivis.eq.46478) then
c                           write(10,*) 'edge', i,j,l, wt(l)
c                        end if

                     END IF
C
                     L = LINKS(L)
                     GO TO 200
                  END IF
                  GO TO 210
C                                       Accumulate total weights in cell
 205              CONTINUE
                  WTSUM = WTSUM + BSUMWT(HEAD(I,J))
C
 210           CONTINUE
 220        CONTINUE
C
         IF (WTSUM.EQ.0.0) THEN
            CALL ERRREPOR (ERRLOGIC, ROUTINE, 'Zero WTSUM')
            GO TO 999
         END IF
C
C Stash the sum of the weights for use in the final loop
C
         NEWWT(IVIS) = WTSUM
         SUMLOCWT = SUMLOCWT + WTSUM
         NSEL = NSEL + 1
C
C End of main loop over visibilities
C
 600  CONTINUE
C
C Save the summed weights for a later run if requested
C
      IF (SVWTS.NE.' ') THEN
         CALL FILSYSRT (SVWTS, WTFILE)
         CALL STRAPPEN(WTFILE, '.SDE')
         CALL FILIMGPU (WTNAM, WTFILE, ' ')
      END IF
C
C Now sort out the normalization & do the actual reweighting.
C
 700  CONTINUE
      IF (SYSDEBUG) THEN
         WRITE (MESSAGE, 1700) SUMLOCWT / NSEL
 1700    FORMAT ('Average Summed Local Weight is',G12.4)
         CALL MSGPUT (MESSAGE, 'I')
      END IF
C
      IF (RMODE.EQ.'NORM') THEN
C                                       Some compilers are brain damaged
         F2 = (5.0*10.0**(-ROBUST))**2 / (SUMLOCWT / NSEL)
         D2 = 1.0
      ELSE IF (RMODE.EQ.'ABS') THEN
         F2 = ROBUST**2
         D2 = 2.0 * DS**2
      ELSE IF (RMODE.EQ.'NONE') THEN
         F2 = 1.0
         D2 = 0.0
      ELSE
         CALL ERRREPOR (ERRLOGIC, ROUTINE, 'Unrecognized RMODE')
         GO TO 999
      END IF
C
      DO 710 IVIS = 1, NVIS
         IF (WT(IVIS).GT.0.0)
     $      NEWWT(IVIS) = WT(IVIS) / (NEWWT(IVIS) * F2 + D2)
 710  CONTINUE
C
c$$$      IVIS = PIXISAMA (NVIS, NEWWT, 1)
c$$$      PRINT *, IVIS, WT(IVIS), NEWWT(IVIS), U(IVIS), V(IVIS)
C
 900  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
C
c$$$      SUBROUTINE PRINT_LIST (HEAD, LINKS, U, V, WT, NU, NV, BSUMWT,
c$$$     $   I, J)
c$$$      INTEGER NU, NV, I, J, HEAD(NU, NV), LINKS(*)
c$$$      REAL U(*), V(*), WT(*), BSUMWT(*)
c$$$      INTEGER L
c$$$C
c$$$C CALL PRINT_LIST (HEAD, LINKS, U, V, WT, NU, NV, BSUMWT,         
c$$$C
c$$$      L = HEAD(I,J)
c$$$      PRINT *, BSUMWT(L)
c$$$ 10   CONTINUE
c$$$      IF (L.NE.0) THEN
c$$$         PRINT *, L, U(L), V(L), WT(L)
c$$$         L = LINKS(L)
c$$$         GO TO 10
c$$$      END IF
c$$$      END
