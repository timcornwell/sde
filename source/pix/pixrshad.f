C
C       National Radio Astronomy Observatory, Socorro, NM 87801
C       Software Development Environment (SDE)
C++
C @(#)pixrshad.f	1.5 01 Mar 1995
C
      SUBROUTINE PIXRSHAD (IMAGE, NX, NY, VIEW, HEIGHT, SHADOW)
C
CD Find the regions of an image which are shadowed from VIEW illumination
C
C       IMAGE   R(NX, NY)       input   Array of heights
C       NX      I               input   dim of IMAGE, SHADOW
C       NY      I               input   dim of IMAGE, SHADOW
C       VIEW    R(3)            input   coordinates of illumination
C       HEIGHT  R               input   Can an object of HEIGHT at some pixel
C                                       of IMAGE be seen from VIEW?
C       SHADOW  R(NX, NY)       inp/out Image of shadow:
C                                       Fill shadowed region with HEIGHT
C                                       if the current pixel is less than HEIGHT
C
C Audit trail:
C       Original version: Audit trail comments go on this line
C       and successive lines
C                               M.A. Holdaway   AAAApril 1 1994
C       Removed IF(ERROR) from those routines without stdinc.h
C                               T.J. Cornwell   December 27, 1994
C
C--------------------------------------------------------------------
#include        "stdinc.h"
C
      INTEGER           NX, NY
      REAL              IMAGE(NX, NY), SHADOW(NX, NY)
      REAL              VIEW(3), HEIGHT
C
      CHARACTER*(*)     ROUTINE
      PARAMETER         (ROUTINE = 'PIXRSHAD')
C
      INTEGER           BLC(2), TRC(2), IX, IY
      LOGICAL           ISBLOCK
C=====================================================================
      IF (ERROR) GO TO 999
C
C     Loop through all pixels
C     
      DO 800 IY = 1, NY
         DO 790 IX = 1, NX
C
C Find the sub-image we must search for interuptions to the Line-O-Site
C            
            CALL RELAVANT (IMAGE, NX, NY, IX, IY, VIEW, BLC, TRC)
C
C Ask the question: is this pixel blocked by any other pixel along
C the line of site in the specified sub-image?
C
            CALL ISBLOCKD (IMAGE, NX, NY, IX, IY, BLC, TRC, VIEW, 
     $           HEIGHT, ISBLOCK)
C
C If so, replace the pixel value in SHADOW only if HEIGHT
C is greater than the current pixel value
C
            IF (ISBLOCK) THEN
               SHADOW(IX, IY) = MAX( SHADOW(IX, IY), HEIGHT )
            ENDIF
 790     CONTINUE
 800  CONTINUE


 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
C
      SUBROUTINE RELAVANT (IMAGE, NX, NY, ITX, ITY, VIEW, BLC, TRC)
C
C       IMAGE   R(NX, NY)       inp
C       NX      INT             inp
C       NY      INT             inp
C       ITX     INT             inp     IS this point shadowed?
C       ITY     INT             inp     IS this point shadowed?
C       BLC     INT(2)          inp     search region
C       TRC(2)  INT(2)          inp     search region
C       VIEW    R(3)            inp     viewing point, in pixels
C       ISBLOCK L               inp     Is Line-of-site from VIEW to ITX, ITY blocked?
C
C
C  Audit:
C
C	Note that variable IMAGE is never used.
C				M. Stupar	Jan 6 1995
C----------------------------------------------------------------------------
      INTEGER           NX, NY, ITX, ITY, BLC(2), TRC(2)
      REAL              VIEW(3), IMAGE(NX, NY)
C
      REAL      YLIN, XLIN
      INTEGER   ISAVE
C=============================================================================
C
C Where does Line-O-Site intersect the boundaries of the image
C which are parallel to the Y axis?
C
      IF (VIEW(1) .EQ. ITX) THEN
         IF (VIEW(2) .LT. ITY) THEN
            BLC(1) = ITX
            BLC(2) = 2
            TRC(1) = ITX
            TRC(2) = ITY
            GOTO 500
         ELSE IF (VIEW(2) .GT. ITY) THEN
            BLC(1) = ITX
            BLC(2) = ITY
            TRC(1) = ITX
            TRC(2) = NY-1
            GOTO 500
         ENDIF
      ENDIF
C
      YLIN = ITY + FLOAT(1 - ITX) * 
     $     (VIEW(2) - FLOAT(ITY))/(VIEW(1) - FLOAT(ITX))
      IF ( 1.0 .LE. YLIN .AND. YLIN .LE. FLOAT(NY)  .AND.
     $     ((VIEW(2) .LT. YLIN .AND. YLIN .LT. FLOAT(ITY)) .OR.
     $      (VIEW(2) .GT. YLIN .AND. YLIN .GT. FLOAT(ITY))) ) THEN
         BLC(1) = 1
         BLC(2) = YLIN
         TRC(1) = ITX
         TRC(2) = ITY
         GOTO 500
      ENDIF
C
      YLIN = ITY + FLOAT(NX - ITX) * 
     $     (VIEW(2) - FLOAT(ITY))/(VIEW(1) - FLOAT(ITX))
      IF ( 1.0 .LE. YLIN .AND. YLIN .LE. FLOAT(NY)  .AND.
     $     ((VIEW(2) .LT. YLIN .AND. YLIN .LT. FLOAT(ITY)) .OR.
     $      (VIEW(2) .GT. YLIN .AND. YLIN .GT. FLOAT(ITY))) ) THEN
         BLC(1) = ITX
         BLC(2) = ITY
         TRC(1) = NX
         TRC(2) = YLIN
         GOTO 500
      ENDIF
C
C Where does Line-O-Site intersect the boundaries of the image
C which are parallel to the X axis?
C
      IF (VIEW(2) .EQ. ITY) THEN
         IF (VIEW(1) .LT. ITX) THEN
            BLC(1) = 2
            BLC(2) = ITY
            TRC(1) = ITX
            TRC(2) = ITY
            GOTO 500
         ELSE IF (VIEW(1) .GT. ITX) THEN
            BLC(1) = ITX
            BLC(2) = ITY
            TRC(1) = NX-1
            TRC(2) = ITY
            GOTO 500
         ENDIF
      ENDIF
      XLIN = ITX + FLOAT(1 - ITY) * 
     $     (VIEW(1) - FLOAT(ITX))/(VIEW(2) - FLOAT(ITY))
      IF ( 1.0 .LE. XLIN .AND. XLIN .LE. FLOAT(NX)  .AND.
     $     ((VIEW(1) .LT. XLIN .AND. XLIN .LT. FLOAT(ITX)) .OR.
     $      (VIEW(1) .GT. XLIN .AND. XLIN .GT. FLOAT(ITX))) ) THEN
         BLC(1) = XLIN
         BLC(2) = 1
         TRC(1) = ITX
         TRC(2) = ITY
         GOTO 500
      ENDIF
C
      XLIN = ITX + FLOAT(NY - ITY) * 
     $     (VIEW(1) - FLOAT(ITX))/(VIEW(2) - FLOAT(ITY))
      IF ( 1.0 .LE. XLIN .AND. XLIN .LE. FLOAT(NX)  .AND.
     $     ((VIEW(1) .LT. XLIN .AND. XLIN .LT. FLOAT(ITX)) .OR.
     $      (VIEW(1) .GT. XLIN .AND. XLIN .GT. FLOAT(ITX))) ) THEN
         BLC(1) = ITX
         BLC(2) = ITY
         TRC(1) = XLIN
         TRC(2) = NY
         GOTO 500
      ENDIF
C
 500  CONTINUE
C
C Validate BLC, TRC
C
         IF (BLC(1) .GT. TRC(1)) THEN
            ISAVE = BLC(1)
            BLC(1) = TRC(1)
            TRC(1) = ISAVE
         ENDIF
         IF (BLC(2) .GT. TRC(2)) THEN
            ISAVE = BLC(2)
            BLC(2) = TRC(2)
            TRC(2) = ISAVE
         ENDIF
         BLC(1) =  MAX(2,  MIN( BLC(1), NX-1) )
         BLC(2) =  MAX(2,  MIN( BLC(2), NY-1) )
         TRC(1) =  MAX(2,  MIN( TRC(1), NX-1) )
         TRC(2) =  MAX(2,  MIN( TRC(2), NY-1) )
C
 999  CONTINUE
      END
C
      SUBROUTINE ISBLOCKD (IMAGE, NX, NY, ITX, ITY, BLC, TRC, VIEW, 
     $     EXTRA, ISBLOCK)
C
C       IMAGE   R(NX, NY)       inp
C       NX      INT             inp
C       NY      INT             inp
C       ITX     INT             inp     IS this point shadowed?
C       ITY     INT             inp     IS this point shadowed?
C       BLC     INT(2)          inp     search region
C       TRC(2)  INT(2)          inp     search region
C       VIEW    R(3)            inp     viewing point, in pixels
C       ISBLOCK L               inp     Is Line-of-site from VIEW to ITX, ITY blocked?
C
C
C       Note:  This comes with a guard against self-shadowing, but not against
C               "back shadowing", ie, if the line of site passes the pixel
C               of interest but is later shadowed, ooops!  Unless the BLC, TRC
C               is set correctly.  
C               A pixel does not shadow itself, though.
C
C       Note:   there will be problems right on the edges, ie, 
C               discard the outer pixel of the shadow image
C----------------------------------------------------------------------------
#include "stdinc.h"
C
      INTEGER           NX, NY, ITX, ITY, BLC(2), TRC(2)
      REAL              VIEW(3), EXTRA, IMAGE(NX, NY)
      LOGICAL           ISBLOCK
C
      INTEGER           LINY, LINY1, LINY2, LINX, LINX1, LINX2
      REAL              XLIN, YLIN, ZLIN, ZLAND, WT1, WT2, HEIGHT
C=============================================================================
C
      ISBLOCK = .FALSE.
      HEIGHT = IMAGE(ITX, ITY) + EXTRA
C
      IF ((TRC(1) - BLC(1)) .GT. (TRC(2) - BLC(2))) THEN
         DO 200 LINX = BLC(1), TRC(1)
C
C On the X grid points, get the precise values of Y and Z for the Line of Site
C
            YLIN = ITY +   FLOAT(LINX - ITX) * 
     $           (VIEW(2) - FLOAT(ITY))/(VIEW(1) - FLOAT(ITX))
            LINY1 = INT(YLIN)
            LINY2 = LINY1 + 1
            IF (LINY1 .GT. NY .OR. LINY2 .GT. NY .OR. 
     $          LINY1 .LT. 1  .OR. LINY2 .LT. 1) GOTO 200
            ZLIN = HEIGHT   +  FLOAT(LINX - ITX) *
     $           (VIEW(3) - HEIGHT)/(VIEW(1) - FLOAT(ITX))
C
C Calculate if the line of site is blocked by any features
C
            WT1 = LINY2 - YLIN
            WT2 = 1 - WT1
            ZLAND = WT1 * IMAGE(LINX, LINY1) + WT2 * IMAGE(LINX, LINY2)
C
            IF (ZLAND .GT. ZLIN .AND. LINX .NE. ITX) THEN
               ISBLOCK = .TRUE.
               GOTO 300
            ENDIF
C
 200     CONTINUE
C
 300     CONTINUE
C
      ELSE
         DO 400 LINY = BLC(2), TRC(2)
C
C On the Y grid points, get the precise values of X and Z for the Line of Site
C
            XLIN = ITX +   FLOAT(LINY - ITY) * 
     $           (VIEW(1) - FLOAT(ITX))/(VIEW(2) - FLOAT(ITY))
            LINX1 = INT(XLIN)
            LINX2 = LINX1 + 1
            IF (LINX1 .GT. NX .OR. LINX2 .GT. NX .OR. 
     $          LINX1 .LT. 1  .OR. LINX2 .LT. 1) GOTO 400
            ZLIN = HEIGHT   +  FLOAT(LINY - ITY) *
     $           (VIEW(3) - HEIGHT)/(VIEW(2) - FLOAT(ITY))
C
C Calculate if the line of site is blocked by any features
C
            WT1 = LINX2 - XLIN
            WT2 = 1 - WT1
            ZLAND = WT1 * IMAGE(LINX1, LINY) + WT2 * IMAGE(LINX2, LINY)
C
            IF (ZLAND .GT. ZLIN .AND. LINY .NE. ITY) THEN
               ISBLOCK = .TRUE.
               GOTO 500
            ENDIF
C
 400     CONTINUE
 500     CONTINUE
C
      ENDIF
C
 999  CONTINUE
      END
