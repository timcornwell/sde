C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)plt2dpts.f	1.6	 11/15/94
C
        SUBROUTINE PLT2DPTS(POINTS, XADDR, YADDR, WADDR, PLOTINFO,
     $   SYMBOL, PTINCR, REVERSE, SQUARE, XAUTOSCL, XMIN, XMAX, XTICK,
     $   XNSUB, YAUTOSCL, YMIN, YMAX, YTICK, YNSUB, ID, LEFT, CHEIGHT,
     $   LWIDTH)
C
C Plot points using the coordinates of two arrays
C
C       POINTS          INT     input   Number of points to plot
C       XADDR           INT     input   Memory Address of X-points
C       YADDR           INT     input   Memory Address of Y-points
C       WADDR           INT     input   Memory Address of weights
C       PLOTINFO        CH*(*)  input   Plot information
C       SYMBOL          INT     input   Symbol
C       PTINCR          INT     input   Increment
C       REVERSE         LOGICAL input   Plot reversed points as well
C       SQUARE          LOGICAL input   Make axes square?
C       XAUTOSCL        LOGICAL input   Autoscale in X?
C       YAUTOSCL        LOGICAL input   Autoscale in Y?
C       XMIN, etc       REAL    input   Required minimum
C	XTICK		REAL	input	Tick interval on x axis
C	XNSUB		INT	input	Number of X subintervals
C       ID		LOGICAL	input	Add ID tag to output?
C	LEFT		LOGICAL	input	Label Y axis on left?
C	CHEIGHT		REAL	input	Character height
C	LWIDTH		INT	input	Line width
C
C Audit trail:
C       Original version
C                                      W. Young, March 13, 1989
C       Added SYMBOL, PTINCR explicitly
C                                      T.J. Cornwell, March 13, 1989
C       Added reverse capability
C                                      T.J. Cornwell, March 13, 1989
C       Added SQUARE and XMAX, XMIN, YMAX, YMIN
C                                      T.J. Cornwell, April 17, 1989
C       Split AUTOSCL to XAUTOSCL and YAUTOSCL
C                                      D.S.Briggs, Sept 3, 1991
C       Added weights
C				       D.S.Briggs     Jul 23 1992
C	Added ID, Left, CHeight, LWidth & Annotate options, plot
C       Amplitude instead of Real part if input is complex.
C				       D.S.Briggs     July 22 1994
C	Pass complex conversion modes in via PLOTINFO
C				       D.S.Briggs     July 28 1994
C	Only adjust limits for 5% slop and force square if autoscaling.
C	That is, given limits are not altered at all.  (Though
C	SQUARE still controls whether PGWINAD or PGWINDOW is called.)
C				       D.S.Briggs     Aug 29 1994
C	Added XTICK, YTICK
C				       D.S.Briggs     Nov 15 1994
C--------------------------------------------------------------------
#include        "stdinc.h"
C
C       Declarations of inputs
C
      INTEGER         POINTS, SYMBOL, PTINCR, LWIDTH
      INTEGER         XADDR, YADDR, WADDR, XNSUB, YNSUB
      CHARACTER *(*)  PLOTINFO(1)
      LOGICAL         REVERSE, XAUTOSCL, YAUTOSCL, SQUARE, LEFT, ID
      REAL            XMIN, XMAX, YMIN, YMAX, CHEIGHT, XTICK, YTICK
C
      REAL            XR, YR, WR, DELTA
C
C       Plot information mapped variables
C       
      INTEGER         I
      CHARACTER *(80) DEVNAME
      CHARACTER *(80) PLOTNAME
      CHARACTER *(80) XLABEL
      CHARACTER *(80) YLABEL
      CHARACTER *1    XTYPE
      CHARACTER *1    YTYPE
      CHARACTER *1    WTYPE
      CHARACTER *(80) ANNOT
      CHARACTER *1    XCONV
      CHARACTER *1    YCONV
C
      REAL            ADD2REAL
C
C       Extract various bits of information from plotinfo
C
      PLOTNAME = PLOTINFO(1)
      XLABEL = PLOTINFO(2)
      YLABEL = PLOTINFO(3)
      DEVNAME = PLOTINFO(4)
      XTYPE = PLOTINFO(5)
      YTYPE = PLOTINFO(6)
      WTYPE = PLOTINFO(7)
      ANNOT = PLOTINFO(8)
      XCONV = PLOTINFO(9)
      YCONV = PLOTINFO(10)
C
C Autoscale in X
C
      IF (XAUTOSCL) THEN
         XMIN = 1.E20
         XMAX = -1.E20
         DO 10 I=1,POINTS,PTINCR
            WR = ADD2REAL(WADDR+I-1, WTYPE, ' ')
            IF (WR.GT.0.0) THEN
               XR = ADD2REAL(XADDR+I-1, XTYPE, XCONV)
               IF(XMIN .GE. XR)XMIN = XR
               IF(XMAX .LT. XR)XMAX = XR
               IF (REVERSE) THEN
                  XR = -ADD2REAL(XADDR+I-1, XTYPE, XCONV)
                  IF(XMIN .GE. XR)XMIN = XR
                  IF(XMAX .LT. XR)XMAX = XR
               END IF
            END IF
 10      CONTINUE
      END IF
C
C Autoscale in Y
C
      IF (YAUTOSCL) THEN
         YMIN = 1.E20
         YMAX = -1.E20
         DO 20 I=1,POINTS,PTINCR
            WR = ADD2REAL(WADDR+I-1, WTYPE, ' ')
            IF (WR.GT.0.0) THEN
               YR = ADD2REAL(YADDR+I-1, YTYPE, YCONV)
               IF(YMIN .GE. YR)YMIN = YR
               IF(YMAX .LT. YR)YMAX = YR
               IF (REVERSE) THEN
                  YR = -ADD2REAL(YADDR+I-1, YTYPE, YCONV)
                  IF(YMIN .GE. YR)YMIN = YR
                  IF(YMAX .LT. YR)YMAX = YR
               END IF
            END IF
 20      CONTINUE
      END IF
C
C Do we want square plot?
C
      IF (SQUARE .AND. (XAUTOSCL.OR.YAUTOSCL)) THEN
         XMAX = MAX(XMAX, YMAX)
         YMAX = XMAX
         XMIN = MIN(XMIN, YMIN)
         YMIN = XMIN
      END IF
C
C       Expand min-max's by 5% for a nice fit
C
      IF (XAUTOSCL) THEN
         DELTA = XMAX - XMIN
         XMAX = XMAX + 0.05*DELTA
         XMIN = XMIN - 0.05*DELTA
      END IF
C
      IF (YAUTOSCL) THEN
         DELTA = YMAX - YMIN
         YMAX = YMAX + 0.05*DELTA
         YMIN = YMIN - 0.05*DELTA
      END IF
C
C       Start the plotting now
C
      CALL PGBEGIN(0, DEVNAME, 1,1)
#if MACH_COMP
      CALL PGSCI(3)
#endif
      CALL PGSCF(2)
      CALL PGSLW (LWIDTH)
      CALL PGSCH(1.25*CHEIGHT)
      CALL PGVSTD
      IF (SQUARE) THEN
         CALL PGWNAD (XMIN, XMAX, YMIN, YMAX)
      ELSE
         CALL PGWINDOW (XMIN, XMAX, YMIN, YMAX)
      END IF
      IF (LEFT) THEN
         CALL PGBOX('ABCNST', XTICK, XNSUB, 'ABCNST', YTICK, YNSUB)
         CALL PGMTXT('L', 2.2, 0.5, 0.5, YLABEL)
      ELSE
         CALL PGBOX('ABCNST', XTICK, XNSUB, 'ABCMST', YTICK, YNSUB)
         CALL PGMTXT('R', 2.7, 0.5, 0.5, YLABEL)
      END IF
      CALL PGMTXT('B', 3.2, 0.5, 0.5, XLABEL)
      CALL PGMTXT('T', 2.0, 0.5, 0.5, PLOTNAME)
      CALL PGSCH(CHEIGHT)
#if MACH_COMP
      CALL PGSCI(5)
#endif
C
C       do the dirty work point by %$#^%$#^% point
C
      DO 30 I=1, POINTS, PTINCR
         WR = ADD2REAL(WADDR+I-1, WTYPE, ' ')
         IF (WR.GT.0.0) THEN
            XR = ADD2REAL(XADDR+I-1, XTYPE, XCONV)
            YR = ADD2REAL(YADDR+I-1, YTYPE, YCONV)
            CALL PGPOINT(1,XR,YR,SYMBOL)
            IF(REVERSE) THEN
               XR = -XR
               YR = -YR
               CALL PGPOINT(1,XR,YR,SYMBOL)
            END IF
         END IF
 30   CONTINUE
C
      IF (ID) CALL PGIDEN
      IF (ANNOT.NE.' ') CALL IMGANNOT (ANNOT)
C
C       Wait for a user response
C
C       CALL PGCURSE(XR,YR, XTYPE)
C
C       All done
C
      CALL PGEND
C      
      RETURN
 100  FORMAT(I2)
      END
C
      REAL FUNCTION ADD2REAL(ADDR, VARTYPE, CONV)
C
C Convert an address to a real number
C
C
C      ADDR    INT     input   Address of number to be converted to real
C      VARTYPE CH*(*)  input   Variable type
C      CONV    CH*(*)  input   Complex conversion mode
C
C Audit Trail:
C       Original Version:
C                      W. Young, March 13, 1989
C	Return amplitude instead of real part if input is complex.
C				D.S.Briggs	July 23 1994
C	Pass complex conversion mode in
C				D.S.Briggs	July 28 1994
C---------------------------------------------------------------------
#include        "stdinc.h"
C
C       Passed variables
C
      INTEGER         ADDR
      CHARACTER *(*)  VARTYPE
      CHARACTER *(*)  CONV
C
      REAL R2D
      PARAMETER (R2D=57.295780)
      REAL	RP, IP
C
C       Initialize to zero
C
      ADD2REAL = 0.0
C
C       Do our thing
C
      IF(VARTYPE(1:1) .EQ. 'R')THEN
         ADD2REAL = MEMR(ADDR)
      ELSE IF(VARTYPE(1:1) .EQ. 'I') THEN
         ADD2REAL = REAL(MEMI(ADDR))
      ELSE IF(VARTYPE(1:1) .EQ. 'D') THEN
         ADD2REAL = REAL(MEMD(ADDR))
      ELSE IF(VARTYPE(1:1) .EQ. 'X') THEN
         IF (CONV(1:1).EQ.'A') THEN
            ADD2REAL = ABS(MEMX(ADDR))
         ELSE IF (CONV(1:1).EQ.'P') THEN
            RP = REAL(MEMX(ADDR))
            IP = IMAG(MEMX(ADDR))
            IF ((RP.EQ.0.0).AND.(IP.EQ.0.0)) THEN
               ADD2REAL = 0.0
            ELSE
               ADD2REAL = ATAN2(IP, RP) * R2D
               ADD2REAL = MOD( (ADD2REAL-180.), 360.) +180.
            END IF
         ELSE IF (CONV(1:1).EQ.'R') THEN
            ADD2REAL = REAL(MEMX(ADDR))
         ELSE IF (CONV(1:1).EQ.'I') THEN
            ADD2REAL = IMAG(MEMX(ADDR))
         END IF
      END IF
C
C       Return the value as a real number
C
      RETURN
      END
      
