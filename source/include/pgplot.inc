C-----------------------------------------------------------------------
C PGPLOT: common block definition.
C-----------------------------------------------------------------------
C General parameters:
C
C IDENT   GRPCKG plot identifier from call to GROPEN, set in PGBEGIN.
C PGOPEN  =0 if PGBEGIN has not been called; =1 between call of
C         PGBEGIN and PGEND. Set to 1 by PGBEGIN and set to 0 by
C         PGEND. Assumed to be initialized to 0 by compiler.
C ADVSET  Set to 0 by PGBEGIN, set to 1 by PGPAGE; used to suppress
C         the prompt for the first page.
C NX      Number of x subdivisions (= argument of PGBEGIN).
C NY      Number of y subdivisions (= argument of PGBEGIN).
C NXC     Ordinal number of current X subdivision.
C NYC     Ordinal number of current Y subdivision (NXC and NYC are
C         initialized by PGBEGIN and adjusted by PGENV).
C XPERIN  Plot x scale in dots/inch, initialized by PGBEGIN.
C YPERIN  Plot y scale in dots/inch, initialized by PGBEGIN.
C XSP     Character X spacing (dots), initialized by PGBEGIN.
C YSP     Character Y spacing (dots), initialized by PGBEGIN.
C XSZ     X dimension of subpage (dots), initialized by PGBEGIN.
C YSZ     Y dimension of subpage (dots), initialized by PGBEGIN.
C TRANS   Transformation matrix for contour plots; copied
C         from argument list by PGCONT and used by PGCP.
C PROMPT  If .TRUE., ask user before clearing page; set by PGASK
C         and (indirectly) by PGBEGIN, used in PGENV.
C FLUSH   Not used.
C PGFAS   Fill-area Style attribute value.
C PGCHSZ  Character Height attribute value.
C PGBLEV  Buffering level: incremented by PGBBUF, decremented by
C         PGEBUF.
C PGROWS  TRUE if subpages are used in row order, FALSE for column
C         order.
C PGPFIX  TRUE if PGPAP has been called, FALSE otherwise.
C PGAHS   arrow-head fill style
C PGAHA   arrow-head angle
C PGAHV   arrow-head vent
C PGTBCI  text background color index
C PGMNCI, PGMXCI range of color indices available to PGGRAY/PGIMAG
C PGITF   type of transfer function used by PGGRAY/PGIMAG
C
C Viewport parameters (all are absolute [device] coordinates):
C
C XOFF    X coordinate of blc of viewport.
C YOFF    Y coordinate of blc of viewport.
C XVP     X coordinate of blc of viewport, relative to blc of subpage.
C YVP     Y coordinate of blc of viewport, relative to blc of subpage.
C XLEN    Width of viewport. 
C YLEN    Height of viewport.
C
C Scaling parameters:
C
C XORG    Absolute coordinate value corresponding to world X=0.
C YORG    Absolute coordinate value corresponding to world Y=0.
C XSCALE  Scale in x (absolute units per world coordinate unit).
C YSCALE  Scale in y (absolute units per world coordinate unit).
C
C Window parameters (all are world coordinate values):
C
C XBLC    X at blc of box.
C XTRC    X at trc of box.
C YBLC    Y at blc of box.
C YTRC    Y at trc of box.
C-----------------------------------------------------------------------
      INTEGER IDENT,PGOPEN,ADVSET,NX,NY,NXC,NYC, PGFAS, PGBLEV, PGAHS
      INTEGER PGTBCI, PGMNCI, PGMXCI, PGCINT, PGCMIN, PGITF
      REAL    XPERIN,YPERIN,XSP,YSP,XSZ,YSZ
      REAL    XVP,YVP,XOFF,YOFF,XLEN,YLEN,XORG,YORG
      REAL    XSCALE,YSCALE,XBLC,XTRC,YBLC,YTRC,TRANS(6),PGCHSZ
      REAL    PGAHA,PGAHV
      LOGICAL PROMPT,FLUSH,PGROWS,PGPFIX
      CHARACTER*32 PGCLAB
      COMMON  /PGPLT1/ IDENT,PGOPEN,ADVSET,NX,NY,NXC,NYC,
     1        XPERIN,YPERIN,XSP,YSP,XSZ,YSZ,
     2        XVP,YVP,XOFF,YOFF,XLEN,YLEN,XORG,YORG,
     3        XSCALE,YSCALE,XBLC,XTRC,YBLC,YTRC,TRANS,
     4        PROMPT,FLUSH,PGFAS,PGCHSZ,PGBLEV,PGROWS,
     5        PGAHS,PGAHA,PGAHV,PGTBCI,PGMNCI,PGMXCI,PGCINT,PGCMIN,
     6        PGPFIX,PGITF
      COMMON  /PGPLT2/ PGCLAB
      SAVE    /PGPLT1/
      SAVE    /PGPLT2/
C-----------------------------------------------------------------------
