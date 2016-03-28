C                            LOCAT2.H
C
C  This is one of two common areas which store values relating to location.
C ---------------------------------------------------------------------------
C
C  Convert Degrees to Radians -                       COND2R in AIPS
      DOUBLE PRECISION    L2COND2R
C
C  Reference Pixel Value -                            RPVAL in AIPS
      DOUBLE PRECISION    L2RPVAL(4)
C
C  Reference Pixel Locations -                        RPLOC in AIPS
      REAL                L2RPLOC(4)
C
C  Axis delta(nu)/nu(x) when a FELO axis is present-  AXDENU in AIPS
      DOUBLE PRECISION    L2AXDENU
C
C  Axis Increments -                                  AXINC in AIPS
      REAL                L2AXINC(4)
C
C  Kind of Axis Code -                                AXFUNC in AIPS
      INTEGER             L2AXFUNC(7)
C
C  Position Axis Code -                               AXTYP in AIPS
      INTEGER             L2AXTYP
C
C  Which Position is Which -                          CORTYP in AIPS
      INTEGER             L2CORTYP
C
C  Special x,y label request -                        LABTYP in AIPS
      INTEGER             L2LABTYP
C
C  Number of characters in L2SAXLAB -               NCHLAB in AIPS
      INTEGER             L2NCHLAB(2)
C
C  Rotation Angle of Position Axes -                  ROT in AIPS
      REAL                L2ROTVAL
C
C  Extra Sign to Apply to Rotation -                  SGNROT in AIPS
      INTEGER             L2ROTSGN
C
C  Value of Idepth from SETLOC call -                 ZDEPTH in AIPS
      INTEGER             L2ZDEPTH(5)
C
C  Relative Number of Zaxis -                         ZAXIS in AIPS
      INTEGER             L2ZAXIS
C
C  Parameters Needed for geometry -                   GEOMDx in AIPS
      DOUBLE PRECISION    L2GEOMD1, L2GEOMD2
      DOUBLE PRECISION    L2GEOMD3, L2GEOMD4
C
C  0-Relative Axis Number  Longitude, Latitude, Frequency, Stokes
      INTEGER             L2KL, L2KM, L2KF, L2KS
C
C  "Primary Axis" 3, "Primary Axis" 4
      INTEGER             L2KA, L2KB
C
C
      COMMON /LOC2N/ L2RPVAL, L2RPLOC,
     2                   L2AXDENU, L2AXINC, L2AXFUNC, L2AXTYP,
     2                   L2CORTYP, L2LABTYP, L2NCHLAB,
     3                   L2ROTVAL, L2ROTSGN, L2ZDEPTH, L2ZAXIS,
     4                   L2GEOMD1, L2GEOMD2, L2GEOMD3, L2GEOMD4,
     5                   L2KL, L2KM, L2KF, L2KS,
     6                   L2KA, L2KB, L2COND2R
C
C  Axis types -                                       CTYP in AIPS
       CHARACTER*8                L2AXTYPS(4)
C
C  X,Y Axes prefixes for labeling -                   CPREF in AIPS
       CHARACTER*8                L2PREFIX(2)
C
C  Labels for axes 3 and 4 values -                   SAXLAB in AIPS
       CHARACTER*20               L2SAXLAB(2)
C
C
       COMMON /LOC2C/ L2AXTYPS, L2PREFIX, L2SAXLAB
