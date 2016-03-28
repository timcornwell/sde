C                            LOCAT1.H
C
C  This is one of two common areas which store values relating to location.
C ---------------------------------------------------------------------------
C
C  Convert Degrees to Radians -                       COND2R in AIPS
      DOUBLE PRECISION    L1COND2R
C
C  Reference Pixel Value -                            RPVAL in AIPS
      DOUBLE PRECISION    L1RPVAL(4)
C
C  Reference Pixel Locations -                        RPLOC in AIPS
      REAL                L1RPLOC(4)
C
C  Axis delta(nu)/nu(x) when a FELO axis is present-  AXDENU in AIPS
      DOUBLE PRECISION    L1AXDENU
C
C  Axis Increments -                                  AXINC in AIPS
      REAL                L1AXINC(4)
C
C  Kind of Axis Code -                                AXFUNC in AIPS
      INTEGER             L1AXFUNC(7)
C
C  Position Axis Code -                               AXTYP in AIPS
      INTEGER             L1AXTYP
C
C  Which Position is Which -                          CORTYP in AIPS
      INTEGER             L1CORTYP
C
C  Special x,y label request -                        LABTYP in AIPS
      INTEGER             L1LABTYP
C
C  Number of characters in L1SAXLAB -               NCHLAB in AIPS
      INTEGER             L1NCHLAB(2)
C
C  Rotation Angle of Position Axes -                  ROT in AIPS
      REAL                L1ROTVAL
C
C  Extra Sign to Apply to Rotation -                  SGNROT in AIPS
      INTEGER             L1ROTSGN
C
C  Value of Idepth from SETLOC call -                 ZDEPTH in AIPS
      INTEGER             L1ZDEPTH(5)
C
C  Relative Number of Zaxis -                         ZAXIS in AIPS
      INTEGER             L1ZAXIS
C
C  Parameters Needed for geometry -                   GEOMDx in AIPS
      DOUBLE PRECISION    L1GEOMD1, L1GEOMD2
      DOUBLE PRECISION    L1GEOMD3, L1GEOMD4
C
C  0-Relative Axis Number  Longitude, Latitude, Frequency, Stokes
      INTEGER             L1KL, L1KM, L1KF, L1KS
C
C  "Primary Axis" 3, "Primary Axis" 4
      INTEGER             L1KA, L1KB
C
C
      COMMON /LOC1N/ L1RPVAL, L1RPLOC,
     1                   L1AXDENU, L1AXINC, L1AXFUNC, L1AXTYP,
     2                   L1CORTYP, L1LABTYP, L1NCHLAB,
     3                   L1ROTVAL, L1ROTSGN, L1ZDEPTH, L1ZAXIS,
     4                   L1GEOMD1, L1GEOMD2, L1GEOMD3, L1GEOMD4,
     5                   L1KL, L1KM, L1KF, L1KS,
     6                   L1KA, L1KB, L1COND2R
C
C  Axis types -                                       CTYP in AIPS
       CHARACTER*8                L1AXTYPS(4)
C
C  X,Y Axes prefixes for labeling -                   CPREF in AIPS
       CHARACTER*8                L1PREFIX(2)
C
C  Labels for axes 3 and 4 values -                   SAXLAB in AIPS
       CHARACTER*20               L1SAXLAB(2)
C
C
       COMMON /LOC1C/ L1AXTYPS, L1PREFIX, L1SAXLAB
