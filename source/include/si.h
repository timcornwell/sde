C
C Include file for SI programs
C
      INTEGER		NANT, NBASE
      PARAMETER		(NANT = 4)
      PARAMETER		(NBASE = NANT * (NANT + 1)/2)
      INTEGER		NUMINT
      REAL		THMAX, THINT, ROT, TINT, FREQ, WAVE, BAND
      REAL		TELDIAM, ANTLOC(NANT)
      COMMON		/SICOM/ THMAX, THINT, TINT, ROT, NUMINT, FREQ, 
     &			WAVE, BAND, ANTLOC, TELDIAM
