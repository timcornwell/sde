C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C
C @(#)ftsinc.h	1.4    11/17/90
C
C Include file for FTS routines
C
      INTEGER		FTSBLOCK, FTSLEN, FTSCARD, FTSNCOLS
      INTEGER		FTSLNKEY, FTSLNOBJ, FTSNKEYS
      PARAMETER		(FTSLEN = 80)
      PARAMETER		(FTSBLOCK = 2880)
      PARAMETER		(FTSCARD = FTSBLOCK / FTSLEN)
      PARAMETER		(FTSNCOLS = 10)
      PARAMETER		(FTSLNKEY = 8)
      PARAMETER		(FTSLNOBJ = 58)
      PARAMETER		(FTSNKEYS = 121)
      CHARACTER*(FTSLNKEY)	FTSKEYS (2, FTSNKEYS)
      COMMON		/FTIC/ FTSKEYS

















