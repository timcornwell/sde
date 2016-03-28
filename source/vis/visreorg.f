C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)visreorg.f	1.5    28 Jul 1995
C
      SUBROUTINE VISREORG (VISROOT, OVISROOT, NPC, NUMINT)
C
CD Reorganizes the visibilities from a sub-directory for each
C pointing-integration to a subdirectory for each pointing.
C
C THE ASSUMPTION IS MADE THAT EACH SUBDIRECTORY is 1 INTEGRATION TIME LONG!
C
C	VISROOT	CH*(*)	input	Head of visibility database
C	OVISROOT CH*(*)	input	Head of OLD visibility database
C	NPC	INT	input	Number of pointing centers
C	NUMINT	INT	input	Number of integrations
C
C Audit trail:	
C	Original version
C				M.A.Holdaway	Dec 11, 1989
C	Now uses HEDCOPY	
C				M.A.Holdaway	Nov 12 1990
C	Now also copies the new SIMUV arrays over as well
C				M.A.Holdaway	Nov 20 1991
C	Fixed a DATGETAR call which GAVE TYPE='R'
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	VISROOT, OVISROOT
      INTEGER		NPC, NUMINT
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISREORG')
C
      CHARACTER*(SYSMXNAM)	OLDVIS, NEWVIS, STRM2, STRRMBL
      INTEGER			INUMINT, IPC, NVIS, IVIS, NVISTOT
      CHARACTER*6		STRINT
      CHARACTER*1		ATYPE
      INTEGER		U1ADD, U2ADD, V1ADD, V2ADD, W1ADD, W2ADD
      INTEGER		B1ADD, B2ADD, T1ADD, T2ADD, VS1ADD, VS2ADD
      INTEGER		WT1ADD, WT2ADD, ADD
      REAL		CRPIX(SYSMXDIM), CDELT(SYSMXDIM), 
     $   		CROTA(SYSMXDIM)
      CHARACTER*8	CTYPE(SYSMXDIM)
      DOUBLE PRECISION	CRVAL(SYSMXDIM)
      INTEGER		NAX, INDEX2, NAXIS(SYSMXDIM), SNAX, SNAXIS(SYSMXDIM)
      INTEGER		ST2ADD, EL2ADD, AZ2ADD, PA2ADD, NANT, IANT
      INTEGER		EL1ADD, AZ1ADD, PA1ADD, DUMMY
C
      LOGICAL		NEWARRS
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      DO 150 IPC = 1, NPC
         OLDVIS = STRM2 (OVISROOT, 'PC'//STRINT(IPC)//'I1' )
         OLDVIS = STRRMBL(OLDVIS)
C        MESSAGE = 'Taking header info from '//OLDVIS
C        CALL MSGPUT (MESSAGE, 'I')
         CALL DATGETAR (STRM2(OLDVIS,'TIME'), NAX, NAXIS, ATYPE, ADD)
         NVIS = NAXIS(1)
         NVISTOT = NAXIS(1) * NUMINT
C
         NEWVIS = STRM2 ( VISROOT, 'PC'//STRINT(IPC) )
         NEWVIS = STRRMBL (NEWVIS)
         CALL DATCREAT (NEWVIS) 
         CALL DATCREAT (STRM2(NEWVIS, 'OBS'))
         CALL DATCREAT (STRM2(NEWVIS, 'OBS/I'))
         CALL DATSETTP (NEWVIS, 'VIS')
         IF (ERROR) GO TO 990
C
         CALL HEDCOPY (OLDVIS, NEWVIS)
C
         CALL CRDGET (STRM2(OLDVIS, 'OBS/I'), NAX, CTYPE, NAXIS, CRVAL,
     $      CRPIX, CDELT, CROTA)
         CALL CRDPUT (STRM2(NEWVIS, 'OBS/I'), NAX, CTYPE, NAXIS, CRVAL, 
     1      CRPIX, CDELT, CROTA)
         IF (ERROR) GO TO 990
C
C Make new arrays under NEWVIS, large enough for NVIS * NUMINT
C
         NAX = 1
         NAXIS(1) = NVISTOT
         CALL DATMAKAR (STRM2(NEWVIS, 'UU'), NAX, NAXIS, 'R', U2ADD)
         CALL DATMAKAR (STRM2(NEWVIS, 'VV'), NAX, NAXIS, 'R', V2ADD)
         CALL DATMAKAR (STRM2(NEWVIS, 'WW'), NAX, NAXIS, 'R', W2ADD)
         CALL DATMAKAR (STRM2(NEWVIS, 'BASELINE'), NAX, NAXIS, 'R', 
     $      B2ADD)
         CALL DATMAKAR (STRM2(NEWVIS, 'TIME'), NAX, NAXIS, 'R', T2ADD)
         CALL DATMAKAR (STRM2(NEWVIS, 'OBS/I/VIS'), NAX, NAXIS, 'X', 
     $      VS2ADD)
         CALL DATMAKAR (STRM2(NEWVIS, 'OBS/I/WT'), NAX, NAXIS, 'R', 
     $      WT2ADD)
C
         IF (ERROR) GO TO 990
         OLDVIS = STRM2 (OVISROOT, 'PC'//STRINT(IPC)//'I1')
         OLDVIS = STRRMBL(OLDVIS)
         CALL ARRCOPY  (STRM2(OVISROOT, 'RX'), STRM2(NEWVIS, 'RX'))
         CALL ARRCOPY  (STRM2(OVISROOT, 'RY'), STRM2(NEWVIS, 'RY'))
         CALL ARRCOPY  (STRM2(OVISROOT, 'RZ'), STRM2(NEWVIS, 'RZ'))
         IF (ERROR) THEN
            CALL ERRCANCE
            NEWARRS = .FALSE.
         ELSE
            NEWARRS = .TRUE.
         ENDIF
C
         IF (NEWARRS) THEN
            CALL DATGETAR (STRM2(NEWVIS, 'RX'), SNAX, SNAXIS, ATYPE, 
     $         DUMMY)
            NANT = SNAXIS(1)
            SNAX = 1
            SNAXIS(1) = NUMINT
            CALL DATMAKAR (STRM2(NEWVIS, 'STIME'), SNAX, SNAXIS, 'R', 
     $         ST2ADD)
            SNAX = 2
            SNAXIS(1) = NANT
            SNAXIS(2) = NUMINT
            CALL DATMAKAR (STRM2(NEWVIS, 'EL'), SNAX, SNAXIS, 'R', 
     $         EL2ADD)
            CALL DATMAKAR (STRM2(NEWVIS, 'AZ'), SNAX, SNAXIS, 'R', 
     $         AZ2ADD)
            CALL DATMAKAR (STRM2(NEWVIS, 'PA'), SNAX, SNAXIS, 'R', 
     $         PA2ADD)
            IF (ERROR) GO TO 990
         ENDIF
C
C Loop through each integration time, copy VIS, etc, into NEWVIS
C
         DO 130 INUMINT= 1, NUMINT
            OLDVIS = STRM2 (OVISROOT, 
     $              'PC'//STRINT(IPC)//'I'//STRINT(INUMINT) )
            OLDVIS = STRRMBL(OLDVIS)
            CALL DATGETAR (STRM2(OLDVIS, 'UU'), 
     $                                  NAX, NAXIS, ATYPE, U1ADD)
            CALL DATGETAR (STRM2(OLDVIS, 'VV'), 
     $          			NAX, NAXIS, ATYPE, V1ADD)
            CALL DATGETAR (STRM2(OLDVIS, 'WW'), 
     $					NAX, NAXIS, ATYPE, W1ADD)
            CALL DATGETAR (STRM2(OLDVIS, 'BASELINE'), 
     $                                  NAX, NAXIS, ATYPE, B1ADD)
            CALL DATGETAR (STRM2(OLDVIS, 'TIME'), NAX, NAXIS, ATYPE, 
     $         T1ADD)
            CALL DATGETAR (STRM2(OLDVIS, 'OBS/I/VIS'), 
     $         				NAX, NAXIS, ATYPE, VS1ADD)
            CALL DATGETAR (STRM2(OLDVIS, 'OBS/I/WT'), 
     $         				NAX, NAXIS, ATYPE, WT1ADD)
            CALL DATGETAR (STRM2(OLDVIS, 'EL'), NAX, NAXIS, ATYPE, 
     $           EL1ADD)
            CALL DATGETAR (STRM2(OLDVIS, 'AZ'), NAX, NAXIS, ATYPE,
     $           AZ1ADD)
            CALL DATGETAR (STRM2(OLDVIS, 'PA'), NAX, NAXIS, ATYPE,
     $           PA1ADD)
            IF (ERROR) GO TO 990
C
C Loop through VISIBILITY type data
C
            DO 120 IVIS = 0, NVIS-1
               INDEX2 = (INUMINT - 1)*NVIS + IVIS
               MEMR (B2ADD + INDEX2)  = MEMR (B1ADD + IVIS)
               MEMR (U2ADD + INDEX2)  = MEMR (U1ADD + IVIS)
               MEMR (V2ADD + INDEX2)  = MEMR (V1ADD + IVIS)
               MEMR (W2ADD + INDEX2)  = MEMR (W1ADD + IVIS)
               MEMR (WT2ADD + INDEX2) = MEMR (WT1ADD + IVIS)
               MEMR (T2ADD + INDEX2)  = MEMR (T1ADD + IVIS)
               MEMX (VS2ADD + INDEX2) = MEMX (VS1ADD + IVIS)
 120        CONTINUE
C
C Loop through ANT, SCAN type data
C
            IF (NEWARRS) THEN
               MEMR (ST2ADD +  INUMINT - 1) = MEMR(T1ADD)
               DO 125 IANT = 0, NANT - 1
                  INDEX2 = (INUMINT - 1)*NANT + IANT
                  MEMR (PA2ADD + INDEX2) = MEMR (PA1ADD + IANT)
                  MEMR (EL2ADD + INDEX2) = MEMR (EL1ADD + IANT)
                  MEMR (AZ2ADD + INDEX2) = MEMR (AZ1ADD + IANT)
 125           CONTINUE
            ENDIF
 130     CONTINUE
 150  CONTINUE

C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
