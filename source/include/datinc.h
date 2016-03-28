C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C	(C)opyright 1989 by Associated Universities, Inc.; all rights reserved
C
C
C Database error variable
C
      INTEGER DATERROR
      COMMON /DATERR/ DATERROR
C
C Database error parameters
C
C No error
      INTEGER DATOK			
      PARAMETER (DATOK = 0)
C Serious internal error
      INTEGER DATFATAL			
      PARAMETER (DATFATAL = 1)
C Database corrupt
      INTEGER DATCRRPT		
      PARAMETER (DATCRRPT = 2)
C Bad ID supplied
      INTEGER DATBADID			
      PARAMETER (DATBADID = 3)
C Bad node or link name supplied
      INTEGER DATBADNM		
      PARAMETER (DATBADNM = 4)
C Bad type code supplied
      INTEGER DATBADCD		
      PARAMETER (DATBADCD = 5)
C Bad size supplied
      INTEGER DATBADSZ		
      PARAMETER (DATBADSZ = 6)
C Bad context supplied
      INTEGER DATBADCN		
      PARAMETER (DATBADCN = 7)
C No memory available
      INTEGER DATNOMEM			
      PARAMETER (DATNOMEM = 8)
C Node not found
      INTEGER DATNONOD		
      PARAMETER (DATNONOD = 9)
C Named linkage not found
      INTEGER DATNOLNK		
      PARAMETER (DATNOLNK = 10)
C Linkage name prevents deletion
      INTEGER DATNODEL			
      PARAMETER (DATNODEL = 11)
C No application array found
      INTEGER DATNOARR			
      PARAMETER (DATNOARR = 12)
C No free context slots
      INTEGER DATNOCNT		
      PARAMETER (DATNOCNT = 13)
C Node has wrong type
      INTEGER DATMISM		
      PARAMETER (DATMISM = 14)
C Returned value truncated
      INTEGER DATTRUNC		
      PARAMETER (DATTRUNC = 15)
C Duplicate application array
      INTEGER DATDUPAR		
      PARAMETER (DATDUPAR = 16)
C Cache is full
      INTEGER DATCCHFL		
      PARAMETER (DATCCHFL = 17)
C Not found
      INTEGER DATNTFND		
      PARAMETER (DATNTFND = 18)
C Already exists
      INTEGER DATEXSTS
      PARAMETER (DATEXSTS = 19)
C Node is null
      INTEGER DATNLLND		
      PARAMETER (DATNLLND = 20)

