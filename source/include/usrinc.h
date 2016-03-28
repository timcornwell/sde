C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C	(C)opyright 1989 by Associated Universities, Inc.; all rights reserved
C
C
C User interface constants
C
C Maximum number input parameters
      INTEGER USRMXPRS               
      PARAMETER (USRMXPRS = 100)
C Maximum parameter array size
      INTEGER USRMXSIZ               
      PARAMETER (USRMXSIZ = 10)
C Maximum parameter array dimension
      INTEGER USRMXDIM               
      PARAMETER (USRMXDIM = 2)
C Database parameter directory
      CHARACTER *(*) USRPRDIR        
      PARAMETER (USRPRDIR = 'usrpardir')
