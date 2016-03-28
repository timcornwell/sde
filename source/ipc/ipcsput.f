C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)ipcsput.f	1.8    7/21/93
C
      SUBROUTINE IPCSPUT (NAME, FILENAME)
C
CD Put ipcs data to a file. 
C
C	NAME		CH*(*)	input	NAME of file as specified to user
C	FILENAME	CH*(*)	input	Name of file
C Audit trail:
C      Cloned from visput
C                              R.G. Marson     Feb 1 1989
C      Added a lot for defaults for header items
C                              R. G. Marson    Dec 10 1992
C      Fixed a small bug in number of CRD axis for FTS files
C                              R.G. Marson     Feb 4 1992
C
C-------------------------------------------------------------------
#include	"stdinc.h"
C
C
C Input Variables
C
       CHARACTER*(*) 	NAME, FILENAME
C
C Parameter Definitions
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IPCSPUT')
C
C Function definitions
C
      CHARACTER*(SYSMXNAM) STRM2
      REAL              PIXRMAX, PIXRMIN
      INTEGER           DATFGETI
      LOGICAL           DATEXIST
C
C Local Variables
C
      CHARACTER*(SYSMXNAM) FILESYS
      CHARACTER*8       CTYPE(SYSMXDIM) 
      CHARACTER*1       ATYPE
      DOUBLE PRECISION  RVAL(SYSMXDIM)
      REAL              RPIX(SYSMXDIM), DELT(SYSMXDIM),ROTA(SYSMXDIM)
      INTEGER           WINDOW_W, WINDOW_H, FR_START, FR_END, GCOUNT
      INTEGER           NAX, NAXIS(SYSMXDIM), ADD, WMIN
      LOGICAL           STRMATCH
C
C==================================================================
C
      IF (ERROR) GO TO 999
C
C Check for null file name
C
      IF (FILENAME.EQ.' ') THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Null file name')
         GO TO 999
      END IF
C
C     Check that name is of the IPCS type
C
      CALL DATCHKTP(NAME, 'IPCS')
      CALL DATSETTP(NAME, 'IPCS')
C
C Set header items that are true for all IPCS groups files
C
      CALL DATPUTL(NAME, 'GROUPS', .TRUE., 1)
      CALL DATPUTI(NAME, 'PCOUNT', 3, 1)
      CALL DATPUTC(NAME, 'PTYPE1', 'XPIXEL', 1)
      CALL DATPUTC(NAME, 'PTYPE2', 'YPIXEL', 1)
      CALL DATPUTC(NAME, 'PTYPE3', 'FRAME', 1)
C
C Check other important header items are present 
C
      IF (.NOT.DATEXIST(STRM2(NAME, 'WINDOW_W'))) THEN
         CALL DATGETAR(STRM2(NAME, 'XPIXEL'), NAX, NAXIS, ATYPE, ADD)
         WMIN = NINT(PIXRMIN(MEMR(ADD), NAXIS(1)))
         IF (WMIN.NE.0) 
     $        CALL PIXRSCAL(MEMR(ADD), 1.0, FLOAT(-WMIN), 
     $        MEMR(ADD), NAXIS(1))
         WINDOW_W = NINT(PIXRMAX(MEMR(ADD), NAXIS(1))) + 1
         CALL DATPUTI(NAME, 'WINDOW_W', WINDOW_W, 1)
         WRITE(MESSAGE, '(A, I3)') 'Setting the Window Width to ', 
     $        WINDOW_W
         CALL MSGPUT(MESSAGE, 'W')
      END IF
C
      IF (.NOT.DATEXIST(STRM2(NAME, 'WINDOW_H'))) THEN
         CALL DATGETAR(STRM2(NAME, 'YPIXEL'), NAX, NAXIS, ATYPE, ADD)
         WMIN = NINT(PIXRMIN(MEMR(ADD), NAXIS(1)))
         IF (WMIN.NE.0) 
     $        CALL PIXRSCAL(MEMR(ADD), 1.0, FLOAT(-WMIN), 
     $        MEMR(ADD), NAXIS(1))
         WINDOW_H = NINT(PIXRMAX(MEMR(ADD), NAXIS(1))) + 1
         CALL DATPUTI(NAME, 'WINDOW_H', WINDOW_H, 1)
         WRITE(MESSAGE, '(A, I3)') 'Setting the Window Height to ', 
     $        WINDOW_H
         CALL MSGPUT(MESSAGE, 'W')
      END IF
C
      IF (.NOT.DATEXIST(STRM2(NAME, 'FR_START'))) THEN
         CALL DATGETAR(STRM2(NAME, 'FRAME'), NAX, NAXIS, ATYPE, ADD)
         FR_START = PIXRMIN(MEMR(ADD), NAXIS(1))
         CALL DATPUTI(NAME, 'FR_START', FR_START, 1)
         WRITE(MESSAGE, '(A, I6)') 'Setting the Starting Frame to ', 
     $        FR_START
         CALL MSGPUT(MESSAGE, 'W')
      END IF
C
      IF (.NOT.DATEXIST(STRM2(NAME, 'FR_END'))) THEN
         FR_END = PIXRMAX(MEMR(ADD), NAXIS(1))
         CALL DATPUTI(NAME, 'FR_END', FR_END, 1)
         WRITE(MESSAGE, '(A, I6)') 'Setting the Ending Frame to ', 
     $        FR_END
         CALL MSGPUT(MESSAGE, 'W')
      END IF
C
      IF (.NOT.DATEXIST(STRM2(NAME, 'SORT'))) THEN
         CALL DATPUTC(NAME, 'SORT', 'IX', 1)
         WRITE(MESSAGE, '(A)') 'Sorting the Data into Increasing Frame'
         CALL MSGPUT(MESSAGE, 'W')
         CALL IPCSSORT(NAME, 'IF')
      END IF
C
      IF (.NOT.DATEXIST(STRM2(NAME, 'FTIME'))) THEN
         CALL DATPUTR(NAME, 'FTIME', 6.E-3, 1)
         WRITE(MESSAGE, '(A)') 'Setting the Frame Time to 6 ms'
         CALL MSGPUT(MESSAGE, 'W')
      END IF
C
      GCOUNT = 0
      IF (DATEXIST(STRM2(NAME, 'GCOUNT'))) THEN
         GCOUNT = DATFGETI(NAME, 'GCOUNT')
      END IF
      IF (GCOUNT.LE.0) THEN
         CALL DATGETAR(NAME, NAX, NAXIS, ATYPE, ADD)
         GCOUNT = NAXIS(NAX)
         CALL DATPUTI(NAME, 'GCOUNT', GCOUNT, 1)
         WRITE(MESSAGE, '(A, I8)') 
     $        'Setting the total number of Photons to ', GCOUNT
         CALL MSGPUT(MESSAGE, 'W')
      END IF
C
C Put the data into standard format
C 
      CTYPE(1) = 'PHOTONS'
      NAXIS(1) = 1
      RVAL(1) = 0.
      RPIX(1) = 1.
      DELT(1) = 1.
      ROTA(1) =  0.
      CTYPE(2) = 'GROUPS'
      NAXIS(2) = GCOUNT
      RVAL(2) = 0.
      RPIX(2) = 1.
      DELT(2) = 1.
      ROTA(2) =  0.
      CALL CRDPUT(NAME, 2, CTYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
C
C Keep a record of the Processing done 
C
      CALL HISINPUT(NAME)
C
C Find file system type (SDE or FTS) by looking at filename
C  extension. (default = FTS)
C
      CALL FILSYSEX (FILENAME, FILESYS)
C
C If this is an SDE file then just write it
C
      IF (STRMATCH (FILESYS, 'SDE')) THEN
         CALL DATWRITE (NAME, FILENAME)
      ELSE IF (STRMATCH (FILESYS, 'FTS')) THEN
C
C Put the groups file
C
         CALL FILGRPPU (NAME, FILENAME, '*', ' ', FILESYS)
      ELSE IF (STRMATCH (FILESYS, 'FIG')) THEN
         CALL IPCSFIG(NAME)
         CALL DATPUTC (NAME,'FILNAME', FILENAME, 1)
         CALL DATPUTC (NAME,'FILACCESS', 'WRITE', 1)
         CALL DATPUTC (NAME,'FILSYS', 'FIG', 1)
         CALL FTSOPEN (NAME)
         CALL FTSIMGWR (NAME)
         CALL FTSCLOSE (NAME)
       ELSE
         MESSAGE = 'File system: '//FILESYS//' not supported'
         CALL ERRREPOR (ERRWRGTP, ROUTINE, MESSAGE)
         GOTO 999
      END IF
C
      IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
 999  CONTINUE
      END
