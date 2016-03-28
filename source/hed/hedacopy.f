C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)hedacopy.f	1.1    6/7/93
C
      SUBROUTINE HEDACOPY (IN, OUT)
C
CD Copy header to another directory entry. Augment only -> Do not overwrite
C   any preexisting item.  Do not copy array: only the header items. Do not
C   copy links either!
C
C	IN	CH*(*)	input	Name of input image
C	OUT	CH*(*)	input	Name of output image
C
C Audit trail:
C	Cloned from HEDCOPY
C				D.S.Briggs	Apr 29 1993
C-------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	IN, OUT
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'HEDACOPY')
C
      CHARACTER*(SYSMXNAM)	NLIST(100)
      CHARACTER*1	NTYPE(100)
      INTEGER		NSIZE(100)
C
      INTEGER		NODES, INODE
      REAL		RVALUE(10)
      INTEGER		IVALUE(10), NDUMMY
      DOUBLE PRECISION	DVALUE(10)
      LOGICAL		LVALUE(10)
      COMPLEX		XVALUE(10)
      CHARACTER*1	CVALUE(1000)
C
      CHARACTER*(SYSMXNAM)	STRM2
      LOGICAL		DATEXIST
C===================================================================
      IF (ERROR) GO TO 999
C
      NODES = 0
      CALL DATHEDLI (IN, NODES, NLIST, NTYPE, NSIZE)
      IF(NODES.EQ.0) THEN
         GO TO 999
      END IF
      IF (ERROR) GO TO 990
      DO 10 INODE = 1, NODES
C
C Don't copy links!
C
         IF (NLIST(INODE).EQ.'DATLNARR') GO TO 10
C
C Don't overwrite anything there already!
C
         IF (DATEXIST(STRM2(OUT,NLIST(INODE)))) GO TO 10
C
         IF (NTYPE(INODE).EQ.'R') THEN
            CALL DATGETR (IN,  NLIST(INODE), RVALUE, NSIZE (INODE),
     1         NDUMMY)
            CALL DATPUTR (OUT, NLIST(INODE), RVALUE, NSIZE (INODE))
         ELSE IF (NTYPE(INODE).EQ.'I') THEN
            CALL DATGETI (IN,  NLIST(INODE), IVALUE, NSIZE (INODE),
     1         NDUMMY)
            CALL DATPUTI (OUT, NLIST(INODE), IVALUE, NSIZE (INODE))
         ELSE IF (NTYPE(INODE).EQ.'D') THEN
            CALL DATGETD (IN,  NLIST(INODE), DVALUE, NSIZE (INODE),
     1         NDUMMY)
            CALL DATPUTD (OUT, NLIST(INODE), DVALUE, NSIZE (INODE))
         ELSE IF (NTYPE(INODE).EQ.'C') THEN
            CALL DATGETC (IN,  NLIST(INODE), CVALUE, NSIZE (INODE), 
     1         NDUMMY)
            CALL DATPUTC (OUT, NLIST(INODE), CVALUE, NSIZE (INODE))
         ELSE IF (NTYPE(INODE).EQ.'L') THEN
            CALL DATGETL (IN,  NLIST(INODE), LVALUE, NSIZE (INODE),
     1         NDUMMY)
            CALL DATPUTL (OUT, NLIST(INODE), LVALUE, NSIZE (INODE))
         ELSE IF (NTYPE(INODE).EQ.'X') THEN
            CALL DATGETX (IN,  NLIST(INODE), XVALUE, NSIZE (INODE),
     1         NDUMMY)
            CALL DATPUTX (OUT, NLIST(INODE), XVALUE, NSIZE (INODE))
         END IF
C         
  10  CONTINUE
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END

