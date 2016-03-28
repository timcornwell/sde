C++
	SUBROUTINE FLYSSCAL (VIS, CLASS, MODEL, SCAL, MODE, TAMP, TPHASE)
C
C	I	INT	input	Patch number
C	CMP	CHAR	output	Name of components image for this patch
C
C Audit trail:
C	Fixed deletion of old solutions
C				T.J.Cornwell	Sept 14 1992
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	VIS, CLASS, MODEL, SCAL, MODE
      REAL		TAMP, TPHASE
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FLYSSCAL')
C
      CHARACTER*(SYSMXNAM)	STRM3
C==================================================================
C
      IF ((MODE.EQ.'AMPPHI').OR.(MODE.EQ.'AMPNORMPHI')) THEN
         IF(TAMP.EQ.TPHASE) THEN
            CALL MSGPUT ('Correcting amplitude and phase together', 
     &         'I')
            CALL DATPUTR (VIS, 'TINT', TAMP, 1)
            CALL VISSCAL (VIS, CLASS, MODEL, SCAL, MODE)
            CALL DATDELET (STRM3(VIS, SCAL, 'ANTGAIN'))
            CALL DATDELET (STRM3(VIS, SCAL, 'GAINTIME'))
            CALL DATDELET (STRM3(VIS, SCAL, 'ORES'))
            CALL DATDELET (STRM3(VIS, SCAL, 'NRES'))
         ELSE 
            CALL MSGPUT (
     &         'Correcting amplitude and phase on different timescales', 
     &         'I')
            CALL MSGPUT ('First phase only', 'I')
            CALL DATPUTR (VIS, 'TINT', TPHASE, 1)
            CALL VISSCAL (VIS, CLASS, MODEL, SCAL, ' ')
            CALL DATDELET (STRM3(VIS, SCAL, 'ANTGAIN'))
            CALL DATDELET (STRM3(VIS, SCAL, 'GAINTIME'))
            CALL DATDELET (STRM3(VIS, SCAL, 'ORES'))
            CALL DATDELET (STRM3(VIS, SCAL, 'NRES'))
            CALL MSGPUT ('Now amplitude and phase', 'I')
            CALL DATPUTR (VIS, 'TINT', TAMP, 1)
            CALL VISSCAL (VIS, SCAL, MODEL, SCAL, MODE)
            CALL DATDELET (STRM3(VIS, SCAL, 'ANTGAIN'))
            CALL DATDELET (STRM3(VIS, SCAL, 'GAINTIME'))
            CALL DATDELET (STRM3(VIS, SCAL, 'ORES'))
            CALL DATDELET (STRM3(VIS, SCAL, 'NRES'))
         ENDIF
      ELSE
         CALL MSGPUT ('Correcting phase only', 'I')
         CALL DATPUTR (VIS, 'TINT', TPHASE, 1)
         CALL VISSCAL (VIS, CLASS, MODEL, SCAL, MODE)
         CALL DATDELET (STRM3(VIS, SCAL, 'ANTGAIN'))
         CALL DATDELET (STRM3(VIS, SCAL, 'GAINTIME'))
         CALL DATDELET (STRM3(VIS, SCAL, 'ORES'))
         CALL DATDELET (STRM3(VIS, SCAL, 'NRES'))
      END IF
C
      END
