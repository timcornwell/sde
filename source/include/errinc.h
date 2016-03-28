C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C	(C)opyright 1989 by Associated Universities, Inc.; all rights reserved
C
C Error system internal include file
C
C Message buffers and error status
C
      CHARACTER ERIIN*256, ERIOUT*256, ERISTAT*80
      COMMON /ERIC/ ERIIN, ERIOUT, ERISTAT
C
C Error system context, message count, unreported
C error status, and internal error status
C
      INTEGER ERICNTXT, ERICOUNT, ERIUNRPT,
     +        ERIINTRN
      COMMON /ERII/ ERICNTXT, ERICOUNT, ERIUNRPT,
     +        ERIINTRN
C
C Error system internal functions
C
      CHARACTER*20 ERINODE
