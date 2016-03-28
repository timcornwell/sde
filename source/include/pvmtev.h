
/*
 *         PVM version 3.3:  Parallel Virtual Machine System
 *               University of Tennessee, Knoxville TN.
 *           Oak Ridge National Laboratory, Oak Ridge TN.
 *                   Emory University, Atlanta GA.
 *      Authors:  A. L. Beguelin, J. J. Dongarra, G. A. Geist,
 *    W. C. Jiang, R. J. Manchek, B. K. Moore, and V. S. Sunderam
 *                   (C) 1992 All Rights Reserved
 *
 *                              NOTICE
 *
 * Permission to use, copy, modify, and distribute this software and
 * its documentation for any purpose and without fee is hereby granted
 * provided that the above copyright notice appear in all copies and
 * that both the copyright notice and this permission notice appear in
 * supporting documentation.
 *
 * Neither the Institutions (Emory University, Oak Ridge National
 * Laboratory, and University of Tennessee) nor the Authors make any
 * representations about the suitability of this software for any
 * purpose.  This software is provided ``as is'' without express or
 * implied warranty.
 *
 * PVM version 3 was funded in part by the U.S. Department of Energy,
 * the National Science Foundation and the State of Tennessee.
 */

/*
 *	pvmtev.h
 *
 *	Libpvm tracing includes.
 *
$Log$
 */

#ifndef	_PVMTEV_H_

#define	_PVMTEV_H_

/* Trace Event Constants */

#define	TEV_FIRST				0
#define	TEV_ADDHOSTS0			0
#define	TEV_ADDHOSTS1			1
#define	TEV_BARRIER0			2
#define	TEV_BARRIER1			3
#define	TEV_BCAST0				4
#define	TEV_BCAST1				5
#define	TEV_BUFINFO0			6
#define	TEV_BUFINFO1			7
#define	TEV_CONFIG0				8
#define	TEV_CONFIG1				9
#define	TEV_DELETE0				10
#define	TEV_DELETE1				11
#define	TEV_DELHOSTS0			12
#define	TEV_DELHOSTS1			13
#define	TEV_EXIT0				14
#define	TEV_EXIT1				15
#define	TEV_FREEBUF0			16
#define	TEV_FREEBUF1			17
#define	TEV_GETFDS0				18
#define	TEV_GETFDS1				19
#define	TEV_GETINST0			20
#define	TEV_GETINST1			21
#define	TEV_GETOPT0				22
#define	TEV_GETOPT1				23
#define	TEV_GETRBUF0			24
#define	TEV_GETRBUF1			25
#define	TEV_GETSBUF0			26
#define	TEV_GETSBUF1			27
#define	TEV_GETTID0				28
#define	TEV_GETTID1				29
#define	TEV_GSIZE0				30
#define	TEV_GSIZE1				31
#define	TEV_HALT0				32
#define	TEV_HALT1				33
#define	TEV_INITSEND0			34
#define	TEV_INITSEND1			35
#define	TEV_INSERT0				36
#define	TEV_INSERT1				37
#define	TEV_JOINGROUP0			38
#define	TEV_JOINGROUP1			39
#define	TEV_KILL0				40
#define	TEV_KILL1				41
#define	TEV_LOOKUP0				42
#define	TEV_LOOKUP1				43
#define	TEV_LVGROUP0			44
#define	TEV_LVGROUP1			45
#define	TEV_MCAST0				46
#define	TEV_MCAST1				47
#define	TEV_MKBUF0				48
#define	TEV_MKBUF1				49
#define	TEV_MSTAT0				50
#define	TEV_MSTAT1				51
#define	TEV_MYTID0				52
#define	TEV_MYTID1				53
#define	TEV_NOTIFY0				54
#define	TEV_NOTIFY1				55
#define	TEV_NRECV0				56
#define	TEV_NRECV1				57
#define	TEV_PARENT0				58
#define	TEV_PARENT1				59
#define	TEV_PERROR0				60
#define	TEV_PERROR1				61
#define	TEV_PKBYTE0				62
#define	TEV_PKBYTE1				63
#define	TEV_PKCPLX0				64
#define	TEV_PKCPLX1				65
#define	TEV_PKDCPLX0			66
#define	TEV_PKDCPLX1			67
#define	TEV_PKDOUBLE0			68
#define	TEV_PKDOUBLE1			69
#define	TEV_PKFLOAT0			70
#define	TEV_PKFLOAT1			71
#define	TEV_PKINT0				72
#define	TEV_PKINT1				73
#define	TEV_PKUINT0				74
#define	TEV_PKUINT1				75
#define	TEV_PKLONG0				76
#define	TEV_PKLONG1				77
#define	TEV_PKULONG0			78
#define	TEV_PKULONG1			79
#define	TEV_PKSHORT0			80
#define	TEV_PKSHORT1			81
#define	TEV_PKUSHORT0			82
#define	TEV_PKUSHORT1			83
#define	TEV_PKSTR0				84
#define	TEV_PKSTR1				85
#define	TEV_PROBE0				86
#define	TEV_PROBE1				87
#define	TEV_PSTAT0				88
#define	TEV_PSTAT1				89
#define	TEV_RECV0				90
#define	TEV_RECV1				91
#define	TEV_RECVF0				92
#define	TEV_RECVF1				93
#define	TEV_SEND0				94
#define	TEV_SEND1				95
#define	TEV_SENDSIG0			96
#define	TEV_SENDSIG1			97
#define	TEV_SETOPT0				98
#define	TEV_SETOPT1				99
#define	TEV_SETRBUF0			100
#define	TEV_SETRBUF1			101
#define	TEV_SETSBUF0			102
#define	TEV_SETSBUF1			103
#define	TEV_SPAWN0				104
#define	TEV_SPAWN1				105
#define	TEV_START_PVMD0			106
#define	TEV_START_PVMD1			107
#define	TEV_TASKS0				108
#define	TEV_TASKS1				109
#define	TEV_TICKLE0				110
#define	TEV_TICKLE1				111
#define	TEV_TIDTOHOST0			112
#define	TEV_TIDTOHOST1			113
#define	TEV_TRECV0				114
#define	TEV_TRECV1				115
#define	TEV_UPKBYTE0			116
#define	TEV_UPKBYTE1			117
#define	TEV_UPKCPLX0			118
#define	TEV_UPKCPLX1			119
#define	TEV_UPKDCPLX0			120
#define	TEV_UPKDCPLX1			121
#define	TEV_UPKDOUBLE0			122
#define	TEV_UPKDOUBLE1			123
#define	TEV_UPKFLOAT0			124
#define	TEV_UPKFLOAT1			125
#define	TEV_UPKINT0				126
#define	TEV_UPKINT1				127
#define	TEV_UPKUINT0			128
#define	TEV_UPKUINT1			129
#define	TEV_UPKLONG0			130
#define	TEV_UPKLONG1			131
#define	TEV_UPKULONG0			132
#define	TEV_UPKULONG1			133
#define	TEV_UPKSHORT0			134
#define	TEV_UPKSHORT1			135
#define	TEV_UPKUSHORT0			136
#define	TEV_UPKUSHORT1			137
#define	TEV_UPKSTR0				138
#define	TEV_UPKSTR1				139
#define	TEV_VERSION0			140
#define	TEV_VERSION1			141
#define	TEV_REG_HOSTER0			142
#define	TEV_REG_HOSTER1			143
#define	TEV_REG_RM0				144
#define	TEV_REG_RM1				145
#define	TEV_REG_TASKER0			146
#define	TEV_REG_TASKER1			147
#define	TEV_NEWTASK				148
#define	TEV_ENDTASK				150
#define	TEV_SPNTASK				152
#define	TEV_ARCHCODE0			154
#define	TEV_ARCHCODE1			155
#define	TEV_CATCHOUT0			156
#define	TEV_CATCHOUT1			157
#define	TEV_GETMWID0			158
#define	TEV_GETMWID1			159
#define	TEV_GETTMASK0			160
#define	TEV_GETTMASK1			161
#define	TEV_HOSTSYNC0			162
#define	TEV_HOSTSYNC1			163
#define	TEV_PACKF0				164
#define	TEV_PACKF1				165
#define	TEV_PRECV0				166
#define	TEV_PRECV1				167
#define	TEV_PSEND0				168
#define	TEV_PSEND1				169
#define	TEV_REDUCE0				170
#define	TEV_REDUCE1				171
#define	TEV_SETMWID0			172
#define	TEV_SETMWID1			173
#define	TEV_SETTMASK0			174
#define	TEV_SETTMASK1			175
#define	TEV_UNPACKF0			176
#define	TEV_UNPACKF1			177
#define	TEV_GATHER0				178
#define	TEV_GATHER1				179
#define	TEV_SCATTER0			180
#define	TEV_SCATTER1			181
#define	TEV_MAX					181

/* event mask kept as ascii string */

#define	TEV_MASK_LENGTH	(TEV_MAX / 8 + 2)

typedef char Pvmtmask[TEV_MASK_LENGTH];

#define	TEV_SET_MASK(m, k) (m[(k) / 8] |= (1 << (((k) / 2) & 3)))

#define	TEV_UNSET_MASK(m, k) (m[(k) / 8] &= ~(1 << (((k) / 2) & 3)))

#define	TEV_CHECK_MASK(m, k) (m[(k) / 8] & (1 << (((k) / 2) & 3)))

#define	TEV_INIT_MASK(m) { \
	int i = TEV_MASK_LENGTH-1; \
	m[i] = 0; \
	while (i-- > 0) \
		m[i] = '@'; \
	}

#ifdef __ProtoGlarp__
#undef __ProtoGlarp__
#endif
#if defined(__STDC__) || defined(__cplusplus)
#define __ProtoGlarp__(x) x
#else
#define __ProtoGlarp__(x) ()
#endif

#ifdef __cplusplus
extern "C" {
#endif

int pvm_gettmask	__ProtoGlarp__(( int, Pvmtmask ));
int pvm_settmask	__ProtoGlarp__(( int, Pvmtmask ));

#ifdef __cplusplus
}
#endif

#endif	/*_PVMTEV_H_*/

