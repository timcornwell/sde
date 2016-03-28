
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
 *	pvmsdpro.h
 *
 *	Catchall protocol between pvmd, task, resource manager, hoster, tasker.
 *
$Log$
 *
 */

#ifndef	_PVMSDPRO_H_

#define	_PVMSDPRO_H_

/* protocol version */

#ifndef	SDPROTOCOL
#define	SDPROTOCOL	4302
#endif

/*
*	t - task
*	d - pvmd
*	R - resource manager
*	H - hoster
*	T - tasker
*/

#define	SM_FIRST		0x80040001		/* first SM_ message */
#define	SM_SPAWN		(SM_FIRST+0)	/* t<>R like TM_SPAWN */
#define	SM_EXEC			(SM_FIRST+1)	/* R->d like DM_EXEC */
#define	SM_EXECACK		(SM_FIRST+2)	/* d->R like DM_EXECACK */
#define	SM_TASK			(SM_FIRST+3)	/* t<>R like TM_TASK */
#define	SM_CONFIG		(SM_FIRST+4)	/* t<>R like TM_CONFIG */
#define	SM_ADDHOST		(SM_FIRST+5)	/* t<>R like TM_ADDHOST */
#define	SM_DELHOST		(SM_FIRST+6)	/* t<>R like TM_DELHOST */
#define	SM_ADD			(SM_FIRST+7)	/* R->d like DM_ADD */
#define	SM_ADDACK		(SM_FIRST+8)	/* d->R like DM_ADDACK */
#define	SM_NOTIFY		(SM_FIRST+9)	/* t->R like TM_NOTIFY */
#define	SM_TASKX		(SM_FIRST+10)	/* d->R notify of task exit */
#define	SM_HOSTX		(SM_FIRST+11)	/* d->R notify sched of host delete */
#define	SM_HANDOFF		(SM_FIRST+12)	/* R->d pvmd to new sched */
#define	SM_SCHED		(SM_FIRST+13)	/* t<>R like TM_SCHED */
/* XXX rest probably shouldn't be here but I'm tired of new entry points */
#define	SM_STHOST		(SM_FIRST+14)	/* d->H start slave pvmds */
#define	SM_STHOSTACK	(SM_FIRST+15)	/* H->d like DM_STARTACK */
#define	SM_STTASK		(SM_FIRST+16)	/* d->T start task */
#define	SM_LAST			(SM_FIRST+16)	/* last SM_ message */

#endif	/*_PVMSDPRO_H_*/

