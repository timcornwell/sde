/*
	GaussFit - A System for Least Squares and Robust Estimation

	Source Code Copyright (C) 1987 by William H. Jefferys,
	Michael J. Fitzpatrick and Barbara E. McArthur
	All Rights Reserved.
*/

/* Print error messages */

#include "protoplasm.h"
#include "defines.h"
#include "compile.h"
#include "machine.h"
#include "symboltable.h"


extern FILE *fp;
extern int running;
extern int inanalyzepool;
short fatality = 0;


#ifdef OBJECTS
#include <CApplication.h>
#include <Exceptions.h>
#include <setjmp.h>
extern jmp_buf bigbooboo;
extern short gInGaussFit;
extern CApplication	*gApplication;
#endif

error(s)  /* print an error message */
char *s;
{
	xprintf(stderr,"%s\n",s);
	if (fp != NULL) {
		xprintf(fp,"%s\n",s);
	}
}

fatalerror(s,t) /* print a fatal error message */
char *s;  /* a format string */
char *t;  /* any string */
{
	if (running) runtimeerror(s,t);
	if (fp != NULL) {
		xprintf(fp,s,t);
		xfflush(fp);
	}
	xprintf(stderr,s,t);
	if (fp != NULL) {
		xprintf(fp,s,t);
	}
#ifndef OBJECTS
	exit(0);
#else
#ifdef DEBUGZ
	Debugger();
#endif
	fatality = 1;
	if (gInGaussFit) {
		longjmp(bigbooboo,1);
	} else {
		if (gApplication->GetPhase() != appInitializing) Failure(kSilentErr,0L);
	}
#endif
}

fatalerror2(s,t,w) /* print a fatal error message */
char *s;  /* a format string */
int t;  /* any integer */
char *w;  /* any string */
{
	xprintf(stderr,s,t,w);
	if (fp != NULL) {
		xprintf(fp,s,t,w);
	}
#ifndef OBJECTS
	exit(0);
#else
#ifdef DEBUGZ
	Debugger();
#endif
	fatality = 1;
	if (gInGaussFit) {
		longjmp(bigbooboo,1);
	} else {
		if (gApplication->GetPhase() != appInitializing) Failure(kSilentErr,0L);
	}
#endif
}

fatalerror2c(s,t,w) /* print a fatal error message */
char *s;  /* a format string */
char *t;  /* any string */
char *w;  /* any string */
{
	xprintf(stderr, s,t,w);
	if (fp != NULL) {
		xprintf(fp,s,t,w);
	}
#ifndef OBJECTS
	exit(0);
#else
#ifdef DEBUGZ
	Debugger();
#endif
	fatality = 1;
	if (gInGaussFit) {
		longjmp(bigbooboo,1);
	} else {
		if (gApplication->GetPhase() != appInitializing) Failure(kSilentErr,0L);
	}
#endif
}

warningerror(s,t,d1,d2) /* print a warning error message */
char *s;  /* a format string */
char *t;  /* any string */
int d1,d2;
{
	xprintf(stderr, s,t,d1,d2);
	if (fp != NULL) {
		xprintf(fp,s,t,d1,d2);
	}
}

extern struct ifile fl_stack[32];

extern int tracelevel;

extern Instruction *core;		/* instruction memory */
extern long pc;				/* program counter */

runtimeerror(str1, str2)
	char *str1, *str2;
{
	short linenum, funcnum, filenum;
	
	xprintf(stderr, str1, str2);
	xprintf(fp, str1, str2);
	linenum = core[pc].linenum;
	funcnum = core[pc].funcnum;
	filenum = core[pc].filenum;
	xprintf(fp, 
		"Model Execution Error on Line %d of File %s in Function %s\n", 
		linenum, fl_stack[filenum].fname, symbolptr[funcnum].name);
	xprintf(stderr,
 		"Model Execution Error on Line %d of File %s in Function %s\n", 
		linenum, fl_stack[filenum].fname, symbolptr[funcnum].name);

	tracelevel = 4;
	traceinstruction(stderr);
	traceinstruction(fp);
	if (!inanalyzepool) AnalyzeDatumPool(1);
#define DIE
#ifdef DIE
#ifndef OBJECTS
	exit(0);
#else
#ifdef DEBUGZ
	Debugger();
#endif
	fatality = 1;
	if (gInGaussFit) {
		longjmp(bigbooboo,1);
	} else {
		if (gApplication->GetPhase() != appInitializing) Failure(kSilentErr,0L);
	}
#endif
#else 
	Debugger();
#endif
}
	
