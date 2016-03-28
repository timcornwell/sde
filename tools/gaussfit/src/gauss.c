/*	GaussFit - A System for Least Squares and Robust Estimation

	Source Code Copyright (C) 1987 by William H. Jefferys,
	Michael J. Fitzpatrick and Barbara E. McArthur
	All Rights Reserved.
*/

/* main loop */

#include <math.h>
#include <stdio.h>
#include "array.h"
#include "datum.h"
#include "simpledefs.h"
#include "symboltable.h"
#include "defines.h"
#include "house.h"
#include "files.h"
#include "alloc.h"
#ifdef THINK_C
#include <Memory.h>
#endif
#include "protoplasm.h"


extern FILE *fp,*fpc,*fpcv;
/*int totsize, totfree, totrealloc;*/
int running = 0;

double tolerance;  /* tolerance */
double fit=999.0;
double envhuber, envtukey, envfair, envtrim;
int envminsum, envirls, envorm, envprec,envdbg;
double lambda, factor;
int MARQ = 0;
int maxiters;
int space;
int limit;
int iterno;
int tracedisplay = 0;
int showresults = 1;
double *OldPrm = NULL;
double gOldSigma = -1.;
void	recopyfiles(char *env);
extern int recopyfilecmd;
extern MIDAS *datatable;

inititers() {
	gOldSigma = -1.;
	space = 0;
}

checkdone() {
	if(tolerance < fit && iterno>= maxiters) {  /* if reached iters without convergence */
		itlimit(stdout);
		itlimit(fp);
	}
	if (getenvval("prvar") && (fit < tolerance))
		CovarianceMtx();
	if (iterno>= maxiters || fit<tolerance) {
		ChiSquare();
	}
}

#ifndef OBJECTS

gaussmain(t,s)  /* main loop routine */
char *t,*s;
{
	int i;

	printcopyright(stdout);
	if (recopyfilecmd) {
		system("cp ./untouched/* .");
	}
	getenvvars(s);
	xparfileopen1();
	xdatafileopen();

	mycompile(t); /* compile file "t" */
			
	inititers();
#ifdef THINK_C
	showresults = 0;
#endif


	for(i=0;i<maxiters;i++) { /* iterate at most maxite times */
		/* inform user of progress */
		if (oneiter(&gOldSigma)) break;
	}
	if(tolerance < fit)   /* if reached iters without convergence */
	{
		itlimit(stdout);
		itlimit(fp);
	}
	if (getenvval("prvar") && (fit < tolerance))
		CovarianceMtx();

	ChiSquare();
	epilog();  /* clean up */
}

#endif

oneiter(oldSigma)
	double *oldSigma;
{
	double Curr_Sigma;

	/* initialize Householder routines */
	xprintf(stdout, "InitHouse\n");
	InitHouse("results", getenvint("prmat"), 10.0);
	xprintf(stdout,"\nStart of Iteration %d...\n\n",iterno+1);
	xprintf(fp,"\nStart of Iteration %d...\n\n",iterno+1);


	Forming_Eqns(); /* form the equations of condition */

	/*if double  iteration method  or Marq thnn proceed with second */
	if ((getenvval("double")) || (MARQ)) {
		datatable = NULL;
		xprintf(stdout, "InitHouse\n");
		InitHouse("results", getenvint("prmat"), 10.0);
		Forming_Eqns(); /* form the equations of condition */
	}

	if (MARQ) {
		if (!space)
			Allocate_Param_Space();
		
		Curr_Sigma=Get_Sigma();


		if (*oldSigma < 0.00 || Curr_Sigma < *oldSigma) {
			*oldSigma = Curr_Sigma;
			Current_into_Old_Params();
			SolveMatrix(); /* solve the estimation problem */
			ShowResults(iterno+1);/* show results and update parameters */
			lambda = lambda * factor;
		} else {
			Old_into_Current_Params();
			lambda = lambda/(sqr(factor));
			*oldSigma = -1.0;
		}
		xprintf(fp,"lambda = %28.17lf\n",lambda);
		printold();
	} else {
		SolveMatrix(); /* solve the estimation problem */
		ShowResults(iterno+1);/* show results and update parameters */
	}
	
	iterno++;
	/*freecolspace();*/
	/*freerowspace();*/
	
	datatable = NULL;
	if (fit<tolerance) return 1;
	
	return 0;
}

Forming_Eqns()
{
		xprintf(stdout,"\nForming Eqs...\n\n");
		/* define global variables */
		setpc2label("_Globals");
		running = 1;
		interpret();
		running = 0;
		
		xprintf(stdout,"xparfile open...\n");
		xparfileopen2();	/* fill in fields in declared params */
		
		/* execute user's model starting at main */
		setpc2label("main");
		running = 1;
		xprintf(stdout,"interpret...\n");
		interpret();
		running = 0;
		ResetDeclareStack(); /* discard all defined variables */
		SumIt(); /* sum sigmas */
}

double UpdateValues()  /* update all parameters */
{
	int i;
	int type;
	double dahat;
	char *name;
	short index[5];
	short symInx, fvInx;
	int indsz,prmsz;
	double error;
	extern double DeltaV;
	double ahat;

    /* cycle through all paramters */
	error = DeltaV; /* error starts with value returned by phi calculation */
	i = 0;

	indsz = getindexsz();
	prmsz = getmaxprmname();
	/* get data on next parameter */
	while(GetDeltaValue(i,&name,index,&type,&dahat))
	{                    
#ifdef OBJECTS
	TaskYield();
#endif
		i++;
		putdelta(name,dahat);
		symInx = findsymbol(name, 0);
		fvInx = FileVarOf(symInx);
		if (filevarptr[fvInx].xvars[4]>0) {
			ahat = getxparval(symInx,index);/* get current value of paramter */
			/* increment value and update paramter file */
			putxparval(symInx,index,ahat + dahat);
		} else {
			ahat = getparamval(symInx); /*get current value of parameter */
			putparamval(symInx,ahat + dahat); /* increment value and update parameter file */
		}
		/* print results in results file and to screen */
		if ((index[4] == 0) && (indsz == 0)) {
			if (showresults)
				xprintf(stdout,"%*s = ",prmsz,name);
			xprintf(fp,"%*s = ",prmsz,name);

		} else if ((index[4] == 0) && (indsz != 0)) {
			if (showresults)
				xprintf(stdout,"%*s%*s = ",prmsz,name,indsz," ");
			xprintf(fp,"%*s = ",prmsz+indsz,name);
		} else {
			if (showresults) {
				xprintf(stdout,"%*s",prmsz, name);
				prIndex(stdout,index,-indsz);
				xprintf(stdout," = ");
			}
			xprintf(fp,"%*s", prmsz,name);
			prIndex(fp,index,-indsz);
			xprintf(fp, " = ");
		}
		if (showresults) {
			printdouble(stdout,ahat+dahat,0);
			xprintf(stdout," delta = ");
			printdouble(stdout,dahat,0);
			xprintf(stdout,"\n");
		}

		printdouble(fp,ahat+dahat,0);
		xprintf(fp," delta = ");
		printdouble(fp,dahat,0);
		xprintf(fp,"\n");

		/* update convergence error */
		error = maxval(fabs(dahat)/(tolerance +fabs(ahat)),error);
	}
	i++;  /* ????? */
	xprintf(stdout,"\n");
	xprintf(fp,"\n");
	return error;
}

ShowResults(count) 
int count;
{

	PrintResults(); /* print results */
	fit = UpdateValues(); /* update the paramters */
	/* print information for user */
	xprintf(stdout,"fit =\t");
	xprintf(fp,"fit =\t");
	printdouble(fp,fit,0);
	printdouble(stdout,fit,0);
	xprintf(stdout,", tol =\t");
	xprintf(fp,", tol =\t");
	printdouble(stdout,tolerance,0);
	printdouble(fp,tolerance,0);
	xprintf(stdout,"\n");
	xprintf(fp,"\n");
	xprintf(stdout,"\nEnd of Iteration %d...\n\n",count);
	xprintf(fp,"\nEnd of Iteration %d...\n\n",count);
	if (count == 1)
		checkvar();
	if (getenvint("prmwrt") != -1)
		writexpar();
}

void
checkvar()  /* check to make sure variances are not a mix of 0 and non-0 */
{
	int z, nz;

	z = getvar_zero();
	nz = getvar_nonzero();
	if ((z == 1) && (nz == 1))
 		fatalerror("FATALERROR! Variances may not be a mix of non-zeros and zeros!\n",
			" ");

}

itlimit(fpp)
FILE *fpp;
{
	xprintf(fpp,"\n***********************************");
	xprintf(fpp,"***********************************\n");
	xprintf(fpp,"\nIteration limit reached.  Convergence has not occurred.\n");
	xprintf(fpp,"\n***********************************");
	xprintf(fpp,"***********************************\n\n");
}

prolog()
{
#ifdef THINK_C
	SetApplLimit((char*)CurStackBase - 48000L);
	MaxApplZone(); /* MAC requires large heap for space allocation */
#endif
}

getenvvars(s) /* get values from environment file and test them */
char *s;
{
	openenvtable(s); /* open environment table "s" */
	tolerance = getenvval("tol"); /* get tolerance from environment */
	if ((tolerance < 1.0e-13)   && (tolerance > -1.0e-13))   
		tolerance = 0.000001;
	envprec = getenvint("prec");
	envfair = getenvval("fair");
	envirls = getenvint("irls");
	envorm  = getenvint("orm");
	envtrim = getenvval("trim");
	envdbg  = getenvint("dbg");
	envhuber  = getenvval("huber");
	envtukey  = getenvval("tukey");
	envminsum = getenvint("minsum");
	if (envminsum != 0 && (notzero(envfair)))
		fatalerror("Both minsum and fair have been set to non-zero in the environment file.\n"," ");
	lambda = getenvval("lambda");
	factor = getenvval("factor");
	tracedisplay = getenvint("trace");
	if (notzero(lambda))
		MARQ = 1.0;
	if ((envminsum != 0) && (MARQ))
		fatalerror("Both minsum and lambda have been set to non-zero in the environment file.\nLevenburg-Marquardt cannot be used with minsum.\n"," ");
	if (((lambda < .0000000000001)||(lambda > 100.0)) && (MARQ)) 
	{
		lambda = .0001;
		warningerror("Keyword lambda missing or less than or equal to zero in the environment file.\n"," ",0,0);
	}
	if (((factor < .0000000000001)||(factor >100.0)) && (MARQ)) 
	{
		factor = 0.1;
		warningerror("Keyword factor missing or less than or equal to zero in the environment file.\n"," ",0,0);
	}
	maxiters = getenvint("iters"); /* get max iterations from environment */
	if ((maxiters < 1) || (maxiters >1000))
	{
		maxiters = 10;
		warningerror("Keyword iters missing or less than zero in environment file.\n"," ",0,0);
	}
#ifdef NOTYET
	if (getenvint("derivs") > 1) {
		do2ndDerivs = 1;
		doObservations = 1;
	}
#endif
}


Allocate_Param_Space()
{
	int i;

	limit = Get_LastCol();
	space++;
	OldPrm = (double*)MemAlloc("OldPrm",(long)(limit+1)*sizeof(double));
	for (i=0;i<=limit;i++)
	{
		*(OldPrm+i) = 0.0;
	}
}

Free_Param_Space() {
	freemem("OldPrm",(char*)OldPrm);
}

Old_into_Current_Params()
{
	int i, type;
	double dahat,ahat;
	char *name;
	short index[5];
	short symInx, fvInx;


 	i = 0;   /* cycle through all paramters */
	xprintf(fp,"\n");
 	while(GetDeltaParams(i,&name,index,&type,&dahat)) /* get data on next parame ter */
 	{
		 i++;
		symInx = findsymbol(name, 0);
		fvInx = FileVarOf(symInx);
		if (type != RightHandType) {
			if (filevarptr[fvInx].xvars[4]>0) {
 				ahat = getxparval(symInx,index);/* get current value of paramter */
 				putxparval(symInx,index,OldPrm[i-1]);/* increment value and update paramter file */
			} else {
 				ahat = getparamval(symInx); /*get current value of parameter */
 				putparamval(symInx,OldPrm[i-1]);
 			}
 		}
	}
	xprintf(fp,"\n");
}


Current_into_Old_Params()
{
	int i, type;
	double dahat;
	char *name;
	short index[5];
	short symInx, fvInx;

	i = 0;   /* cycle through all paramters */
	xprintf(fp,"\n");
	while(GetDeltaParams(i,&name,index,&type,&dahat)) /* get data on next parameter */
	{                    
#ifdef OBJECTS
	TaskYield();
#endif
		i++;
		symInx = findsymbol(name, 0);
		fvInx = FileVarOf(symInx);
		if (type != RightHandType) {
			if (filevarptr[fvInx].xvars[4]>0) {
				OldPrm[i-1] = getxparval(symInx,index);/* get current value of paramter */
			} else {
				OldPrm[i-1] = getparamval(symInx); /*get current value of parameter */
			}
		}
	}

	xprintf(fp,"\n");
}

epilog()
{
	if (fp != NULL) { fclose(fp); fp = NULL; }
	if (fpc != NULL) { fclose(fpc); fpc = NULL; }
	if (fpcv != NULL) { fclose(fpcv); fpcv = NULL; }
	freeworkspace();
	freerowspace();
	freecolspace();
	freematspace();
	freemeandu();
	freestacks();
	if (MARQ) Free_Param_Space();
	savexpar(0); /* save parameter file */
	saveenv(0);  /* save environment file */
}

printold()
{
	int i;
	xprintf(fp,"\n");
	for (i=0;i<=limit;i++)
	{
		xprintf(fp,"OldPrm[%d] = %28.17lf\n",i,OldPrm[i]);
	}
	xprintf(fp,"\n");
}

#ifdef FORGET_YOU_EVER_WROTE_THIS
extern int pfn;
extern int dfn;
extern MIDAS *xpartable[MAXPARAMFILES];
extern MIDAS *xdattable[MAXDATASETS];

void	recopyfiles(char *env) {
	char name[256],bakname[256];
	char *mptr;
	short i, c;
	FILE *fin, *fout;
	
	if (!(mptr = getenvstr("bak"))) {
		strcpy((char*)bakname,"untouched");
	} else {
		strcpy((char*)bakname,mptr);
	}
	saveenv(1);
	xprintf(stdout, "Recopying files from folder '%s'.\n",bakname);

	for (i = 0; i < pfn; i++) {
		sprintf(name, "%s/%s",bakname,xpartable[i]->fname);
		fin = fopen(name,"r");
		if (!fin) {
			xprintf(stdout, "Can't open backup of '%s'\n", xpartable[i]->fname);
			continue;
		}
		fout = fopen(name,"w");
		if (!fout) {
			xprintf(stdout, "Can't open new file for '%s'\n", xpartable[i]->fname);
			fclose(fin);
			continue;
		}
		while ((c = getc(fin)) != EOF) putc(c, fout);
		fclose(fin);
		fclose(fout);
		midasfree(xpartable[i]);
		xpartable[i] = NULL;
	}
	for (i = 0; i < dfn; i++) {
		sprintf(name, "%s/%s",bakname,xdattable[i]->fname);
		fin = fopen(name,"r");
		if (!fin) {
			xprintf(stdout, "Can't open backup of '%s'\n", xdattable[i]->fname);
			continue;
		}
		fout = fopen(name,"w");
		if (!fout) {
			xprintf(stdout, "Can't open new file for '%s'\n", xdattable[i]->fname);
			fclose(fin);
			continue;
		}
		while ((c = getc(fin)) != EOF) putc(c, fout);
		fclose(fin);
		fclose(fout);
		midasfree(xdattable[i]);
		xdattable[i] = NULL;
	}
	strcpy(name, (char*)env);
	sprintf(name, "%s/%s",bakname,env);
	if (!fin) {
		xprintf(stdout, "Can't open backup of '%s'\n", env);
		goto skip;
	}
	fout = fopen(name,"w");
	if (!fout) {
		xprintf(stdout, "Can't open new file for '%s'\n", env);
		fclose(fin);
		goto skip;
	}
	while ((c = getc(fin)) != EOF) putc(c, fout);
	fclose(fin);
	fclose(fout);
	skip:
	openenvtable((char*)env);
	xparfileopen1();
	xdatafileopen();
}
#endif
