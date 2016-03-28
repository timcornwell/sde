/*

	GaussFit - A System for Least Squares and Robust Estimation
	Source Code Copyright (C) 1987 by William H. Jefferys,
	Michael J. Fitzpatrick and Barbara E. McArthur
	All Rights Reserved.

*/

/*  Interface between interpreter and table I/O routines */

#include "array.h"
#include "datum.h"
#include "simpledefs.h"
#include "files.h"
#include "symboltable.h"
#include "defines.h"
#include <stdio.h>
#include "alloc.h"
#include "protoplasm.h"
#include "strings.h"



extern FILE *fp;

MIDAS *datatable = NULL; /* pointer to data table (none initially)*/
MIDAS *xpartable[MAXPARAMFILES];	/* pointer to parameter table */
MIDAS *xdattable[MAXDATASETS];	/* pointer to parameter table */
FITS *envtable = NULL;   /* pointer to environment file*/
int nfindices = 0;		/* number of fast index tables */
FINDEX fsdx[MAXFINDEX];	/* fastindices to parameters(from all p files */

       int pfn = 0;   		    /* number of parameter files*/
       int dfn = 0;   		    /* number of data files*/
static int datarow = -1;        /* pointer to data row (none initially)*/
static int datatableid = -1;    /* pointer to data table I.D. (none initially)*/
static int xparrow = -1;        /* pointer to data row (none initially)*/
static int (*Qcompare)();		/* compare routine for quicksort */
static int (*Qexchange)();		/* exchange routine for quicksort */
static int *Frow;		/* globals passed to qwiksort */
static int Ffile;		/* globals passed to qwiksort */
static short *Fxvars;
int FCompare(), FExchange();

saveenv(dispose)     /* close any open environment table */
int dispose;
{
	if(envtable)
		envtable = fitsclose(envtable,dispose);
}



openenvtable(s)   /* open environment table named s */
	char *s;
{    
	if(!envtable)     /* only open if no table open */
	{
		/*xprintf(stdout,"Opening Environment File %s For Input\n",s);*/
		envtable = fitsopen(s);
	}
}

double
getprowcolval(int file, int row, int col) {
	char str[256];
	if (file < 0 || file > pfn) {
		sprintf(str, "getprowcolval: file number %d out of range [%d,%d]\n",file,0,pfn);
		fatalerror(str,"");
	} else if (row < 0 || row > xpartable[file]->numrows) {
		sprintf(str, "getprowcolval: row number %d out of range [%d,%d]\n",row,0,
			xpartable[file]->numrows);
		fatalerror(str,"");
	} else if (col < 0 || col > xpartable[file]->numcols) {
		sprintf(str, "getprowcolval: col number %d out of range [%d,%d]\n",col,0,
			xpartable[file]->numcols);
		fatalerror(str,"");
	}
	return xpartable[file]->rows[row][col].d;
}

double
getdrowcolval(int file, int row, int col) {
	char str[256];
	if (file < 0 || file > pfn) {
		sprintf(str, "getdrowcolval: file number %d out of range [%d,%d]\n",file,0,pfn);
		fatalerror(str,"");
	} else if (row < 0 || row > xdattable[file]->numrows) {
		sprintf(str, "getdrowcolval: row number %d out of range [%d,%d]\n",row,0,
			xdattable[file]->numrows);
		fatalerror(str,"");
	} else if (col < 0 || col > xdattable[file]->numcols) {
		sprintf(str, "getdrowcolval: col number %d out of range [%d,%d]\n",col,0,
			xdattable[file]->numcols);
		fatalerror(str,"");
	}
	return xdattable[file]->rows[row][col].d;
}


char *
getenvstr(s) /* get string corresponding to keyword s from environment*/
	char *s;
{
	if(!envtable)/* if no table open, error */
	{
		fatalerror("No environment file has been opened. Cannot get string.\n","");
	}
	return getfitsstr(envtable,s);  /* find string in FITS table */
}



double 
getenvval(s) /* get float(double) for keyword s from environment*/
	char *s;
{
	if(!envtable)         /* error if environment not open */
	{
		fatalerror("No environment file has been opened. Cannot get double value.\n","");
	}
	return getfitsval(envtable,s); /* find value in environment */
}



putenvval(name,value) /* put value into keyword name in environment */
	char *name;
	double value;
{
	if(!envtable)    /* check that environment is open */
		fatalerror("No enviroment file has been opened.  Cannot put double value.\n","");
	putfitsval(envtable,name,value);  /* put value into table */
}



getenvint(s)  /* get an integer value from environemnt table */
	char *s;
{
	double ds;
	double fabs();

	ds = getenvval(s); /* integers are stored as doubles internally */
	if (fabs(ds) < 1.0e9)
		return (int)ds; /* integers are stored as doubles internally */
	else
		fatalerror("The absolute value of the integer %s is greater than 10E9.\n",s); 
}



datafileopen(s)  /* open a data file prefixed by s */
	char *s;
{
	char *theName;
	char str[10];
	short i;
	
	if(datatable != NULL) /* see if data table is open */
	{
		datatable = midaswrite(datatable,datatable->fname,0); /* close it */
		if(getenvint("triang")) /* if triangularization flag is set */
			triangularize1(); /* triangularize the matrix */
	}
	datarow = -1; /* no row available as file is closed */

	while(++datatableid < MAXDATASETS) /* get next data table id */
	{
		sprintf(str,"%s%d",s,datatableid); /* concatenate "s" with "id" */
		if(theName = getenvstr(str)) /* is such a keyword in the env? */
		{
			for (i=0; i<dfn; ++i) {
				if (strcmp(theName, xdattable[i]->fname)==0) break;
			}
			if (i==dfn) fatalerror("Data file %s not found.\n",theName);
			datatable = xdattable[i]; 
			xprintf(stdout,"%s\n",theName);
			return 1; /*success */
		}
	}
	datatableid = -1; /* done, reset id to -1 */
	return 0; /* failure, no more data to read */
}



xdatafileopen()  /* open all data files  */
{
	char *theName;
	char str[10];
	int i,j,k;

	k = 0;
	dfn = 0;
	/* open all data files */
	while(++k < MAXDATASETS) { /* get next data table id */
		sprintf(str,"%s%d","data\0",k); /* concat "data" + "id" */
		xdatopen(str);
	}
	if (dfn != 0)
		return 1; /* success */
	else
		fatalerror("No Data Files Specified.\nUse keyword  data# * to specify file name in the environment file\n","");
}

xparfileopen1()  /* open all  parameter files  */
{
	char *theName;
	char str[10];
	int i,j,k;
	int symInx, fvInx;

	k = 0;
	pfn = 0;
	initfindex();  /* initialize fast index col names */
	xparopen("params");
	/* open all parameter files */
	for(i=0; i < MAXPARAMFILES; ++i) { /* get next parameter table id */
		sprintf(str,"%s%d","param\0",i); /* concat "param" + "id" */
		xparopen(str);
	}
	if (pfn != 0)
		return 1; /* success */
	else
		fatalerror("No Parameter Files Specified.\nUse keyword * params * or param# * to specify file name in the environment file\n","");
}

xparfileopen2()  /* open all  parameter files  */
{
	char *theName;
	char str[10];
	int i,j,k;
	int symInx, fvInx;

	for (i = 0; i < pfn; i++) {
		for (j=0;j < getnumcols(xpartable[i]); j++) {
			symInx = findsymbol(getcolname(j,xpartable[i]), 0);
			/* there can be parameters that are not used!!!! */
			if (symInx != -1) {
				fvInx = FileVarOf(symInx);
				if (fvInx != -1) {
					FileNumOf(fvInx) = i;
					ColNumOf(fvInx) = j;
				}
			}
		}
	}
	if (pfn != 0)
		return 1; /* success */
	else
		fatalerror("No Parameter Files Specified.\nUse keyword * params * or param# * to specify file name in the environment file\n","");
}


xparopen(pfname)
	char *pfname;
{
	char *theName;
	if(theName = getenvstr(pfname)) { /* is such a keyword in the env? */
		xpartable[pfn] = midasopen(theName); /* if so open the corresponding table file */
		pfn++;
		if (pfn >=MAXPARAMFILES)
			fatalerror("Too Many Parameter Files Specified.\nThere is a limit of 10.\n","");
		xprintf(stdout,"Parameter file read:  %s\n\n",theName);
	}
}

xdatopen(dfname)
	char *dfname;
{
	char *theName;
	if(theName = getenvstr(dfname)) { /* is such a keyword in the env? */
		xdattable[dfn] = midasopen(theName); /* if so open the corresponding table file */
		dfn++;
		if (dfn >=MAXDATASETS)
			fatalerror("Too Many Data Files Specified.\nThere is a limit of 100.\n","");
	}
}


getitemnumber() /* get next row of the table */
{       
	/* succeeds if rows remain, else fails */
	return ++datarow < datatable->numrows; 
}


extern int exportptr;

import()    /* import data */
{
	doexport();  /* export pending equations of condition */
	if(!datatable) { /* make sure data file is open */
		if(!datafileopen("data")) {
			fatalerror("No data file name specifications found in the environment file (e.g. no data0, data1, data2, datan)\n","");
			return 0;		/* Couldn't open a datafile at all. */
		}
	}
	while(!getitemnumber()) { /* done with current data file? */
		if(!datafileopen("data")) { /* try opening the next possitble data file */
			PushFalse();	/* Last Datafile */
			/* if no success, result of report is false */
			return 0;
		}
	}
	PushTrue();  /* success, result is true */
	return 1;
}

double 
getdataval(s)   /* get (double) data for keyword "s" from current row of datable */
	char *s;
{
	double dnum;
	if(datatable)  /* check that table is open */
	{
		/* get from column "s", row datarow, from table datable */
		dnum =  getmidasval(datatable,s,datarow);
		if (dnum == UND) {
			if (strpos(s,'_') == -1)
				fatalerror2c("Datavalue %s does not exist in datafile %s.\n",
					s,datatable->fname);
			else return 0.0;
		}
		return dnum;
	}
	fatalerror("No MIDAS table is open to get value:%s \n",s);
}

strpos(s, c)
	register char *s, c;
{	
	char *s0 = s;

	while ((*s != c) && *s) s++;
	return ( *s == c ? s-s0 : -1 );
}

putresidual(name,val) /* put a residual into data table */
	char *name;
	double val;
{
	char resname[64];
	strcpy(resname,"_"); /* concatenate "_" before name of variable */
	strcat(resname,name);
	putdataval(resname,-1.0*val);  /* insert val into table */
}

double 
getresidual(name)  /* get residual from data table */
	char *name;
{

	char resname[64];
	strcpy(resname,"_");/* concatenate "_" before name of variable */

	strcat(resname,name);

	return (-1.0*getdataval(resname)); /* get residual */

}

double 
gettheobs(corename) /* get current updated observation */
	char *corename;
{
	/* value is sum of observation +  residual */
	return getdataval(corename) + getresidual(corename);
}

getdataint(s) /* get integer from table */
	char *s;
{
	double ds;
	double fabs();

	ds = getdataval(s); /* integers are stored as doubles internally */
	if (fabs(ds) < 1.0e9)
		return (int)ds; /* integers are stored as doubles internally */
	else
		fatalerror("The absolute value of the integer %s is greater than 10E9.\n",s); 
}

putdataval(s,val)  /* put a data value into the data table */
	char *s;
	double val;
{
	if(datatable)  /* check that table is open */
	{
		putmidasval(datatable,s,datarow,val);/* put value into table */
		return;
	}
	fatalerror("No MIDAS table is open to put value :%s .\n",s);
}

int
BCompare(a, indx)		/* compare multi-dimension indices for binsearch */
	int a;
	short indx[5];
{
	int i, ndims, vala, valb;
	int symInx, fvInx;
	int row, col;

	ndims = Fxvars[4];
	row = Frow[a];

	for (i=0; i<ndims; ++i) {
		symInx = Fxvars[i];
		fvInx = FileVarOf(symInx);
		col = ColNumOf(fvInx);
		/*vala = xpartable[Ffile]->rows[row][col].d;*/
		vala = getprowcolval(Ffile, row, col);
		valb = indx[i];
		if (vala < valb) return -1;
		else if (vala > valb) return 1;
	}
	return 0;
}



searchmidas(file,symInx,thisindx,indx) /*search parameter "s" for indx */ 
	int file,thisindx,symInx;
	short indx[5];
{
	/* convert to binary search format for quicksort */
	int rownum,insz,i;
	int fvInx;

	/* look first in the same file for the indx */
	fvInx = FileVarOf(symInx);
	
	Fxvars = filevarptr[fvInx].xvars;
	Ffile = file;
	Frow = fsdx[thisindx].row;
	rownum = BinSearch(indx);

	/* if index cannot be found */
	if (rownum == -1) {
		insz = getindexsz();
		xprintf(stderr, "Indexed parameter/datum \"%s",NameOf(symInx));
		prIndex(stderr, indx,insz);
		xprintf(stderr, "\" not found in file.\n");
		if (fp != NULL) {
			xprintf(fp, "Indexed parameter/datum \"%s",NameOf(symInx));
			prIndex(fp, indx,insz);
			xprintf(fp, "\" not found in file.\n");
		}
		fatalerror("","");
	}

	return Frow[rownum];
}


BinSearch(indx)
	short indx[5];
{
	int low, high, mid, cmp;
	int here = -1;

	low = 0;
	high = xpartable[Ffile]->numrows - 1;

	while (low <= high) {
		mid = (low + high)/2;
		if ((cmp = BCompare(mid, indx)) > 0) 
			high = mid - 1;
		else if (cmp < 0)
			low = mid + 1;
		else {
			here = mid;
			break;
		}
	}
	return here;
}





savexpar(int dispose) /* save paramter tables */
{
	int i;

	for (i=0;i<MAXPARAMFILES;i++) {
		if(xpartable[i])  {
			/* check if open then save & close*/
			xpartable[i] = midaswrite(xpartable[i],xpartable[i]->fname,dispose);
		}
	}

	for (i=0;i<MAXFINDEX;i++) {
		if	(fsdx[i].symInx != -1 && fsdx[i].row != NULL) {
			freemem ("fsdxrow", (char*)fsdx[i].row);
			fsdx[i].symInx = -1;
			fsdx[i].row = NULL;
		}
	}
	nfindices = 0;
}


writexpar() /* write updated paramters to files without closing paramter tables */
{ 
	int i;

	for (i=0;i<MAXPARAMFILES;i++)
		if(xpartable[i])  /* check if open then save & close*/
			 midaswrite(xpartable[i],xpartable[i]->fname,0);

}



initfindex() {
	int num,i,j;

	for (i=0;i<MAXFINDEX;i++) {		
		fsdx[i].symInx = -1;
		fsdx[i].row = NULL;
	}
	if (filevarptr && fvars)
		for (i=0; i<fvars->numrecs; ++i) {
			filevarptr[i].findex = -1;
		}
	nfindices = 0;
}


putxparval(symInx,indx,val) /* put item into parameter table */
	int symInx;
	short indx[5];
	double val;
{
	int therow,thefile, fvInx;
	int thisindx;
	short *xvars;

	/* get which file has parameter in it */
	thefile = getxparnum(symInx);
	
	fvInx = FileVarOf(symInx);
	xvars = filevarptr[fvInx].xvars;
	thisindx = fastindex(thefile,symInx,xvars);

	/* look for index on column name */
	therow = searchmidas(thefile,symInx,thisindx,indx);

	/* replace value in this row in column "s" */	
	putmidasval(xpartable[thefile],NameOf(symInx),therow,val);

	return;
}



double getxparval(symInx,indx) /* get item from paramter table */
	int symInx; /* pointer to parameter */
	short indx[5]; /* index values */
{
	int therow,thefile; 
	int thisindx, fvInx;
	double dnum;
	short *xvars;

	/* get whixparvalch file has parameter in it */
	thefile = getxparnum(symInx);

	fvInx = FileVarOf(symInx);
	xvars = filevarptr[fvInx].xvars;
	thisindx = fastindex(thefile,symInx,xvars);

	/* look for index in column name */
	therow = searchmidas(thefile,symInx,thisindx,indx);
	/* get value in this row from column "s" */
	dnum = getmidasval(xpartable[thefile],NameOf(symInx),therow);
	dnum = (dnum == UND) ? 0.0 : dnum;

	return dnum;
}

fastindex(thefile,symInx,xvars)  /* Has fast index been created ? */
	int thefile;
	short xvars[5];
	int symInx;
{
	int thisindx, fvInx;
	fvInx = FileVarOf(symInx);
	if ((thisindx = filevarptr[fvInx].findex) == -1) {
		thisindx = filevarptr[fvInx].findex = makeindex(thefile,symInx,xvars);
	}
	return thisindx;
}


makeindex(file, symInx, xvars) /* fast index column s of xpartable */
	int file, symInx;
	short xvars[5];
{
	int i,j,k, m,n;
	int fvInx;

	/* load column name to be fast-indexed in */

	i = nfindices ++ ;
	fvInx = FileVarOf(symInx);
	
	/* allocate space for index */
	fsdx[i].symInx = symInx;
	fsdx[i].row = (int*)MemAlloc("fastsearch",(long)xpartable[file]->numrows*sizeof(int));	
	for (n=0; n<xpartable[file]->numrows;n++)
		fsdx[i].row[n] = n;

	Qcompare = FCompare;
	Qexchange = FExchange;
	Frow = fsdx[i].row;
	Fxvars = filevarptr[fvInx].xvars;
	Ffile = file;

	qwiksort(0, xpartable[file]->numrows-1);

	return i;
}

int
FExchange(a,b)		/* exchange two elements in findex */
	register int a, b;
{
	register int temp;
	temp = Frow[a];
	Frow[a] = Frow[b];
	Frow[b] = temp;
}

int
FCompare(a, b)		/* compare multi-dimension indices for sort */
	int a, b;
{
	int i, ndims, symInx, fvInx, vala, valb;
	int col, r;

	ndims = Fxvars[4];
	r = 0;
	for (i=0; i<ndims; ++i) {
		symInx = Fxvars[i];
		fvInx = FileVarOf(symInx);
		col = ColNumOf(fvInx);
		/*vala = xpartable[Ffile]->rows[Frow[a]][col].d;*/
		/*valb = xpartable[Ffile]->rows[Frow[b]][col].d;*/
		vala = getprowcolval(Ffile, Frow[a], col);
		valb = getprowcolval(Ffile, Frow[b], col);
		if (vala < valb) { r = -1; break; }
		else if (vala > valb) { r = 1; break; }
	}
	return r;
}

qwiksort(left,right)  /* an adaptation of Hoare's quick sort */
	int left, right;
{
	register int i,j,mid;
	i = left;
	j = right;
	mid = (left + right)/2;

	do {
		while (i < right && (*Qcompare)(i,mid) < 0) i++;
		while (j > left  && (*Qcompare)(j,mid) > 0) j--;
		if (i <= j) {
			if (i == mid) mid = j;
			else if (j == mid) mid = i;
			(*Qexchange)(i++,j--);
		}
	} while (i <= j);

	if (left  < j) qwiksort(left,j);
	if (i < right) qwiksort(i,right);
}

double 
getparamval(symInx) /* get a value of a parameter (global) */
	int   symInx;
{
	int thefile;
	double dnum;

	/* get which file has parameter in it */
	thefile = getxparnum(symInx);

	/* find in column anme, row 0 */
	dnum = getmidasval(xpartable[thefile],NameOf(symInx),0);
	if (dnum == UND) {	/*	return 0.0;*/
		fatalerror2c("Parameter value %s does not exist in paramfile %s.\n",
			NameOf(symInx),xpartable[thefile]->fname);
	}
	return dnum;
}



putparamval(symInx, value)/* put a value of a global paramter into table */
	int symInx;
	double value;
{
	int thefile;


	/* get which file has parameter in it */
	thefile = getxparnum(symInx);

	/* put into column name, row 0 */
	putmidasval(xpartable[thefile],NameOf(symInx),0,value); 
}


saveparams()
{
	savexpar(0);  /* save parameter table */
}

getxparnum(symInx)
	int symInx;
{
	int fvInx;
	fvInx = FileVarOf(symInx);
	if (fvInx == -1)
		fatalerror("Parameter value %s does not exist in any parameter file.\n",
					NameOf(symInx));
	return  FileNumOf(fvInx);
}

FITS *
getenvtabpntr() {
	if(envtable) return(envtable);
}

load_sigmas(name,indx, value)
	char *name;
	short indx[5];
	double value;
{
	if (indx[4] == 0)
		putsigma(name,value);
	else
		putindexsigma(name,indx,value);
}

putsigma(name,value) /* put the sigma value of a non-indexed prm into the prm table */
	char *name;
	double value;
{
	int thefile;
	char signame[64];


	/* get which file has parameter in it */
	thefile = getxparnum(findsymbol(name, 0));

	/* concatenate "sigma" onto the parameter name */
	strcpy(signame,"sigma_");
	strcat(signame,name);


	/* put into column name, row 0 */
	putmidasval(xpartable[thefile],signame,0,value); 
}

putdelta(name,value) /* put the sigma value of a non-indexed prm into the prm table */
	char *name;
	double value;
{
	int thefile;
	char delname[64];


	/* get which file has parameter in it */
	thefile = getxparnum(findsymbol(name, 0));

	/* concatenate "delta" onto the parameter name */
	strcpy(delname,"delta_");
	strcat(delname,name);


	/* put into column name, row 0 */
	putmidasval(xpartable[thefile],delname,0,value); 
}

putindexsigma(name,indx,val)
/* put the sigma value of an indexed prm into the prm table */
	char *name;
	short indx[5];
	double val;
{
	int therow,thefile;
	int thisindx;
	short *xvars;
	char signame[64];
	int symInx, fvInx;

	/* get integer symbol of name */
	symInx = findsymbol(name, 0);
	fvInx = FileVarOf(symInx);

	/* get which file has parameter in it */
	thefile = getxparnum(symInx);
	
	xvars = filevarptr[fvInx].xvars;
	thisindx = fastindex(thefile,symInx,xvars);

	/* look for index on column name */
	therow = searchmidas(thefile,symInx,thisindx,indx);

	/* concatenate "sigma" onto the parameter name */
	strcpy(signame,"sigma_");
	strcat(signame,name);

	/* add sigma value in this row in column "s" */	
	putmidasval(xpartable[thefile],signame,therow,val);

	return;
}

