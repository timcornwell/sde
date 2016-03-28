/*
 GaussFit - A System for Least Squares and Robust Estimation

	Source Code Copyright (C) 1987 by William H. Jefferys,
	Michael J. Fitzpatrick and Barbara E. McArthur
	All Rights Reserved.
*/

/******************************************/
/*Householder Reduction System Functions*/
/******************************************/

#include <time.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "alloc.h"
#include "house.h"
#include "array.h"
#include "datum.h"
#include "simpledefs.h"
#include "files.h"
#include "protoplasm.h"
#include "symboltable.h"
#include "defines.h"
#ifdef THINK_C
#include <Files.h>
#endif
#include "alloc.h"

extern double envhuber, envtukey, envfair, envtrim;
extern int iterno,envdbg, envminsum, envirls, envorm, envprec;
char *PtoCstr();
unsigned char *CtoPstr();
static int CountErrors = 0;
static int DontDump =0;



double MAD();			/* Median Absolute Deviation Function */


FITS *getenvtabpntr();

#define Matrix(i,j)matrix[RowPtr[i]][ColPtr[j]] /* All arrays are alloc- */
#define RowType(i)rowtype[RowPtr[i]]/* ated dynamically and*/
#define ColType(i)coltype[ColPtr[i]]/* are accessed indirect-*/
#define Deltap(i) deltap[ColPtr[i]]/* ly through arrays*/
#define CondModulus(i)condmodulus[ColPtr[i]] /* ColPtr and RowPtr.*/
#define ConstModulus(i) constmodulus[ColPtr[i]]
#define ConstTest(i)consttest[ColPtr[i]]
#define CondTest(i)condtest[ColPtr[i]]
#define Number(i,j) number[ColPtr[i]][j]
#define NumberX(i) number[ColPtr[i]]
#define VarName(i)varname[ColPtr[i]]


extern double lambda;
extern int MARQ;

/* Allocate pointers for each matrix & vector */
static double **matrix = NULL;	/* Matrix for Eqns of Condition	 */

static int *RowPtr = NULL;	/* RowPtr matrix */
static int *ColPtr = NULL;	/* ColPtr matrix */
static int *rowtype = NULL;	/* Type of each row (condition, constraint) */
static int *coltype = NULL;	/* Type of each col (parameter,indexed parameter, right hand side) */
static short (*number)[5] = NULL;	/* index of a columns parameter */
static double *condmodulus = NULL;/* modulus of condition eqns in a column */
static double *constmodulus = NULL;/* modulus of condition eqns in a row */
static double *condtest = NULL;	
static double *consttest = NULL;
static double *deltap = NULL;	/* vector of parameter corrections */
static double *vector = NULL;	
static double *colvec = NULL;	
static double *sigvec = NULL;	
static double *sigval = NULL;	

static int matcol=MINCOLS;
static int matrow=MINROWS;

/*
extern int totsize;
extern int totrealloc;
extern int totfree;
*/

double *resdum = NULL;/* For minsum routine */
int numresiduals;	/* For minsum routine */

static char **varname = NULL;

#ifdef THINK_C
#define WIDTH  60
extern unsigned char *CtoPstr();
#else
#define WIDTH  120
#endif

typedef int INDEX[3];	/* arrays for "sort" of columns by pivot ability*/
static INDEX *indexarray=NULL;

double *matstart,*matend;


static MEMSIZE matsize; /* pointer to current matrix size */

static int		LastNonzero;	/* Last possible pivot row */
static int		CurrentRow;	/* The row being eliminated */
static int		Recalculate;	/* Flag to recalculate moduli */
static int		LastCol;	/* Last column active in matrix */
static double	Tol;			/* Acceptable Tolerance */
static int		PrintFlag;	/* Print intermediate results */
static int		LastRow;	/* Last row active in matrix */
static int		FirstPivots;	/*	0 = pivot plates first;
									1 = pivot stars first;
								   -1 = let program choose pivots */
static int		Constraints;	/* Number of constraints entered */
static int		Touched = 0;	/* Whether more info has been added */
static int 		noindex = 0;   /* there are no indexed parameters */
static int 		lbl = 0;       /*  the indexes are large; put out line by line*/
static double	DOF;			/* Count of Degrees of Freedom */
static double	NEQN;			/* Count of Eqs of Condition */
double Sigma;				/* Current value of Sigma */
double Sigma1;				/* (extern) Current value of Sigma1 */
double Sigma1Sq;			/* (extern) Current value of Sigma1Sq */
double SumRho = 0.0;			/* (extern) Running Sum for Rho */
double SumPsi = 0.0;			/* (extern) Running Sum for Psi */
double SumPsiSq = 0.0;			/* (extern) Running Sum for PsiSq */
double SumPsiP = 0.0;			/* (extern) Running Sum for PsiP */
double SumPsiPSq = 0.0;			/* (extern) Running Sum for PsiPSq */
double ScaleFac = 1.0;			/* (extern) ScaleFac */
double DeltaV = 0.0;			/* (extern) Largest Residual Change */
double ChiSqr;                 /* Chi Square for real variances*/
static int		StartAt = 0;	/* Column to start pivoting */
int				firstprint = 1; /* for printing out doubles */

FILE *fp = NULL;
FILE *fpc = NULL;
FILE *fpcv = NULL;



double integral();

#define BOUNDS
#ifdef BOUNDS


int rowptr(i)
int i;
{
	int k;

	if((i>=matsize.nrows)||(i<0)) {
		xprintf(stdout, "Row Index Error %d, Rows = %d\n",i,matsize.nrows);
	}
	k = RowPtr[i];
	if((k>=matsize.nrows)||(k<0))
		xprintf(stdout, "Row Pointer Error %d, Rows = %d\n",k,matsize.nrows);
	return k;
}

int colptr(i)
int i;
{
	int k;

	if((i>=matsize.ncols)||(i<0))
		xprintf(stdout, "Column Index Error %d, Cols = %d\n",i,matsize.ncols);
	k = ColPtr[i];
	if((k>=matsize.ncols)||(k<0))
		xprintf(stdout, "Column Pointer Error %d, Cols = %d\n",k,matsize.ncols);
	return k;
}
#else
#define rowptr(i) RowPtr[i]
#define colptr(i) ColPtr[i]
#endif 

#define TOOSMALL 0

long totalalloc=0, totalfree=0, freememry0;

char *MemAlloc(s,size)/* Allocate 'size' bytes */
	char *s;
	long size;
{
	char *x, **z;
	long freememry;
	unsigned usize;

#ifdef USEANSI
	void *malloc();
#else
	char *malloc();
#endif
	/*totsize = totsize+(int)size;*/
	usize = (unsigned)size;
	/* error if not enough memory */
#ifdef DEBUGMEM
	if (totalalloc == 0) {
		freememry0 = FreeMem();
	}
	totalalloc ++;
	freememry = FreeMem();
	if (size>TOOSMALL) 
		xprintf(stdout, "memalloc : %s : size %ld  %ld - %ld = %ld : used %ld   free %ld", 
			s, usize, totalalloc, totalfree, totalalloc-totalfree, 
			freememry0-freememry, freememry);
#endif

	if((x = (char *)malloc(usize)) == NULL) /* Allocate the memory */
			fatalerror("Memory Manager Error in Allocation--%s\n",s); /* error if not enough memory */

#ifdef DEBUGMEM
	if (size>TOOSMALL) xprintf(stdout, ".\n");
#endif
	return x;
}

char *Reallocate(s,size,x)/* use for both initial and reallocation */
	char *s;
	long size;
	char *x;
{
	char *z;
	unsigned usize;

#ifdef USEANSI
	void *realloc();
	void *vx;
#else
	char *realloc();
#endif
	/*totrealloc = totrealloc+ (int)size;	*/
	usize = (unsigned)size;
#ifdef DEBUGMEM
	if (size>TOOSMALL) xprintf(stdout, "realloc : %s : size %ld  #[%ld]", s, usize, totalalloc);
#endif

	if (x == NULL) {
		fatalerror("Attempt to reallocate a NULL ptr in %s", s);
	}
#ifdef USEANSI
    vx = (void*)(x);
	if ((z = (char*)realloc(vx,usize)) == NULL)/* reallocate rowarray */
			fatalerror("Memory Manager Error in Reallocation. Allocated pointer is NULL. --%s\n",s);
#else
	
	if ((z = realloc(x,usize)) == NULL)/* reallocate rowarray */
			fatalerror("Memory Manager Error in Reallocation. Allocated pointer is NULL. --%s\n",s);
#endif


#ifdef DEBUGMEM
	if (size>TOOSMALL) xprintf(stdout, ".\n");
#endif
	return z;
}

freemem(s, ptr)
	char *s;
	char *ptr;
{
#ifdef DEBUGMEM
	totalfree ++;
#endif
	
	/*totfree = totfree + sizeof(ptr);*/
	if (ptr != NULL) {
	
#ifdef DEBUGFREE
	xprintf(stdout, "freemem : %s : size %ld  free %ld  %ld - %ld = %ld", s, *(long*)ptr, FreeMem(),
		totalalloc, totalfree,  totalalloc-totalfree);
#endif
		free(ptr);
	} else {
		/*xprintf(stdout, "FreeMem: NULL Ptr!\n");*/
	}
}

double LSB()/*Computes the size of the LSB*/
/*of a word assuming the MSB is 1.0*/
{
	double x, y;

	x = 1.0;
	do
	    {
		x = 0.5 * x;	/* Divide by 2 repeatedly until no change is */
		y = 1.0 + x;	/* seen when result is added to 1	*/
	}
	while( y != 1.0);
	return 2.0 * x;
}

CountVars(type)		/* Count the number of variables of type 'type' */
int type;
{
	int k,count;

	count=0;	/* initialize */
	for(k=0;k<=LastCol;k++)	/* do for each columsn that has a variable */
		if(type==ColType(k)) /* if the desired type */
			count++;	/* increment count */
	return count;
}

DumpColNames() {
	int i;
    xprintf(stderr, "\n");
	xprintf(fp, "\n"); 

	for (i=0; i<=LastCol; i++) {
		xprintf(fp, "%s %d  %d[%d,%d,%d,%d]\n",
			VarName(i), ColType(i), Number(i,4),
			Number(i,0), Number(i,1), Number(i,2), Number(i,3));
		xprintf(stderr, "%s %d  %d[%d,%d,%d,%d]\n",
			VarName(i), ColType(i), Number(i,4), 
			Number(i,0), Number(i,1), Number(i,2), Number(i,3));
	}
	xprintf(stderr, "\n");
	xprintf(fp, "\n");

}
 
GetDeltaValue(k,name,index,type,value) 	/* Get data which was computed for */
	int k;									/* k'th parameter		 */
	char **name;
	short *index;
	int *type;
	double *value;
{
	if(k<LastCol)			/* done? */
	{
		*type = ColType(k);	/* No; return the data of parameter */
		*name = VarName(k);
		index[0] = Number(k,0); 
		index[1] = Number(k,1); 
		index[2] = Number(k,2);
		index[3] = Number(k,3);
		index[4] = Number(k,4);
		*value = Deltap(k);
		return 1;		/* return TRUE */
	} else return 0;		/* all done; return FALSE */
}


GetDeltaParams(k,name,index,type,value) /* Get data which was computed for */
	int k;									/* k'th parameter		 */
	short *index;
	char **name;
	int *type;
	double *value;
{
	if(k<=LastCol) {			/* done? */
		*type = ColType(k);	/* No; return the data of parameter */
		*name = VarName(k);
		index[0] = Number(k,0); 
		index[1] = Number(k,1); 
		index[2] = Number(k,2);
		index[3] = Number(k,3);
		index[4] = Number(k,4);
		*value = Deltap(k);
		return 1;		/* return TRUE */
	} else return 0;		/* all done; return FALSE */
}


getcolumn(type,name,indices)	
/* Get the number of column corresponding to type; name[indices] */
int type;
short *indices;
char *name;
{
	int k;
	int CP;
	short *NumPtr;

	for(k=0;k<=LastCol;k++) {		/* search all columns */
		CP = ColPtr[k];
		NumPtr = (short *)(number + CP);
		/* if all conditions satisfied */
		if( type==coltype[CP]
			&& NumPtr[0] == indices[0]
			&& NumPtr[1] == indices[1]
			&& NumPtr[2] == indices[2]
			&& NumPtr[3] == indices[3]
			&& NumPtr[4] == indices[4]
			&& strcmp(name, varname[CP]) == 0)
			return k;	/* return k */
	
	}

	if(++LastCol >= matsize.ncols) { /* column doesn't exist; add to table */
		matsize.lastcol = matsize.ncols;
		matsize.ncols = LastCol + 10;/*maxval(1.0,0.2*LastCol);*/
		colspace();
		matspace(); 
	}
	ColType(LastCol) = type; /* enter data into tables- coltype filled*/

	NumPtr = (short *)(number + ColPtr[LastCol]);
	NumPtr[0] = indices[0]; /* number filled */
	NumPtr[1] = indices[1]; /* number filled */
	NumPtr[2] = indices[2]; /* number filled */
	NumPtr[3] = indices[3]; /* number filled */
	NumPtr[4] = indices[4]; /* number filled */
	VarName(LastCol) = name; /* varname filled */
	/* Correct degrees of freedom if not RAS */
	if(type == ParameterType) {
		DOF--;
	}
	/* 
		 DOF = n - k + r where
		n = # of eqs of cond
		k = # of parameters
		r = # of constraints
		*/
			return LastCol;	/* return pointer to new column */
}

double getdeltas(type,name,i)	/* gets parameter correction from array */
char *name;
int type;
short *i;
{
	int k;

	k = getcolumn(type,name,i);	/* get the column number */
	return Deltap(k);	/* fetch parameter correction */
}


PrintMatrix() 
{			/* Print current matrix of eqns of condition */
	int i, begin, width,inc;
	int colwid;

	colwid = getmaxprmname();
	if (colwid <6) colwid = 6;
	width = (WIDTH-colwid)/(colwid+2);

	if (PrintFlag /*&& Touched*/) { /* Only print if matrix has been altered */
				 /* and user wants it */
		begin = 0;
		inc = width;
		i = 1;
		while (inc < LastCol) {
			PrinttheMatrix(begin,inc,colwid);
			i++;
			begin = inc + 1;
			inc = i * width;
		}
		PrinttheMatrix(begin,LastCol,colwid);
	}
}

PrinttheMatrix(begin,end,colwid)
int begin, end,colwid;
{
	int i, j,ndim;
	double matint;

	xprintf(fp,"%*s  ",colwid," ");
	for (j = begin;j<=end;j++)	/* print column number */
		xprintf(fp,"%*d  ",colwid,colptr(j));	
	xprintf(fp,"\n");

	xprintf(fp,"%*s  ",colwid,"Type");		/* print name of row */
	for (j = begin;j<=end;j++)	/* print each item of vector */
		xprintf(fp,"%*d  ",colwid,coltype[colptr(j)]);
	xprintf(fp,"\n");		/* print carriage return */

	xprintf(fp,"%*s  ",colwid,"Name");	/* print name of each variable*/
	for (j = begin;j<=end;j++)
		xprintf(fp,"%*s  ",colwid,VarName(j));
	xprintf(fp,"\n");

	ndim = getdimnum();
	for (j = 0; j<ndim; j++) {
#ifdef OBJECTS
	TaskYield();
#endif
		if (j == 0)
			xprintf(fp,"%*s  ",colwid,"Index");	
		else
			xprintf(fp,"%*s  ",colwid," ");
		for(i=begin;i<end;i++)	{
			if(Number(i,4) != 0) {
				prSIndex(fp,NumberX(i),colwid,j);
				xprintf(fp, "  ");
			} 
			else{
				xprintf(fp,"%*s", colwid,"");
				xprintf(fp, "  ");
			}
		}
		xprintf(fp,"\n");
	}

	xprintf(fp,"\n");
	xprintf(fp,"\n");
	for (i = 0;i<=LastRow;i++)	/* print each row of matrix */
	{
#ifdef OBJECTS
	TaskYield();
#endif
		/* print row number and type */
		xprintf(fp,"%*d%*d  ",(colwid/2),rowptr(i), (colwid/2),RowType(i)); 
		for (j = begin;j<=end;j++) /* print normalized matrix entry */
		{
			matint = 100.0* Matrix(i,j);
			if (fabs(matint) < 1.0E9)
				xprintf(fp,"%*d  ",colwid,trunc(matint));
		}
		xprintf(fp,"\n");
	}
	xprintf(fp,"\n");
}

PrintResults()			/* Print results of iteration */
{
	int i;

	fflush(fp);			/* flush out buffer fo results file */ 
	PrintMatrix();		/* Print the condition eqn matrix */
}

InitHouse(s,PrintVal,Tolerance)/* Initialize HouseHolder routines */
char *s;
int PrintVal;
double Tolerance;
{
	int i, j;

	if (!fp) {
		filecheck(s);
		printcopyright(fp);
		printdate(fp);
		printenvtoRes(fp);
	}
	if(!matrix)	{	/* If no space allocated, allocate it */
		/* Allocate space for each array */
		matsize.ncols = MINCOLS;
		matsize.nrows = MINROWS;
		matsize.lastrow = 0;
		matsize.lastcol = 0;
		matspace();
	}
	if (!RowPtr) rowspace();
	if (!ColPtr) colspace();
	Tol = Tolerance * LSB();/* Save tolerance for reduction */
	PrintFlag = PrintVal;	/* Should results be printed? */
	Constraints = 0;		/* Number of Constraints so far */
	LastRow = -1;			/* Number of Rows so far, -1 */
	LastCol = 0;			/* Number of Columns so far, -1 */
	Touched = 0;			/* Matrix is pristine */
	DOF = 0.0;				/* Start with no eqs read in */
	NEQN = 0.0;
	StartAt = 0;			/* Start looking for pivots in col 0 */
	for (i = 0;i<matsize.nrows;i++)/*Set up row headers */
		RowPtr[i] = i;
	for (j = 0;j<matsize.ncols;j++)/*Set up column headers*/
	{
		ColPtr[j] = j;
		ConstModulus(j) = 0.0;
		CondModulus(j) = 0.0;
	}

	VarName(LastCol) = "_RHS";/* Set up right hand side column varname filled*/
	ColType(LastCol) = RightHandType;/* coltype filled */
	Number(LastCol,0) = 0; /* number filled */
	Number(LastCol,1) = 0; /* number filled */
	Number(LastCol,2) = 0; /* number filled */
	Number(LastCol,3) = 0; /* number filled */
	Number(LastCol,4) = 0; /* number filled */
	/* take these next two lines out - they serve no purpose */
	/*Sigma1 = getenvval("sigma1");*/	/* find current sigma */
	/*Sigma1Sq = sqr(Sigma1);*/		/* get its square*/
	SumRho = 0.0;			/* Initialize all sums */
	SumPsi = 0.0;
	SumPsiSq = 0.0;
	SumPsiP = 0.0;
	SumPsiPSq = 0.0;
	DeltaV = 0.0;
	ScaleFac = getenvval("scale");	/* Get current scale factor */
	if (ScaleFac < 0.0001)
		ScaleFac = 1.0;
	xprintf(stdout,"Initial Scale= ");
	printdouble(stdout,ScaleFac,0);
	xprintf(stdout,"\n");
	numresiduals = 0;		/* number of residuals starts at zero */
	initwobblefix();
}

ReserveSpace()
{
	rowspace();
	colspace();
	matspace();
}


rowspace()
{
	int i;
	long introw, doublerow;

	introw =(long)matsize.nrows*sizeof(int);
	doublerow =(long)matsize.nrows*sizeof(double);
	if (!RowPtr) { /*if first pass allocate space with malloc */
		RowPtr = (int *)MemAlloc("RowPtr",introw);
		rowtype = (int *)MemAlloc("rowtype",introw);
		indexarray = (INDEX*)MemAlloc("indexarray",(long)matsize.nrows*sizeof(INDEX));
		resdum = (double *)MemAlloc("resdum",MATSIZE*doublerow);
	} else {  /* if subsequent pass reallocate space with realloc */
		RowPtr = (int *)Reallocate("RowPtr",introw,(char*)RowPtr);
		rowtype = (int *)Reallocate("rowtype",introw,(char*)rowtype);
		indexarray = (INDEX*)Reallocate("indexarray",(long)matsize.nrows*sizeof(INDEX),(char*)indexarray);
		resdum = (double *)Reallocate("resdum",MATSIZE*doublerow,(char*)resdum);
	}
	/* initialize any new rows */
	for (i = matsize.lastrow;i<matsize.nrows;i++)/*Set up row headers */
		RowPtr[i] = i;
}

freerowspace() {
	if (RowPtr) {
		freemem("RowPtr", (char*)RowPtr);
		freemem("rowtype",(char*)rowtype);
		freemem("indexarray",(char*)indexarray);
		freemem("resdum",(char*)resdum);
	}
	RowPtr = NULL;
	rowtype = NULL;
	indexarray = NULL;
	resdum = NULL;
}

colspace() {
	int j;
	long intcol, doublecol,charcol, shortcol;

	shortcol =(long)matsize.ncols*sizeof(short);
	intcol =(long)matsize.ncols*sizeof(int);
	doublecol =(long)matsize.ncols*sizeof(double);
	charcol =(long)matsize.ncols*sizeof(char*)*4;
	if (!ColPtr) {
		ColPtr = (int *)MemAlloc("ColPtr",intcol);
		coltype = (int *)MemAlloc("coltype 5",intcol);
		number = (short (*)[5])MemAlloc("number 6",5*shortcol);
		condmodulus = (double *)MemAlloc("comdmodulus",doublecol);
		constmodulus = (double *)MemAlloc("constmodulus",doublecol);
		condtest = (double *)MemAlloc("condtest",doublecol);
		consttest = (double *)MemAlloc("consttest",doublecol);
		deltap = (double *)MemAlloc("deltap",doublecol);
		vector = (double *)MemAlloc("vector",doublecol);
 		colvec = (double *)MemAlloc("colvec",doublecol);
 		sigvec = (double *)MemAlloc("sigvec",doublecol);
 		sigval = (double *)MemAlloc("sigval",doublecol);
		varname = (char **)MemAlloc("varname",charcol);
	} else {
		ColPtr = (int *)Reallocate("ColPtr",intcol,(char*)ColPtr);
		coltype = (int *)Reallocate("coltype",intcol,(char*)coltype);
		number = (short (*)[5])Reallocate("number",5*shortcol,(char*)number);
		condmodulus = (double *)Reallocate("condmodulus",doublecol,(char*)condmodulus);
		constmodulus = (double *)Reallocate("constmodulus",doublecol,(char*)constmodulus);
		condtest = (double *)Reallocate("condtest",doublecol,(char*)condtest);
		consttest = (double *)Reallocate("consttest",doublecol,(char*)consttest);
		deltap = (double *)Reallocate("deltap",doublecol,(char*)deltap);
		vector = (double *)Reallocate("vector",doublecol,(char*)vector);
 		colvec = (double *)Reallocate("colvec",doublecol,(char*)colvec);
 		sigvec = (double *)Reallocate("sigvec",doublecol,(char*)sigvec);
 		sigval = (double *)Reallocate("sigval",doublecol,(char*)sigval);
		varname = (char **)Reallocate("varname",charcol,(char*)varname);
	}
	for (j = matsize.lastcol;j<matsize.ncols;j++)/*Set up column headers*/
	{
		ColPtr[j] = j;
		ConstModulus(j) = 0.0;/* Moduli are zero to start */
		CondModulus(j) = 0.0;
	}
}

freecolspace() {
	if (ColPtr) {
		freemem("ColPtr", (char*)ColPtr);
		freemem("coltype", (char*)coltype);
		freemem("number", (char*)number);
		freemem("condmodulus", (char*)condmodulus);
		freemem("constmodulus", (char*)constmodulus);
		freemem("condtest", (char*)condtest);
		freemem("consttest", (char*)consttest);
		freemem("deltap", (char*)deltap);
		freemem("vector", (char*)vector);
		freemem("colvec", (char*)colvec);
		freemem("sigvec", (char*)sigvec);
		freemem("sigval", (char*)sigval);
		freemem("varname", (char*)varname);
	}
	ColPtr = NULL;
	coltype = NULL;
	colvec = NULL;
	sigvec = NULL;
	sigval = NULL;
	varname = NULL;
	vector = NULL;
	deltap = NULL;
	consttest = NULL;
	condtest = NULL;
	constmodulus = NULL;
	condmodulus = NULL;
	number = NULL;
}

int firstmat = 1;

matspace() {
	int i,j;
	long doublerowcol,rowdptr;
	double *rowallocate(), *rowreallocate();

	rowdptr = (long)matsize.nrows*sizeof(double**);
	if (firstmat == 1) {  /* Matrix is set up as a double indirection array */
		matrix=(double **)MemAlloc("matrix",rowdptr);
		for (i=0;i <MINROWS; i++)
			matrix[i] = rowallocate();/* allocate space for double values */
		firstmat++;
	} else {
		if (matcol < matsize.ncols) /* column reallocate */
			for(i=0;i<matsize.nrows;i++) {
				matrix[i] = rowreallocate((char*)matrix[i]); /* allocate space for double values */
			}
		if (matrow < matsize.nrows) {
			matrix=(double **)Reallocate("matrix",rowdptr,(char*)matrix);
			for(i=matrow;i<matsize.nrows;i++)
				matrix[i] = rowallocate(); /* allocate space for double values */
		}
	}
	matcol = matsize.ncols;
	matrow = matsize.nrows;
}

freematspace() {
	short i;
	for(i=0;i<matsize.nrows;i++) {
		freemem("MATRIX ROW",(char*)matrix[i]);
		matrix[i] = NULL;
	}
	freemem("matrix", (char*)matrix);
	matrix = NULL;
	firstmat = 1;
}

double *rowallocate() {
	double *x;
	int i;
	long rowsize;

	rowsize = (long)matsize.ncols*sizeof(double);
	x = (double*)MemAlloc("MATRIX ROW",rowsize);
	for (i=0;i<matsize.ncols;i++)
		x[i] = 0.0;
	return x;
}

double *rowreallocate(Pointr)
char *Pointr;
{
	double *x;
	long rowsize;
	int i;

	rowsize = (long)matsize.ncols*sizeof(double);
	x = (double*)Reallocate("MATRIX ROW",rowsize,Pointr);
	for (i = matcol;i<matsize.ncols;i++) /* zero out end of row */
		x[i] = 0.0;
	return x;
}

filecheck(s)
char *s;
{
	char *fname;
	char corrname[64];
	char covname[64];
	char corrsuf[6];
	char covsuf[6];
#ifdef THINK_C
	short  vol, retc;
	OSErr error;
	FInfo theInfo;
#endif


	/* check for result file name */
	fname = NULL;
	if(!(fname=getenvstr(s)))
		fatalerror("No Result File Specified.Use keyword * results * to specify file name in the environment file\n","");

	if(!(fp = fopen(fname,"w"))) /* open result file */
		fatalerror("Unable to open result file %s.\n",fname);

/* open  a file to write the correlation matrix to so that you can excel it */
	strcpy(corrsuf,".corr"); /* concatenate .corr after the name of variable */
	strcpy(corrname,fname);
	strcat(corrname,corrsuf);

/* for mac version give this file Excel creator type */
 
#ifdef THINK_C
 	GetVol(0L,&vol);
	retc = GetFInfo(CtoPstr(corrname), vol, &theInfo);
	if ((retc == fnfErr) || (retc == noErr))
	{  
			retc = Create((unsigned char *)corrname, vol, 'XCEL', 'TEXT');
			if (retc == dupFNErr)
			{
				retc = FSDelete((unsigned char *)corrname,vol);	
				if (retc == noErr)
					retc = Create((unsigned char *)corrname, vol, 'XCEL', 'TEXT');
				else
					fatalerror("File %s error, file  cannot be  created. \n", 
						PtoCstr((unsigned char *)corrname));
			}
	} else {
		fatalerror("File %s error, file cannot be created. \n",PtoCstr((unsigned char *)corrname));
	}
	PtoCstr((unsigned char *)corrname);
#endif	

	if(!(fpc = fopen(corrname,"w"))) /* open result file */
		fatalerror("Unable to open correlation matrix file %s.\n",corrname);
	
/* open  a file to write the covariance matrix to so that you can excel it */
	strcpy(covsuf,".cov"); /* concatenate .cov after the name of variable */
	strcpy(covname,fname);
	strcat(covname,covsuf);

/* for mac version give this file Excel creator type */

#ifdef THINK_C
 	GetVol(0L,&vol);
	retc = GetFInfo(CtoPstr(covname), vol, &theInfo);
	if ((retc == fnfErr) || (retc == noErr))
	{  
			retc = Create((unsigned char *)covname, vol, 'XCEL', 'TEXT');
			if (retc == dupFNErr)
			{
				retc = FSDelete((unsigned char *)covname,vol);	
				if (retc == noErr)
					retc = Create((unsigned char *)covname, vol, 'XCEL', 'TEXT');
				else
					fatalerror("File %s error, file  cannot be  created. \n",
						PtoCstr((unsigned char *)covname));
			}
	}
	PtoCstr((unsigned char *)covname);
#endif
 
	if(!(fpcv = fopen(covname,"w"))) /* open result file */
		fatalerror("Unable to open covariance matrix file %s.\n",covname);
}


printcopyright(fp)
FILE *fp;
{
	xprintf(fp,"*********************************************");
	xprintf(fp,"***********************************\n\n");
	xprintf(fp,"GaussFit - A System for Least Squares and Robust Estimation\n\n");
	xprintf(fp,"Copyright (C) 1987-93 by William H. Jefferys,\n");
	xprintf(fp,"Barbara E. McArthur, James McCartney,and Mike Fitzpatrick\n");
	xprintf(fp,"All Rights Reserved. Version 3.04\n");
	xprintf(fp,"\n*********************************************");
	xprintf(fp,"***********************************\n\n");
}

SumInit(eqtype)		/* Initialize condition or constraint equations now */
int eqtype;
{
	int i, j, k;

	NEQN++;			/* increment number of equations */
	DOF++;			/* 
						 DOF = n - k + r where
						n = # of eqs of cond
						k = # of parameters
						r = # of constraints
					*/
		LastRow = LastRow + 1;
		Touched = 1;		/* Matrix no longer pristine */
	if(LastRow >= matsize.nrows)	/* Check that row exists*/
	{
		matsize.lastrow = matsize.nrows;
		matsize.nrows = LastRow + 10;/*LastRow*maxval(1.0,0.2*LastRow);*/
		rowspace();
		matspace(); 
	}
	if(eqtype == Constraint)/* increment constraints if it's a constraint */
		Constraints += 1;
	RowType(LastRow) = eqtype;/* fix row type */
	for (i = 0;i<matsize.ncols;i++)	/* zero out entire row */
	{
		Matrix(LastRow,i) = 0.0;
	}
}

SumAdd(type,name, i, x)		/* Add term to matrix */
int type;
short i[5];
char *name;
double x;
{
	int k;
#ifdef DEBUG	
	xprintf(fp,"Sumadd %s = %lf\n",name,x);
#endif
	k = getcolumn(type, name, i);	/* find column corresponding to var */
	Matrix(LastRow,k) += x;		/* ??????add in the value */
	StartAt = minval(StartAt,k);	/* Possible pivot column */
}

SwitchCol (i, j)	/* switch 2 columns of a matrix by switching pointers */
int i, j;
{
	int k;

	k = colptr(i);
	ColPtr[i] = colptr(j);
	ColPtr[j] = k;
}

int Modulus (Col, i)
int Col, i;
/*This has only 3 values: 
**	2 if the row is a potential constraint pivot; 
**	1 if it is a potential condition equation pivot 
**	zero otherwise (if its leading term in the pivot column is zero)
*/
{
	if (Matrix(i,Col) == 0.0)
		return 0;
	else
		return RowType(i);
}

SortPivotColumn (ThisCol)
int ThisCol;			/* only sort rows >= ThisCol */
/*This is an order(n) sort for the special case we have here.*/
{
	int n[3];		/* number of rows of each type */
	int i, j, k;

	for (i = 0;i<= 2;i++)	/* zero out number vector */
		n[i] = 0;
	/* collect the possible pivots of each type */
	for (i = ThisCol;i<=LastRow;i++) {
		j = Modulus(ThisCol, i);
		indexarray[n[j]][j] = rowptr(i);/*put row pointers into array*/
		n[j]++;		/* increment number of pivots of this type */
	}
	k = ThisCol;
	/* Order rows so nonzero pivot constraints 
					 come before nonzero pivot conditions which
					 come before zero pivot rows
					*/
	for (j = 2;j>=0;j--)
		for (i = 0;i<n[j];i++) {
			RowPtr[k] = indexarray[i][j];
			if (j > 0)
				LastNonzero = k;
			k++;
		}
}

ApplyConstraintTransform (column)
int column;
/*This is actually just an elementary Gaussian elimination */
/*on each row that has a leading nonzero element.This*/
/*is the limit of a Givens transformation on that row as */
/*the weight of the pivot row tends to infinity. */
{
	int j, k, colj,rowclm;

	double divisor, s, Old, New, Delta,f1;
	if (notzero(Matrix(column,column)))
		divisor = 1.0 / Matrix(column,column);	/* compute reciprocal of pivot*/
	else
		fatalerror("Apply Constraint Transformation Division by zero--\n"," "); /* error if attempt to divide by zero */
	for (j = column + 1;j<=LastCol;j++)	/* Do for all columns */
	{
		s = divisor * Matrix(column,j);	/* compute normalized element from pivot row */
		if (s != 0.0)	/* only need to do it for nonzero element */
		{
			for (k = column + 1;k<=LastNonzero;k++)/*do for each col */
			{
				Old = Matrix(k,j);/*subtract element from row */
				Delta = s * Matrix(k,column);
				New = Old - Delta;
				Matrix(k,j) = New;
				if (RowType(k) == Condition)
					CondModulus(j) -= 
					    /*CondModulus(j)update 
						condition Modulus -*/ 
					Delta * (Old + New);
				else
					ConstModulus(j) -= 
					    /*ConstModulus(j)update 
						constraint modulus -*/ 
					Delta * (Old + New);
			}
			/* see Lawson and Hanson for this step */
			ConstModulus(j) = RoundToZero(ConstModulus(j)
			    - sqr(Matrix(column,j)),ConstTest(j));

		}
	}
}

ApplyHouseholderTransform (column)
int column;
/*This is straight out of Lawson and Hanson, except that */
/*we check to see if s==0 and don"t transform a column */
/*if that is the case. */
{
	int j, k;
	int columnCP, columnRP, jCP;
	double divisor, s, s2, h, b, Result,f1, f2;
	double *matRow;

	columnCP = ColPtr[column];
	columnRP = RowPtr[column];
	s = 0.0;
	for (j = column;j<=LastNonzero;j++) { /* Form modulus of the pivot column */
		s2 = matrix[RowPtr[j]][columnCP];
		s += s2 * s2;
	}
	s = sqrt(s);		/* square root of the sum of the squares */
	h = matrix[columnRP][columnCP];	/* compute b (reflection coefficient) */
	if (h > 0.0) s = -s;
	h -= s;
	matrix[columnRP][columnCP] = s;	
	b = s * h;
	if (notzero(b)) {		/* ONly do it if b != 0 */
		divisor = 1.0 / b;
		/* Zero out the column and replace it with the 
					orthogonal transformation vector */
		for (j = column + 1;j<=LastCol;j++) {
			jCP = ColPtr[j];
			s = h * matrix[columnRP][jCP];
			for (k = column + 1;k<=LastNonzero;k++) {
				matRow = matrix[RowPtr[k]];
				s += matRow[columnCP] * matRow[jCP];
			}
			s = s * divisor;
			if (s != 0.0) { /*no need to transform the column if s==0*/
				Result = matrix[columnRP][jCP] += s * h;
				f1 = condmodulus[jCP] - sqr(Result);
				f2 = condtest[jCP];
				condmodulus[jCP] = RoundToZero(f1,f2);
				for (k = column + 1;k<=LastNonzero;k++) {
					matRow = matrix[RowPtr[k]];
					matRow[jCP] += s * matRow[columnCP];
				}
			}
		}
	}
}

ApplyTransformation (ThisCol)	/* Apply transformation to each column */
int ThisCol;
{
	SortPivotColumn(ThisCol);/* First sort the column to get the pivot */
	if (RowType(ThisCol) == Constraint)	/* if pivot is constrained */
		ApplyConstraintTransform(ThisCol);/* apply constraint transform */
	else			/* otherwise appy HouseHolder transform */
		ApplyHouseholderTransform(ThisCol);
}

CalculateModuli() /* Modulus of each column */
{
	double ConstSum, CondSum;
	int i, j;

	for (j = CurrentRow;j<LastCol;j++) /* do for each column */
	{
		ConstSum = 0.0; /*separate moduli for conditions and constraints */
		CondSum = 0.0;
		/* sum over all rows starting at current row */
		for (i = CurrentRow;i<=LastRow;i++) {
			if (RowType(i) == Constraint)/* add approriate term to sum */
				ConstSum += sqr(Matrix(i,j));
			else 
				CondSum += sqr(Matrix(i,j));
		}
		ConstModulus(j) = ConstSum;
		ConstTest(j) = ConstSum * Tol;/* used to test if precision has been lost
				consttest filled */
		CondModulus(j) = CondSum;
		CondTest(j) = CondSum * Tol;/* used to test if precision has been lost
				condtest filled*/
	}
	Recalculate = 0;/* Set recalculation flag to zero */
	/*
		Provision is made to recalculate the moduli
		if precision is lost.
	*/
}

CalculateColumnSum() /* Normalized value of each column */
{
	int i, j;
	double colsum;

	for (j = 0;j<LastCol;j++) /* do for each column */
	{
		colvec[j] = 0.0;
		colsum = 0.0;
		/* sum over all rows starting at current row */
		for (i = 0;i<=LastRow;i++)
		{
		  /* add approriate term to sum */
			colsum += sqr(Matrix(i,j));
		}
		colvec[j] = sqrt(colsum);
	}
/*			for (j = 0;j<LastCol;j++)
			xprintf(fp,"%lf\t",colvec[j]);
			xprintf(fp,"\n  \n");
			xprintf(fp,"Matrix before new values \n");
			for (i = 0;i<=LastCol;i++)
				{
				for (j = 0;j<=LastRow;j++)
					xprintf(fp,"%lf\t",Matrix(i,j));
				xprintf(fp,"\n");
				}*/
	for (j = 0;j<LastCol;j++) /* do for each column */
	{
		sigvec[j] = Matrix(j,j);
		for (i = 0;i<=LastRow;i++)
		{
		  /* load new values into matrix */
			Matrix(i,j) = Matrix(i,j)/colvec[j];
		}
	}
}
	

CalculateRowSum() /* Normalized value of each row */
{
	int i, j;
	double rowsum,sqrsum;
	double mxrow,mnrow;	

	mxrow = 0.0;
	mnrow = 10000000000000000.0;
	for (i = 0;i<LastRow;i++) { /* do for each row */
		rowsum = 0.0;
		/* sum over all rows starting at current row */
		for (j = 0;j<LastCol;j++)
		{
		  /* add approriate term to sum */
			rowsum += sqr(Matrix(i,j));
		}
		sqrsum = sqrt(rowsum);

		mnrow = minval(mnrow,sqrsum);
		mxrow = maxval(mxrow,sqrsum);
	}
	if (mnrow != 0.0) {
		xprintf(fp,"Condition = %20.10le\n\n",mxrow/mnrow);
	} else {
		xprintf(fp,"mxrow = %lf,  mnrow = %lf\n\n",mxrow, mnrow);
	}

}
	
	
double TypeModulus (i)
	int i;
{
	if(ColType(i)==RightHandType)
		return (double)RightHandType;
	else if(ConstModulus(i))
		return (double)Constraint;
	else
		return (double)Condition;
}


double NameModulus(i)
	int i;
{
	if(ConstModulus(i) || CondModulus(i))
		return (double)ColType(i);
	else
		return 0.0;
}

double CCModulus(i)
	int i;
{
	double x;

	if(x = ConstModulus(i))
		return x;
	else
		return CondModulus(i);
}

double CompareCol (i, j)/* Compare two columns to determine */
	int i,j;/* precedence>-for pivoting. */
{
	double x;

	if(x = TypeModulus(i) - TypeModulus(j))
		return x; /* Constraint >- Condition >- _RHS */
	else if(x = NameModulus(i) - NameModulus(j))
		return x;/* Subscripted >- Globals */
	else
		return CCModulus(i) - CCModulus(j); /* largest >- smallest */
}

FindPivotColumn (ThisCol)
	int ThisCol;
	/*Pivoting strategy is:*/
	/*Never pivot on RHS */
	/*Constraints before condition equations*/
	/*Low-numbered subscripts after high-numbered*/
	/*Globals after subscripted variables*/
	/*Large column (measured by Sum-Of-Squares) before small */
{
	int i, maximum;

	maximum = ThisCol;/* limits of search */
	CurrentRow = ThisCol;
	if (Recalculate)/* recalculate moduli if significance lost */
		CalculateModuli();
	for (i = ThisCol + 1;i<=LastCol;i++) /* get maximum pivot */
		if (CompareCol(maximum,i) < 0.0)
			maximum = i;
			
	SwitchCol(ThisCol, maximum);/* switch it into first place */
}

double sign(dvalu)
double dvalu;
{
	if (dvalu < 0.0)
		return (-1.0);
	else
		return (1.0);
}

SolveLinearSystem()
{
	int i, k, rpi, cpi;
	double s, smallest, largest,whichsign,mxi;

	if(LastRow<LastCol-1)	/* can't solve system if fewer rows than cols */
		fatalerror(
		"Linear system has fewer rows than columns - unable to solve system\n"
		,"");
	smallest = 1.0 / Tol;
	largest = 0.0;
#define TRYNEWOPTIMIZATION
#ifdef TRYNEWOPTIMIZATION
	for (i = 0;i<LastCol;i++) {	/* zero out parameter correction */
		cpi = ColPtr[i];
		deltap[cpi] = 0.0; /* deltap filled */
 		if (MARQ) {
			rpi = RowPtr[i];
			mxi = matrix[rpi][cpi];
 			whichsign = sign(mxi);
 			/* adjust matrix diagonal with lambda */
 			matrix[rpi][cpi] = whichsign*(sqrt(sqr(mxi) + sqr(lambda)));
  		}
 	}
#else
 	for (i = 0;i<LastCol;i++) {	/* zero out parameter correction */
		Deltap(i) = 0.0; /* deltap filled */
  		if (MARQ) {
 			whichsign = sign(Matrix(i,i));
 			/* adjust matrix diagonal with lambda */
			Matrix(i,i) = whichsign*(sqrt(sqr(Matrix(i,i)) + sqr(lambda)));
	   	}
	}
#endif
 	CalculateColumnSum();
 	CalculateRowSum();
	for (i = LastCol-1;i>=0;i--)	/* Go from last to first column */
	{
		s = Matrix(i,LastCol);	/* get RHS */
		for (k = LastCol-1;k>i;k--) /* Subtract terms already solved for */
			s -= Matrix(i,k) * Deltap(k);
		if (Matrix(i,i) == 0.0) /* Check if matrix is singular */
		{
			fatalerror("Matrix Singular!\n","");
			Deltap(i) = 0.0; /* This line can never be activated. */
		} else {
			/*if nonsingular, compute parameter correction */
			Deltap(i) = s / Matrix(i,i);/* deltap filled */
		}
		/* update smallest and largest matrix element*/
		/*smallest = minval(smallest, fabs(Matrix(i,i)));
		largest = maxval(largest, fabs(Matrix(i,i)));*/
	}
	for (i = LastCol-1;i>=0;i--)	/* Go from last to first column */
 	{
 		if (colvec[i] != 0.0)
 			Deltap(i) = Deltap(i)/colvec[i];
 	}
	/* estimate condition as ratio */
	/*xprintf(fp,"Condition = %7.1le\n\n", largest/maxval(smallest,Tol));*/
}

SumIt() 
{
	double KFAC;
	if(DOF <= 0.0)		/* can't solve if DOF <= 0/0 */
		fatalerror("The observation set is underdetermined!\n","");
	SumRho *= 2.0/DOF;	/* Sums for this iteration */
	SumPsi /= (NEQN - Constraints);
	SumPsiSq /= DOF;
	SumPsiP /= (NEQN - Constraints);
	SumPsiPSq /= (NEQN - Constraints);

	/* compute new sigma and scale factor */

	KFAC = 1.0 + (NEQN - Constraints-DOF)/(NEQN - Constraints)*(SumPsiPSq - sqr(SumPsiP));
	Sigma1Sq = KFAC*SumPsiSq/SumPsiP;
	Sigma1 = sqrt(Sigma1Sq);
	Sigma = Sigma1*(ScaleFac==0.0?1.0:ScaleFac);
	putenvval("sigma",Sigma);
	ScaleFac = (ScaleFac==0.0?1.0:ScaleFac)*sqrt(SumRho);
	putenvval("scale",ScaleFac);

	/* Print results of iteration */
	
	if (iterno == 0)
	{
		xprintf(fp,"\nNEQN = %5d, DOF = %5d\n\n",(int)NEQN,(int)DOF);
		xprintf(stdout,"\nNEQN = %5d, DOF = %5d\n\n",(int)NEQN,(int)DOF);
	}

	if (envdbg)
	{
		printiter_results(stdout,KFAC);
		printiter_results(fp,KFAC);
	}
}

printiter_results(fp,KFAC)
FILE *fp;
double KFAC;
{
		xprintf(fp,"scale     =\t");
		printdouble(fp,ScaleFac,0);
		xprintf(fp,"\n");
		xprintf(fp,"SumRho    =\t");
		printdouble(fp,SumRho,0);
		xprintf(fp,"\n");
		xprintf(fp,"SumPsi    =\t");
		printdouble(fp,SumPsi,0);
		xprintf(fp,"\n");
		xprintf(fp,"SumPsiSq  =\t");
		printdouble(fp,SumPsiSq,0);
		xprintf(fp,"\n");
		xprintf(fp,"SumPsiP   =\t");
		printdouble(fp,SumPsiP,0);
		xprintf(fp,"\n");
		xprintf(fp,"SumPsiPSq =\t");
		printdouble(fp,SumPsiPSq,0);
		xprintf(fp,"\n");
		xprintf(fp,"KFAC      =\t");
		printdouble(fp,KFAC,0);
		xprintf(fp,"\n");
		xprintf(fp,"Sigma     =\t");
		printdouble(fp,Sigma,0);
		xprintf(fp,"\n");
		xprintf(fp,"\n");
}

CovarianceMtx()
{
	int i,j,k,width,begin,inc;
	int indsize, mxprnam, colwid;
	double s;

	if(envminsum)	{ /* see if covariance matrix can be provided */
			xprintf(stdout, "Sorry, can't get covariance matrix when doing minsum\n");
			return 0;		/*Can't get cov mtx when doing minsum */
		}
	if(envirls) {
		if ((envfair) || (envtrim) || 
			(envtukey) || (envhuber) ||
			(envminsum))
		{
			xprintf(stdout, "Sorry, covariance matrix not implemented for robust estimation using IRLS\n");
			return 0;		/* Can't get covariance mtx when doing IRLS */
		}
	}

	xprintf(stdout, "Calculating Covariance Matrix\n");
	for(k=LastCol-1; k>=0; k--)	{ /* Do for each column */
#ifdef OBJECTS
	TaskYield();
#endif
		for (i=0; i<LastCol; i++)	/* zero out the vector */
			vector[i] = 0.0; /*vector filled */
		for (i=k; i>=0;i--)	{ /* compute inverse of upper triang. matrix */
			s = deltafn(i,k)*deltafn(RowType(i),Condition);
			/* Handle covariance of condition eqn */
			/* rhs vector is zero for a constraint,
								 1 on diagonal for a condition */
			for (j=k; j>i; j--)
				s -= Matrix(i,j) * vector[j];
			if (Matrix(i,i) == 0.0) { /* Back substitute the column */
				xprintf(fp,"Matrix Singular!\n");
				vector[i] = 0.0; /* vector filled */
			} else		/* recompute column of inverse matrix */
				vector[i] = s / Matrix(i,i); /* vector filled */
		}

		for(i=0;i<LastCol;i++) { /*put inverse as a row in lower triangle */
			Matrix(k,i) = vector[i];	/* R Transpose to Matrix */
		}
	}

	for(k=LastCol-1; k>=0; k--)	{ /* multiply matrix by it's transpose */
#ifdef OBJECTS
	TaskYield();
#endif
		for(i=0; i<=k; i++) {
			s = 0.0;
			for(j=k; j<LastCol; j++)
				s += Matrix(j,i) *Matrix(j,k);
			Matrix(i,k) = s*Sigma1Sq; /* times variance of unit weight */
		}
	}

	/* print correlation matrix if cleared */

	indsize = getindexsz();
	mxprnam = getmaxprmname();

	if (indsize > 8) lbl = 1;

	if (indsize == 0) {
			indsize = mxprnam;
			noindex = 1;
		}

	if (lbl) colwid = 8;
	else colwid = indsize;

	if (lbl) width = WIDTH-(mxprnam+8)/colwid+2;	
	else  width = WIDTH-(mxprnam+indsize)/colwid+2;	

	begin = 0;
	inc = width;
	i = 1;
	prSigma(indsize,mxprnam);
	while (inc <LastCol) {
#ifdef OBJECTS
	TaskYield();
#endif
		PrintCMatrix(fp,begin,inc,indsize,colwid,mxprnam,100,0);
		i++;
		begin = inc;
		inc = i* width;
	}
	PrintCMatrix(fp,begin,LastCol,indsize,colwid,mxprnam,100,0);
	PrintCMatrix(fpc,0,LastCol,indsize,colwid,mxprnam,1,0);
	PrintCMatrix(fpcv,0,LastCol,indsize,colwid,mxprnam,1,1);
}	

PrintCMatrix(fptr,start,end,indsize,colwid,mxprnam,mult,type)
int start, end,indsize,mxprnam, colwid,mult,type;
FILE *fptr;
{
	int i,j,ndim;
	double	div,matint;

	ndim = getdimnum();
	xprintf(fptr,"\n");
	if (type == 0) /* then it's a correlation matrix */
		xprintf(fptr, "Correlation Matrix * %d\n",mult);
	else		/* then it's a covariance matrix */
		xprintf(fptr, "Covariance Matrix\n");
	xprintf(fptr,"\n");

	if (noindex)
		xprintf(fptr,"%*s\t",mxprnam," ");
	else
		xprintf(fptr,"%*s\t",mxprnam+indsize," ");

	for(i=start;i<end;i++)	
		xprintf(fptr,"%*s\t", colwid,VarName(i));
	xprintf(fptr,"\n");

	if ((!noindex)  && (!lbl)){
		xprintf(fptr,"%*s\t",indsize+mxprnam," ");
		for(i=start;i<end;i++)	{
#ifdef OBJECTS
	TaskYield();
#endif
			if(Number(i,4) != 0) {
				prIndex(fptr,NumberX(i),indsize);
				xprintf(fptr, "\t");
			} else {
				xprintf(fptr,"%*s", indsize," ");
				xprintf(fptr, "\t");
			}
		}
		xprintf(fptr,"\n");
	}

	if ((!noindex)  && (lbl)) {
		for (j = 0; j<ndim; j++) {
#ifdef OBJECTS
	TaskYield();
#endif
			xprintf(fptr,"%*s ",indsize+mxprnam," ");
			for(i=start;i<end;i++)	{
				if(Number(i,4) != 0) {
					prSIndex(fptr,NumberX(i),colwid,j);
					xprintf(fptr, "\t");
				} 
				else{
					xprintf(fptr,"%*s", colwid,"");
					xprintf(fptr, "\t");
				}
			}
			xprintf(fptr,"\n");
		}
	}
	xprintf(fptr,"\n");

	for(i=0;i<LastCol;i++) {
#ifdef OBJECTS
	TaskYield();
#endif
		if ((Number(i,4) == 0) && (noindex==1)) {
			xprintf(fptr,"%*s\t", mxprnam,VarName(i));
			}
		else if ((Number(i,4) == 0) && (noindex==0)){
			xprintf(fptr,"%*s", mxprnam,VarName(i));
			xprintf(fptr,"%*s", indsize," ");
			xprintf(fptr,"\t");
			}
		else {
			xprintf(fptr,"%*s",mxprnam,VarName(i));
			prIndex(fptr,NumberX(i),-indsize);
			xprintf(fptr,"\t");
		}
		for(j=start;j<end;j++) {	/*print the values now */
	 		div =  (sqrt(Matrix(i,i)* Matrix(j,j)));
			if (notzero(div)) {
				matint = (double)mult * (Matrix(i,j)/div);
			}
			if (type == 0) { /* it's a correlation matrix */
				if (i >j) {
					xprintf(fptr,"%*s\t", colwid,"");
				} else {
					if (fabs(matint) < 1.0E9) {
						if (mult >10)
	 						xprintf(fptr,"%*d\t",colwid,(int)matint);
						else
	 						xprintf(fptr,"%le\t",matint);
					}
				}
			} else { /* it's a covariance matrix */
				if (i >j) {
					matint = (double)mult * (Matrix(j,i)/div);
	 				xprintf(fptr,"%le\t",matint*sigval[i]*sigval[j]);
				} else {
	 				xprintf(fptr,"%le\t",matint*sigval[i]*sigval[j]);
	 			}
		 	}
		}
		xprintf(fptr,"\n");
	}
	xprintf(fptr,"\n");
}



prSigma(indsize,mxprnam)
int indsize, mxprnam;
{
    int i,j;
	short snum[5];

	xprintf(fp,"\n");
	xprintf(fp, "Sigma Values \n");
	xprintf(fp,"\n");
	for(i=0;i<LastCol;i++) {
		if ((Number(i,4) == 0) &&(noindex))
			xprintf(fp,"\tSigma %*s   ", mxprnam,VarName(i));
		else if ((Number(i,4) == 0) &&(!noindex))
			xprintf(fp,"\tSigma %-*s", mxprnam+indsize,VarName(i));
		else {
			xprintf(fp,"\tSigma %*s",mxprnam,VarName(i));
			prIndex(fp,NumberX(i),-indsize);
			xprintf(stdout, "   ");
		}
		sigval[i] = (sqrt(Matrix(i,i)))*(1.0/colvec[i]);
		xprintf(fp," %20.15le\n",sigval[i]);
		for (j=0;j<5; j++)
			snum[j] = Number(i,j);
		load_sigmas(VarName(i),snum,sigval[i]);
	}
}

prIndex(fd, indx, indsize)
	FILE *fd;
	short indx[5];
	int indsize;
{
	int i;
	char string[100];
	char tiny[10];

	/*
	if (indx[4] < 1 || indx[4] > 4) {
		xprintf(stderr, "\nprintIndex: Number of dimensions = %d\n",indx[4]);
		exit(-1);
	}
	*/
	strcpy(string,"[");
	for (i=0; i<indx[4]; ++i) {
		if (i!=0) strcat(string, ",");
		sprintf(tiny, "%d",indx[indx[4]-1-i]);
		strcat(string,tiny);
	}
	strcat(string, "]");
	xprintf(fd,"%*s",indsize,string);
}


prSIndex(fd,indx,indsize,level)
	FILE *fd;
	short indx[5];
	int indsize,level;
{
	int i;
	char tiny[10];

	if (indx[4] < 1 || indx[4] > 4) {
		xprintf(stderr, "\nprintIndex: Number of dimensions = %d\n",indx[4]);
		fatalerror("","");
	}
	sprintf(tiny, "[%d]",indx[indx[4]-1-level]);
	xprintf(fd,"%*s",indsize,tiny);
}

printdouble(fp,dvalue,preon)
	double dvalue;
	FILE *fp;
	int preon;
{
	double floor() ;
	double truncnum,diffround,tnum;
	char dnum[30],dnum2[30];
	int i, j;
	static int prec = 18;
	static int file_prec = 18;
	static int maxi = 0;
	static double two_thirds;


	if ((dvalue < UUND) && (dvalue > LUND))
	{
		xprintf(fp,"#N/A\t");
		return;
	}

	if (firstprint)
	{
		firstprint = 0;
		prec = envprec;
		file_prec = machine_prec();
		two_thirds = 0.66 * file_prec;
		if ((file_prec < 14) || (file_prec >  30))
			file_prec  = DBL_DIG;
		if ((prec < 4) || (prec >  30))
			prec  = file_prec;
	}
 
	if ((prec != file_prec) && (preon == 0))
		xprintf(fp,"% .*g\t",prec,dvalue);
	else
 	{
		if (((fabs(dvalue) < 1.0E-10) || (fabs(dvalue) > 1.0E+9))
			&& (dvalue != 0.0))
			xprintf(fp,"% .*e\t",file_prec,dvalue);
		else
		{
			sprintf(dnum,"% .*f",file_prec,dvalue);
			i = strlen(dnum)-1;
			if (i > maxi) maxi = i;
			if (DBL_DIG < i) i = DBL_DIG;
			if ((dnum[i] != '0') && (dnum[i-1] == '0'))
				i--;
			while (dnum[i] == '0')	
				i--;
			i++;
			if (dnum[i-1] == '.') i++;
			if (dvalue < 0.0) i++;
			dnum[i] = '\0';
			if (i > (int)two_thirds)
				{
				xprintf(fp,"%-*s\t",maxi,dnum);
				}
			else
				{
				xprintf(fp,"%-s\t",dnum);
				}
		}
	}
}

getindexsz()
{
	int indxsiz,i,j,k;
	long pow10;
	int max[4];
	static int sumsize = -1;

	if (sumsize == -1)
	{
		sumsize = 0;
		for (i=0;i<4; i++)
			max[i] = -1;
		for (i=0;i<LastCol;i++)
			for(j=0;j<Number(i,4);j++)
				if (Number(i,j) > max[j]) max[j] = Number(i,j);
		/* find size of indices */
		for (i=0; i<4;i++)
		{
			if (max[i] > -1)
			{
				k = 1;
				pow10 = 10;
				while (max[i] >= pow10) { k++; pow10 *= 10; }
				sumsize = sumsize + k;
			}
				
		}
		/* add space for commas*/
		for (i=0;i<4;i++)
			if (max[i] > -1)
				sumsize++;
		if (sumsize > 0) sumsize ++;
	}		
	return (sumsize);
}
 	 
getmaxprmname()
{
	int length,i;
	static int maxp = -1;

	if (maxp == -1)
	{
		maxp = 0;
		for (i = 0; i< LastCol; i++)
			if (maxp < (length = strlen(varname[i])))
				maxp =length;
	}
	return maxp;
}

getdimnum()
{
	int i;
	static int maxd = -1;

	if (maxd == -1)
	{
		maxd = 0;
		for (i = 0; i< LastCol; i++)
			if (maxd < (Number(i,4))) maxd  = Number(i,4);
	}
	return maxd;
}


Solve1()		/* Solve matrix as it exists so far */
{
	int ThisCol;
	int i,j;
	int EndAt;


	if(!Touched)
		return;		/* Matrix has already been transformed */
	FirstPivots = 0;/* we now always start at zero */
	Recalculate = 1;	/* recalculate moduli after solution */
	/*xprintf(fp,"\nTransformation Number ");*/
	EndAt = minval(LastRow,LastCol); /* last row to transform */
	for (ThisCol = StartAt;ThisCol<=EndAt;ThisCol++) /* transform each column */
	{
#ifdef OBJECTS
	TaskYield();
#endif
		/*xprintf(fp,", %3d", ThisCol);*/
		if ((ThisCol % 20) == 0) xprintf(fp,"\n");
		FindPivotColumn(ThisCol); /* Find pivot columns and */
		ApplyTransformation(ThisCol);/* apply appropriate transformation */
	}
	for(i=1;i<=EndAt;i++) {	/* zero out the lower triangle */
#ifdef OBJECTS
	TaskYield();
#endif
		for(j=0;j<i;j++) {
			Matrix(i,j) = 0.0;
		}
	}
	LastRow = EndAt;		/* reset rows/cols that have been */
	StartAt = LastCol;		/* transformed 			*/
	PrintMatrix();			/* Print matrix if desired*/
	Touched = 0;			/* Matrix now pristine*/
}

SolveMatrix()			/* Use chosen algorithm to solve matrix */
{
	int kode,iter;
	double error;
	short iz[5];


	iz[0] = iz[1] = iz[2] = iz[3] = iz[4] = 0;
	xprintf(stdout, "Solving Matrix...\n\n");
	if(envminsum)	{	/* Do minsum method? */
		int i,j,k,pivtype;

		/* compute scale factor */
		ScaleFac = 1.48*MAD(resdum,numresiduals)
			*(ScaleFac==0.0?1.0:ScaleFac);
		putenvval("sigma",ScaleFac); /* insert into environment file */
		putenvval("scale",ScaleFac);

		xprintf(stdout, "ScaleFac = ");
		printdouble(stdout,ScaleFac,0);
		xprintf(stdout, "Sigma    = ");
		printdouble(stdout,ScaleFac,0);
		xprintf(stdout, ", DOF = %d\n\n",(int)DOF);

		xprintf(fp,"ScaleFac = ");
		printdouble(fp,ScaleFac,0);
		xprintf(fp,"Sigma    = ");
		printdouble(fp,ScaleFac,0);
		xprintf(fp,", DOF = %d\n\n",(int)DOF);
		/* Set up for minsum algorithm */
		kode = 0;
		iter = 10*LastRow;
		i = getcolumn(RightHandType,"_RHS",iz); /* move RHS to last col*/
		SwitchCol(i,LastCol);

		i = 0;
		j = LastRow;		/* move conditions before constraints */
		pivtype = RowType(j);
		do {
#ifdef OBJECTS
	TaskYield();
#endif
			for(;i<j && RowType(i) <= pivtype;)
				i = i+1;
			for(;j>i && RowType(j) >= pivtype;)
				j = j-1;
			if(i<j) {
				k = rowptr(i);
				RowPtr[i] = rowptr(j);
				RowPtr[j] = k;
			}
		} while (i<j);
		k = rowptr(LastRow);
		RowPtr[LastRow] = rowptr(i);
		RowPtr[i] = k;

		PrintMatrix();
		if ((matsize.nrows < LastRow +3) || /*minsum needs 2 extra rows*/
		(matsize.ncols < LastCol+2))/* minsum needs 1 extra column */
		{
			if (matsize.ncols < LastCol+2) {
				matsize.lastcol = matsize.ncols;
				matsize.ncols = matsize.ncols +1;
				colspace();
			}
			if (matsize.nrows < LastRow +3) {
				matsize.lastrow = matsize.nrows;
				matsize.nrows = matsize.nrows +2;
				rowspace();
			}
			matspace();
		}
		cl1(matsize.nrows,matsize.ncols,LastRow+1-Constraints,Constraints,0,LastCol,
		matrix,deltap,resdum,RowPtr,ColPtr,
		&kode,1.0e-10,&error,&iter);/* call minsum routine */
		/* Print results */
		xprintf(fp,"Error = %lf\n",error);
		xprintf(fp,"Kode= %d\n",kode);
		xprintf(fp,"Iters = %d\n\n",iter);
		xprintf(fp,"\n");
	} else {
		Solve1();		/* Solve matrix */
		SolveLinearSystem();	/* back substitute for parameters */
	}
}

triangularize1()	/* Triangularize matrix */
{
	if(!envminsum) { /* Can't triangularize if doing minsum */
		xprintf(stdout, "\nTriangularizing Matrix...\n\n");
		Solve1();	/* Call triangularize routine */
	}
}

triangularize()		/* Externally callable triangularize routine */
{
	triangularize1();
	PushTrue();
}

insertresidual(x)
	double x;
{
	if (!envminsum)
		return;
	if (numresiduals >= matsize.nrows)
	{
		matsize.lastrow = matsize.nrows;
		matsize.nrows = numresiduals + 10;/*numresiduals*maxval(1.0,0.2*numresiduals);*/
		rowspace();
		matspace(); 
	}
	resdum[numresiduals]= x;
	numresiduals++;
}

dump(s)
	char *s;
{
	int i, j,k,l;
	int collimit, rowlimit;

	if(DontDump)
		return;
	if(CountErrors>100)
		return;
	CountErrors++;
	xprintf(fp,s);
	xprintf(fp,", Entry # %d\n",CountErrors);
	collimit = matsize.ncols;
	rowlimit = minval(LastRow,30);
	xprintf(fp,"Matrix\n");
	for (j = 0; j <collimit; j++)
		xprintf(fp,"%3d ",j);
	xprintf(fp,"\n");
	for (i = 0; i <rowlimit; i++)
	{
		xprintf(fp,"%7d",i);
		for (j = 0; j <collimit; j++)
		{
			xprintf(fp,"%7.3f",Matrix(i,j));
		}
		xprintf (fp,"\n");
	}
	xprintf (fp,"\n");

	xprintf(fp,"RowType\n");
	for (i = 0; i <rowlimit; i++)
	xprintf(fp,"%7d \n",RowType(i));
	xprintf(fp,"indexarray\n");
	for (i = 0; i <rowlimit; i++)
	xprintf(fp,"%7d \n",indexarray[i]);
	xprintf(fp,"RowPtr\n");
	for (i=0;i<rowlimit;i++)
	xprintf(fp,"%7d \n",RowPtr[i]);
	xprintf(fp,"ColPtr\n");
	for (i=0;i<collimit;i++)
	xprintf(fp,"%7d \n",ColPtr[i]);
	xprintf(fp,"ColType\n");
	for (j = 0; j <collimit; j++)
	 xprintf(fp,"%7d \n",ColType(j));
	xprintf(fp,"Number\n");
	for (j = 0; j <collimit; j++)
	 xprintf(fp,"%7d %7d %7d %7d     %7d\n",Number(j,0),Number(j,1),Number(j,2),
		Number(j,3),Number(j,4));
	xprintf(fp,"ConstModulus\n");
	for (j = 0; j <collimit; j++)
	 xprintf(fp,"%7.3f \n",ConstModulus(j));
	xprintf(fp,"CondTest\n");
	for (j = 0; j <collimit; j++)
	 xprintf(fp,"%7.3f \n",CondTest(j));
	xprintf(fp,"CondModulus\n");
	for (j = 0; j <collimit; j++)
	 xprintf(fp,"%7.3f \n",CondModulus(j));
	xprintf(fp,"ConstTest\n");
	for (j = 0; j <collimit; j++)
	 xprintf(fp,"%7.3f \n",ConstTest(j));
	xprintf(fp,"Deltap\n");
	for (j = 0; j <collimit; j++)
	 xprintf(fp,"%7.3f \n",Deltap(j));
	xprintf(fp,"vector\n");
	for (j = 0; j <collimit; j++)
	 xprintf(fp,"%7.3f \n",vector[j]);
	xprintf(fp,"VarName\n");
	for (j = 0; j <collimit; j++)
	 xprintf(fp,"%7s \n",VarName(j));

}
	
	
dump8() {
	int inc;
	inc = 0;
	xprintf(fp,"dump8\n");
	while (inc < LastCol+20) {
		d8(inc,inc+20);
		inc = inc + 20;
	}
	d8(inc,LastCol);
}
	
d8(FCol,LCol)
	int LCol,FCol;
{
	int i, j;
	double matint;

	for (j=0;j<8;j++) {
		xprintf (fp,"%d",j);
		for (i=FCol;i<LCol;i++)
		{
			matint = 1000.0* Matrix(i,j);
			if (fabs(matint) < 1.0E9)
				xprintf(fp,"%4d ",(int)matint);
		}
		xprintf(fp,"\n");
	}
	xprintf(fp,"\n");
}



printenvtoRes(fp)
FILE *fp;
{
	FITS *fitpnt;
	fitpnt = getenvtabpntr();
	xprintf(fp,"Contents of the Environment File for this Run: \n\n");
	if (fitpnt)
		fitswrite(fp, fitpnt);
}

printdate(fp)
FILE *fp;
{
	time_t ltime;

	time(&ltime);
	xprintf(fp,"\nTime of GaussFit Run:  %s\n",ctime(&ltime));

}

Get_LastCol() { return LastCol; }
 
double Get_Sigma() { return Sigma; }


ChiSquare()
{

		ChiSqr = Sigma * Sigma * DOF;
		printmessage(fp);
		printmessage(stdout);
		xprintf(fp,"\nChi-Square  = \t");
		printdouble(fp,ChiSqr,0);
		xprintf(fp,"\t DOF = %d\n\n",(int)DOF);
		xprintf(stdout, "\nChi-Square  = \t");
		printdouble(stdout,ChiSqr,0);
		xprintf(stdout, "\t DOF = %d\n\n",(int)DOF);
}

printmessage(fpoint)
FILE *fpoint;
{
	xprintf(fpoint,"\n***********************************");
	xprintf(fpoint,"***********************************\n");
	xprintf(fpoint,"If the input variances are real, the chi-square");
	xprintf(fpoint," is correct and should\nbe equivalent to the");
	xprintf(fpoint," degrees of freedom(DOF).  Otherwise, chi-square\n");
	xprintf(fpoint,"should be divided by a proportionality factor.");
	xprintf(fpoint,"\n***********************************");
	xprintf(fpoint,"***********************************\n");
}



machine_prec()

{

/*
		This program prints hardware-determined double-precision
		machine constants obtained from rmachar.  Dmachar is a C
		translation of the Fortran routine MACHAR from W. J. Cody,
		"MACHAR: A subroutine to dynamically determine machine
		parameters".  TOMS (14), 1988.

		Descriptions of the machine constants are given in the
		prologue comments in rmachar.

		Subprograms called

		  rmachar

		Original driver: Richard Bartels, October 16, 1985

		Modified by: W. J. Cody
						 July 26, 1988

		**********
*/
		double prec;
		int it;

		rmachar(&it);
		prec = ceil(it*0.3);
		return (int)prec;
}


rmachar(it)

		int *it;

/*

	This subroutine is intended to determine the parameters of the
	 floating-point arithmetic system specified below.  The
	 determination of the first three uses an extension of an algorithm
	 due to M. Malcolm, CACM 15 (1972), pp. 949-951, incorporating some,
	 but not all, of the improvements suggested by M. Gentleman and S.
	 Marovich, CACM 17 (1974), pp. 276-277.  An earlier version of this
	 program was published in the book Software Manual for the
	 Elementary Functions by W. J. Cody and W. Waite, Prentice-Hall,
	 Englewood Cliffs, NJ, 1980.  The present program is a
	 translation of the Fortran 77 program in W. J. Cody, "MACHAR:
	 A subroutine to dynamically determine machine parameters".
	 TOMS (14), 1988.

	Parameter values reported are as follows:

		  it		- the number of base ibeta digits in the floating-point
						significand

		Latest revision - August 4, 1988

		Author - W. J. Cody
					Argonne National Laboratory

*/

{
		int i,irnd;
		int itmp;
		double a,b,beta,one,y,z,zero;
		double betah,t,tmp,tmp1,two;

		irnd = 1;
		one = (double)irnd;
		two = one + one;
		a = two;
		b = a;
		zero = 0.0e0;

/*
  determine beta ala malcolm
*/

		tmp = ((a+one)-a)-one;

		while (tmp == zero) {
			a = a+a;
			tmp = a+one;
			tmp1 = tmp-a;
			tmp = tmp1-one;
		}

		tmp = a+b;
		itmp = (int)(tmp-a);
		while (itmp == 0) {
			b = b+b;
			tmp = a+b;
			itmp = (int)(tmp-a);
		}

		beta = (double)itmp;

/*
  determine  it
*/

		(*it) = 0;
		b = one;
		tmp = ((b+one)-b)-one;

		while (tmp == zero) {
			*it = *it+1;
			b = b*beta;
			tmp = b+one;
			tmp1 = tmp-b;
			tmp = tmp1-one;
		}
}
