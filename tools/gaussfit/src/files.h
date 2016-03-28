/*
	GaussFit - A System for Least Squares and Robust Estimation
	Source Code Copyright (C) 1987 by William H. Jefferys,
	Michael J. Fitzpatrick and Barbara E. McArthur
	All Rights Reserved.
*/
#define _H_files

#define MAXDATASETS 100         /* maximum number of data files */
#define MAXPARAMFILES 10        /* maximum number of parameter files */
#define MAXFINDEX 100           /* maximum number of fast index entries */
#define MAXPINDEX 150           /* maximum number of fast index entries */

/*  header file for routines that access data files */
double getdataval();    /* get data from data file */
double getenvval();     /* get data from environement file */
double getparamval();   /* get parameter from parameter file */
double getresidual();   /* get residual from data file */
double gettheobs();     /* get observation from data file */
double getxparval();    /* get indexed parameer value */
FITS *getenvtabpntr();		


char *getenvstr();      /* get string (pointer) from environment file */
char *getnam();         /* get name from name list */

typedef struct findex {
		int  symInx;            /* symbol table index of param indexed  */
		int  *row;              /* fastindex array  */
} FINDEX;

typedef struct paramindex
{
		char pname[64];				/* name of parameter*/
		int filenum;				/* number of file parameter is in */
} PARAMINDEX;

