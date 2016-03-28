/*

	GaussFit - A System for Least Squares and Robust Estimation
	Source Code Copyright (C) 1987 by William H. Jefferys,
	Michael J. Fitzpatrick and Barbara E. McArthur
	All Rights Reserved.
*/



/* typedef definitions of FITS files for main  */

#ifndef SIMPLEDEFS_H
#define SIMPLEDEFS_H

#define FITSROWS 64  /* max number of rows in FITS table */
#define KEYLEN 9      /* max length of a key word */
#define FSTRLEN 23    /* max length of a string to be rad in */

/* Header file for Tables */

#define MINCOLS 16                   /* max columns */
#define MINROWS 16                   /* max rows */
#define MIDASCOLS 16                   /* max columns */
#define CMIDASCOLS 1                  /* max columns */
#define MIDASROWS 16                   /* max rows */
#define STRLEN 10                     /* length of a string */
#define LSTRLEN 64                    /* length of a string */
#define MAXLIN  2100                   /* maximum size of column line */
#define UND 3.2767E25                   /* define Bill's NaN */
#define LUND 3.2E25                   /* lower limit NaN */
#define UUND 3.3E25                   /* upper limit NaN */


enum
{
	LOGICAL,      /* types of data */
	REAL,
	INTEGER,
	STRING
};



typedef union object  /* data that the FITS file can have */
{
	int ival;
	double dval;
	char sval[FSTRLEN];
	int lval;
} 
OBJECT;


typedef struct entre  /* Entry in FITS file */
{
	char name[KEYLEN];      /* name of entry */
	OBJECT value;           /* value of entry */
	int type;               /* type of entry */
} 
ENTRE;

typedef struct fits   /* FITS file header */
{
	int nrows;              /* number of rows in table */
	char filename[64];      /* name of file */
	ENTRE *value;           /* pointer to table */
} 
FITS;

typedef union {
	double d;
	char *s;
} MidasElem;

enum {
	midasDouble, midasString
};

typedef struct {
	char name[64];
	short type;
	long namesum;
} MidasColumn;

typedef struct {
	char fname[64];
	int numrows, maxrows;
	int numcols, maxcols;
	MidasColumn *cols;
	MidasElem **rows;
} MIDAS;


typedef struct info
{
	char fname[64];               /* name of table file */
	int  row;                     /* current row at in file*/
	int  col;                     /* current col at in file*/
}
INFO;


typedef struct memsize
{
	int    nrows;
	int    ncols;
	int    lastrow;
	int    lastcol;
}
MEMSIZE;


#endif
