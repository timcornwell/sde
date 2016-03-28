/*

	GaussFit - A System for Least Squares and Robust Estimation
	Source Code Copyright (C) 1987 by William H. Jefferys,
	Michael J. Fitzpatrick and Barbara E. McArthur
	All Rights Reserved.
*/
/* program Householder    */

/* Header file for Householder transformations */

#ifndef HOUSE_H
#define HOUSE_H

#ifndef sgi
#define trunc(x)          ((int)(x))     /* integer part of x */
#endif

#define DBL_DIG    18	/* fractional significance IEEE standard */	
#define deltafn(i,j) (((i)==(j))?1:0)        /* Kronedcker delta */
#define sqr(x)            ((x)*(x))      /* square of x */
#define minval(x,y)       (((x)<=(y))?(x):(y)) /* minimum of 2 arguments */
#define maxval(x,y)       (((x)>=(y))?(x):(y)) /* maximum of 2 arguments */
#define RoundToZero(x,y)  ((fabs(x)<(y))?(0.0):(x))
/* 
	Rounds a number that has lost 
	significance  (i.e., is <y ) to zero
*/


typedef struct key
{
	char name[64];              /* name of column indexed  */
	int  col;                   /* col where info resides */
	int  type;                 /*  type of var */
} KEY;

#define MATSIZE 4			/* Max order of matrices for Weights */

typedef double MATRIX[MATSIZE][MATSIZE];   /* MATRIX typedef */
typedef double VECTOR[MATSIZE];            /* VECTOR typedef */

#endif
