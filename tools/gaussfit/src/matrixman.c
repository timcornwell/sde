/*
	GaussFit - A System for Least Squares and Robust Estimation

	Source Code Copyright (C) 1987 by William H. Jefferys,
	Michael J. Fitzpatrick and Barbara E. McArthur
	All Rights Reserved.
*/

/* Matrix operations to calculate weights and phi vector */

#include <stdio.h>
#include <math.h>
#include "defines.h"
#include "array.h"
#include "datum.h"
#include "simpledefs.h"
#include "protoplasm.h"
#include "symboltable.h"
#include "house.h"
#include "files.h"
#include "robust.h"
#include "alloc.h"

int maxdusiz[MATSIZE] = {0,0,0,0};
int curdusiz[MATSIZE] = {0,0,0,0};
/*
int maxdusiz[MATSIZE] = {0,0,0,0,0,0,0,0,0,0};
int curdusiz[MATSIZE] = {0,0,0,0,0,0,0,0,0,0};
*/

double *meandu[MATSIZE];
int prban = 1;
float *prevdu[MATSIZE];
char  *duwobble[MATSIZE];
char  *wobbleon[MATSIZE];
extern double envhuber, envtukey, envfair, envtrim;
extern int envminsum, envirls, envorm, envprec;

extern FILE *fp;
extern double tolerance;           /* number of residuals */


extern int iterno;
static int var_zero = 0;
static int var_nonzero = 0;

matxpose(A,B,p,q)   /* transpose p x q matrix A into B */
MATRIX A,B;
int p;
int q;
{
	int i,j;

	for(i=0;i<p;i++)
		for(j=0;j<q;j++)
			B[j][i] = A[i][j];
}

matcopy(A,B,p,q)  /* copy p x q matrix A into B */
MATRIX A,B;
int p;
int q;
{
	int i,j;

	for(i=0;i<p;i++)
		for(j=0;j<q;j++)
			B[i][j] = A[i][j];
}

matadd(A,B,C,p,q)  /* copy p x q matrix A into B */
MATRIX A,B,C;
int p;
int q;
{
	int i,j;

	for(i=0;i<p;i++)
		for(j=0;j<q;j++)
			C[i][j] = A[i][j] + B[i][j];
}

matsub(A,B,C,p,q)  /* copy p x q matrix A into B */
MATRIX A,B,C;
int p;
int q;
{
	int i,j;

	for(i=0;i<p;i++)
		for(j=0;j<q;j++)
			C[i][j] = A[i][j] - B[i][j];
}

matmpy(A,B,C,p,r,q)     /* multiply p x q matrix A by z x r matrix B into C */
MATRIX A,B,C;
int p;
int q;
int r;
{
	int i,j,k;
	double sum;

	for(i=0;i<p;i++)
		for(j=0;j<q;j++)
		{
			sum = 0.0;
			for(k=0;k<r;k++)
				sum += A[i][k]*B[k][j];
			C[i][j] = sum;
		}
}

matmpyd(A,B,C,p,r,q)     /* multiply, and get only diagonal terms */
MATRIX A,B,C;
int p;
int q;
int r;
{
	int i,j,k;
	double sum;

	p = p<q ? p : q;
	for(i=0;i<p;i++)
		{
			sum = 0.0;
			for(k=0;k<r;k++)
				sum += A[i][k]*B[k][i];
			C[i][i] = sum;
		}
}

matvecmpy(A,B,C,p,q)  /* multiply p x q matrix A by q-vector B into vector C */
MATRIX A;
VECTOR B,C;
int p;
int q;                              
{
	int i,j;
	double sum;

	for(i=0;i<p;i++)
	{
		sum = 0.0;
		for(j=0;j<q;j++)
			sum += A[i][j]*B[j];
		C[i] = sum;
	}
}

cholesky(A,p)	/* Calculates Upper Triangular Cholesky Factor of symmetric
                   p x p matrix A, Result to A */
MATRIX A;
int p;
{
	int i,j,k;
	double sum,beta;

	if(p>MATSIZE)          /* max size is MATSIZE */
		fatalerror("Matrix Square Root, Order too high\n","");
	if(A[0][0] <= 0.0)     /* can't do it if diagonal element is <= 0 */
		fatalerror("Matrix Square Root Error 0\n","");
	A[0][0] = sqrt(A[0][0]);  /* 1 x 1 is just square root */
	for(i=1;i<p;i++)  /* generate i x i from (i-1)x(i-1) */
	{
		beta = A[i][i];   /* pivot */
		for(j=0;j<i;j++)
		{
			sum = A[j][i];
			A[i][j] = 0.0;
			for(k=0;k<j;k++)
			{
				sum -= A[k][j]*A[k][i];
			}
			if(A[j][j] <= 0.0)
				fatalerror("Cholesky: Matrix not pos def\n","");
			A[j][i] = sum = sum/A[j][j];
			beta -= sqr(sum);
		}
		if(beta < 0.0)
			fatalerror("Cholesky: Negative diagonal term\n","");
		A[i][i] = sqrt(beta);
	}
}


mat_inv(A,p) /* Inverse of matrix */
MATRIX A;
int p;
{
	MATRIX L,U;

	matcopy(A,U,p,p);
	cholesky(U,p);
	ut_inv(U,p);
	matxpose(U,L,p,p);
	matmpy(U,L,A,p,p,p);
}



ut_inv(A,p) /* Inverse of Upper-triangularp x p Matrix A */
MATRIX A;
int p;
{
	int i,j,k;
	double sum;

	if(p>MATSIZE)
		fatalerror("Matrix Inverses Order Too Great\n","");
	for(i=p-1;i>=0;i--)
	{
		for(j=i;j>=0;j--)
		{
			sum = deltafn(i,j);  /* right hand side is unti matrix */
			for(k=j+1;k<p;k++) /* back substitute */
				sum -= A[j][k]*A[k][i];
			if(A[j][j] == 0.0)   /* if point is zero, error */
				fatalerror("Weight Matrix is Singular!\n","");
			A[j][i] = sum/A[j][j];  /* compute inverse */
		}
	}
}


matprint(A,p,q)  /* print out a p x q matrix A (for debug) */
MATRIX A;
int p,q;
{
	int i,j;

	xprintf(stdout,"\n");
	for(i=0;i<p;i++)
	{
		for(j=0;j<q;j++)
			xprintf(stdout,"%lf ",A[i][j]);
		xprintf(stdout,"\n");
	}
	xprintf(stdout,"\n");
}

matfprint(A,p,q)  /* print out result file a p x q matrix A */
MATRIX A;
int p,q;
{
	int i,j;

	for(i=0;i<p;i++)
	{
		for(j=0;j<q;j++)
			xprintf(fp,"%lf ",A[i][j]);
		xprintf(fp,"\n");
	}
	xprintf(fp,"\n");
}

search(n, names, str)  /* look for an observational datum name */
	char *str;
	char *names[MATSIZE];
	int *n;
{
	int i;

	for(i=0;i<*n;i++) { /* look through what currently exists */
		if(str==names[i])   /* if found, return the index */
			return i;
	}
	if(*n >= MATSIZE)  /* if not found and no more room */
		fatalerror("No More Room In Matrix Name Table\n","");/*fatal error */
	names[*n] = str;  /* add name to table */
	return (*n)++; /* and return its value */
}

covarname(x,y,name)  /* concatenate name(x) and name(y) with underscores */
char *x, *y, *name;
{
	strcpy(name,x);  /* result is 8 characters long */
	strcat(name,"_");
	strcat(name,y);
}


double getsigma(x,y)
char *x, *y;
{
	char name[64];
	double cov;


	covarname(x,y,name);  /* make the name for the covariance */
	if((cov = getdataval(name)) != 0.0)  /* look for it in datafile */
	{

		if (x == y)
			var_nonzero = 1;
		return cov;  /* if nonzero, return it */
	}
	else
	{
		if (x == y)
			var_zero = 1;
	}
	if(x!=y)  /* if not found, if x!= y then */
	{
		covarname(y,x,name); /* its a covariance, look for it with
		                                        names in reverse order */
		if((cov = getdataval(name)) != 0.0);
			return cov;
	}
	if ((iterno == 0) && (prban == 1))
	{
		banner();
		prban++;
	}

	return 1.0;  /* otherwise its a variance, return 1.0 */
}

banner()
{
	xprintf(fp,"\n***********************************");
	xprintf(fp,"***********************************\n");
	xprintf(fp,"\nNo value given for variance in data table.\n");
	xprintf(fp,"Variance assumed equal to 1.0.\n");
	xprintf(fp,"Covariance assumed equal to 0.0.\n");
	xprintf(fp,"\n***********************************");
	xprintf(fp,"***********************************\n\n");
	xprintf(stdout, "\n***********************************");
	xprintf(stdout, "***********************************\n");
	xprintf(stdout, "\nNo value given for variance in data table.\n");
	xprintf(stdout, "Variance assumed equal to 1.0.\n");
	xprintf(stdout, "Covariance assumed equal to 0.0.\n");
	xprintf(stdout, "\n***********************************");
	xprintf(stdout, "***********************************\n\n");
}

int wobblefixon = 0;

computewt(m,k,rhs,L) /* compute weight and phi vector */
	int m;    /* number of conditional equations */
	long k[MATSIZE];  /* pointer to equations of condition */
	VECTOR rhs;   /* phi vector */
	MATRIX L;   /* weight matrix */
{
	int i,j,h;
	MATRIX sigma,fx,sigfxt,U,W,fxt,mx,nx;
	VECTOR uhat,vhat,temp1,temp2,psizero,duhat,dvhat;
	MATRIX Q,Qt,R,Rt,D,A,At,B,Bt,C,Ct;
	char *names[MATSIZE];
	int n;
	long list;
	extern double SumRho, SumPsi, SumPsiP, SumPsiSq, SumPsiPSq;
	extern double DeltaV, ScaleFac;
	double trace[MATSIZE];
	double fxx[MATSIZE][MATSIZE][MATSIZE];
	
	n = 0;
	for(i=0; i<m; i++) { /* do for all eqs of condition in this batch */
		for(j=0; j<MATSIZE; j++) fx[i][j] = 0.0; /* zero out fx matrix row */
		list = k[i];  /* get equation of condition from vector */
		rhs[i] = - ValueOf(list); /* get the righthand side */
		while((list = NextOf(list))!=-1 && OrderOf(list)==1) { /* look at derivative term */
			if(VarTypeOf(list, 0) == ObservationType)  {
				/* get the position of the variable in the matrix */
				j = search(&n, names, NameOf(SymbolOf(list, 0))); 
				fx[i][j] = ValueOf(list); /* enter derivative into fx matrix */
			}
		}
		if (do2ndDerivs) {
			/* zero out fxx matrix row */
			for(j=0; j<MATSIZE; j++) for(h=0; h<MATSIZE; h++) fxx[i][j][h] = 0.0;
			while(list != -1 && OrderOf(list)==2) { /* look at derivative term */
				if(VarTypeOf(list, 0) == ObservationType)  {
					/* get the position of the variable in the matrix */
					j = search(&n, names, NameOf(SymbolOf(list, 0))); 
					h = search(&n, names, NameOf(SymbolOf(list, 1))); 
					fxx[i][j][h] = ValueOf(list); /* enter derivative into fx matrix */
				}
				list = NextOf(list);
			}
		}
	}
	for(i=0;i<n;i++) {			/* Get Sigma */
		vhat[i] = getresidual(names[i]); /* get residual */
		for(j=i;j<n;j++) {
			/* fill covariance matrix */
			sigma[j][i] = sigma[i][j] = sqr(ScaleFac == 0.0 ? 1.0 : ScaleFac)
				* getsigma(names[i],names[j]);  /* use names entered into name vector */
		}
	}
	matcopy(sigma,Rt,n,n);
	cholesky(Rt,n);  /* square root of covariance matrix */
	matxpose(Rt,R,n,n);  /* square root of covariance matrix */
	matcopy(Rt,Qt,n,n);  /* sigma (-0.5) */
	ut_inv(Qt,n);
	matxpose(Qt,Q,n,n);
	matvecmpy(Q,vhat,uhat,n,n);   /* normalized residual vector */
	for(i=0;i<n;i++) {
		double temp;
		double M,N;

		N = n;
		M = m; /* M = m ? 1? */
		if (envminsum) {
			insertresidual(uhat[i]); 
		} 
		SumRho += rhofn(uhat,i,n)*M/N; /* compute rho(u) */
		psizero[i] = temp = psifn(uhat,i,n);  /* compute psr(u) */
		SumPsi += temp*M/N;  /* add into sum */
		SumPsiSq += sqr(temp);  /* add into sum squared */
		temp = psipfn(uhat,i,i,n);  /* psi'(u) */
		SumPsiP += temp*M/N;  /* add into sum */
		SumPsiPSq += sqr(temp)*M/N;  /* add into sum squared */

		for(j=0;j<n;j++)   /* compute D matrix */	
		if(envirls) {  /* value depend on IRLS or Newton's method */
			if(i == j) {
				D[i][j] = Weightfn(uhat,i,n);
			} else {
				D[i][j] = 0.0;
			}
		}
		else
			D[i][j] = psipfn(uhat,i,j,n);
	}
	mat_inv(D,n);
	matmpy(fx,R,A,m,n,n);/* compute product of matrices: A = f(x)*signam 0.5 */
	matxpose(A,At,m,n);
	matmpy(A,D,C,m,n,n);/* C = A . D */
	matxpose(C,Ct,m,n);  
	matmpy(C,At,U,m,n,m);  /* U = A . D . A' */
	cholesky(U,m);	       /* (ADA')  0.5 */
	ut_inv(U,m);           /* (ADA')  (-0.5) */

	matxpose(U,L,m,m);
	matmpy(U,L,W,m,m,m);   /* (ADA')  (-1) */

	if (do2ndDerivs) {
		matxpose(fx,fxt,m,n);
		matmpy(sigma,fxt,mx,n,n,m);
		matmpy(mx,W,nx,n,m,m);
		matmpy(nx,fx,mx,n,m,n);
		matmpy(mx,sigma,nx,n,n,n);
		matsub(sigma,nx,mx,n,n);
		for(i=0; i<m; i++) {
			matmpyd(fxx[i],mx,nx,n,n,n);   
			for (j=0, trace[i] = 0.; j<n; ++j) trace[i] += nx[j][j];
			trace[i] *= .5;
			rhs[i] += trace[i];
		}
	}
	if(!envirls) {
		for(i=0; i<m; i++) {
			for(j=0; j<n; j++)
				rhs[i] += C[i][j] * psizero[j]; /* rhs = ADpsf */
		}
	} else {
		for(i=0; i<m; i++) {
			for(j=0; j<n; j++) rhs[i] += A[i][j] * uhat[j]; 
			/* rhs = A. sigma  (-0.5) . vhat */
		}
	}
	matvecmpy(W,rhs,temp1,m,m);  /* W . rhs */
	matvecmpy(At,temp1,temp2,n,m);  /* A' . W . rhs */
	if (!envirls) {
		for(i=0;i<n;i++) {
			temp2[i] -= psizero[i]; /* subtract psfzero */
		}
	}
	matvecmpy(D,temp2,duhat,n,n);  /* duhat */
	if (!envirls) {
	
#if 0
		/* commented out vis Dick French's problem */
		for(i=0;i<n;i++) {
			double du;

			if((du = fabs(duhat[i])) > 1.0)  /* don't step too far */
				duhat[i] = duhat[i]/du; 
		}
#endif

	} else {
		for(i=0;i<n;i++) {
			duhat[i] = duhat[i] - uhat[i];
		}
	}
	matvecmpy(R,duhat,dvhat,n,n);
	{
		double val;
		for(i=0;i<n;i++) {
			/* compare delta V to largest so far */
			DeltaV = maxval(DeltaV, fabs(duhat[i]) * Weightfn(duhat, i, n));
			if ((wobbleon[i] != NULL) &&(wobbleon[i][curdusiz[i]])) {
				val = vhat[i]+dvhat[i]*.5;
			} else {
				val = vhat[i]+dvhat[i];
			}
			putresidual(names[i], val); /* put residual into datafile */
			addmeandu(i, dvhat[i]);
		}
	}
}

qcompar(a, b)
	double *a, *b;
{
	if (*a>*b) return 1;
	if (*a<*b) return -1;
	return 0;
}

initwobblefix() {
	static int ft = 0;
	int i, j, k;

	if (ft++) {
		for (i=0; i<MATSIZE; ++i) {
			if (curdusiz[i]) {
				for (j=0; j<curdusiz[i]; ++j) {
					if (duwobble[i][j] >= 3) {
						wobbleon[i][j] = 1;
						if (!wobblefixon) {
		xprintf(stdout,      "*** Turning ON oscillation damping of residuals for some observations. ***\n");
		xprintf(fp, "*** Turning ON oscillation damping of residuals for some observations. ***\n");
							wobblefixon = 1;
						}
					}
				}
			}
		}
	}
	outtahere:
	for (i=0; i<MATSIZE; ++i) {
		curdusiz[i] = 0;
	}
}

#define MAXDUGROW 50

freemeandu() {
	short i;
	wobblefixon = 0;
	for (i=0; i<MATSIZE; ++i) {	
		if (maxdusiz[i] != 0) {
			maxdusiz[i] = 0;
			curdusiz[i] = 0;
			if (meandu[i]) {
				freemem("meandu", (char*)meandu[i]);
				freemem("duwobble", (char*)duwobble[i]);
				freemem("wobbleon", (char*)wobbleon[i]);
				freemem("prevdu", (char*)prevdu[i]);
				meandu[i] = NULL;
				duwobble[i] = NULL;
				wobbleon[i] = NULL;
				prevdu[i] = NULL;
			}
		}
	}	
}

addmeandu(i, val) 
	int i;
	double val;
{
	int j,k;
	double ratio;
	if (maxdusiz[i] == 0) {
		maxdusiz[i] = MAXDUGROW;
		meandu[i]   = (double*)MemAlloc("meandu",   (long)maxdusiz[i]*sizeof(double));
		duwobble[i] = (char*)  MemAlloc("duwobble", (long)maxdusiz[i]*sizeof(char));
		wobbleon[i] = (char*)  MemAlloc("wobbleon", (long)maxdusiz[i]*sizeof(char));
		prevdu[i]   = (float*) MemAlloc("prevdu",   (long)maxdusiz[i]*sizeof(float));
		for (k=0; k<maxdusiz[i]; ++k) {
			prevdu[i][k] = 0.;
			duwobble[i][k] = 0;
			wobbleon[i][k] = 0;
		}
	} else if (curdusiz[i] >= maxdusiz[i]) {
		maxdusiz[i] += MAXDUGROW;
		meandu[i]   = (double*)Reallocate("meandu",   (long)maxdusiz[i]*sizeof(double), (char*)meandu[i]);
		duwobble[i] = (char*)  Reallocate("duwobble", (long)maxdusiz[i]*sizeof(char), (char*)duwobble[i]);
		wobbleon[i] = (char*)  Reallocate("wobbleon", (long)maxdusiz[i]*sizeof(char), (char*)wobbleon[i]);
		prevdu[i]   = (float*) Reallocate("prevdu",   (long)maxdusiz[i]*sizeof(float), (char*)prevdu[i]);
		for (k=maxdusiz[i]-MAXDUGROW; k<maxdusiz[i]; ++k) {
			prevdu[i][k] = 0.;
			duwobble[i][k] = 0;
			wobbleon[i][k] = 0;
		}
	}
	meandu[i][curdusiz[i]] = val<0 ? -val : val;
	if (val) {
		ratio = prevdu[i][curdusiz[i]]/val;
		if (ratio < -0.9 && ratio > -1.1) {
			duwobble[i][curdusiz[i]]++;
		/*
		xprintf(stdout, "%1d %3d :  ratio1 = %8.2f  count = %d\n", 
			i, curdusiz[i], ratio, duwobble[i][curdusiz[i]]);
		*/
		} else {
			duwobble[i][curdusiz[i]] = 0;
		}
	} else {
		duwobble[i][curdusiz[i]] = 0;
	}
	prevdu[i][curdusiz[i]] = val;
	curdusiz[i]++;
}

int
getvar_zero()
{
	return var_zero;
}

int
getvar_nonzero()
{
	return var_nonzero;
}
