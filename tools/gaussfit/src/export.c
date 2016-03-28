/*
	GaussFit - A System for Least Squares and Robust Estimation

	Source Code Copyright (C) 1987 by William H. Jefferys,
	Michael J. Fitzpatrick and Barbara E. McArthur
	All Rights Reserved.
*/



#include <stdio.h>
#include "defines.h"
#include "house.h"
#include "array.h"
#include "datum.h"
#include "simpledefs.h"
#include "files.h"
#include "protoplasm.h"
#include "symboltable.h"
#include "alloc.h"


extern int tracelevel;    /* trace if this falg is on */
extern FILE *fp;         /* result file */

long exportstack[MATSIZE];  /* stack to save values pending export */
int exportptr=0;  /* export stack pointer */

saveexport(list)  /* save item on export stack */
long list;
{	
	if(exportptr < MATSIZE)  /* see if there is room */
	{
		exportstack[exportptr] = list; /* yew, put on stack and increment pointer */
		exportptr++;
		return 0;
	}
	else
		fatalerror("Export Stack Overflow\n",""); /* stack overflow */
}

unsaveexport()  /* drop everything from export stack */
{
	int i;
	for (i=0; i<exportptr; ++i) {
		FreeNodeList(exportstack[i]);
	}
	exportptr = 0;
}


doexport()   /* export pending equations of condition */
{
	long in1;
	if(exportptr>0) { /* only export if there are pending equations */
		exportn(exportptr,exportstack); /* export all items in export stack */
		unsaveexport(); /* discard items from stack */
		in1 = popval();  /* pop top of stack */
		FreeNodeList(in1);
	}
}

export()  /* export equation of condition found on top of stack */
{
	long in1;
	in1 = popval();
	saveexport(in1);
	PushTrue(); /* push true as result of export */
}

export2() /* export top two equations of condition. To be DISCARDED eventually */
{
	long in1, in2;
	in1 = popval();
	in2 = popval();
	saveexport(in1);
	saveexport(in2);
	PushTrue();
}

exportn(m,k) /* actually send pending equations of condition to least squares routine */
	int m;
	long k[MATSIZE];
{
	long list;
	double rhs[MATSIZE];
	double wt[MATSIZE][MATSIZE];
	int i,j;
	short iz[5];

	iz[0] = iz[1] = iz[2] = iz[3] = iz[4] = 0;
	pushcopy(k[0]); /* push something on top of stack (to be popped later) */
	computewt(m,k,rhs,wt); /* compute weight matrix and right hand side vector*/
	for(i=0;i<m;i++) { /* add m equations of condition to matrix */
		SumInit(Condition);/* initialize new row of matrix as condition */
		for(j=0;j<=i;j++) { /* combine first i equations with weight matrix */
			/* weight righhand side and export it */
			SumAdd(RightHandType,"_RHS", iz, wt[i][j]*rhs[j]);
			list = k[j];  /* get unweighted equation of condition */
			while((list = NextOf(list))!=-1 && OrderOf(list)==1) { /* do for each derivative in equation */
				char *str;
				int type;

				str = NameOf(SymbolOf(list, 0)); /* get the name of the variable */
				if (VarTypeOf(list, 0) == ParameterType) { 
					if (IndexedOf(list, 0)) {
						SumAdd(ParameterType, str, IndexOf(list, 0),
							wt[i][j] * ValueOf(list)); 
					} else {
						SumAdd(ParameterType, str, iz, wt[i][j] * ValueOf(list));
					}
				}
			}
		}
	}
}

exportconstraint() { /* export a constraint */
	SumInit(Constraint); /* initialize matrix to receive a constraint */
	sumconstraint(); /* add in the constraint */
}

sumconstraint()
{
	long list;
	short iz[5];

	iz[0] = iz[1] = iz[2] = iz[3] = iz[4] = 0;
	list = popval(); /* pop top of stack */
	pushval(list);	 /* push a result onto stack */
	SumAdd(RightHandType,"_RHS",iz,-ValueOf(list));/* add right hand side to matrix */
	while((list = NextOf(list))!=-1 && OrderOf(list)==1) { /* add each derivative term to matrix */
		char *str;

		str = NameOf(SymbolOf(list, 0)); /* get name of parameter */
		if (VarTypeOf(list, 0) == ParameterType) { 
			if (IndexedOf(list, 0)) {
				SumAdd(ParameterType, str, IndexOf(list, 0), ValueOf(list));
			} else {
				SumAdd(ParameterType, str, iz, ValueOf(list)); /* add into matrix */
			}
		}
	}
}
