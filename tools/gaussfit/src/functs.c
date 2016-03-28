
#include <errno.h>
#include <stdio.h>
#include <math.h>
#include "array.h"
#include "datum.h"
#include "simpledefs.h"
#include "protoplasm.h"
#include "opcodes.p"
#include "machine.h"
#include "alloc.h"


extern FILE *outfile;

#define SingleCross(u, v, val)													\
	NextOf(rnode) = rnext = GetFreeRecord(nodes); /* allocate node */			\
	newnode = datumptr + rnext;													\
	*newnode = mtNode2;															\
	newnode->deriv[0] = datumptr[u].deriv[0];     /* copy 1st var */			\
	newnode->deriv[1] = datumptr[v].deriv[0];     /* copy 2nd var */			\
	newnode->value = val;   /* set expression value */							\
	rnode = rnext;			/* link node in chain */							\
/* end of macro SingleCross */

static double npow(double, long);

long
DxSquared(in1)
	long in1;
{
	long out1, rnode, rnext, unode, unode0;
	short type;
	double uval;
	DatumPtr newnode;
	
	unode0 = datumptr[in1].next;
	
	out1 = rnode = NewNode(0);			/* create 0th order term */
	datumptr[rnode].value = 0.0;
	unode = unode0;
	/* scan through list */
	while (unode0 > -1 && datumptr[unode0].order == 1) {
		type = VarTypeOf(unode0, 0);
		if (type == ParameterType   && doParams
			|| type == ObservationType && doObservations) {
			/* compute squared term */
			SingleCross(unode0, unode0, datumptr[unode0].value * datumptr[unode0].value);
			unode = datumptr[unode0].next;
			/* compute cross terms */
			while (unode > -1 && datumptr[unode].order == 1) {
				type = VarTypeOf(unode, 0);
				if (type == ParameterType   && doParams
					|| type == ObservationType && doObservations) {
					SingleCross(unode, unode, 
						2.0 * datumptr[unode].value * datumptr[unode].value);
				}
				unode = datumptr[unode].next;
			}
		}
		unode0 = datumptr[unode0].next;
	}
	return out1;
}

void
SqrtFn() {
	long list1, list2, unode;
	double uval;
	double coef1, coef2, coef3;

	reg[regn++] = list1 = unode = popval();  
	trace4list(list1);
	uval = datumptr[unode].value;
	if (uval < 0.0) {
		fatalerror("Square root of negative number attempted in model\n","");
	}
	
	if (do2ndDerivs) list2 = DxSquared(list1);
	
	/* 0th order term */
	datumptr[unode].value = coef1 = sqrt(uval);
	unode = datumptr[unode].next;
	
	/* first order terms */
	/*   1/2 * u^(-1/2) * du/dx   */
	coef2 = 0.5 / coef1;
	while (unode > -1) {
		datumptr[unode].value *= coef2;
		unode = datumptr[unode].next;
	}
	
	/* second order terms */
	if (do2ndDerivs) {
		unode = datumptr[list2].next;
		/* -1/4 * u^(-3/2) * du/dx   */
		coef3 = -0.125 / (uval * coef1);
		while (unode > -1) {
			datumptr[unode].value *= coef3;
			unode = datumptr[unode].next;
		}
		list1 = MergeNodeLists(list1, list2);
	}
	pushval(list1);
}

void
SinFn() {
	long list1, list2, unode;
	double uval;
	double coef1, coef2, coef3;
	
	reg[regn++] = list1 = unode = popval();
	trace4list(list1);
	
	uval = datumptr[unode].value;

	if (do2ndDerivs) list2 = DxSquared(list1);
	
	/* 0th order term */
	datumptr[unode].value = coef1 = sin(uval);
	unode = datumptr[unode].next;
	
	/* first order terms */
	/*   cos(u) * du/dx   */
	coef2 = cos(uval);
	while (unode > -1) {
		datumptr[unode].value *= coef2;
		unode = datumptr[unode].next;
	}
	
	/* second order terms */
	if (do2ndDerivs) {
		unode = datumptr[list2].next;
		/*   -sin(u) * du/dx   */
		coef3 = -coef1 * 0.5;
		while (unode > -1) {
			datumptr[unode].value *= coef3;
			unode = datumptr[unode].next;
		}
		list1 = MergeNodeLists(list1, list2);
	}
	pushval(list1);
}

void
CosFn() {
	long list1, list2, unode;
	double uval;
	double coef1, coef2, coef3;
	
	reg[regn++] = list1 = unode = popval();  
	trace4list(list1);
	uval = datumptr[unode].value;

	if (do2ndDerivs) list2 = DxSquared(list1);
	
	/* 0th order term */
	datumptr[unode].value = coef1 = cos(uval);
	unode = datumptr[unode].next;
	
	/* first order terms */
	/*   -sin(u) * du/dx   */
	coef2 = -sin(uval);
	while (unode > -1) {
		datumptr[unode].value *= coef2;
		unode = datumptr[unode].next;
	}
	
	/* second order terms */
	if (do2ndDerivs) {
		unode = datumptr[list2].next;
		/*   -cos(u) * du/dx   */
		coef3 = -coef1 * 0.5;
		while (unode > -1) {
			datumptr[unode].value *= coef3;
			unode = datumptr[unode].next;
		}
		list1 = MergeNodeLists(list1, list2);
	}
	pushval(list1);
}

void
LogFn() {
	long list1, list2, unode;
	double uval;
	double coef1, coef2, coef3;
	
	reg[regn++] = list1 = unode = popval();  
	trace4list(list1);
	uval = datumptr[unode].value;
	if (uval < 0.0) {
		fatalerror("Natural log of negative number attempted in model\n","");
	}

	if (do2ndDerivs) list2 = DxSquared(list1);
	
	/* 0th order term */
	datumptr[unode].value = coef1 = log(uval);
	unode = datumptr[unode].next;
	
	/* first order terms */
	/*   1/u * du/dx   */
	coef2 = 1.0/uval;
	while (unode > -1) {
		datumptr[unode].value *= coef2;
		unode = datumptr[unode].next;
	}
	
	/* second order terms */
	if (do2ndDerivs) {
		unode = datumptr[list2].next;
		/*   -1/u^2 * du/dx   */
		coef3 = - coef2 * coef2 * 0.5;
		while (unode > -1) {
			datumptr[unode].value *= coef3;
			unode = datumptr[unode].next;
		}
		list1 = MergeNodeLists(list1, list2);
	}
	pushval(list1);
}

void
Log10Fn() {
	long list1, list2, unode;
	double uval;
	double coef1, coef2, coef3;
	
	reg[regn++] = list1 = unode = popval();  
	trace4list(list1);
	uval = datumptr[unode].value;
	if (uval < 0.0) {
		fatalerror("Base 10 log of negative number attempted in model\n","");
	}

	if (do2ndDerivs) list2 = DxSquared(list1);
	
	/* 0th order term */
	coef1 = 1.0 / log(10.0);
	datumptr[unode].value = log(uval) * coef1;
	unode = datumptr[unode].next;
	
	/* first order terms */
	/*   1/(ln(10)*u) * du/dx   */
	coef2 = coef1/uval;
	while (unode > -1) {
		datumptr[unode].value *= coef2;
		unode = datumptr[unode].next;
	}
	
	/* second order terms */
	if (do2ndDerivs) {
		unode = datumptr[list2].next;
		/*   -1/(ln(10)*u^2) * du/dx   */
		coef3 = - coef1 / (uval * uval) * 0.5;
		while (unode > -1) {
			datumptr[unode].value *= coef3;
			unode = datumptr[unode].next;
		}
		list1 = MergeNodeLists(list1, list2);
	}
	pushval(list1);
}

void
ExpFn() {
	long list1, list2, unode;
	double uval;
	double coef1, coef2, coef3;
	
	reg[regn++] = list1 = unode = popval();  
	trace4list(list1);
	uval = datumptr[unode].value;

	if (do2ndDerivs) list2 = DxSquared(list1);
	
	/* 0th order term */
	datumptr[unode].value = coef1 = exp(uval);
	unode = datumptr[unode].next;
	
	/* first order terms */
	/*   e^u * du/dx   */
	while (unode > -1) {
		datumptr[unode].value *= coef1;
		unode = datumptr[unode].next;
	}
	
	/* second order terms */
	if (do2ndDerivs) {
		unode = datumptr[list2].next;
		/*   e^u * du/dx   */
		coef3 = coef1 * 0.5;
		while (unode > -1) {
			datumptr[unode].value *= coef3;
			unode = datumptr[unode].next;
		}
		list1 = MergeNodeLists(list1, list2);
	}
	pushval(list1);
}


void
TanFn() {
	long list1, list2, unode;
	double uval;
	double coef1, coef2, coef3;
	
	reg[regn++] = list1 = unode = popval();  
	trace4list(list1);
	uval = datumptr[unode].value;

	if (do2ndDerivs) list2 = DxSquared(list1);
	
	/* 0th order term */
	datumptr[unode].value = coef1 = tan(uval);
	if (errno == ERANGE) {
		fatalerror("Derivative of tangent((2n+1)pi) is infinite","");
	}
	unode = datumptr[unode].next;
	
	/* first order terms */
	/*   (1 + tan(u)^2) * du/dx   */
	coef2 = coef1 * coef1;
	coef3 = 1.0 + coef2;
	while (unode > -1) {
		datumptr[unode].value *= coef3;
		unode = datumptr[unode].next;
	}
	
	/* second order terms */
	if (do2ndDerivs) {
		unode = datumptr[list2].next;
		/*   2tan(u)*(1 + tan(u)^2) * du/dx   */
		coef3 = coef1 * coef3;
		while (unode > -1) {
			datumptr[unode].value *= coef3;
			unode = datumptr[unode].next;
		}
		list1 = MergeNodeLists(list1, list2);
	}
	pushval(list1);
}

void
ArcSinFn() {
	long list1, list2, unode;
	double uval;
	double coef1, coef2, coef3;
	
	reg[regn++] = list1 = unode = popval();  
	trace4list(list1);
	uval = datumptr[unode].value;
	if (fabs(uval) >= 1.0) {
		fatalerror("Arcsin argument domain error.","");
	}

	if (do2ndDerivs) list2 = DxSquared(list1);
	
	/* 0th order term */
	datumptr[unode].value = asin(uval);
	unode = datumptr[unode].next;
	
	/* first order terms */
	/*   1/sqrt(1 - u^2) * du/dx   */
	coef2 = 1.0 / sqrt(1.0 - uval * uval);
	while (unode > -1) {
		datumptr[unode].value *= coef2;
		unode = datumptr[unode].next;
	}
	
	/* second order terms */
	if (do2ndDerivs) {
		unode = datumptr[list2].next;
		/*   u/(1 - u^2)^(3/2) * du/dx   */
		coef3 = uval * coef2 * coef2 * coef2 * 0.5;
		while (unode > -1) {
			datumptr[unode].value *= coef3;
			unode = datumptr[unode].next;
		}
		list1 = MergeNodeLists(list1, list2);
	}
	pushval(list1);
}

void
ArcCosFn() {
	long list1, list2, unode;
	double uval;
	double coef1, coef2, coef3;
	
	reg[regn++] = list1 = unode = popval();  
	trace4list(list1);
	uval = datumptr[unode].value;
	if (fabs(uval) >= 1.0) {
		fatalerror("Arccos argument domain error.","");
	}

	if (do2ndDerivs) list2 = DxSquared(list1);
	
	/* 0th order term */
	datumptr[unode].value = acos(uval);
	unode = datumptr[unode].next;
	
	/* first order terms */
	/*   -1 / sqrt(1 - u^2)   */
	coef2 = -1.0 / sqrt(1.0 - uval * uval);
	while (unode > -1) {
		datumptr[unode].value *= coef2;
		unode = datumptr[unode].next;
	}
	
	/* second order terms */
	if (do2ndDerivs) {
		unode = datumptr[list2].next;
		/*   u/(1 - u^2)^(3/2) * du/dx   */
		coef3 = uval * coef2 * coef2 * coef2 * 0.5;
		while (unode > -1) {
			datumptr[unode].value *= coef3;
			unode = datumptr[unode].next;
		}
		list1 = MergeNodeLists(list1, list2);
	}
	pushval(list1);
}


void
ArcTanFn() {
	long list1, list2, unode;
	double uval;
	double coef1, coef2, coef3;
	
	reg[regn++] = list1 = unode = popval();  
	trace4list(list1);
	uval = datumptr[unode].value;

	if (do2ndDerivs) list2 = DxSquared(list1);
	
	/* 0th order term */
	datumptr[unode].value = atan(uval);
	unode = datumptr[unode].next;
	
	/* first order terms */
	/*   1/(1 + u^2) * du/dx   */
	coef2 = 1.0 / (1.0 + uval * uval);
	while (unode > -1) {
		datumptr[unode].value *= coef2;
		unode = datumptr[unode].next;
	}
	
	/* second order terms */
	if (do2ndDerivs) {
		unode = datumptr[list2].next;
		/*   -2u/(1 + u^2)^2 * du/dx   */
		coef3 = - uval * coef2 * coef2 ;
		while (unode > -1) {
			datumptr[unode].value *= coef3;
			unode = datumptr[unode].next;
		}
		list1 = MergeNodeLists(list1, list2);
	}
	pushval(list1);
}


void
AbsFn() {
	long list1, list2, unode, prev;
	double uval;
	double coef1;
	
	reg[regn++] = list1 = unode = popval();  
	trace4list(list1);
	uval = datumptr[unode].value;
	/*
	if (uval == 0.0) {
		fatalerror("Derivative of abs(0) undefined\n","");
	}
	*/
	
	if (do2ndDerivs) list2 = DxSquared(list1);
	
	/* for efficiency terms should be removed from list rather than zeroed */
	/* 0th order term */
	datumptr[unode].value = fabs(uval);
	unode = datumptr[unode].next;
	
	/* first order terms */
	if (uval < 0.0) {
		while (unode > -1) {
			datumptr[unode].value = -datumptr[unode].value;
			unode = datumptr[unode].next;
		}
	} else if (uval > 0.0) {
		while (unode > -1) {
			unode = datumptr[unode].next;
		}
	} else {
		while (unode > -1) {
			datumptr[unode].value = 0.0;
			unode = datumptr[unode].next;
		}
	}
	
	/* second order terms */
	if (do2ndDerivs) {
		unode = datumptr[list2].next;
		while (unode > -1) {
			datumptr[unode].value = 0.0;
			unode = datumptr[unode].next;
		}
		list1 = MergeNodeLists(list1, list2);
	}
	pushval(list1);
}

static double npow(x,n)   /* raise (double)x to (int)n */
double x;
long n;
{
	double y;
	if (n < 0) {        /* raise to negative power */
		if (x == 0.0)   /* error of x = 0.0 */
			fatalerror("Attempt to raise 0 to negative power in model\n","");
		x = 1.0/x;  /* reciprocal of x */
		n = -n;     /* change sign of n */
	}         
	                                
	/* binary decomposition of n gives efficient exponenetiation */
	y = 1.0;         /* X 0 = 1.0 */
	while(n > 0) {   /* if n > 0 */
		if (n & 0x0001)   /* if n is odd */
			y *= x;  /* multiply y by x */
		x *= x;                  /* square x */
		n >>= 1;                 /* divide n by 2 */
	}
	return y;
}

void
Power() {
	long list1, list2, list3, unode;
	double nval, uval;
	double pwr;
	long n;
	
	reg[regn++] = list3 = popval();   /* pop two items on top of stack */
	reg[regn++] = list1 = unode = popval();  
	trace4list(list1);
	trace4list(list3);
	n = nval = datumptr[list3].value;
	if (n != nval) {
		fatalerror("Fractional powers not allowed in model\n","");
	}
	uval = datumptr[unode].value;
	
	if (do2ndDerivs) list2 = DxSquared(list1);
	
	/* 0th order term */
	pwr = npow(uval, n);
	datumptr[unode].value = pwr;
	unode = datumptr[unode].next;
	
	/* first order terms */
	pwr = nval * npow(uval, n-1);
	while (unode > -1) {
		datumptr[unode].value *= pwr;
		unode = datumptr[unode].next;
	}
	
	/* second order terms */
	if (do2ndDerivs) {
		unode = datumptr[list2].next;
		pwr = 0.5 * (nval*nval - nval) * npow(uval, n-2);
		while (unode > -1) {
			datumptr[unode].value *= pwr;
			unode = datumptr[unode].next;
		}
		list1 = MergeNodeLists(list1, list2);
	}
	FreeNodeList(list3);
	pushval(list1);
}
	


