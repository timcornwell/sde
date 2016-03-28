
#include <stdio.h>
#include <stdlib.h>
#include "array.h"
#include "datum.h"
#include "simpledefs.h"
#include "files.h"
#include "protoplasm.h"
#include "declare.h"
#include "symboltable.h"
#include "opcodes.p"
#include "machine.h"
#include "alloc.h"

extern int tracelevel;

/*

	Valuestack procedures
	
*/

long *valuestack = NULL;
long vstackptr = 0;
long vstacksiz = 0;

double *conststack = NULL;
long cstackptr = 0;
long cstacksiz = 0;



initstacks(void) {
	vstacksiz = 64;
	valuestack = (long*)MemAlloc("valuestack", vstacksiz * sizeof(long));
	vstackptr = 0;
	
	cstacksiz = 32;
	conststack = (double*)MemAlloc("conststack", cstacksiz * sizeof(double));
	cstackptr = 0;
}

freestacks(void) {
	freemem("valuestack",(char*)valuestack);
	freemem("conststack",(char*)conststack);
	valuestack = NULL;
	conststack = NULL;
	cstackptr = 0;
	cstacksiz = 0;
	vstackptr = 0;
	vstacksiz = 0;
}

long
addconst(value)
	double value;
{
	if (cstackptr+1 >= cstacksiz) {	/* if stack overflows, double its size */
		cstacksiz <<= 1;
		conststack = (double*)Reallocate("ConstStack", cstacksiz * sizeof(double), (char*)conststack);
	}
	conststack[cstackptr++] = value;
	return cstackptr - 1;
}

pushval(list)
	long list;
{
	if (vstackptr+1 >= vstacksiz) {	/* if stack overflows, double its size */
		vstacksiz <<= 1;
		valuestack = (long*)Reallocate("ValueStack", vstacksiz * sizeof(long), (char*)valuestack);
	}
	valuestack[vstackptr++] = list;
}

long
pushcopy(list)
	long list;
{
	list = CopyNodeList(list);
	pushval(list);
	return list;
}

long
popval(void) {
	if (vstackptr <= 0) {
		return -1;			/* this had better be an opDrop though .. or else! */
		/*fatalerror("Stack underflow !\n","");*/
	}
	return valuestack[--vstackptr];
}

long
topval(void) {
	if (vstackptr <= 0) return -1;
	else return valuestack[vstackptr-1];
}

void
Swap(void) {
	long o1, o2;
	o1 = reg[regn++] = popval();
	o2 = reg[regn++] = popval();
	trace4list(o1);
	trace4list(o2);
	pushval(o1);
	pushval(o2);
}

void
PushCon(symInx)
	short symInx;
{
	long list, cInx;
	double value;
	
	if (badsym(symInx)) {
		fatalerror("PushCon: Bad symbol index.", "");
	}
	/* use file var as a pointer to a datum */
	cInx = symbolptr[symInx].filevar;
	if (cInx < 0) {	/* has once only conversion been done yet ?? */
		/* convert string to a double */
		sscanf(symbolptr[symInx].name, "%lf", &value);
		cInx = symbolptr[symInx].filevar = addconst(value);
	}
	list = NewNode(0);
	ValueOf(list) = conststack[cInx];
	pushval(list);
}

void
Push(symInx)
	short symInx;
{
	int m, theType;
	short index[5];
	long dclInx, list, next, fvarInx;
	char *theName ;
	
	if (badsym(symInx)) {
		fatalerror("Push: Bad symbol index.", "");
	}
	dclInx = symbolptr[symInx].dclInx;
	list = pushcopy(declareptr[dclInx].list);
	if ((theType = declareptr[dclInx].type) != VariableType) {
		theName = symbolptr[symInx].name;	
		if (declareptr[dclInx].indexed == 0) {
			switch (theType) {
				case ConstantType :
				case ParameterType :
					ValueOf(list) = getparamval(symInx);
					break;
				case DataType :
					ValueOf(list) = getdataval(theName);
					break;
				case ObservationType :
					ValueOf(list) = gettheobs(theName);
					break;
				case EnvironmentType :
					ValueOf(list) = getenvval(theName);
					break;
			}
		} else {
			fvarInx = symbolptr[symInx].filevar;
			index[0] = index[1] = index[2] = index[3] = 0;
			index[4] = filevarptr[fvarInx].xvars[4];
			for (m=0; m<index[4]; ++m) {
				index[m] = getdataint(symbolptr[filevarptr[fvarInx].xvars[m]].name);
				if (index[m] < 0) {
					fatalerror("Indexed variable %s is not currently defined\n",
						theName);
				}
			}
			ValueOf(list) = getxparval(symInx, index);
			if ((next = NextOf(list)) != -1) {
				datumptr[next].deriv[0].index[0] = index[0];
				datumptr[next].deriv[0].index[1] = index[1];
				datumptr[next].deriv[0].index[2] = index[2];
				datumptr[next].deriv[0].index[3] = index[3];
				datumptr[next].deriv[0].index[4] = index[4];
			}
		}
	}
}
	
void
PushVec(symInx)
	short symInx;
{
	long dims, index, subscript;
	long dclInx, list, next, d1;
	int j;
	ArrayBlockPtr a;
	short number[5];
	
	if (badsym(symInx)) {
		fatalerror("PushVec: Bad symbol index.", "");
	}
	dclInx = symbolptr[symInx].dclInx;
	list = declareptr[dclInx].list;
	if (ArrayOf(list) == NULL) {	/* indexed parameter */
		
		number[0] = number[1] = number[2] = number[3] = 0;
		number[4] = filevarptr[symbolptr[symInx].filevar].xvars[4];
		for (j=0; j<number[4]; ++j) {
			reg[regn++] = d1 = popval();
			trace4list(d1);
			number[j] = ValueOf(d1);
			FreeNodeList(d1);
		}
		list = pushcopy(list);
		ValueOf(list) = getxparval(symInx, number);		

		if ((next = NextOf(list)) != -1) {
			datumptr[next].deriv[0].index[0] = number[0];
			datumptr[next].deriv[0].index[1] = number[1];
			datumptr[next].deriv[0].index[2] = number[2];
			datumptr[next].deriv[0].index[3] = number[3];
			datumptr[next].deriv[0].index[4] = number[4];
		}
	} else {	/* indexed variable */
		long aindex;
		
		a = ArrayOf(list);
		dims = a->dim[4];
		
		/* generate 1D index from multiple indices */
		aindex = 0;
		for (j=dims-1; j>=0; --j) {
			reg[regn++] = d1 = popval();
			trace4list(d1);
			subscript = ValueOf(d1);
			if (subscript < 0 || subscript >= a->dim[j]) {
				fatalerror("Array index out of bounds.\n","");
			}
			aindex = aindex * a->dim[j] + subscript;
			FreeNodeList(d1);
		}
		
		/* push it */
		if ((list = ((long*)a->data)[aindex]) != -1) {
			pushcopy(list);
		} else PushFalse();
	}
}

void
Store(symInx)
	short symInx;
{
	long list0, list1;
	long dclInx;
	ArrayBlockPtr a0, a1;
	
	if (badsym(symInx)) {
		fatalerror("Store: Bad symbol index.", "");
	}
	reg[regn++] = list1 = popval();
	trace4list(list1);
	dclInx = symbolptr[symInx].dclInx;
	if (declareptr[dclInx].type != VariableType) {
		fatalerror("Attempt to change non-variable data type.", "");
	}
	list0 = reg[regn++] = declareptr[dclInx].list;
	trace4list(list0);
	a0 = ArrayOf(list0);
	a1 = ArrayOf(list1);
	if (a0 != NULL && a1 == NULL) {
		fatalerror("Attempt to store scalar variable in vector %s.\n",
			symbolptr[symInx].name);
	}	
	
	FreeNodeList(list0);
	declareptr[dclInx].list = list1;
}

void
StoreVec(symInx)
	short symInx;
{
	long list0, list1, list2, subscript, aindex;
	int j;
	ArrayBlockPtr a0, a1;
	long dclInx;
	
	if (badsym(symInx)) {
		fatalerror("StoreVec: Bad symbol index.", "");
	}
	dclInx = symbolptr[symInx].dclInx;
	list0 = declareptr[dclInx].list;
	reg[regn++] = list1 = popval();
	trace4list(list1);
	a0 = ArrayOf(list0);
	a1 = ArrayOf(list1);
	if (a0 == NULL) {
		fatalerror("Attempt to index into scalar %s.\n",
			symbolptr[symInx].name);
	}
	if (a1 != NULL) {
		fatalerror("Attempt to store array into an array element in %s.\n",
			symbolptr[symInx].name);
	}
	aindex = 0;
	for (j=a0->dim[4]-1; j >= 0; --j) {
		reg[regn++] = list2 = popval();
		trace4list(list2);
		subscript = ValueOf(list2);
		if (subscript < 0 || subscript >= a0->dim[j]) {
			fatalerror("Array index out of bounds %s\n", symbolptr[symInx].name);
		}
		aindex = aindex * a0->dim[j] + subscript;
		FreeNodeList(list2);
		reg[regn-1] = -1;
	}
	FreeNodeList(((long *)a0->data)[aindex]);
	((long *)a0->data)[aindex] = list1;
}

void
Drop(void) {
	long list;
	list = popval();
	FreeNodeList(list);
}
