#include <stdio.h>
#include "array.h"
#include "datum.h"
#include "simpledefs.h"
#include "protoplasm.h"
#include "machine.h"
#include "declare.h"
#include "symboltable.h"
#include "alloc.h"


DeclarationPtr declareptr;
ArrayPtr decls = NULL;
short indexlist[5] = {-1, -1, -1, -1, 0};

short numdims = 0;
static ArrayPtr decl1;

char *
SizeDeclareStack(array, size)
	PoolPtr array;
	long size;
{
	array->maxrecs = size;
	array->data = 
		Reallocate("SizeDeclareStack", array->recsize * array->maxrecs, array->data);
	declareptr = (DeclarationPtr)array->data;
	return array->data;
}

void
InitDeclareStack() {
	if (decls) FreeDeclareStack();
	decl1 = decls = NewArray((long)sizeof(Declaration), 100L, 100L);
	decls->resize = SizeDeclareStack;
	declareptr = (DeclarationPtr)decls->data;
}

FreeDeclareStack() {
	FreeArray(decls);
}

void
ResetDeclareStack() {
	int i;
	for (i=0; i<decls->numrecs; ++i) {
		FreeNodeList(declareptr[i].list);
	}
	DropNStack(decls, decls->numrecs);
}

void
DropStackFrame(n) 
	long n;
{
	int i;
	for (i=n; i<decls->numrecs; ++i) {
		DclInxOf(declareptr[i].symInx) = declareptr[i].prev;
		
		FreeNodeList(declareptr[i].list);
	}
	DropNStack(decls, decls->numrecs - n);
}


DefOne(type, symInx)
	short type;
	short symInx;
{
	long dclInx, list;
	DeclarationPtr dcl;
	
	/* fill in declare stack record */
	dclInx = PushStack(decls);
	dcl = declareptr + dclInx;
	dcl->list = list = reg[regn++] = NewNode(0);
	trace4list(list);
	dcl->type = type;
	dcl->symInx = symInx;
	dcl->indexed = 0;
	
	/* link in scope chain */
	dcl->prev = symbolptr[symInx].dclInx;
	symbolptr[symInx].dclInx = dclInx;
	
	if (type != VariableType && type != EnvironmentType) {
		if (FileVarOf(symInx) == -1) {
			FileVarOf(symInx) = AddFileVar(indexlist);
		}
		if (indexlist[4] > 0) dcl->indexed = 1;
		indexlist[0] = indexlist[1] = indexlist[2] = indexlist[3] = -1;
		indexlist[4] = 0;
	}
	
	return list;
}

void
DefTwo (type, symInx)
	short type;
	short symInx;
{
	long dclInx, list;
	
	list = DefOne(type, symInx);
	datumptr[list].next = NewNode(1);
	list = datumptr[list].next;
	datumptr[list].value = 1.0;
	datumptr[list].deriv[0].type = type;
	datumptr[list].deriv[0].indexed = declareptr[DclInxOf(symInx)].indexed;
	datumptr[list].deriv[0].symInx = symInx;
}

void
DefIndex(symInx)
	short symInx;
{
	short noindices[5];
	if (indexlist[4] >= 4) {
		fatalerror("Too many dimensions in array.\n","");
	}
	indexlist[indexlist[4]++] = symInx;
	if (FileVarOf(symInx) == -1) {
		noindices[0] = -1;
		noindices[1] = -1;
		noindices[2] = -1;
		noindices[3] = -1;
		noindices[4] =  0;
		FileVarOf(symInx) = AddFileVar(noindices);
	}
}

void
DefVar(symInx)
	short symInx;
{
	DefOne(VariableType, symInx);
}

void
DefVarTwo(symInx)
	short symInx;
{
	DefTwo(VariableType, symInx);
}

void
DefDat(symInx)
	short symInx;
{
	DefOne(DataType, symInx);
}

void
DefConst(symInx)
	short symInx;
{
	DefOne(ConstantType, symInx);
}

void
DefPar(symInx)
	short symInx;
{
	DefTwo(ParameterType, symInx);
}

void
DefObs(symInx)
	short symInx;
{
	DefTwo(ObservationType, symInx);
}

void
DefEnvir(symInx)
	short symInx;
{
	DefTwo(EnvironmentType, symInx);
}

void
DefVec(symInx)
	short symInx;
{
	long d0 ,d1;
	short dim[5];
	int i;
	
	d0 = DefOne(VariableType, symInx);
	dim[0] = dim[1] = dim[2] = dim[3] = 1;
	dim[4] = numdims;
	for (i=0; i<numdims; ++i) {
		d1 = popval();
		dim[i] = datumptr[d1].value;
		FreeNodeList(d1);
	}
	datumptr[d0].array = NewArrayBlock(dim);
	IncArrayRef(d0);
	numdims = 0;
}

void
DecArrayRef(list)
	long list;
{
	if (datumptr[list].array) {
		datumptr[list].array->refcount--;
		if (datumptr[list].array->refcount <= 0) {
			FreeArrayNodes(list);
		}
	}
}

IncArrayRef(list)
	long list;
{
	if (datumptr[list].array) {
		datumptr[list].array->refcount++;	
	}
}

void
PushDim(symInx)
	short symInx;
{
	PushCon(symInx);
	numdims ++ ;
}

 
void
DefArg(symInx)  /* define a function argument on the declare stack */
	short symInx;
{
	short dclInx;
	long list;
	
	DefOne(VariableType, symInx);
	dclInx = DclInxOf(symInx);
	FreeNodeList(ListOf(dclInx));
	ListOf(dclInx) = list = popval();
}

DefFunc() { }

