
#include "array.h"
#include "datum.h"
#include "simpledefs.h"
#include "protoplasm.h"
#include "symboltable.h"
#include "alloc.h"
#include <stdio.h>


ArrayPtr syms = NULL;
SymbolPtr symbolptr;

char *
SizeSymbolTable(array, size)
	PoolPtr array;
	long size;
{
	array->maxrecs = size;
	symbolptr = (SymbolPtr)(array->data = 
		Reallocate("SizeSymbolTable", array->recsize * array->maxrecs, array->data));
	return array->data;
	
}

InitSymbolTable() {
	if (syms) FreeSymbolTable();
	syms = NewArray((long)sizeof(Symbol), 80L, 80L);
	syms->resize = SizeSymbolTable;
	symbolptr = (SymbolPtr)syms->data;
}

FreeSymbolTable() {
	int i;
	/*for (i=0; i<syms->numrecs; ++i) {
		freemem("SymTableNames", symbolptr[i].name);
	}*/
	FreeArray(syms);
}

short
AddSymbol(name)
	char *name;
{
	long inx;
	inx = AddRecords(syms, 1L);
	symbolptr[inx].name = name;
	symbolptr[inx].dclInx  = -1;
	symbolptr[inx].filevar = -1;
	symbolptr[inx].cInx = -1;
	symbolptr[inx].globalindices = -1;
	symbolptr[inx].localindices = -1;
	return inx;
}

short
findsymbol(name, enternew)
	char *name;
	int enternew;
{
	short i;
	for (i=0; i<syms->numrecs; ++i) {
		if (strcmp(name, symbolptr[i].name) == 0) return i;
	}
	if (enternew) return AddSymbol(name);
	else return -1;
}

badsym(int inx) {
	if (inx < 0 || inx >= syms->numrecs) return 1;
	return 0;
}

SymbolTablePostCompileCleanup() {
	int i;
	for (i=0; i<syms->numrecs; ++i) {
		symbolptr[i].cInx = -1;
		symbolptr[i].filevar = -1;
	}
}	

ArrayPtr fvars = NULL;
FileVarPtr filevarptr = NULL;
	
char *
SizeFileVarTable(array, size)
	PoolPtr array;
	long size;
{
	array->maxrecs = size;
	array->data = 
		Reallocate("SizeFileVarTable", array->recsize * array->maxrecs, array->data);
	filevarptr = (FileVarPtr)array->data;
	return array->data;
}

InitFileVarTable() {
	if (fvars) FreeArray(fvars);
	fvars = NewArray((long)sizeof(FileVar), 40L, 40L);
	fvars->resize = SizeFileVarTable;
	filevarptr = (FileVarPtr)fvars->data;
}	

long
AddFileVar(xvars)
	short xvars[5];
{
	long inx;
	inx = AddRecords(fvars, 1L);
	filevarptr[inx].xvars[0] = xvars[0];
	filevarptr[inx].xvars[1] = xvars[1];
	filevarptr[inx].xvars[2] = xvars[2];
	filevarptr[inx].xvars[3] = xvars[3];
	filevarptr[inx].xvars[4] = xvars[4];
	filevarptr[inx].findex  = -1;
	filevarptr[inx].filenum = -1;
	filevarptr[inx].colnum  = -1;
	return inx;
}


char *wordfill(token)
char *token;
{
	char *p;
	int num;
	long strsize;

	strsize = (long)strlen(token) + 1;  
	p=(char*)MemAlloc("wordfill", strsize); 
	strcpy(p,token);
	return p;
}

DumpSymbolTable() {
	int i, j;
	
	xprintf(stdout, "Symbol Table :\n");
	for (i=0; i<syms->numrecs; ++i) {
		xprintf(stdout, "%4d %s\n", i, symbolptr[i].name);
	}
}	

wipelocalindices() {
	int i;
	for(i=0; i<syms->numrecs; i++) { /* look through the table */
		symbolptr[i].localindices = -1;
	}
}
