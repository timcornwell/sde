/* 

	new datum list manangement improvements :

	* no more garbage collections, nodes are freed immediately.
	* nodes are allocated and copied less often, they are reused if possible.
	* all arrays use the same set of management routines in a semi-object oriented way
		(i.e. inheritance but no message passing and very limited polymorphism)
	* all arrays are dynamically sized
	* use of short and long rather than int when warranted for improved memory efficiency
		on ported machines.
	* ?? try cacheing parameter indices looked up to eliminate some binary searches ??
	
*/ 

#include <stdio.h>
#include <stdlib.h>
#include "array.h"
#include "datum.h"
#include "simpledefs.h"
#include "protoplasm.h"
#include "symboltable.h"
#include "alloc.h"

Datum mtNode0 = 
	{ -1, 0.0, 0, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, NULL }; 
	/* empty node for initializing */
Datum mtNode1 = 
	{ -1, 0.0, 1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, NULL }; 
	/* empty node for initializing */
Datum mtNode2 = 
	{ -1, 0.0, 2, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, NULL }; 
	/* empty node for initializing */

PoolPtr nodes = NULL;

DatumPtr datumptr;

int do2ndDerivs = 0;
int doObservations = 0;
int doParams = 0;

char *
SizeDatums(array, size) /* sic.  'Data' - I know */
	PoolPtr array;
	long size;
{
	char *ptr;
	long i;
	array->firstfree = array->maxrecs;
	array->numrecs = array->maxrecs = size;
	array->data = 
		Reallocate("SizeDatums", array->recsize * array->maxrecs, array->data);
	datumptr = (DatumPtr)array->data;
	ptr = (char*)array->data + array->firstfree * array->recsize;
	for (i=array->firstfree; i<array->maxrecs-1; ++i, ptr += array->recsize) {
		((DatumPtr)ptr)->order = -99;
		((DatumPtr)ptr)->array = 0;
		((DatumPtr)ptr)->next = i+1;
	}
	((DatumPtr)ptr)->next = -1;
	return array->data;
}

InitDatumPool() {
	long i;
	if (nodes) FreeDatumPool();
	nodes = NewPool((long)sizeof(Datum), 400L, 200L);
	nodes->resize = SizeDatums;
	datumptr = (DatumPtr)nodes->data;
	for (i=0; i>=0; i = datumptr[i].next) {
		datumptr[i].array = 0;
		datumptr[i].order = -99;
	}
}	

void
FreeDatumPool(void) {
	FreePool(nodes);
}

FreeNodeList(list) /* free a list of nodes & derivatives */
	register long list;
{
	register long next;
	DatumPtr d;
	while (list > -1) {
		d = datumptr + list;
		if (d->order == -99) {
			fatalerror("Attempt to free a free node\n","");
		}
		next = d->next;
		if (d->array) DecArrayRef(list);
		d->order = -99;
		d->array = 0;
		FreeRecord(nodes, list);
		list = next;
	}
}


long ArrayNodeLen();

long
NodeListLen(list) /* get length of a list of nodes & derivatives */
	long list;
{
	long next, count = 0;
	char str[32];

	if (list < 0 || list > nodes->numrecs) {
		sprintf(str, "Bad Node %ld  [0,%ld]\n",list, nodes->numrecs);
		runtimeerror(str,"");
	}
	while (list > -1) {
		next = datumptr[list].next;
		count ++;
		if (datumptr[list].array) count += ArrayNodeLen(list);
		list = next;
		if (list < 0 || list > nodes->numrecs) {
			sprintf(str, "Bad Node %ld  [0,%ld]\n",list, nodes->numrecs);
			runtimeerror(str,"");
		}
	}
	return count;
}

long
CopyNodeList(list) /* copy a list of nodes & derivatives */
	register long list;
{
	register long newnode, prevnode, next;
	register long firstnode = -1;

	prevnode = -1;
	IncArrayRef(list);
	while (list > -1) {
		next = datumptr[list].next;
		newnode = CopyRecord(nodes, list);
		if (firstnode == -1) firstnode = newnode;
		if (prevnode > -1) {
			datumptr[prevnode].next = newnode;
		}
		prevnode = newnode;
		list = next;
	}
	return firstnode;
}



long
NewNode(order)
	int order;
{
	long list;
	
	list = GetFreeRecord(nodes);
	datumptr[list] = mtNode0;
	datumptr[list].order = order;
	return list;
}	


long
MergeNodeLists(list1, list2)  /* merge two lists in canonical order */
	long list1, list2;
{
	long this, prev;
	long oldlist2, outlist;
	int i;
	
	outlist = prev = -1;
	while (list1 > -1 || list2 > -1) {
		i = CompareNodes(list1, list2);
		if (i<0) {
			this = list1;
			list1 = datumptr[list1].next;
		} else if (i>0) {
			this = list2;
			list2 = datumptr[list2].next;
		} else {
			this = list1;
			datumptr[list1].value += datumptr[list2].value;
			oldlist2 = list2;
			list1 = datumptr[list1].next;
			list2 = datumptr[list2].next;
			FreeRecord(nodes, oldlist2);
		}
		if (outlist == -1) outlist = this;
		if (prev > -1) {
			datumptr[prev].next = this;
		}
		prev = this;
	}
	return outlist;
}

int 
CompareIndices(d1, d2)
	register DerivPtr d1, d2;
{
	register int rval;
	
	switch (d1->index[4]) {
		case -1 :
		case 0 :
			return 0;
		case 1 :
			return d1->index[0] - d2->index[0];
		case 2 :
			if      (rval = d1->index[0] - d2->index[0]) return rval;
			else return     d1->index[1] - d2->index[1];
		case 3 :
			if      (rval = d1->index[0] - d2->index[0]) return rval;
			else if (rval = d1->index[1] - d2->index[1]) return rval;
			else return     d1->index[2] - d2->index[2];
		case 4 :
			if      (rval = d1->index[0] - d2->index[0]) return rval;
			else if (rval = d1->index[1] - d2->index[1]) return rval;
			else if (rval = d1->index[2] - d2->index[2]) return rval;
			else return     d1->index[3] - d2->index[3];
	}
}
		
int
CompareVars(d1, d2)
	register DerivPtr d1, d2;
{
	register int rval;
	
	/* go by first variable name */
	if (rval = d1->symInx - d2->symInx) return rval;
	
	/* go by array index */
	return CompareIndices(d1, d2);
}


int
CmpNodeVars(node1, node2, i, j)
	long node1, node2;
	int i, j;
{
	DerivPtr d1, d2;
	int rval;
	
	d1 = &datumptr[node1].deriv[i];
	d2 = &datumptr[node2].deriv[j];
	
	/* go by first variable name */
	if (rval = d1->symInx - d2->symInx) return rval;
	
	/* go by array index */
	return CompareIndices(d1, d2);	
}

int
CompareNodes(node1, node2)
	long node1, node2;
{
	int rval;
	DatumPtr n1, n2;
	
	/* sorting order :
	
		a', b', c', a'a', a'b', a'c', b'b', b'c', c'c'
			
	*/
	
	if (node1 < 0) {
		if (node2 < 0) return 0;
		else return 1;
	} else if (node2 < 0) return -1;
	
	n1 = datumptr + node1;
	n2 = datumptr + node2;

	/* go by order */
	if (rval = n1->order - n2->order) return rval;
	
	/* go by 1st variable */
	if (rval = CompareVars(&n1->deriv[0], &n2->deriv[0])) return rval;
	
	/* go by 2nd variable */
	return CompareVars(&n1->deriv[1], &n2->deriv[1]);
}

ArrayBlockPtr 
InitArrayBlock(a, nelems, dim)
	ArrayBlockPtr a;
	long nelems;
	short *dim;
{
	long i, *eptr;

	a->dim[0] = dim[0];
	a->dim[1] = dim[1];
	a->dim[2] = dim[2];
	a->dim[3] = dim[3];
	a->dim[4] = dim[4];
	a->refcount = 0;
	a->numrecs = nelems;
	eptr = (long *)a->data;
	for (i=0; i<nelems; ++i) *eptr++ = -1L;
}

ArrayBlockPtr 
NewArrayBlock(dim)
	short *dim;
{
	ArrayBlockPtr array;
	long size, nelems;
	long i;

	for (i=0, nelems=1; i<dim[4]; ++i) nelems *= dim[i];
	array = (ArrayBlockPtr)MemAlloc("NewArrayBlock", (long)sizeof(ArrayBlock));
	InitArray((ArrayPtr)array, (long)sizeof(long), nelems, 8L);
	InitArrayBlock(array, nelems, dim);
	return array;
}

FreeArrayNodes(list) 
	long list;
{
	register ArrayBlockPtr array;
	long size;
	register long nelems;
	register long i, *eptr;
	
	array = datumptr[list].array;
	nelems = array->numrecs;
	eptr = (long *)array->data;
	for (i=0; i<nelems; ++i) {
		FreeNodeList(*eptr++);
	}
	FreeArray(array);
	/*freemem("Free Array Nodes", (char*)array->data);*/
	/*freemem("Free Array Block", (char*)array);*/
	datumptr[list].array = NULL;
}

long
ArrayNodeLen(list) 
	long list;
{
	ArrayBlockPtr array;
	long size, nelems;
	long i, *eptr, count;
	char str[32];
	
	if (list < 0 || list > nodes->numrecs) {
		sprintf(str, "Bad Node %ld  [0,%ld]\n",list, nodes->numrecs);
		runtimeerror(str,"");
	}
	array = datumptr[list].array;
	nelems = array->numrecs;
	count = 0;
	eptr = (long *)array->data;
	for (i=0; i<nelems; ++i) {
		count += NodeListLen(*eptr++);
	}
	return count;
}

DumpNodeList(file, list)
	FILE *file;
	long list;
{	
	if (list > -1) {
		do {
			DumpNode(file, list);
			list = datumptr[list].next;
		} while (list > -1 && datumptr[list].order != -99);
		xprintf(file, " End\n\n");
	} else {
		xprintf(file, " Empty\n\n");
	}
}


DumpNode(file, list)
	FILE *file;
	long list;
{
	char str1[32], str2[32];
	
	if (list > -1) {
		if (datumptr[list].array) {
				xprintf(file, "%4ld ** List node is an array\n",list);
		} else {
			if (datumptr[list].order == 0) {
				xprintf(file, "%4ld %14.6g\n", list,
					datumptr[list].value);
			} else if (datumptr[list].order == 1) {
			/*
				if (datumptr[list].deriv[0].index > -1) {
					sprintf(str1, "[%d]", datumptr[list].deriv[0].index);
				} else str1[0] = 0;
			*/
				str1[0] = 0;
				xprintf(file, "%4ld %14.6g x deriv( %s%s )\n", list, 
					datumptr[list].value,
					symbolptr[datumptr[list].deriv[0].symInx].name, str1);
			} else if (datumptr[list].order == 2) {
			/*
				if (datumptr[list].deriv[0].index > -1) {
					sprintf(str1, "[%d]", datumptr[list].deriv[0].index);
				} else str1[0] = 0;
				if (datumptr[list].deriv[1].index > -1) {
					sprintf(str2, "[%d]", datumptr[list].deriv[1].index);
				} else str2[0] = 0;
			*/
				str1[0] = 0;
				str2[0] = 0;
				xprintf(file, "%4ld %14.6e x deriv( %s%s ) x deriv( %s%s )\n", list, 
					datumptr[list].value,
					symbolptr[datumptr[list].deriv[0].symInx].name, str1,
					symbolptr[datumptr[list].deriv[1].symInx].name, str2);
			} else if (datumptr[list].order == -99) {
				xprintf(file, "%4ld ** List node  is *FREE* !\n", list);
			} else {
				xprintf(file, "%4ld ** List node contains bad order value : %d\n", 
					list, datumptr[list].order);
			}
		}
	} else {
		xprintf(file, "NIL\n");
	}
}
