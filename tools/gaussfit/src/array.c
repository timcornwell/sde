/* methods for dynamically sized array, pool, & stack objects */

#include <stdio.h>
#include <stdlib.h>
#include "array.h"
#include "datum.h"
#include "defines.h"
#include "simpledefs.h"
#include "protoplasm.h"
#include "alloc.h"

void DisposePool(PoolPtr array);

char *
SizeArray(array, size)
	ArrayPtr array;
	long size;
{
	array->maxrecs = size;
	return array->data = 
		Reallocate("SizeArray", array->recsize * array->maxrecs, array->data);
}

char *
SizePool(array, size)
	PoolPtr array;
	long size;
{
	char *ptr;
	long i;
	array->numrecs = array->maxrecs = size;
	array->data = 
		Reallocate("SizePool", array->recsize * array->maxrecs, array->data);
	ptr = (char*)array->data + array->firstfree * array->recsize;
	for (i=array->firstfree; i<array->maxrecs-1; ++i, ptr += array->recsize) {
		((FreeListNodePtr)ptr)->nextfree = i+1;
	}
	((FreeListNodePtr)ptr)->nextfree = -1;
	return array->data; 
}


ArrayPtr 
NewArray(recsize, maxrecs, growamt)
	long recsize;
	long maxrecs;
	long growamt;
{
	ArrayPtr array;
	array = (ArrayPtr)MemAlloc("NewArray", (long)sizeof(Array));
	InitArray(array, recsize, maxrecs, growamt);
	return array;
}

void
FreeArray(ArrayPtr array) {
	freemem("FreeArrayData", (char*)array->data);
	freemem("FreeArray",     (char*)array);
}

InitArray(array, recsize, maxrecs, growamt)
	ArrayPtr array;
	long recsize;
	long maxrecs;
	long growamt;
{
	array->numrecs = 0;
	array->maxrecs = maxrecs;
	array->recsize = recsize;
	array->growamt = growamt;
	array->data = MemAlloc("InitArray", maxrecs * recsize);
	array->resize = SizeArray;
}

PoolPtr 
NewPool(recsize, maxrecs, growamt)
	long recsize;
	long maxrecs;
	long growamt;
{
	PoolPtr array;
	long i;
	char *ptr;
	array = (PoolPtr)MemAlloc("NewPool", (long)sizeof(Pool));
	InitPool(array, recsize, maxrecs, growamt);
	return array;
}

void
DisposePool(PoolPtr array) {
	freemem("FreePoolData", (char*)array->data);
	freemem("FreePool",     (char*)array);
}

InitPool(array, recsize, maxrecs, growamt)
	PoolPtr array;
	long recsize;
	long maxrecs;
	long growamt;
{
	char *ptr;
	long i;
	InitArray((ArrayPtr)array, recsize, maxrecs, growamt);
	array->numrecs = maxrecs;
	FreePool(array);	/* init free list */
	array->resize = SizePool;
	array->numused = 0;
}


char *
GetRecordPtr(array, index)
	ArrayPtr array;
	long index;
{
	if (index >= array->numrecs) return NULL;
	return (char *)((char*)array->data + index * array->recsize);
}

int
GetRecord(array, index, record)
	ArrayPtr array;
	long index;
	char *record;
{
	if (index < 0 || index >= array->numrecs) return 0;
	memcpy(record, (char*)array->data + index * array->recsize, array->recsize);
	return 1;
}

int
GetNRecords(array, index, n, record)
	ArrayPtr array;
	long index, n;
	char *record;
{
	if (index < 0 || index+n >= array->numrecs) return 0;
	memcpy(record, (char*)array->data + index * array->recsize, n * array->recsize);
	return 1;
}

int
PutRecord(array, index, record)
	ArrayPtr array;
	long index;
	char *record;
{
	if (index < 0 || index >= array->numrecs) return 0;
	memcpy((char*)array->data + index * array->recsize, record, array->recsize);
	return 1;
}

int
PutNRecords(array, index, n, record)
	ArrayPtr array;
	long index;
	char *record;
{
	if (index < 0 || index+n >= array->numrecs) return 0;
	memcpy((char*)array->data + index * array->recsize, record, array->recsize * n);
	return 1;
}

int
InsertRecord(array, index, record)
	ArrayPtr array;
	long index;
	char *record;
{
	char *ptr;
	if (index < 0 || index >= array->numrecs) return 0;
	if (++array->numrecs > array->maxrecs) 
		(*array->resize)(array, array->maxrecs + array->growamt);
	ptr = (char*)array->data + index * array->recsize;
	memmove(ptr + array->recsize, ptr, array->recsize);
	memcpy(ptr, record, array->recsize);
	return 1;
}

int
InsertNRecords(array, index, n, record)
	ArrayPtr array;
	long index, n;
	char *record;
{
	char *ptr;
	if (index < 0 || index >= array->numrecs) return 0;
	if ((array->numrecs+=n) >= array->maxrecs)
		(*array->resize)(array, array->maxrecs + array->growamt + n);
	ptr = (char*)array->data + index * array->recsize;
	memmove(ptr + array->recsize, ptr, array->recsize);
	memcpy(ptr, record, array->recsize);
	return 1;
}

int
DeleteRecord(array, index, record)
	ArrayPtr array;
	long index;
	char *record;
{
	char *ptr;
	-- array->numrecs;
	if (index < 0 || index >= array->numrecs) return 0;
	ptr = (char*)array->data + index * array->recsize;
	if (record != NULL) memcpy(record, ptr, array->recsize);
	memmove(ptr, ptr + array->recsize, array->recsize);
	return 1;
}

int
DeleteNRecords(array, index, n, record)
	ArrayPtr array;
	long index, n;
	char *record;
{
	char *ptr;
	long nbytes;
	if (index < 0 || index+n >= array->numrecs) return 0;
	array->numrecs -= n;
	ptr = (char*)array->data + index * array->recsize;
	nbytes = n * array->recsize;
	if (record != NULL) memcpy(record, ptr, nbytes);
	memmove(ptr, ptr + nbytes, nbytes);
	return 1;
}

long
AddRecords(array, n)
	ArrayPtr array;
	long n;
{
	long i;
	i = array->numrecs;
	array->numrecs += n ;
	if (array->numrecs > array->maxrecs) {
		(*array->resize)(array, array->maxrecs + array->growamt + n);
	}
	return i;
}

long
SubRecords(array, n)
	ArrayPtr array;
	long n;
{
	long i;
	if (array->numrecs-n < 0) return -1;
	array->numrecs -= n ;
	return array->numrecs;
}

int
PushRecord(array, record)
	ArrayPtr array;
	char *record;
{
	char *ptr;
	if (array->numrecs+1 > array->maxrecs) {
		(*array->resize)(array, array->maxrecs + array->growamt);
	}
	ptr = (char*)array->data + array->numrecs * array->recsize;
	memcpy(ptr, record, array->recsize);
	array->numrecs ++ ;
	return 1;
}

long
PushStack(array)
	ArrayPtr array;
{
	char *ptr;
	if (array->numrecs+1 > array->maxrecs) {
		(*array->resize)(array, array->maxrecs + array->growamt);
	}
	return array->numrecs ++ ;
}

int
PopRecord(array, record)
	ArrayPtr array;
	char *record;
{
	char *ptr;
	if (array->numrecs == 0) return 0;
	array->numrecs -- ;
	ptr = (char*)array->data + array->numrecs * array->recsize;
	memcpy(record, ptr, array->recsize);
	return 1;
}

int
DropNStack(array, n)
	ArrayPtr array;
	long n;
{
	if (array->numrecs-n < 0) return 0;
	array->numrecs -= n ;
	return 1;
}


FreeRecord(array, index)
	register PoolPtr array;
	long index;
{
	char *ptr;
	ptr = (char*)array->data + index * array->recsize;
	((FreeListNodePtr)ptr)->nextfree = array->firstfree;
	array->firstfree = index;
	array->numused --;
}

FreePool(array)
	PoolPtr array;
{
	char *ptr;
	long i;
	
	/* initialize free list == free all records */
	array->firstfree = 0;
	ptr = (char*)array->data;
	for (i=0; i<array->maxrecs-1;  i++, ptr = (char *)((char*)ptr + array->recsize)) {
		((FreeListNodePtr)ptr)->nextfree = i+1;
	}
	((FreeListNodePtr)ptr)->nextfree = -1; /* end of list */
}

DumpPool(file, array)
	FILE *file;
	PoolPtr array;
{
	char *ptr;
	long i, j;
	
	/* count free records */
	i = array->firstfree;
	ptr = (char*)((char*)array->data + array->recsize * i);
	j = 0;
	while (i > -1) {
		++ j;
		i = ((FreeListNodePtr)ptr)->nextfree;
		ptr = (char *)((char*)array->data + array->recsize * i);
	}
	xprintf(file, "maxrecs = %ld\n", array->maxrecs);
	xprintf(file, "numused = %ld\n", array->numused);
	xprintf(file, "numfree = %ld\n", j);
	xprintf(file, "unaccounted = %ld\n\n", array->maxrecs-array->numused-j);

}

long
GetFreeRecord(array)
	register PoolPtr array;
{
	char *ptr;
	long i;
	if (array->firstfree == -1) {
		array->firstfree = array->maxrecs;
		(*array->resize)(array, array->maxrecs + array->growamt);
	}
	i = array->firstfree;
	ptr = (char*)array->data + array->firstfree * array->recsize;
	array->firstfree = ((FreeListNodePtr)ptr)->nextfree;
	array->numused ++;
	return i;
}

long
CopyRecord(array, index)
	register PoolPtr array;
	register long index;
{
	register char *ptr1, *ptr2;
	register long newindex;

	newindex = GetFreeRecord(array);
	ptr1 = (char*)array->data + index * array->recsize;
	ptr2 = (char*)array->data + newindex * array->recsize;
	memcpy(ptr2, ptr1, array->recsize);
	return newindex;
}

#ifdef NEEDMEMMOVE

memmove(s1, s2, len)
	register char *s1, *s2;
	register long len;
{
	if (s1 > s2) {
		s1 += len-1;
		s2 += len-1;
		while (len--) *s1-- = *s2--;
	} else {
		while (len--) *s1++ = *s2++;
	}
}

#endif

