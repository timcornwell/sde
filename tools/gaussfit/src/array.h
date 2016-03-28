#ifndef ARRAY_H
#define ARRAY_H

typedef struct {
	char *(*resize)();
	long recsize;
	long numrecs;
	long maxrecs;
	long growamt;
	char *data;
} Array, *ArrayPtr ;

typedef struct {
	char *(*resize)();
	long recsize;
	long numrecs;
	long maxrecs;
	long growamt;
	char *data;
	long firstfree;
	long numused;
} Pool, *PoolPtr ;

typedef struct {
	long nextfree;
} FreeListNode, *FreeListNodePtr;

extern PoolPtr nodes;
extern PoolPtr derivs;

#endif
