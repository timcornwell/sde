#ifndef DATUM_H
#define DATUM_H

enum {
	RightHandType,
	Condition,
	Constraint,
	VariableType,
	ParameterType,
	ConstantType,
	ObservationType,
	DataType,
	EnvironmentType,
	NumTypes
};

typedef struct aBlock {
	char *(*resize)();
	long recsize;
	long numrecs;
	long maxrecs;
	long growamt;
	char *data;
	short dim[5];
	short refcount;
} ArrayBlock, *ArrayBlockPtr;	

typedef struct {
	char type;				/* variable type */
	char indexed;			/* flag determining if indexed or not */
	short symInx;			/* index in name table of variable */
	short index[5];			/* array index of variable */
} Deriv, *DerivPtr;

typedef struct {
	long next;				/* index of the next datum in list */
	double value;			/* value of node's coefficient */
	short order;			/* order of derivative (0, 1 or 2) */
	Deriv deriv[2];			/* variable references of derivative */
	ArrayBlockPtr array;	/* pointer to array for vectors */
} Datum, *DatumPtr;


extern Datum mtNode, mtNode2;		/* empty node for initializing */

extern DatumPtr datumptr;

extern int do2ndDerivs;
extern int doParams;
extern int doObservations;


ArrayBlockPtr NewArrayBlock();

#define ValueOf(node)         datumptr[node].value
#define NextOf(node)          datumptr[node].next
#define OrderOf(node)         datumptr[node].order
#define SymbolOf(node, var)   datumptr[node].deriv[var].symInx
#define VarTypeOf(node, var)  datumptr[node].deriv[var].type
#define IndexOf(node, var)    datumptr[node].deriv[var].index
#define IndexedOf(node, var)  datumptr[node].deriv[var].indexed
#define ArrayOf(node)         datumptr[node].array

#endif
