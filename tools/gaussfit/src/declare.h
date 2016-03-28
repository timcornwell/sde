
typedef struct {
	char type;		/* variable type */
	char indexed;	/* 1 if indexed type */
	short symInx;	/* index of name in symbol table */
	short prev;		/* previous variable with same name */
	long list;		/* index into datum pool */
} Declaration, *DeclarationPtr;

extern DeclarationPtr declareptr;
extern ArrayPtr decls;

#define ListOf(dclInx) declareptr[dclInx].list
#define PrevOf(dclInx) declareptr[dclInx].prev

char *SizeDeclareStack();		
