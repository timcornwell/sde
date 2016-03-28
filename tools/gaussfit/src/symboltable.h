#if defined(__STDC__) || defined(sgi)

typedef struct {
	char *name;		/* name of the variable */
	short dclInx;	/* index into declare stack (doubles as index into label stack) */
	short filevar;	/* index to file variable descriptor table */
	short cInx;		/* index into constant stack */
	signed char globalindices;		/* number of indices of var at global level */
	signed char localindices;		/* number of indices of var at local level */
} Symbol, *SymbolPtr;

#else

typedef struct {
	char *name;		/* name of the variable */
	short dclInx;	/* index into declare stack (doubles as index into label stack) */
	short filevar;	/* index to file variable descriptor table */
	short cInx;		/* index into constant stack */
	char globalindices;		/* number of indices of var at global level */
	char localindices;		/* number of indices of var at local level */
} Symbol, *SymbolPtr;

#endif

extern ArrayPtr syms;
extern SymbolPtr symbolptr;
	
typedef struct {
	short findex;
	short filenum;
	short colnum;
	short xvars[5];
} FileVar, *FileVarPtr;

extern ArrayPtr fvars;
extern FileVarPtr filevarptr;

#define NameOf(symInx)    symbolptr[symInx].name
#define FileVarOf(symInx) symbolptr[symInx].filevar
#define DclInxOf(symInx)  symbolptr[symInx].dclInx

#define FileNumOf(fvInx) filevarptr[fvInx].filenum
#define ColNumOf(fvInx)  filevarptr[fvInx].colnum

char *SizeSymbolTable();		
short AddSymbol();		
short findsymbol();		
char *SizeFileVarTable();		
long AddFileVar();		
