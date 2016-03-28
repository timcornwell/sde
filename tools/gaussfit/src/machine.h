

enum {
	opSwap,		
	opDrop,		
	opPushCon,		
	opPush,		
	opPushVec,		
	opStore,		
	opStoreVec,		
	opNegate,		
	opAdd,		
	opSubtract,		
	opMultiply,		
	opDivide,		
	opPushTrue,		
	opPushFalse,		
	opNotFn,		
	opAndFn,		
	opOrFn,		
	opExclusiveOrFn,		
	opEqualsFn,		
	opNotEqualsFn,		
	opLessThanFn,		
	opGreaterThanFn,		
	opLessOrEqualFn,		
	opGreaterOrEqualFn,		
	opDefIndex,		
	opDefVar,		
	opDefArg,		
	opDefDat,		
	opDefConst,		
	opDefPar,		
	opDefObs,		
	opDefEnvir,		
	opDefVec,
	opDefFunc,	
	opPushDim,		
	opPower,
	
	opTrace0,
	opTrace1,
	opTrace2,
	opTrace3,
	opTrace4,
	opLabel,
	opGoTo,
	opIf,
	opIfNot,
	opFuncCall,
	opReturn,
	
	opNumOpcodes	
};

#ifdef __STDC__
typedef void (*opfunptr)(void);
typedef void (*opfunptr1)(short);
#else
typedef (*opfunptr)();
typedef (*opfunptr1)();
#endif

typedef struct {
	short opindex;
	short numwords;
	opfunptr opfunc;
	char *opname;
} Opcodes;

extern Opcodes opcodetable[opNumOpcodes];

extern long reg[8];	/* operand list node index registers for diagnostic in case of error */
extern long regn;	/* number of registers used */

typedef struct {
	short filenum;
	short linenum;
	short funcnum;
	short opcode;
	short operand;
} Instruction;

