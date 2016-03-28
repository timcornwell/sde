
#include <stdio.h>
#include "array.h"
#include "datum.h"
#include "simpledefs.h"
#include "symboltable.h"
#include "opcodes.p"
#include "machine.h"
#include "declare.h"
#include "compile.h"
#include "alloc.h"
#include "protoplasm.h"
#include "functs.h"

Opcodes opcodetable[opNumOpcodes] = {
	opSwap,				1,	(opfunptr)Swap,				"Swap",		
	opDrop,				1,	(opfunptr)Drop,				"Drop",		
	opPushCon,			2,	(opfunptr)PushCon,			"PushCon",		
	opPush,				2,	(opfunptr)Push,				"Push",		
	opPushVec,			2,	(opfunptr)PushVec,			"PushVec",		
	opStore,			2,	(opfunptr)Store,			"Store",		
	opStoreVec,			2,	(opfunptr)StoreVec,			"StoreVec",		
	opNegate,			1,	(opfunptr)Negate,			"Negate",		
	opAdd,				1,	(opfunptr)Add,				"Add",		
	opSubtract,			1,	(opfunptr)Subtract,			"Subtract",		
	opMultiply,			1,	(opfunptr)Multiply,			"Multiply",		
	opDivide,			1,	(opfunptr)Divide,			"Divide",		
	opPushTrue,			1,	(opfunptr)PushTrue,			"PushTrue",		
	opPushFalse,		1,	(opfunptr)PushFalse,		"PushFalse",		
	opNotFn,			1,	(opfunptr)NotFn,			"Not",		
	opAndFn,			1,	(opfunptr)AndFn,			"And",		
	opOrFn,				1,	(opfunptr)OrFn,				"Or",		
	opExclusiveOrFn,	1,	(opfunptr)ExclusiveOrFn,	"ExclusiveOr",		
	opEqualsFn,			1,	(opfunptr)EqualsFn,			"Equals",		
	opNotEqualsFn,		1,	(opfunptr)NotEqualsFn,		"NotEquals",		
	opLessThanFn,		1,	(opfunptr)LessThanFn,		"LessThan",		
	opGreaterThanFn,	1,	(opfunptr)GreaterThanFn,	"GreaterThan",		
	opLessOrEqualFn,	1,	(opfunptr)LessOrEqualFn,	"LessOrEqual",		
	opGreaterOrEqualFn,	1,	(opfunptr)GreaterOrEqualFn,	"GreaterOrEqual",		
	opDefIndex,			2,	(opfunptr)DefIndex,			"DefIndex",		
	opDefVar,			2,	(opfunptr)DefVar,			"DefVar",		
	opDefArg,			2,	(opfunptr)DefArg,			"DefArg",		
	opDefDat,			2,	(opfunptr)DefDat,			"DefDat",		
	opDefConst,			2,	(opfunptr)DefConst,			"DefConst",		
	opDefPar,			2,	(opfunptr)DefPar,			"DefPar",		
	opDefObs,			2,	(opfunptr)DefObs,			"DefObs",		
	opDefEnvir,			2,	(opfunptr)DefEnvir,			"DefEnvir",		
	opDefVec,			2,	(opfunptr)DefVec,			"DefVec",
	opDefFunc,			1,	(opfunptr)DefFunc,			"DefFunc",		
	opPushDim,			2,	(opfunptr)PushDim,			"PushDim",		
	opPower,			1,	(opfunptr)Power,			"Power",	
		
	opTrace0,			1,	(opfunptr)Trace0,			"Trace0",
	opTrace1,			1,	(opfunptr)Trace1,			"Trace1",
	opTrace2,			1,	(opfunptr)Trace2,			"Trace2",
	opTrace3,			1,	(opfunptr)Trace3,			"Trace3",
	opTrace4,			1,	(opfunptr)Trace4,			"Trace4",
	opLabel,			2,	(opfunptr)Label,			"Label",
	opGoTo,				2,	(opfunptr)GoTo,				"GoTo",
	opIf,				1,	(opfunptr)If,				"If",
	opIfNot,			1,	(opfunptr)IfNot,			"IfNot",
	opFuncCall,			2,	(opfunptr)FuncCall,			"FuncCall",
	opReturn,			1,	(opfunptr)NULL,				"Return",
};


short cur_func;
extern int line_number;
extern IncludeFile *fl_stack[NUMINCLUDES];	/* error recovery list		*/
extern IncludeFile fl_list[NUMINCLUDES];	/* error recovery list		*/
extern int numincls;				/* current working file descriptor  */
extern int cur_fd;				/* current working file descriptor  */
extern FILE *fp;
FILE *fp_asm;
extern int running;
extern int tracedisplay;

int tracelevel = 0;
int ftn = 0;             /* print out ftn file  */

Instruction *core = NULL;		/* instruction memory */
long coremax = 0;		/* size of core */
long pc;				/* program counter */
long pcmax = 0;			/* amount of core currently used */
long newpc;				/* next value of pc */

			/* operand list node index registers for diagnostic in case of error */
long reg[8] = { -1, -1, -1, -1, -1, -1, -1, -1 };
long regn;	/* number of registers used */

initcorespace() {
	coremax = 0;
	pcmax = 0;
	if (core) freemem("core", (char*)core);
	corespace();
}

corespace() /* dynamically allocate corespace */
{
	long j;
	long size;

	coremax += 600;
	size = (long)coremax*sizeof(Instruction);

	if (core == NULL) core = (Instruction *)MemAlloc("core", size);
	else core = (Instruction *)Reallocate("core", size, (char *)core);
	
	if (core == NULL) 
		printf ("CORE == NULL!!!!!!!!!!!\n");
	for (j = pcmax; j < coremax; j++) {
		core[j].filenum = -1;
		core[j].funcnum = -1;
		core[j].linenum = -1;
		core[j].opcode  = -1;
		core[j].operand = -1;
	}
}

int inanalyzepool = 0;

traceinstruction(fd) 
	FILE *fd;
{
	int i;
	short op;
	
	op = core[pc].opcode;
	if (tracelevel > 1 || (tracelevel == 1 && (op == opStore || op == opStoreVec))) {
		if (op == opStore || op == opStoreVec) {
			if (tracelevel >= 3 ) {
				printopcode(fd, pc);
				xprintf(fd, "Register 0 Contents :\n");
				DumpNodeList(fd, reg[0]);
			} else if (tracelevel == 1 ) {
				double value;
				char *name;
				short filenum;
				short funcnum;
				short linenum;
				char *filename, *funcname;
				
				filenum = core[pc].filenum;
				funcnum = core[pc].funcnum;
				linenum = core[pc].linenum;
				
				name = symbolptr[core[pc].operand].name;
				value = datumptr[reg[0]].value;
				xprintf(fd, "%12s = %22.15f", name, value);
				if (filenum < 0) filename = "none";
				else filename = fl_stack[filenum]->fname;
			
				if (funcnum < 0) funcname = "none";
				else funcname = symbolptr[funcnum].name;

				xprintf(fd, "  ; %s : %d | %s()\n", filename, linenum, funcname);

			} else if (tracelevel == 2 ) {
				printopcode(fd, pc);
				xprintf(fd, "Register 0 Value :\n");
				DumpNode(fd, reg[0]);
			}
		} else {
			printopcode(fd, pc);
			if (tracelevel >= 3) {
				xprintf(fd, "Top of Stack Contents :\n");
				DumpNodeList(fd, topval());
			} else {
				xprintf(fd, "Top of Stack Value :\n");
				DumpNode(fd, topval());
			}
		}
		/*if (tracelevel >= 4) dumpregisters(fd);*/
		xfflush(fd);
	}
}

trace4list(list) 
	long list;
{
	if (tracelevel >= 4) {
		xprintf(stderr, "Register Contents :\n");
		DumpNodeList(stderr, list);
		xprintf(fp, "Register Contents :\n");
		DumpNodeList(fp, list);
	}
}

dumpregisters(fd) 
	FILE *fd;
{
	int i;
	for (i=0; i<regn; ++i) {
		xprintf(fd, "Register %d Contents :\n", i);
		DumpNodeList(fd, reg[i]);
	}
}

printopcode(fd, pc)
	FILE *fd;
	long pc;
{
	short op;
	short filenum;
	short funcnum;
	short linenum;
	char *filename, *funcname;
	
	filenum = core[pc].filenum;
	funcnum = core[pc].funcnum;
	linenum = core[pc].linenum;
	op = core[pc].opcode;

	xprintf(fd, "%5ld %-12s", 
		pc, opcodetable[op].opname);
	if (opcodetable[op].numwords == 2) {
		xprintf(fd, " %-16s", symbolptr[core[pc].operand].name);
	} else {
		xprintf(fd, " ................");
	}
	if (filenum < 0) filename = "none";
	else filename = fl_stack[filenum]->fname;

	if (funcnum < 0) funcname = "none";
	else funcname = symbolptr[funcnum].name;

	xprintf(fd, "  ; %s : %d | %s()\n", filename, linenum, funcname);
	
#ifdef TOCONSOLE
	xprintf(stdout, "%5ld %-12s", 
		pc, opcodetable[op].opname);
	if (opcodetable[op].numwords == 2) {
		xprintf(stdout, " %-16s", symbolptr[core[pc].operand].name);
	} else {
		xprintf(stdout, " ................");
	}
	if (filenum < 0) filename = "none";
	else filename = fl_stack[filenum]->fname;

	if (funcnum < 0) funcname = "none";
	else funcname = symbolptr[funcnum].name;

	xprintf(stdout, "  ; %s : %d | %s()\n", filename, linenum, funcname);
#endif
	
}	

instruction(op1, op2)
	short op1, op2;
{
	if (op1 < 0 || op1 >= opNumOpcodes) {
		fatalerror("Illegal opcode generated\n.","");
	}
	
	if (pcmax >= coremax) corespace();
	core[pcmax].opcode = op1;
	if (opcodetable[op1].numwords == 2) core[pcmax].operand = op2;
	else core[pcmax].operand = -1;
	
	if (op1 == opDefFunc) {
		cur_func = op2;
		symbolptr[op2].dclInx = pcmax;
	} else if (op1 == opLabel) symbolptr[op2].dclInx = pcmax;
	
	core[pcmax].linenum = (cur_fd < 0) ? 9999 : fl_stack[cur_fd]->line_number;
	core[pcmax].funcnum = cur_func;
	core[pcmax].filenum = cur_fd;
    if (ftn) {
    	printopcode(fp_asm, pcmax);
    }
	++pcmax;
}


print_instr(op1,t)
	short op1;
	char *t;
{
	char arg[30];
	short op2;
	
	op2 = findsymbol(t, 1);
	if (op2 == -1)
		xprintf(stdout, "OP2 = -1!!!!!!!!!!!!!!!\n");
	instruction(op1, op2);
}


setpc2label(label)
	char *label;
{
	int linx;
	linx = findsymbol(label, 0);
	if (linx != -1) {
		pc = symbolptr[linx].dclInx;
		if (pc < 0 || pc > pcmax) {
			fatalerror("setpc2label: pc out of core.", "");
		}
	} else {
		DumpSymbolTable();
		fatalerror("Label '%s' not found.\n",label);
	}
}

interpret() {
	opfunptr func;
	int i;
	short op, operand;
	
	for (;;) {	/* execute until opReturn */
#ifdef OBJECTS
		TaskYield();
#endif
		for (i=0; i<regn; ++i) reg[i] = -1;		/* clear registers */
		regn = 0;
		
		op = core[pc].opcode;
		if (op < 0 || op >= opNumOpcodes) {
			fatalerror("Illegal opcode.\n","");
		}
		if (op == opReturn) {
			i = 0;
			return;
		}
		func = opcodetable[op].opfunc;
		newpc = pc + 1;		/* set newpc to default */
		if (opcodetable[op].numwords == 2) {
			operand = core[pc].operand;
			(*(opfunptr1)func)(operand);
		} else {
			(*func)();
		}

#ifdef DEBUGINTERP
		tracelevel = 3;
#endif
        if (tracedisplay == 0)  {	/* write to results file & screen */
            traceinstruction(fp);
            traceinstruction(stderr);
        } else if (tracedisplay == 1) {		/* write only to results file */
            traceinstruction(fp);
        } else if (tracedisplay == 2) {		/* write only to screen */
            traceinstruction(stderr);
        }
		pc = newpc;	/* increment program counter */
	}
}

void
GoTo(symInx)
	short symInx;
{
	
	if (badsym(symInx)) {
		fatalerror("GoTo: Bad symbol index.", "");
	}
	newpc = symbolptr[symInx].dclInx;
	if (newpc < 0 || newpc > pcmax) {
		fatalerror("GoTo: pc out of core.", "");
	}
}

void
FuncCall(symInx)
	short symInx;
{
	long returnaddress, stackframe;
	
	int addr;

	if (badsym(symInx)) {
		fatalerror("FuncCall: Bad symbol index.", "");
	}
	addr = symbolptr[symInx].dclInx;
	if (addr < 0) { /* function is a built in */
		short binx;
		if (addr == -1) {
			int i;
			char *name;
			name = NameOf(symInx);
			for (i=0; i<fnNumFns; ++i) {
				if (strcmp(builtins[i].name, name) == 0) {
					symbolptr[symInx].dclInx = - 2 - i;
					break;
				}
			} 
			if (i == fnNumFns) {
				fatalerror("Function %s called but not defined.", name);
			}
			binx = i;
		} else {
			binx = - addr - 2;
		}
		(*builtins[binx].func)();
	} else {	
		stackframe = decls->numrecs;
		returnaddress = pc + 1;
		pc = addr;
			interpret();
		newpc = returnaddress;
		DropStackFrame(stackframe);
	}
}

void
Trace0() { tracelevel = 0; }

void
Trace1() { tracelevel = 1; }

void
Trace2() { tracelevel = 2; }

void
Trace3() { tracelevel = 3; }

void
Trace4() { 
	tracelevel = 4; 
}

void
Label() { }

void
If() {
	long in1;
	in1 = popval();
	if (!ValueOf(in1)) newpc = pc + 2; 
	FreeNodeList(in1);
}

void
IfNot() {
	long in1;
	in1 = popval();
	if ( ValueOf(in1)) newpc = pc + 2;
	FreeNodeList(in1);
}

mycompile(s)   /* compile model */
	char *s;
{
	FILE *tfp;
	int retc;

	if((tfp= fopen(s,"r")) == NULL) /* try to open the file */
		fatalerror("Model file %s Does Not Exist\n",s);
	else
		fclose(tfp);

	xprintf(stdout, "Compiling %s\n",s);
	if (getenvint("ftn")) {
		ftn = 1;
		fp_asm = fopen("FTN","w");  /* open listing file */
	}
	retc = compile(s); /* compile from file s */

	if (ftn) fclose(fp_asm);
}

long NodeListLen();
extern long vstackptr;
extern long *valuestack;
extern long exportstack[];
extern int exportptr;  /* export stack pointer */

AnalyzeDatumPool(flag) 
	int flag;
{
	long declcount, stakcount, expcount, lostcount, i, len;
	return;
	xprintf(stdout, "!");
	declcount = 0;
	for (i=0; i<decls->numrecs; ++i) {
		declcount += NodeListLen(declareptr[i].list);
	}
	stakcount = 0;
	for (i=0; i<vstackptr; ++i) {
		stakcount += NodeListLen(valuestack[i]);
	}
	expcount = 0;
	for (i=0; i<exportptr; ++i) {
		expcount += NodeListLen(exportstack[i]);
	}
	lostcount = nodes->numused-stakcount-declcount-expcount;
	if (flag || lostcount) {
		xprintf(stdout, "Declared:\n");
		for (i=0; i<decls->numrecs; ++i) {
			len = NodeListLen(declareptr[i].list);
			xprintf(stdout, "  %s : %ld\n", NameOf(declareptr[i].symInx), len);
		}
		xprintf(stdout, "num in stack = %ld\n", stakcount);
		xprintf(stdout, "num declared = %ld\n", declcount);
		xprintf(stdout, "num exported = %ld\n", expcount);
		xprintf(stdout, "numlost      = %ld\n", lostcount);
		DumpPool(stderr, nodes);

		if (lostcount) {
			inanalyzepool = 1;
			fatalerror("Data has been lost.\n","");
		}

		
	}
}

