
/*
	GaussFit - A System for Least Squares and Robust Estimation

	Source Code Copyright (C) 1987 by William H. Jefferys,
	Michael J. Fitzpatrick and Barbara E. McArthur
	All Rights Reserved.
*/

/*
**	COMPILE  - routine for ML.  At first writing only basic routines to
**	     satisfy YACC but easily modified to do more.
**
**
**	Programming begun 09/11/1986  by Mike Fitzpatrick
**
**	MODIFICATION HISTORY:
**
*/


#include <stdio.h>
#include <ctype.h>
#include <math.h>
#include "symtab.h"
#include "compile.h"
#include "def.h"
#include "array.h"
#include "datum.h"
#include "simpledefs.h"
#include "protoplasm.h"
#include "declare.h"
#include "symboltable.h"
#include "opcodes.p"
#include "machine.h"
#include "alloc.h"



#define VARIABLE      264		/* TOKEN values we need */
#define FUNC_NAME     259
#define OP            512
#define Y_LP          267
#define Y_RP          268
#define Y_LB          269
#define Y_RB          270
#define Y_LC          271
#define Y_RC          272
#define Y_COM	      273
#define Y_SMC         274
#define Y_CLN         275

extern int ftn;         /* print out ftn file  */
extern FILE *fp,*fpc,*fpcv;
extern FILE *fp_asm;

int cur_fd;		/* current file descriptor 	*/

extern short cur_func;		/* current function 	*/
extern int line_number;		/* current line number		*/
extern int label_stack[]; 	/* stack of labels 		*/
extern int lstack_index; 	/* index to label stack		*/
extern int yytstk_index;	/* index of yytext stack        */
extern int label_counter;	/* counter to insure unique labs*/
extern struct tlist *tlist;	/* token list for error recovery*/
FILE *yyin, *yyout;             /* stdin and out for lexyy.h    */
int NLOFILE;			/* NULL output file		*/

IncludeFile *fl_stack[NUMINCLUDES];	/* error recovery list		*/
IncludeFile fl_list[NUMINCLUDES];	/* error recovery list		*/
int numincls = 0;


/*
**  COMPILE() - This is the entry point for the compiler.  Only 
**  input requireed is an input and output file name.  All other
** routines are called from here.
**
**  RETURNS: A struct containg the error message and line number
** 	     if an error was found, otherwise NULL
*/

int compile(ifname)
	char *ifname;
{
	char arg[30];
		    
	if(!init(ifname)) return -1;	/* Initialize   */
	
	InitSymbolTable();   /* initialize symbol table */
	initcorespace();
	/* Let's get started on output */
	findsymbol(wordfill(""),1);
	findsymbol(wordfill("_Globals"),1);
	findsymbol(wordfill("_EndOfGlobals"),1);
	print_instr(opLabel,"_Globals");
	
    yyin = stdin;
	yyout = stdout;
	if(!yyparse()) {		/* call parser and begin compilation */
		cur_fd = -1;
		cur_func = -1;
		print_instr(opLabel, "_EndOfGlobals");
		print_instr(opReturn, "");
		cleanup();
		SymbolTablePostCompileCleanup();
		initstacks();
		InitDatumPool();  /* initialize list structure */
		InitDeclareStack();
		InitFileVarTable();   /* initialize file var table */
		return 0;
	} else {
		if (ftn) {
			xprintf(fp_asm,
			"\"%s\":line %d: Syntax error\n",
			fl_stack[cur_fd]->fname,
			fl_stack[cur_fd]->line_number); /* print message */
		}
		xprintf(stderr,
			"\"%s\":line %d: Syntax error\n",
			fl_stack[cur_fd]->fname,
			fl_stack[cur_fd]->line_number); /* print message */
		fatalerror("","");
	}
}



/*
**  INIT() - Initialize all file names and variables for compilation
**
**  RETURNS: 0		if unable to initlialize
**	     1		initialization went OK
*/

init(ifname) 
char *ifname;
{
	struct tlist *init_token_list();

			/* Initialize File pointer to input */
	cur_fd = 0;
	if((fl_list[numincls].file = fopen(ifname,"r")) == NULL) {
		xprintf(stderr,"Unable to open input file  %s\n",ifname);
		return(0);
	}
	fl_list[numincls].fname = wordfill(ifname);
	fl_list[numincls].line_number = 1;
	fl_stack[cur_fd] = fl_list + numincls;
	numincls++;
	
	init_table(); 			/* Initialize symbol_table */
	tlist = init_token_list(); 	/* Initialize token list   */
	
	return(1);
}


yyerror(s)		/* Standard YACC error print */
	char *s;
{
	xprintf(stderr,"%s\n",s);
}


char *
itos(n)		/* Convert integer to character pointer */
	int n;
{
	char *s;
	long k;
	int m;
	
	if (n<0) m = -n;
	else m = n;
	if (m<10) k = 2;
	else if (m<100) k = 3;
	else if (m<1000) k = 4;
	else if (m<10000) k = 5;
	else if (m<100000) k = 6;
	else if (m<1000000) k = 7;
	else if (m<10000000) k = 8;
	else if (m<100000000) k = 9;
	else if (m<1000000000) k = 10;
	else k = 11;
	if (n<0) k++;
	s = (char *)MemAlloc("itos", k);
	sprintf(s,"%d",n);
	return((char *)s);
}


/* 
** RECOVER  -  Perhaps the most intelligent routine yet 8-) Here we look at the
** token list and attempt to figure out what happened.  Recovery involves only
** detecting the type of error and printing a message.
*/

recover()
{
	extern struct tlist *tlist;
	struct tlist *tp;
	extern int paren_level;
	extern char errmess[80];
	int i;


	/*  Print out filename and line number where error encountered */
	xprintf(stderr,"\"%s\", line %d: ",
		fl_stack[cur_fd]->fname, fl_stack[cur_fd]->line_number);
	
	tp = tlist;		/* set token pointer */
	
	/* Try to figure out what type of error it was */
	if ((i=ftn_err(tp))) {
		sprintf(errmess, "Syntax error: '%s' is Fortran syntax",
			tlist_symbol(tlist,i));
	} else if( paren_level < 0) {
		sprintf(errmess,"Missing left parenthesis");
	} else if(paren_level >0) {
		sprintf(errmess,"Missing right parenthesis");
	} else if(tlist_token(tp,2)==FUNC_NAME && 
		(tlist_token(tp,1)==Y_RP||tlist_token(tp,1)==Y_COM)){
		sprintf(errmess,
			"Illegal use of function or undeclared variable: '%s'",
			tlist_symbol(tlist,2));
	} else if(tlist_token(tp,1)==Y_RB && tlist_token(tp,2)==Y_LB) {
		sprintf(errmess,
			"Missing subscript in vector: '%s'",
			tlist_symbol(tlist,3));
	} else if(tlist_token(tp,2)==FUNC_NAME && tlist_token(tp,1)!=Y_LP){
		sprintf(errmess,"Undeclared variable '%s'",
			tlist_symbol(tlist,2));
	} else if(tlist_token(tp,1)==Y_LC && tlist_token(tp,4)==FUNC_NAME) {
		sprintf(errmess,"Syntax error - missing bracket?");
	} else if(tlist_token(tp,1) != OP) {
		sprintf(errmess,
			"Missing operand or semicolon near symbol '%s'",
			tlist_symbol(tlist,1));
	} else {
		sprintf(errmess, "Syntax error at or near symbol '%s'",
			tlist_symbol(tp,2));
	}
	
	yyerror(errmess);	/* output the message to the screen */
	xfflush(stderr);
}



/*
**  TLIST_TOKEN - Return the token of tlist-nback tokens in the
**  token list.
*/

tlist_token(tlist,nback)     	      /* Return token value of (tlist-nback) */
struct tlist *tlist;		      /* pointer to current structure        */
int nback;			      /* number back from current tlist      */
{
struct tlist *tp;
register int i;

for (i=0, tp = tlist; i<nback; i++) tp = tp->back;
return(tp->token);
}


/*
**  TLIST_SYMBOL - Return the symbol name of tlist-nback tokens in the
**  token list.
*/

char *tlist_symbol(tlist,nback)       /* Return symbol name of (tlist-nback) */
	struct tlist *tlist;		      /* pointer to current structure        */
	int nback;			      /* number back from current tlist      */
{
	struct tlist *tp;
	register int i;
	
	tp = tlist;
	for(i=0;i<nback;i++) 
		tp = tp->back;
	return(tp->symbol);
}



/*
**  FTN_ERR - Check a dictionary to see if the symbol that caused the error
**  is perhaps a fortran statement.  Loop back through 4 tokens and check the
**  symbols.  
**
**  Returns:  The number back in the token list.
**
*/

ftn_err(tp)
struct tlist *tp;
{
	extern struct tlist *tlist;
	register int i,j;
	char buf[16];
	static char *ftn_dict[] = { 
			"assign", 	"call", 	"common",
			"continue",	"dimension", 	"do",
			"entry", 	"equivalence", 	"external",
			"function", 	"goto", 	"implicit",
			"open", 	"pause", 	"print",
			"program", 	"read", 	"rewind",
			"save", 	"stop", 	"subroutine",
			"write"
			};
	static int num_dict = 22;

	for(i=1;i<=4;i++) {	              /* Loop through the tokens */
strcpy(buf, tlist_symbol(tp,i));	       /* Get the symbol */
j = 0;
while(buf[j] != '\0') {		/* convert to lower case */
if(isupper(buf[j])) buf[j] = tolower(buf[j]);
j++;
}
for(j=0; j<num_dict; j++) {		 /* Check the dictionary */
if(strcmp(buf, ftn_dict[j]) == 0)
return (i);	      	        /* found one!!!! */
}
}

return(0);				        /* nothing found */
}




/* 
**  RECERROR() - For a specific error that was trapped print the message
**  in the same format as RECOVER().
*/

recerror(s,a1,a2)
char *s, *a1, *a2;
{
	extern char errmess[];

	/*  Print out filename and line number where error encountered */
	xprintf(stderr,"\"%s\", line %d: ",
		fl_stack[cur_fd]->fname,fl_stack[cur_fd]->line_number);
	
	sprintf(errmess,s,a1,a2);
	yyerror(errmess);
}


/*  
**  LABGEN  -  Generate a label for the stack.  The top value is changed in the
**  grammar but label values are always increasing and so always unique
*/
labgen(top,number)
int *top, number;
{
	register int i;
	for(i=0;i<number;i++) {
		label_stack[*top] = label_counter;     
                label_counter++;     
		(*top)++;
	}
}

/*  
**  PROCESS_TOKEN  -  Add the token to TLIST and return it to the parser
*/

process_token(symbol,TOKEN)
	char *symbol;
	int TOKEN;
{

	tlist->token = TOKEN;

	if (tlist->symbol) {
		freemem ("tlist symbol", tlist->symbol);
		tlist->symbol = NULL;
	}

	tlist->symbol = wordfill(symbol);
	tlist = tlist->next;		/*increment tlist pointer */
	/*DumpTlist();*/
	
	return(TOKEN);
}





/*
**   CLEANUP - Free all of the pointers allocated thus far.  And reset whatever
**   needs it for the next time.
**
**   Currently freed pointers include:
**		symtab		tlist
**		yytext_stack	func_stack
*/

cleanup()
{
	extern struct nlist *symtab;
	extern struct tlist *tlist;
	struct nlist *np, *np0;
	struct tlist *tp, *tp0;

	np = symtab;		
	while(np != NULL) {	/* Now kill of the symtab  */
		if (np->sname) freemem("symtab name", (char *)np->sname);
		np0 = np;
		np = np->next;
		freemem("nlist", (char *)np0);
	}
	
	tp = tlist;			/* First delete token list */
	tp->back->next = NULL;		/* sever circular link     */
	while(tp!=NULL) {		/* do the rest of the list */
		if (tp->symbol) freemem ("tlist symbol", (char *)tp->symbol);
		tp0 = tp;
		tp = tp->next;
		freemem("tlist", (char *)tp0);
	}
	symtab = NULL;
	tlist = NULL;
}


DumpTlist() {
	struct tlist *tp;
	tp = tlist;			/* First delete token list */
	do {		/* do the rest of the list */
		if (tp->symbol) {
			xprintf(stdout, "   %12s...%3d\n", tp->symbol, tp->token);
		} else {
			xprintf(stdout, "   %12s...%3d\n", "NULL", tp->token);
		}
		tp = tp->next;
	} while(tp!=tlist) ;
}

/*
**  ECHO() - DEBUG PRINT
*/

echo(yyt)
char *yyt;
{
#ifdef MLDEBUG
	xprintf(stderr,"yytext = :%s:\n",yyt);
	xfflush(stderr);
#endif
}




