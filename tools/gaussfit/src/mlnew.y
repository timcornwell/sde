/*
**	GAUSS - A System for Least Squares and Robust Estimation
**
**	Source Code Copyright (C) 1987 by William H. Jefferys,
**	Michael J. Fitzpatrick and Barbara E. McArthur
**	All Rights Reserved.
*/


/*
**
**	ML - Written as a  yacc grammar to convert a
**	     C-like input language to a stack-interpreter language.  In-
**           tended to look as much like C as possible but has no I/O	
**	     facility. (yet)
**
**	Program started 9/9/86 by Mike Fitzpatrick
**
**      MODIFICATION HISTORY:
**		- Author showing suicidal tendencies 9/18/86
**		- Expressions implemented successfully 10/27/86
**		- Symbol table lookup begun 11-3-86
**		- array indices/subscript added 11-6-86
**		- all loops completed 11-9-86
**		- if-else constructs added 11-11-86
**		- declarations added to grammar 12-1-86
**		- unary plus implemented 12-1-86
**		- if-else constructs fixed 12-??-86
**
**
*/


%{
#include "Array.h"
#include "Datum.h"
#include "simpledefs.h"
#include "SymbolTable.h"
#include "defines.h"
#include "alloc.h"
#include <stdio.h>
#include <ctype.h>
#include "Prototypes.p"
#include "strings.h"
#include "def.h"
#include "compile.h"
#include "Opcodes.p"
#include "Machine.h"
#include "symtab.h"

#define  Echo    echo(yytext)
#define  OP	 512

FILE *fd_list[10];			/* Max no. of include files - fd    */
					/* kept in a stack for convenience  */
extern FILE *fd_out;			/* Output file descriptor           */
extern IncludeFile *fl_stack[NUMINCLUDES];	/* error recovery list		*/
extern IncludeFile fl_list[NUMINCLUDES];	/* error recovery list		*/
extern int numincls;				/* current working file descriptor  */
extern int cur_fd;				/* current working file descriptor  */
int colnum = 1;				/* column no. in input - not used   */
int nindices = 0;			/* number of array indices			*/
struct nlist *np;			/* Symbol table entry pointer       */
char errmess[120];			/* Error message buffer             */
/* char *poptxt(), *toptxt(); */
char *Index();
char *strbuf;				/* Generic string buffer	    */


	/*
	**  NOTES ON THE SYMBOL TABLE:
	**	The symbol table for the compiler is implemented as a linked
	**  list of structures containing the name and value of the token as
	**  well as pointers to maintain the list.  Initially all reserved
	**  words are inserted into the symtab with the appropriate token.
	**  New declared variables or functions are inserted and an entry is
	**  made as to their bracket level (with global vars and functions
	**  having a bracket level of zero).  After exit from each  bracket
	**  level names in the symtab with a level higher than the one just 
	**  exited are deletd from the symbol table.  This assures that locally
	**  declared variabled get used correctly.
	**
	**	The pointer 'symtab' contains a pointer to the first entry in 
	**  the table.  Likewise 'symtab_last' is a pointer to the last entry.
	**  Both of these are updated as needed.
	*/

extern struct nlist *symtab, 		/* Start pointer to symbol table    */
                    *symtab_last;	/* Tail pointer to symbol table     */
extern struct sysw syswords[];		/* Array containing reserved word   */
					/* structures.			    */
extern struct tlist *tlist;		/* Current pointer in token list    */

struct fstack{				/* Characteristics of each function */
	char *funcnm;			/* are kept to assure that arg count*/
	int argcnt;			/* will be correct                  */
};

struct fstack func_stack[MAX_STACK_SZ];	/* Stack of function names          */
  				        /*   encountered in each function   */
struct fstack *fsp, *get_func();	/* Get the no. of args to this func */
char *funcname;				/* Generic buffer for function names*/

char *yytext_stack[MAX_STACK_SZ]; 	/* Keep track of text found in      */
				    	/* analyzer so we can output it.    */
int label_stack[MAX_STACK_SZ];	  	/* Label stack - not yet implemented*/
int fstack_index = -1;		  	/* func_stack index 		    */
int yytstk_index=0;		  	/* yytext_stack index 		    */
int lstack_index=0;		  	/* label_stack index 		    */
int label_counter = 10001;	  	/* make sure we make unique labels  */
int line_number = 1;		  	/* current line number on input     */
int npar[10];		/* # of parameters we've seen so far 		    */
int inpar = 0;		/* array counter for npar 			    */
int paren_level = 0;	/* # of parenthese encountered: +1 for (, etc       */
int bracket_level = 0;	/* # of brackets encountered: +1 for {, etc .       */

int inargs = 0;		/* in the arglist ? */
int saw_nl = 0;		/* seen a newline yet?? 			    */
int saw_param = 0;	/* seen PARAMETER keyword yet? 			    */
int end_globals = 0;	/* end of global declarations 			    */
int par_type = 0;	/* type of parameter declaration 		    */


/*  ERROR MESSGES - (not complete, see 'recover' in compile.c) */
char *undecl_err =
	"Variable '%s' has not been declared.";
char *already_decl_err =
	"Variable '%s' previously declared at this level.";
char *arg_count_error = 
	"Function '%s' previously called with %s arguments";
char *arg_type_error = 
	"Variable '%s' is of wrong type to be initialized.";
char *bad_lhs_error =
	"Invalid data type variable '%s' on LHS of equation.";
char *bad_vec_error =
	"DATA or OBSERVATION variable types may not be indexed/dimensioned";
char *decl_level_error =
	"Variable declarations are not allowed in statement blocks. ";
char *indx_type_error =
	"Only PARAMETER or CONSTANT variable types may be indexed. ";
char *mult_index_error =
	"Indented vectors are not legal on LHS of statement ";
char *o_indx_type_error =
	"Variable '%s' is of wrong type to be subscripted with '%s'";
char *twod_nyi_error =
	"Two-dimensional arrays are not yet implemented. ";
char *varbl_type_error =
	"Only type VARIABLE is allowed to be explicitly dimensioned. ";
char *wrong_num_indices_error =
	"Reference of '%s' uses different number of indices than declared.";


%}


%start block

%token DIGIT 			/* [0-9] */
%token LETTER			/* [a-zA-Z] */

%token CONST     		/* constant */

%token IDENTIFIER

%token Y_NEWLINE		/* newline character */
%token Y_STRING   		/* character constant */
%token Y_LP			/* ( */
%token Y_RP			/* ) */
%token Y_LB			/* [ */
%token Y_RB			/* ] */
%token Y_LC			/* { */
%token Y_RC			/* } */
%token Y_COM			/* , */
%token Y_SMC			/* ; */
%token Y_CLN			/* : */
%token EOFILE			/* EOF in current input file - not used */
%token EQUALS

				/* SYSTEM WORDS */
%token IF, ELSE, ELSEIF, BREAK, CONTINUE, FOR, DO, WHILE, UNTIL, RETURN
%token PARAMETER  ,OBSERVATION
%token DATUM ,VARIABL, INCLUDE, LOOP, CONSTANT, INDEX, FUNC_NAME

				/* ESTABLISH PRECEDENCE RULES */
%right	'='    '+='    '-='    '*='    '/='    '^='  	/* +=, -=, etc.	*/
%left	YOP_OR
%left	YOP_AND
%left	YOP_EQ YOP_NE 
%left	YOP_LT YOP_GT YOP_LE YOP_GE
%left	YOP_PLUS  YOP_MINUS 
%left	YOP_MUL  YOP_DIV  
%right  YOP_EXP
%right	YOP_INCR
%right	YOP_NOT UPLUS UMINUS	/* supplies precedence for unary minus	*/

%token ENVIR

%%

block :    func block  
	|  func  EOFILE
	|  func 
	|  error { yyerrok;	} 
	;

func	:  declarations  func_decl compound_stmt
		{
			extern short cur_func;
			print_instr(opReturn, "");
			print_instr(opLabel,itos(label_stack[--lstack_index]));
			wipelocalindices();
			cur_func = findsymbol("_Globals", 0);
		}
	;

declarations :  opt_decl
	;

opt_decl:  decl_list
	|
	;

decl_list: declaration
	|  decl_list declaration
	;

declaration: par_decl_type  parg_list Y_SMC
		{ saw_param = 0;	}
	;

par_decl_type:   
	   PARAMETER         {  if(bracket_level > 1) {
				    recerror(decl_level_error,"","");
				    YYABORT;
				} 
				saw_param++;  
				par_type = PARAMETER;  
			     }
	|  OBSERVATION       {  if(bracket_level > 1) {
				    recerror(decl_level_error,"","");
				    YYABORT;
				} 
				saw_param++;  
				par_type = OBSERVATION;
			     }
	|  DATUM             { if(bracket_level > 1) {
				    recerror(decl_level_error,"","");
				    YYABORT;
				} 
				saw_param++;  
				par_type = DATUM;      
			     }
	|  VARIABL           {  if(bracket_level > 1) {
				    recerror(decl_level_error,"","");
				    YYABORT;
				} 
				saw_param++;  
				par_type = VARIABL;    
			     }
	|  CONSTANT	     {  if(bracket_level > 1) {
				    recerror(decl_level_error,"","");
				    YYABORT;
				} 
				saw_param++;  
				par_type = CONSTANT;   
			     }
    |  ENVIR         {  if(bracket_level > 1) {
                    recerror(decl_level_error,"","");
                    YYABORT;
                }
                saw_param++;
                par_type = ENVIR;
                 }
	;


parg_list: parg
	|  parg Y_COM parg_list
	;

param1	: IDENTIFIER
		{
			char *p;
			p = toptxt();
			if ((np = lookup(p)) == NULL) {
				insert(p, par_type); 
			} else {
				if (np->blevel == 0 && np->par_type == INDEX
					&& bracket_level == 0 && (par_type == PARAMETER
					|| par_type == CONSTANT || par_type == DATUM)) {
	
					np->tokval = IDENTIFIER;
					np->par_type = par_type;
					np->blevel = bracket_level;
				} else if (np->blevel == bracket_level) {
					recerror(already_decl_err, p, "");
					YYABORT;
				}
			}
		}
	;

parg	:  param1
		{    /* RULE:  parg:  param1      		   */

		     /* Declare the variable with appropriate type */
		     parm_decl(par_type);
		}
	|  param1 Y_LB decl_index_list Y_RB
		{   /* RULE:  parg  |  param1 Y_LB decl_index_list Y_RB   */
			int symInx;
			char *p;
		    /*  Handle an indexed variable		   */
					/* Only allow certain types*/
			if(par_type != PARAMETER && par_type != CONSTANT) {
				if(par_type == DATUM || par_type == OBSERVATION)
					recerror(bad_vec_error,"","");
		     	else
					recerror(indx_type_error,"","");
				YYABORT;   /* print error and die		   */
			}
			p = poptxt();
			if(par_type == PARAMETER) {       /* PARAMETER index*/
				print_instr(opDefPar,p);
			} else if (par_type == CONSTANT) {/* CONSTANT index */
				print_instr(opDefConst,p);
			}
			symInx = findsymbol(p, 1);
			if(bracket_level == 0) {
				symbolptr[symInx].globalindices = nindices;
			} else {
				symbolptr[symInx].localindices = nindices;
			}
			symbolptr[symInx].filevar = par_type;
			nindices = 0;
		}
		
	|  param1 Y_LB decl_const_index_list Y_RB
		{   /* RULE:   parg  |  param1 Y_LB CONST Y_RB*/
			int symInx;
			char *p;
			if(par_type != VARIABL) {
				if(par_type == DATUM || par_type == OBSERVATION)
					recerror(bad_vec_error,"","");
		     	else
		       		recerror(varbl_type_error,"","");
		   		YYABORT;
			}
		    /* Define a vector variable			*/
		    p = poptxt();
		    print_instr(opDefVec, p);
			symInx = findsymbol(p, 1);
			if(bracket_level == 0) {
				symbolptr[symInx].globalindices = nindices;
			} else {
				symbolptr[symInx].localindices = nindices;
			}
			symbolptr[symInx].filevar = par_type;
			nindices = 0;
		} 
		
	|  param1 EQUALS constref
		{  /* RULE:   parg  |  param1 EQUALS constref     */
			int symInx;
			char *p;
			
		   /* Allow type param1 to be initialized      */
		 p = poptxt();
		 if(par_type != VARIABL) {  /* Wrong type for this */
		     recerror(arg_type_error,p,"");
		     YYABORT;    /* print message and die   */
	         }

		print_instr(opDefVar,p);
		symInx = findsymbol(p, 1);
		if(bracket_level == 0) {
			symbolptr[symInx].globalindices = 0;
		} else {
			symbolptr[symInx].localindices = 0;
		}
		symbolptr[symInx].filevar = par_type;
		print_instr(opStore,p);
		}
	|  param1 EQUALS negconstref
		{  /* RULE:   parg  |  param1 EQUALS constref     */
			int symInx;
			char *p;
			
		   /* Allow type param1 to be initialized      */
		 p = poptxt();
		 if(par_type != VARIABL) {  /* Wrong type for this */
		     recerror(arg_type_error,p,"");
		     YYABORT;    /* print message and die   */
	         }

		print_instr(opDefVar,p);
		symInx = findsymbol(p, 1);
		if(bracket_level == 0) {
			symbolptr[symInx].globalindices = 0;
		} else {
			symbolptr[symInx].localindices = 0;
		}
		symbolptr[symInx].filevar = par_type;
		print_instr(opStore,p);
		}
	;


dindex : IDENTIFIER
		{
			char *p;
			p = toptxt();
			if ((np = lookup(p)) == NULL) {
				insert(p, INDEX); 
			}		
		}
	;

decl_index_list : dindex			/* Handle index variables */
		{
		   print_instr(opDefIndex,poptxt());
		   nindices = 1;
		}
				|  dindex Y_COM dindex
		{
		   print_instr(opDefIndex,poptxt());
		   print_instr(opDefIndex,poptxt());
		   nindices = 2;
		}
				|  dindex Y_COM dindex Y_COM dindex
		{
		   print_instr(opDefIndex,poptxt());
		   print_instr(opDefIndex,poptxt());
		   print_instr(opDefIndex,poptxt());
		   nindices = 3;
		}
				|  dindex Y_COM dindex Y_COM dindex Y_COM dindex
		{
		   print_instr(opDefIndex,poptxt());
		   print_instr(opDefIndex,poptxt());
		   print_instr(opDefIndex,poptxt());
		   print_instr(opDefIndex,poptxt());
		   nindices = 4;
		}
				|  dindex Y_COM dindex Y_COM dindex Y_COM dindex Y_COM
		{
			recerror("Too many indices","","");
		}
	;
	
decl_const_index_list : /* Handle const index variables */
				CONST	
		{
		   print_instr(opPushDim,poptxt());
		   nindices = 1;
		}
				|  CONST Y_COM CONST
		{
		   print_instr(opPushDim,poptxt());
		   print_instr(opPushDim,poptxt());
		   nindices = 2;
		}
				|  CONST Y_COM CONST Y_COM CONST
		{
		   print_instr(opPushDim,poptxt());
		   print_instr(opPushDim,poptxt());
		   print_instr(opPushDim,poptxt());
		   nindices = 3;
		}
				|  CONST Y_COM CONST Y_COM CONST Y_COM CONST
		{
		   print_instr(opPushDim,poptxt());
		   print_instr(opPushDim,poptxt());
		   print_instr(opPushDim,poptxt());
		   print_instr(opPushDim,poptxt());
		   nindices = 4;
		}
				|  CONST Y_COM CONST Y_COM CONST Y_COM CONST Y_COM
		{
			recerror("Too many indices","","");
		}

	;
	

	/**	   Here we state the rules for how to call/declare user-
	***	defined functions.  Functions may be called with no arguments
	***	or with expressions as arguments.
	**/


func_decl :				/* Function declaration */
	   func_name_decl  Y_LP formal_args  Y_RP 
		{   /*  RULE:  func_del :  func_name_decl Y_LP formal_args Y_RP */
			char *p;

		    fsp = get_func(funcname);
			
			if (fsp->argcnt != npar[inpar] && fsp->argcnt != -1) { /* error in number of args    */
				recerror(arg_count_error,
					(bracket_level!=0?p:funcname),
					(fsp->argcnt<npar[inpar]?"fewer":"more"));
				YYABORT;
			} else if(fsp->argcnt == -1) {   /*  OK  */
				fsp->argcnt = npar[inpar];
			}
			inpar--;
		}
	;

func_call :				/* Function call */
	   func_name_call  Y_LP { inargs=1; } actual_args  Y_RP 
		{   /*  RULE:  func_del :  func_name_call Y_LP actual_args Y_RP */
			char *p;
			inargs = 0;

		    p = poptxt();
		    print_instr(opFuncCall,p);
		    fsp = get_func(p);
		    
			if (fsp->argcnt != npar[inpar] && fsp->argcnt != -1) { /* error in number of args    */
				recerror(arg_count_error,
					(bracket_level!=0?p:funcname),
					(fsp->argcnt<npar[inpar]?"fewer":"more"));
				YYABORT;
			} else if(fsp->argcnt == -1) {   /*  OK  */
				fsp->argcnt = npar[inpar];
			}
			inpar--;
		}
	;

func_name_decl :    IDENTIFIER    
		{   /* RULE:  func_name_decl :  IDENTIFIER                     */

			char *p;
		    /* Here we declare a new function, otherwise it gets
		    ** handle up above
		    */
			p = poptxt();
			if ((np = lookup(p)) == NULL) {
				insert(p, FUNC_NAME);
			}
			
		    fsp = get_func(p);
		    if(fsp == NULL) {  /* not yet seen */
				/* Push it on the func_name stack */	
				++fstack_index;
				func_stack[fstack_index].funcnm = wordfill(p);
				func_stack[fstack_index].argcnt = -1;
			}
		    npar[++inpar] = 0;
			labgen(&lstack_index, 1);
			print_instr(opGoTo,itos(label_stack[lstack_index-1]));
			print_instr(opDefFunc,p);
			funcname = wordfill(p);
		}
	;

func_name_call :    IDENTIFIER    
		{   /* RULE:  func_name_call :  IDENTIFIER                     */
			char *p;
			p = toptxt();
			if ((np = lookup(p)) == NULL) {
				insert(p, FUNC_NAME);
			}
		    fsp = get_func(p);
		    if(fsp == NULL) {    /* not yet seen */
				/* Push it on the func_name stack */	
				++fstack_index;
				func_stack[fstack_index].funcnm = wordfill(p);
				func_stack[fstack_index].argcnt = -1;
			}
		    npar[++inpar] = 0;
		}
	;


arg1 : IDENTIFIER
		{
			char *p;
			p = toptxt();
			if ((np = lookup(p)) == NULL) {
				bracket_level = 1;
				insert(p, VARIABL); 
				bracket_level = 0;
			}	
		}
	;

formal_args  :     
	|  arg1
		{   /* RULE:  formal_args :  arg1   	   */
			char *p;
		    /*  Handle local arg names				   */
			int symInx;
			p = poptxt();
	        print_instr(opDefArg,p);
			symInx = findsymbol(p, 1);
			symbolptr[symInx].cInx = -1;
			symbolptr[symInx].filevar = VARIABL;
			symbolptr[symInx].localindices = -99;			
		    npar[inpar]++;	/* count number of args           */
		}
	|  arg1  Y_COM   formal_args
		{   /* RULE:  formal_args :  arg1  Y_COM   formal_args   	   */
			char *p;
		    /*  Handle local arg names				   */
			int symInx;
			p = poptxt();
	        print_instr(opDefArg,p);
			symInx = findsymbol(p, 1);
			symbolptr[symInx].cInx = -1;
			symbolptr[symInx].filevar = VARIABL;
			symbolptr[symInx].localindices = -99;			
		    npar[inpar]++;	/* count number of args           */
		}
	;

actual_args  :     	/* empty - e.g. no arguments in function call/decl */
	|  expr
		{   /* RULE:  actual_args :  expr Y_COM actual_args   	   */
		    npar[inpar]++;	/* count number of args           */
		}
	|  expr  Y_COM   actual_args
		{   /* RULE:  actual_args :  expr Y_COM actual_args   	   */
		    npar[inpar]++;	/* count number of args           */
		}
	;



	/*
	**     Now we finally get to the rules regarding statements.  
	*/

compound_stmt 
		:  Y_LC  stmt_list Y_RC
			{   /* RULE:  compound_stmt:  Y_LC stmt_list Y_RC */
			    --bracket_level;
			}
	;


stmt_list : 
	|  stmt_list stmt
	;

stmt	:  compound_stmt		/* more recursion for statements */
	|  declaration
	|  stmt1   Y_SMC
	|  WHILE Y_LP 
		{      /* RULE:  stmt  | WHILE Y_LP           		*/
			labgen(&lstack_index,3); /* Initialize labels   */
			print_instr(opLabel,itos(label_stack[lstack_index-3]));
			print_instr(opLabel,itos(label_stack[lstack_index-2]));
	        }
	   expr 
		{      /* RULE:  WHILE.....  expr			*/ 
			print_instr(opIfNot, "");  /* evaluate expression*/
			print_instr(opGoTo,itos(label_stack[lstack_index-1]));
		}
	   Y_RP   stmt
		{      /* RULE:  WHILE.....  Y_RP stmt			*/
			
			print_instr(opGoTo,itos(label_stack[lstack_index-2]));
			print_instr(opLabel,itos(label_stack[lstack_index-1]));

			lstack_index -= 3;
		}
	|  FOR Y_LP 
		{	/* RULE:  stmt |  FOR  Y_LP			*/	
			labgen(&lstack_index,5);  /* Initialize labels	*/
			print_instr(opLabel,itos(label_stack[lstack_index-5]));
		}

	   stmt1  Y_SMC
		{	/* RULE: FOR... stmt1 Y_SMC....			*/	
			print_instr(opLabel,itos(label_stack[lstack_index-4]));
		}
	   expr  Y_SMC
		{	/* RULE: FOR... expr Y_SMC....			*/	
			print_instr(opIfNot, ""); /* evaluate expression */
			print_instr(opGoTo,itos(label_stack[lstack_index-1]));
			print_instr(opGoTo,itos(label_stack[lstack_index-3]));
			print_instr(opLabel,itos(label_stack[lstack_index-2]));
		}
	   stmt1 Y_RP 
		{ 	/* RULE:  FOR....  stmt1 Y_RP...		*/	
			print_instr(opGoTo,itos(label_stack[lstack_index-4]));
			print_instr(opLabel,itos(label_stack[lstack_index-3]));
		}
	   stmt
		{	/* RULE:  FOR....  stmt...			*/	
			print_instr(opGoTo,itos(label_stack[lstack_index-2]));
			print_instr(opLabel,itos(label_stack[lstack_index-1]));
			
			lstack_index -= 5;
		}
	|  doprefix   stmt  WHILE  Y_LP expr  Y_RP  Y_SMC
		{	/* RULE: doprefix stmt WHILE Y_LP expr Y_RP Y_SMC */	

			print_instr(opIfNot, "");  /* evaluate expression  */
			print_instr(opGoTo,itos(label_stack[lstack_index-1]));
			print_instr(opGoTo,itos(label_stack[lstack_index-2]));
			print_instr(opLabel,itos(label_stack[lstack_index-1]));

			lstack_index -= 3;
		}  
	|  doprefix   stmt  UNTIL  Y_LP  expr  Y_RP  Y_SMC
		{	/* RULE: doprefix stmt UNTIL Y_LP expr Y_RP Y_SMC */	

		 	print_instr(opIf, "");  /* evaluate expression  */
			print_instr(opGoTo,itos(label_stack[lstack_index-1]));
			print_instr(opGoTo,itos(label_stack[lstack_index-2]));
			print_instr(opLabel,itos(label_stack[lstack_index-1]));

			lstack_index -= 3;
		}  
	|  ifprefix stmt
		{	/* RULE: stmt | ifprefix stmt			*/	
			print_instr(opLabel,itos(label_stack[lstack_index-2]));
			lstack_index -= 2;
		}
	|  ifelprefix stmt
		{	/* RULE: stmt | ifelprefix stmt			*/	
			print_instr(opLabel,itos(label_stack[lstack_index-1]));
			lstack_index -= 2;
		}
	|  func_call Y_SMC 
		{   	/* RULE: func_del Y_SMC				*/

			/* Straight function call - drop result */
			print_instr(opDrop, "");
		}
	|  RETURN  { inargs = 1; }  expr Y_SMC
		{    	/* RULE:  stmt | RETURN expr Y_SMC		*/
			inargs = 0;
			print_instr(opReturn, "");
		}
	|  BREAK Y_SMC
		{    	/* RULE:  stmt | BREAK Y_SMC			*/
			print_instr(opGoTo,itos(label_stack[lstack_index-1]));
		}
 	|  Y_SMC 
	;


stmta	:  lval1  expr  
		{  /* RULE:  stmt1  | lval1 EQUALS expr */
			char *p;
			/* check for legal LHS */
			p = poptxt();
			print_instr(opStore,p);
		}
	;
	
stmtb	:  lval2  expr  
		{  /* RULE:  stmt2  | lval2 EQUALS expr */
			char *p;
			p = poptxt();
			print_instr(opStoreVec,p);
		}
	;


stmt1 : stmta
	| stmtb
	;



ifprefix:  IF  expr
		{ /* RULE: ifprefix: IF expr				*/
		  
		  /* Begginning to IF-THEN expression  */
		  labgen(&lstack_index,2);
	  	  print_instr(opIfNot, "");
		  print_instr(opGoTo,itos(label_stack[lstack_index-2]));
		}
	;



ifelprefix : ifprefix stmt ELSE
		{ /* RULE: ifelprefix: ifprefix stmt ELSE		*/
		  
		  /* Begginning to IF-THEN-ELSE  expression  */
		  /*lstack_index += 2;   /* restore label_stack */
			print_instr(opGoTo,itos(label_stack[lstack_index-1]));
		 	print_instr(opLabel,itos(label_stack[lstack_index-2]));
		}
	;





doprefix:   do_or_loop
		{	/* RULE:  doprefix : do_or_loop			*/
	
			/* Initialize labels for DO loop		*/
			labgen(&lstack_index,3);
			print_instr(opLabel,itos(label_stack[lstack_index-3]));
			print_instr(opLabel,itos(label_stack[lstack_index-2]));
		}


do_or_loop:  DO
	|    LOOP
	;




	/**	  The following rules establish how to evaluate an 
	*** 	arithmetic expression.  'expr' is also called by other
	***	rules to correctly push function arguments.
	**/


lval1	:   IDENTIFIER EQUALS
		{   /* RULE: lval :  IDENTIFIER			*/
			int symInx;
			char *p;
			
		    p = toptxt();
			np=lookup(p); 
			if (np == NULL) {
				recerror(undecl_err, p, "");
				YYABORT;
			}
			if(np->tokval == IDENTIFIER) {
				if(!is_valid_lhs(np->par_type) ){
					recerror(bad_lhs_error,p,"");
					YYABORT;
				}
			}
			symInx = findsymbol(p, 1);
#ifdef WONTWORK
			if (symbolptr[symInx].localindices != -99 
				&& symbolptr[symInx].filevar == VARIABL) {
				if (symbolptr[symInx].localindices == -1) {
					if (nindices != symbolptr[symInx].globalindices) {
						recerror(wrong_num_indices_error, p,"");
						YYABORT;
					}	
				} else {
					if (nindices != symbolptr[symInx].localindices) {
						recerror(wrong_num_indices_error, p,"");
						YYABORT;
					}	
				}
		    }
#endif
		}
	;
	
lval2	:   IDENTIFIER  Y_LB  index_list  Y_RB  EQUALS
		{   /* RULE: lval :  IDENTIFIER Y_LB index_list Y_RB */
			int symInx;
			char *p;
			
		    p = toptxt();
			np=lookup(p); 
			if (np == NULL) {
				recerror(undecl_err, p, "");
				YYABORT;
			}
			if(np->tokval == IDENTIFIER) {
				if(!is_valid_lhs(np->par_type) ){
					recerror(bad_lhs_error,p,"");
					YYABORT;
				}
			}
			symInx = findsymbol(p, 1);
			if (symbolptr[symInx].localindices != -99) {
				if (symbolptr[symInx].localindices == -1) {
					if (nindices != symbolptr[symInx].globalindices) {
						recerror(wrong_num_indices_error, p,"");
						YYABORT;
					}	
				} else {
					if (nindices != symbolptr[symInx].localindices) {
						recerror(wrong_num_indices_error, p,"");
						YYABORT;
					}	
				}
			} 
			nindices = 0;
			/*
		    if(left_vec && !infunc) {
				recerror(mult_index_error,"","");
				YYABORT;
		    }
		    */
		} 
	;
	
	
ref		:   IDENTIFIER
		{   /* RULE: ref :  IDENTIFIER			*/
			char *p;
			
	    	p = poptxt();
			np=lookup(p); 
			if (np == NULL) {
				recerror(undecl_err, p, "");
				YYABORT;
			}
			if (!inargs) {
				int symInx;
				symInx = findsymbol(p, 1);
				if (symbolptr[symInx].localindices != -99 
					&& symbolptr[symInx].filevar == VARIABL) {
					if (symbolptr[symInx].localindices == -1) {
						if (nindices != symbolptr[symInx].globalindices) {
							recerror(wrong_num_indices_error, p,"");
							YYABORT;
						}	
					} else {
						if (nindices != symbolptr[symInx].localindices) {
							recerror(wrong_num_indices_error, p,"");
							YYABORT;
						}	
					}
				}
			}
			print_instr(opPush,p); 
	    }
	|   IDENTIFIER  Y_LB  index_list  Y_RB 
		{   /* RULE: ref :  IDENTIFIER Y_LB index_list Y_RB */
			int symInx;
			char *p;
			
		    p = poptxt();
			np=lookup(p); 
			if (np == NULL) {
				recerror(undecl_err, p, "");
				YYABORT;
			}
			symInx = findsymbol(p, 1);
			if (symbolptr[symInx].localindices != -99) {
				if (symbolptr[symInx].localindices == -1) {
					if (nindices != symbolptr[symInx].globalindices) {
						recerror(wrong_num_indices_error, p,"");
						YYABORT;
					}	
				} else {
					if (nindices != symbolptr[symInx].localindices) {
						recerror(wrong_num_indices_error, p,"");
						YYABORT;
					}	
				}
			} 
			nindices = 0;
			print_instr(opPushVec,p);
		} 
	;



index_list :   expr 
		{ nindices = 1; }
	|   expr  Y_COM  expr  
		{ nindices = 2; }
	|   expr  Y_COM  expr  Y_COM  expr  
		{ nindices = 3; }
	|   expr  Y_COM  expr  Y_COM  expr  Y_COM  expr  
		{ nindices = 4; }
	;

constref : CONST 
		{ 
			print_instr(opPushCon, poptxt());	
		}
	;

negconstref : YOP_MINUS CONST %prec UMINUS 
		{
			char *strbuf, str[80];
			sprintf(str,"-%s\0", poptxt());
			strbuf = MemAlloc("PushCon", (long)strlen(str)+1);
			strcpy(strbuf, str);
			print_instr(opPushCon, strbuf);
		}
	;

/*
			if(infunc||saw_eql) {
				print_instr(opPushCon, poptxt());	
			}
		}
*/
	


expr	:	expr1
		|	constref 
		|	ref
	;



expr1	:	Y_LP expr Y_RP
		{    $$ = $2;	}
	|   expr  YOP_PLUS  expr		/* addition */
		{   print_instr(opAdd, ""); 
		}  
	|   expr  YOP_MINUS  expr		/* subtraction */
		{   print_instr(opSubtract, "");
		}  
	|   expr  YOP_MUL  expr			/* multiplication */
		{   print_instr(opMultiply, "");
		}  
	|   expr  YOP_DIV  expr			/* division */
		{   print_instr(opDivide, "");
		}  
	|   expr  YOP_EXP  expr			/* exponentiation */
		{   print_instr(opPower, "");
		}  
	|   YOP_MINUS  expr	%prec UMINUS	/* negation */
		{   $$ =   - $2;  print_instr(opNegate, "");
		}
	|   YOP_PLUS  expr	%prec UPLUS	/* unary plus */
		{   $$ =   $2;  
		}
	|    expr  YOP_EQ   expr		/* equality */
		{   print_instr(opEqualsFn, "");
		}
	|    expr  YOP_NE   expr		/* inequality */
		{   print_instr(opNotEqualsFn, "");
		}
	|    expr  YOP_GT   expr		/* greater than */
		{   print_instr(opGreaterThanFn, "");
		}
	|    expr  YOP_LT   expr		/* less than */
		{   print_instr(opLessThanFn, "");
		}
	|    expr  YOP_GE   expr		/* greater than or equal */
		{   print_instr(opGreaterOrEqualFn, "");
		}
	|    expr  YOP_LE   expr		/* less than or equal */
		{   print_instr(opLessOrEqualFn, "");
		}
	|    expr  YOP_OR   expr		/* logical OR */
		{   print_instr(opOrFn, "");
		}
	|    expr  YOP_AND   expr		/* logical grammar */
		{   print_instr(opAndFn, "");
		}
	|    YOP_NOT   expr	 %prec YOP_NOT  	/* logical negation */
		{   print_instr(opNotFn, "");
		}
	|  func_call				/* function call */
	;





%%


	/*  
	**  Here we parse the program and return tokens.  Token definitions
	**  are given above.  We first search for a character string and see
	**  if it's either a function name or a system_word.  
	**
	*/

#include "lexyy.h"


dumptxtstak() {
   	int i;
   	printf("\nyytstk_index = %d\n",yytstk_index);
   	printf("\nyytext = '%s'\n",yytext);
   	for (i=0; i<yytstk_index; ++i) {
   		printf("%d '%s'\n",i,yytext_stack[i]);
   	}
   	printf("\n");
}

char *
poptxt() {
	if (--yytstk_index < 0) {
		dumptxtstak();
		DumpTlist();
		fatalerror("YYTEXT EMPTY\n","");
	}
	return yytext_stack[yytstk_index];
}

char *
toptxt() {
	if (yytstk_index-1 < 0) {
		dumptxtstak();
		DumpTlist();
		runtimeerror("YYTEXT EMPTY\n","");
	}
	return yytext_stack[yytstk_index-1];
}

pushtxt(p) 
	char *p;
{
	yytext_stack[yytstk_index] = p;
	yytstk_index++;
}



/*
**  YYWRAP - Override YACC function.  When and EOF is encountered handle
**  file stack or return code to end input processing.
*/

yywrap()
{

	fclose(fl_stack[cur_fd].file);		/* for include files     */
	cur_fd--;

	return( (cur_fd>=0 ? 0 : 1) );
}


/* 
**  GET_FUNC() - Search the function stack for the given name and return the 
**  struct.  Otherwise return a NULL.
*/
struct fstack *get_func(name)
char *name;
{
	register int i;

	for(i=0; i<=fstack_index; i++) {
		if( strcmp(name,func_stack[i].funcnm) == 0 )
			return(&(func_stack[i]));
	}
	return((struct fstack *)NULL);
}




/*
**  PARM_DECL() - For a given delaration type print the proper instruction
**  and argument.
*/

parm_decl(type)
short type;
{
	int symInx;
	char *p;

	p = poptxt();
	switch(type) {
	    case PARAMETER:
		    print_instr(opDefPar,p); 
		    break;
	    case OBSERVATION:
		    print_instr(opDefObs,p); 
		    break;
	    case DATUM:
		    print_instr(opDefDat,p); 
		    break;
	    case VARIABL:
		    print_instr(opDefVar,p); 
	    	    break;
	    case CONSTANT:
		    print_instr(opDefConst,p); 
	    	    break;
		case ENVIR:
			print_instr(opDefEnvir,p);
				break;

	    default:
		    break;
	}
	symInx = findsymbol(p, 1);
	if(bracket_level == 0) {
		symbolptr[symInx].globalindices = 0;
	} else {
		symbolptr[symInx].localindices = 0;
	}
	nindices = 0;
	symbolptr[symInx].filevar = par_type;
}




	