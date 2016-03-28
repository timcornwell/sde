/**	GAUSS - A System for Least Squares and Robust Estimation
**
**	Source Code Copyright (C) 1987 by William H. Jefferys,
**	Michael J. Fitzpatrick and Barbara E. McArthur
**	All Rights Reserved.
*/



#include <math.h> /* for atof() in lexyy.h */
#include "array.h"
#include "datum.h"
#include "simpledefs.h"
#include "symboltable.h"
#include "defines.h"
#include "alloc.h"
#include <stdio.h>
#include <ctype.h>
#include "protoplasm.h"
#include "strings.h"
#include "def.h"
#include "compile.h"
#include "opcodes.p"
#include "machine.h"
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
extern struct sysw syswords[];		/* array containing reserved word   */
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


# define DIGIT 257
# define LETTER 258
# define CONST 259
# define IDENTIFIER 260
# define Y_NEWLINE 261
# define Y_STRING 262
# define Y_LP 263
# define Y_RP 264
# define Y_LB 265
# define Y_RB 266
# define Y_LC 267
# define Y_RC 268
# define Y_COM 269
# define Y_SMC 270
# define Y_CLN 271
# define EOFILE 272
# define EQUALS 273
# define IF 274
# define ELSE 275
# define ELSEIF 276
# define BREAK 277
# define CONTINUE 278
# define FOR 279
# define DO 280
# define WHILE 281
# define UNTIL 282
# define RETURN 283
# define PARAMETER 284
# define OBSERVATION 285
# define DATUM 286
# define VARIABL 287
# define INCLUDE 288
# define LOOP 289
# define CONSTANT 290
# define INDEX 291
# define FUNC_NAME 292
# define YOP_OR 298
# define YOP_AND 299
# define YOP_EQ 300
# define YOP_NE 301
# define YOP_LT 302
# define YOP_GT 303
# define YOP_LE 304
# define YOP_GE 305
# define YOP_PLUS 306
# define YOP_MINUS 307
# define YOP_MUL 308
# define YOP_DIV 309
# define YOP_EXP 310
# define YOP_NOT 311
# define UPLUS 312
# define UMINUS 313
# define ENVIR 314
#define yyclearin yychar = -1
#define yyerrok yyerrflag = 0
extern int yychar;
extern short yyerrflag;
#ifndef YYMAXDEPTH
#define YYMAXDEPTH 150
#endif
#ifndef YYSTYPE
#define YYSTYPE int
#endif
YYSTYPE yylval, yyval;
# define YYERRCODE 256





	/*  
	**  Here we parse the program and return tokens.  Token definitions
	**  are given above.  We first search for a character string and see
	**  if it's either a function name or a system_word.  
	**
	*/

#include "lexyy.h"



dumptxtstak() {
   	int i;
   	xprintf(stdout, "\nyytstk_index = %d\n",yytstk_index);
   	xprintf(stdout, "\nyytext = '%s'\n",yytext);
   	for (i=0; i<yytstk_index; ++i) {
   		xprintf(stdout, "%d '%s'\n",i,yytext_stack[i]);
   	}
   	xprintf(stdout, "\n");
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
	fclose(fl_stack[cur_fd]->file);		/* for include files     */
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

int
parm_decl(type)
int type;
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




	short yyexca[] ={
-1, 1,
	0, -1,
	-2, 0,
-1, 2,
	260, 8,
	-2, 3,
-1, 95,
	263, 39,
	-2, 81,
	};
# define YYNPROD 110
# define YYLAST 486
short yyact[]={

  69, 114,  44,   9,  10,  11,  12,  26,  46,  13,
  59, 112, 113, 114,  63, 106, 107,  58, 167,  52,
  67,  51, 108,  57,   9,  10,  11,  12, 100,  68,
  13, 182,  69,  14, 185,  30,  99, 181, 161,  26,
  26, 127,  59,  31,  85,  83,  63,  77,  28,  58,
  45,  52,  67,  51,  14,  57,   9,  10,  11,  12,
 184,  68,  13, 121, 122, 115, 116, 118, 117, 120,
 119, 110, 111, 112, 113, 114, 176, 110, 111, 112,
 113, 114, 170, 169, 174, 132,  14, 131, 121, 122,
 115, 116, 118, 117, 120, 119, 110, 111, 112, 113,
 114, 173,  75,  74,  71, 121, 122, 115, 116, 118,
 117, 120, 119, 110, 111, 112, 113, 114, 121, 122,
 115, 116, 118, 117, 120, 119, 110, 111, 112, 113,
 114, 166,  29, 164, 156, 121, 122, 115, 116, 118,
 117, 120, 119, 110, 111, 112, 113, 114, 157,  73,
  72, 189, 171, 165,  70, 154, 152, 137, 136,  96,
 121, 122, 115, 116, 118, 117, 120, 119, 110, 111,
 112, 113, 114,  79,  78, 138,  27, 121, 122, 115,
 116, 118, 117, 120, 119, 110, 111, 112, 113, 114,
 121, 122, 115, 116, 118, 117, 120, 119, 110, 111,
 112, 113, 114, 121, 122, 115, 116, 118, 117, 120,
 119, 110, 111, 112, 113, 114, 121, 122, 115, 116,
 118, 117, 120, 119, 110, 111, 112, 113, 114, 122,
 115, 116, 118, 117, 120, 119, 110, 111, 112, 113,
 114,   3, 115, 116, 118, 117, 120, 119, 110, 111,
 112, 113, 114,   3,  40,  41, 135,  16, 118, 117,
 120, 119, 110, 111, 112, 113, 114,  41,  35,   9,
  10,  11,  12,  44,  95,  13,  24,  90,  19, 178,
 159,   9,  10,  11,  12, 103,  76,  13, 130,  50,
  94, 129,  33,  87,  47,  88,  21,  39,  48,  14,
  49,   7,   1,   7,  89,  15,  62,  20,  66,  61,
  65,  14,  60,  84,  55,  54,  25,  53, 190, 186,
  92,  91, 175,  56, 172,  93,  36,  42, 105, 160,
 104,  32,  34, 128,  64,  18,  43,  38,  37,  23,
  22,   8,   6,   5,  56,  56,  56,  17,  80,  81,
  82,   4,  86,   2,  97,  98,   0,   0,   0,   0,
   0,   0,   0,   0, 101,   0,   0,   0,   0,   0,
   0,   0, 102, 109,   0,   0,   0,   0,   0, 123,
 124, 125, 126,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0, 133,   0, 134,   0,   0,   0, 139,
 140, 141, 142, 143, 144, 145, 146, 147, 148, 149,
 150, 151,   0,   0,   0,   0,   0, 155,   0, 153,
   0,   0,   0,   0,   0, 162, 163,   0,   0, 158,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0, 168,   0,   0,   0,
   0,   0,   0,   0,   0, 155,   0,   0,   0,   0,
   0, 180,  56,   0,   0, 183, 179, 177,   0,   0,
   0,   0,   0,   0, 187,   0, 188,   0,   0,   0,
   0,  56,   0,   0,   0, 191 };
short yypact[]={

  -3,-1000, -15,-1000,  18,-1000,-281,-1000,  16,-1000,
-1000,-1000,-1000,-1000,-1000,-1000,-1000,-227, -87,-1000,
-1000,-222,-137,-230,-1000,-1000,-1000,   8,-1000,  16,
  -5,-257,-260,-110,-165,-1000,-1000,-116,-117,-166,
-167,-1000,-1000,-1000,-1000,  27,-1000,-1000,-1000,-1000,
-223, -89, -90,-228,-228,-228,-225,-1000,-226,-1000,
-1000,-1000,-1000,  14,-104,  14,  14,-1000,-1000,-237,
-1000,   8,-1000,-1000,   7,  26,-1000,-1000,-1000,-1000,
-266,-253,-1000,-1000,  14,-1000, -82,-1000,-1000,-1000,
  14,  14,  14,  14,-1000,-224,-1000, -82, -82,-1000,
  14,-1000,-182,-184,  14,  -4,-105,-106,-1000, -95,
  14,  14,  14,  14,  14,  14,  14,  14,  14,  14,
  14,  14,  14,-108,-1000,-1000,-1000,  14,  14,-132,
-121,   7,  21, -82,-232,-237,  14,  14,-1000,-297,
-297,-309,-309,-309, -44, -44,-229,-229,-229,-229,
 -70, -58,-1000,-133,-111,-138,-255,  14,-186,-187,
-112,-1000,-163,-180,-1000,-1000,  14,-1000,-193,   7,
  20,-228,  14,-233,-239,-1000,  14,-1000,-1000,-1000,
-210,-1000,-1000,-235,-1000,  14,  -4, -82,-113,-1000,
-228,-1000 };
short yypgo[]={

   0, 302, 353, 351, 347, 298, 343, 342, 300, 341,
 296, 340, 339, 338, 337, 295, 336, 297, 335, 292,
 290, 334, 333, 155, 332, 288, 331, 294, 289, 330,
 329, 328, 324, 319, 318, 317, 315, 314, 313, 312,
 310, 309, 308, 306, 291, 304, 293 };
short yyr1[]={

   0,   1,   1,   1,   1,   2,   3,   6,   6,   7,
   7,   8,   9,   9,   9,   9,   9,   9,  10,  10,
  12,  11,  11,  11,  11,  11,  17,  13,  13,  13,
  13,  14,  14,  14,  14,   4,  22,  20,  18,  21,
  24,  19,  19,  19,  23,  23,  23,   5,  26,  26,
  27,  27,  27,  29,  30,  27,  31,  32,  33,  34,
  27,  27,  27,  27,  27,  27,  38,  27,  27,  27,
  39,  41,  28,  28,  36,  37,  35,  43,  43,  40,
  42,  45,  45,  44,  44,  44,  44,  15,  16,  25,
  25,  25,  46,  46,  46,  46,  46,  46,  46,  46,
  46,  46,  46,  46,  46,  46,  46,  46,  46,  46 };
short yyr2[]={

   0,   2,   2,   1,   1,   3,   1,   1,   0,   1,
   2,   3,   1,   1,   1,   1,   1,   1,   1,   3,
   1,   1,   4,   4,   3,   3,   1,   1,   3,   5,
   7,   1,   3,   5,   7,   4,   0,   5,   1,   1,
   1,   0,   1,   3,   0,   1,   3,   3,   0,   2,
   1,   1,   2,   0,   0,   7,   0,   0,   0,   0,
  13,   7,   7,   2,   2,   2,   0,   4,   2,   1,
   2,   2,   1,   1,   2,   3,   1,   1,   1,   2,
   5,   1,   4,   1,   3,   5,   7,   1,   2,   1,
   1,   1,   3,   3,   3,   3,   3,   3,   2,   2,
   3,   3,   3,   3,   3,   3,   3,   3,   2,   1 };
short yychk[]={

-1000,  -1,  -2, 256,  -3,  -6,  -7,  -8,  -9, 284,
 285, 286, 287, 290, 314,  -1, 272,  -4, -18, 260,
  -8, -10, -11, -12, 260,  -5, 267, 263, 270, 269,
 265, 273, -26, -19, -24, 260, -10, -13, -14, -17,
 259, 260, -15, -16, 259, 307, 268, -27,  -5,  -8,
 -28, 281, 279, -35, -36, -37, -20, 283, 277, 270,
 -39, -41, -43, 274, -21, -40, -42, 280, 289, 260,
 264, 269, 266, 266, 269, 269, 259, 270, 263, 263,
 -27, -27, -27, 270, -38, 270, -25, -46, -15, -45,
 263, 307, 306, 311, -20, 260, 263, -25, -25, 273,
 265, -19, -17, 259, -29, -31, 281, 282, 275, -25,
 306, 307, 308, 309, 310, 300, 301, 303, 302, 305,
 304, 298, 299, -25, -25, -25, -25, 265, -22, -44,
 -25, 269, 269, -25, -28, 260, 263, 263, 270, -25,
 -25, -25, -25, -25, -25, -25, -25, -25, -25, -25,
 -25, -25, 264, -44, -23, -25, 266, 269, -17, 259,
 -30, 270, -25, -25, 266, 264, 269, 273, -25, 269,
 269, 264, -32, 264, 264, -23, 269, -17, 259, -27,
 -25, 270, 270, -25, 270, 269, -33, -25, -28, 264,
 -34, -27 };
short yydef[]={

   8,  -2,  -2,   4,   0,   6,   7,   9,   0,  12,
  13,  14,  15,  16,  17,   1,   2,   0,   0,  38,
  10,   0,  18,  21,  20,   5,  48,  41,  11,   0,
   0,   0,   0,   0,  42,  40,  19,   0,   0,  27,
  31,  26,  24,  25,  87,   0,  47,  49,  50,  51,
   0,   0,   0,   0,   0,   0,   0,  66,   0,  69,
  72,  73,  76,   0,   0,   0,   0,  77,  78,  39,
  35,  41,  22,  23,   0,   0,  88,  52,  53,  56,
   0,  63,  64,  65,   0,  68,  74,  89,  90,  91,
   0,   0,   0,   0, 109,  -2,  36,  70,  71,  79,
   0,  43,  28,  32,   0,   0,   0,   0,  75,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,  98,  99, 108,   0,  44,   0,
  83,   0,   0,  54,   0,   0,   0,   0,  67,  93,
  94,  95,  96,  97, 100, 101, 102, 103, 104, 105,
 106, 107,  92,   0,   0,  45,   0,   0,  29,  33,
   0,  57,   0,   0,  82,  37,  44,  80,  84,   0,
   0,   0,   0,   0,   0,  46,   0,  30,  34,  55,
   0,  61,  62,  85,  58,   0,   0,  86,   0,  59,
   0,  60 };

#
# define YYFLAG -1000
# define YYERROR goto yyerrlab
# define YYACCEPT return(0)
# define YYABORT return(1)

/*	parser for yacc output	*/

#ifdef YYDEBUG
int yydebug = 0; /* 1 for debugging */
#endif
YYSTYPE yyv[YYMAXDEPTH]; /* where the values are stored */
int yychar = -1; /* current input token number */
int yynerrs = 0;  /* number of errors */
short yyerrflag = 0;  /* error recovery flag */

yyparse() {

	short yys[YYMAXDEPTH];
	short yyj, yym;
	register YYSTYPE *yypvt;
	register short yystate, *yyps, yyn;
	register YYSTYPE *yypv;
	register short *yyxi;

	yystate = 0;
	yychar = -1;
	yynerrs = 0;
	yyerrflag = 0;
	yyps= &yys[-1];
	yypv= &yyv[-1];

 yystack:    /* put a state and value onto the stack */

#ifdef YYDEBUG
	if( yydebug  ) xprintf(stdout,  "state %d, char 0%o\n", yystate, yychar );
#endif
		if( ++yyps> &yys[YYMAXDEPTH] ) { yyerror( "yacc stack overflow" ); return(1); }
		*yyps = yystate;
		++yypv;
		*yypv = yyval;

 yynewstate:

	yyn = yypact[yystate];

	if( yyn<= YYFLAG ) goto yydefault; /* simple state */

	if( yychar<0 ) if( (yychar=yylex())<0 ) yychar=0;
	if( (yyn += yychar)<0 || yyn >= YYLAST ) goto yydefault;

	if( yychk[ yyn=yyact[ yyn ] ] == yychar ){ /* valid shift */
		yychar = -1;
		yyval = yylval;
		yystate = yyn;
		if( yyerrflag > 0 ) --yyerrflag;
		goto yystack;
		}

 yydefault:
	/* default state action */

	if( (yyn=yydef[yystate]) == -2 ) {
		if( yychar<0 ) if( (yychar=yylex())<0 ) yychar = 0;
		/* look through exception table */

		for( yyxi=yyexca; (*yyxi!= (-1)) || (yyxi[1]!=yystate) ; yyxi += 2 ) ; /* VOID */

		while( *(yyxi+=2) >= 0 ){
			if( *yyxi == yychar ) break;
			}
		if( (yyn = yyxi[1]) < 0 ) return(0);   /* accept */
		}

	if( yyn == 0 ){ /* error */
		/* error ... attempt to resume parsing */

		switch( yyerrflag ){

		case 0:   /* brand new error */

			recover();return(1);
		yyerrlab:
			++yynerrs;

		case 1:
		case 2: /* incompletely recovered error ... try again */

			yyerrflag = 3;

			/* find a state where "error" is a legal shift action */

			while ( yyps >= yys ) {
			   yyn = yypact[*yyps] + YYERRCODE;
			   if( yyn>= 0 && yyn < YYLAST && yychk[yyact[yyn]] == YYERRCODE ){
			      yystate = yyact[yyn];  /* simulate a shift of "error" */
			      goto yystack;
			      }
			   yyn = yypact[*yyps];

			   /* the current yyps has no shift onn "error", pop stack */

#ifdef YYDEBUG
			   if( yydebug ) xprintf(stdout,  "error recovery pops state %d, uncovers %d\n", *yyps, yyps[-1] );
#endif
			   --yyps;
			   --yypv;
			   }

			/* there is no state on the stack with an error shift ... abort */

	yyabort:
			return(1);


		case 3:  /* no shift yet; clobber input char */

#ifdef YYDEBUG
			if( yydebug ) xprintf(stdout,  "error recovery discards char %d\n", yychar );
#endif

			if( yychar == 0 ) goto yyabort; /* don't discard EOF, quit */
			yychar = -1;
			goto yynewstate;   /* try again in the same state */

			}

		}

	/* reduction by production yyn */

#ifdef YYDEBUG
		if( yydebug ) xprintf(stdout, "reduce %d\n",yyn);
#endif
		yyps -= yyr2[yyn];
		yypvt = yypv;
		yypv -= yyr2[yyn];
		yyval = yypv[1];
		yym=yyn;
			/* consult goto table to find next state */
		yyn = yyr1[yyn];
		yyj = yypgo[yyn] + *yyps + 1;
		if( yyj>=YYLAST || yychk[ yystate = yyact[yyj] ] != -yyn ) yystate = yyact[yypgo[yyn]];
		switch(yym){
			
case 4:

{ yyerrok;	} break;
case 5:

{
			extern short cur_func;
			print_instr(opReturn, "");
			print_instr(opLabel,itos(label_stack[--lstack_index]));
			wipelocalindices();
			cur_func = findsymbol("_Globals", 0);
		} break;
case 11:

{ saw_param = 0;	} break;
case 12:

{  if(bracket_level > 1) {
				    recerror(decl_level_error,"","");
				    YYABORT;
				} 
				saw_param++;  
				par_type = PARAMETER;  
			     } break;
case 13:

{  if(bracket_level > 1) {
				    recerror(decl_level_error,"","");
				    YYABORT;
				} 
				saw_param++;  
				par_type = OBSERVATION;
			     } break;
case 14:

{ if(bracket_level > 1) {
				    recerror(decl_level_error,"","");
				    YYABORT;
				} 
				saw_param++;  
				par_type = DATUM;      
			     } break;
case 15:

{  if(bracket_level > 1) {
				    recerror(decl_level_error,"","");
				    YYABORT;
				} 
				saw_param++;  
				par_type = VARIABL;    
			     } break;
case 16:

{  if(bracket_level > 1) {
				    recerror(decl_level_error,"","");
				    YYABORT;
				} 
				saw_param++;  
				par_type = CONSTANT;   
			     } break;
case 17:

{  if(bracket_level > 1) {
                    recerror(decl_level_error,"","");
                    YYABORT;
                }
                saw_param++;
                par_type = ENVIR;
                 } break;
case 20:

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
		} break;
case 21:

{    /* RULE:  parg:  param1      		   */

		     /* Declare the variable with appropriate type */
		     parm_decl(par_type);
		} break;
case 22:

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
		} break;
case 23:

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
		} break;
case 24:

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
		} break;
case 25:

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
		} break;
case 26:

{
			char *p;
			p = toptxt();
			if ((np = lookup(p)) == NULL) {
				insert(p, INDEX); 
			}		
		} break;
case 27:

{
		   print_instr(opDefIndex,poptxt());
		   nindices = 1;
		} break;
case 28:

{
		   print_instr(opDefIndex,poptxt());
		   print_instr(opDefIndex,poptxt());
		   nindices = 2;
		} break;
case 29:

{
		   print_instr(opDefIndex,poptxt());
		   print_instr(opDefIndex,poptxt());
		   print_instr(opDefIndex,poptxt());
		   nindices = 3;
		} break;
case 30:

{
		   print_instr(opDefIndex,poptxt());
		   print_instr(opDefIndex,poptxt());
		   print_instr(opDefIndex,poptxt());
		   print_instr(opDefIndex,poptxt());
		   nindices = 4;
		} break;
case 31:

{
		   print_instr(opPushDim,poptxt());
		   nindices = 1;
		} break;
case 32:

{
		   print_instr(opPushDim,poptxt());
		   print_instr(opPushDim,poptxt());
		   nindices = 2;
		} break;
case 33:

{
		   print_instr(opPushDim,poptxt());
		   print_instr(opPushDim,poptxt());
		   print_instr(opPushDim,poptxt());
		   nindices = 3;
		} break;
case 34:

{
		   print_instr(opPushDim,poptxt());
		   print_instr(opPushDim,poptxt());
		   print_instr(opPushDim,poptxt());
		   print_instr(opPushDim,poptxt());
		   nindices = 4;
		} break;
case 35:

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
		} break;
case 36:

{ inargs=1; } break;
case 37:

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
		} break;
case 38:

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
		} break;
case 39:

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
		} break;
case 40:

{
			char *p;
			p = toptxt();
			if ((np = lookup(p)) == NULL) {
				bracket_level = 1;
				insert(p, VARIABL); 
				bracket_level = 0;
				/*insert(p, par_type); */
			}	
		} break;
case 42:

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
		} break;
case 43:

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
		} break;
case 45:

{   /* RULE:  actual_args :  expr Y_COM actual_args   	   */
		    npar[inpar]++;	/* count number of args           */
		} break;
case 46:

{   /* RULE:  actual_args :  expr Y_COM actual_args   	   */
		    npar[inpar]++;	/* count number of args           */
		} break;
case 47:

{   /* RULE:  compound_stmt:  Y_LC stmt_list Y_RC */
		    --bracket_level;
		} break;
case 53:

{      /* RULE:  stmt  | WHILE Y_LP           		*/
			labgen(&lstack_index,3); /* Initialize labels   */
			print_instr(opLabel,itos(label_stack[lstack_index-3]));
			print_instr(opLabel,itos(label_stack[lstack_index-2]));
	        } break;
case 54:

{      /* RULE:  WHILE.....  expr			*/ 
			print_instr(opIfNot, "");  /* evaluate expression*/
			print_instr(opGoTo,itos(label_stack[lstack_index-1]));
		} break;
case 55:

{      /* RULE:  WHILE.....  Y_RP stmt			*/
			
			print_instr(opGoTo,itos(label_stack[lstack_index-2]));
			print_instr(opLabel,itos(label_stack[lstack_index-1]));

			lstack_index -= 3;
		} break;
case 56:

{	/* RULE:  stmt |  FOR  Y_LP			*/	
			labgen(&lstack_index,5);  /* Initialize labels	*/
			print_instr(opLabel,itos(label_stack[lstack_index-5]));
		} break;
case 57:

{	/* RULE: FOR... stmt1 Y_SMC....			*/	
			print_instr(opLabel,itos(label_stack[lstack_index-4]));
		} break;
case 58:

{	/* RULE: FOR... expr Y_SMC....			*/	
			print_instr(opIfNot, ""); /* evaluate expression */
			print_instr(opGoTo,itos(label_stack[lstack_index-1]));
			print_instr(opGoTo,itos(label_stack[lstack_index-3]));
			print_instr(opLabel,itos(label_stack[lstack_index-2]));
		} break;
case 59:

{ 	/* RULE:  FOR....  stmt1 Y_RP...		*/	
			print_instr(opGoTo,itos(label_stack[lstack_index-4]));
			print_instr(opLabel,itos(label_stack[lstack_index-3]));
		} break;
case 60:

{	/* RULE:  FOR....  stmt...			*/	
			print_instr(opGoTo,itos(label_stack[lstack_index-2]));
			print_instr(opLabel,itos(label_stack[lstack_index-1]));
			
			lstack_index -= 5;
		} break;
case 61:

{	/* RULE: doprefix stmt WHILE Y_LP expr Y_RP Y_SMC */	

			print_instr(opIfNot, "");  /* evaluate expression  */
			print_instr(opGoTo,itos(label_stack[lstack_index-1]));
			print_instr(opGoTo,itos(label_stack[lstack_index-2]));
			print_instr(opLabel,itos(label_stack[lstack_index-1]));

			lstack_index -= 3;
		} break;
case 62:

{	/* RULE: doprefix stmt UNTIL Y_LP expr Y_RP Y_SMC */	

		 	print_instr(opIf, "");  /* evaluate expression  */
			print_instr(opGoTo,itos(label_stack[lstack_index-1]));
			print_instr(opGoTo,itos(label_stack[lstack_index-2]));
			print_instr(opLabel,itos(label_stack[lstack_index-1]));

			lstack_index -= 3;
		} break;
case 63:

{	/* RULE: stmt | ifprefix stmt			*/	
			print_instr(opLabel,itos(label_stack[lstack_index-2]));
			lstack_index -= 2;
		} break;
case 64:

{	/* RULE: stmt | ifelprefix stmt			*/	
			print_instr(opLabel,itos(label_stack[lstack_index-1]));
			lstack_index -= 2;
		} break;
case 65:

{   	/* RULE: func_del Y_SMC				*/

			/* Straight function call - drop result */
			print_instr(opDrop, "");
		} break;
case 66:

{ inargs = 1; } break;
case 67:

{    	/* RULE:  stmt | RETURN expr Y_SMC		*/
			inargs = 0;
			print_instr(opReturn, "");
		} break;
case 68:

{    	/* RULE:  stmt | BREAK Y_SMC			*/
			print_instr(opGoTo,itos(label_stack[lstack_index-1]));
		} break;
case 70:

{  /* RULE:  stmt1  | lval1 EQUALS expr */
			char *p;
			/* check for legal LHS */
			p = poptxt();
			print_instr(opStore,p);
		} break;
case 71:

{  /* RULE:  stmt2  | lval2 EQUALS expr */
			char *p;
			p = poptxt();
			print_instr(opStoreVec,p);
		} break;
case 74:

{ /* RULE: ifprefix: IF expr				*/
		  
		  /* Begginning to IF-THEN expression  */
		  labgen(&lstack_index,2);
	  	  print_instr(opIfNot, "");
		  print_instr(opGoTo,itos(label_stack[lstack_index-2]));
		} break;
case 75:

{ /* RULE: ifelprefix: ifprefix stmt ELSE		*/
		  
		  /* Begginning to IF-THEN-ELSE  expression  */
		  /*lstack_index += 2;   /* restore label_stack */
			print_instr(opGoTo,itos(label_stack[lstack_index-1]));
		 	print_instr(opLabel,itos(label_stack[lstack_index-2]));
		} break;
case 76:

{	/* RULE:  doprefix : do_or_loop			*/
	
			/* Initialize labels for DO loop		*/
			labgen(&lstack_index,3);
			print_instr(opLabel,itos(label_stack[lstack_index-3]));
			print_instr(opLabel,itos(label_stack[lstack_index-2]));
		} break;
case 79:

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
		} break;
case 80:

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
		} break;
case 81:

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
	    } break;
case 82:

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
		} break;
case 83:

{ nindices = 1; } break;
case 84:

{ nindices = 2; } break;
case 85:

{ nindices = 3; } break;
case 86:

{ nindices = 4; } break;
case 87:

{ 
			print_instr(opPushCon, poptxt());	
		} break;
case 88:

{
			char *strbuf, str[80];
			sprintf(str,"-%s\0", poptxt());
			strbuf = MemAlloc("PushCon", (long)strlen(str)+1);
			strcpy(strbuf, str);
			print_instr(opPushCon, strbuf);
		} break;
case 92:

{    yyval = yypvt[-1];	} break;
case 93:

{   print_instr(opAdd, ""); 
		} break;
case 94:

{   print_instr(opSubtract, "");
		} break;
case 95:

{   print_instr(opMultiply, "");
		} break;
case 96:

{   print_instr(opDivide, "");
		} break;
case 97:

{   print_instr(opPower, "");
		} break;
case 98:

{   yyval =   - yypvt[-0];  print_instr(opNegate, "");
		} break;
case 99:

{   yyval =   yypvt[-0];  
		} break;
case 100:

{   print_instr(opEqualsFn, "");
		} break;
case 101:

{   print_instr(opNotEqualsFn, "");
		} break;
case 102:

{   print_instr(opGreaterThanFn, "");
		} break;
case 103:

{   print_instr(opLessThanFn, "");
		} break;
case 104:

{   print_instr(opGreaterOrEqualFn, "");
		} break;
case 105:

{   print_instr(opLessOrEqualFn, "");
		} break;
case 106:

{   print_instr(opOrFn, "");
		} break;
case 107:

{   print_instr(opAndFn, "");
		} break;
case 108:

{   print_instr(opNotFn, "");
		} break;
		}
		goto yystack;  /* stack new state and value */

	}
