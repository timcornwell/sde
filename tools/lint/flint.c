/************************************************************************

  Flint is a FORTRAN checker. It takes FORTRAN code as its input, and
  generates warnings about dubious FORTRAN. These messages are written
  both to the terminal, and a listing file.

  There are some places where better use could have been made of stdio.
  However Flint is intended to work on a number of systems, and so
  it has been written to avoid some bugs in some non-UNIX stdio packages.
  Systems on which Flint is believed to work are Berkeley UNIX (Sun, Alliant,
  Convex), UNICOS, VMS, Turbo-C (MS-DOS) and CTSS (hcc compiler).

  Checks it performs include:
    * undeclared variables.
    * variable declared but never used.
    * variables used before being assigned.
    * variables assigned to but not otherwise used.
    * names longer than 8 characters, or containing $ or _ characters.
    * lines longer than 72 characters, or containing an odd number of
      quote characters.
    * subroutine argument consistency: number, type and intent.

  Areas for Improvement:
    * Improved error checking and handling.
    * Perform checks on statements out of correct order (e.g. DATA
      statements amongst executables, etc).
    * Improve "initialisation checking" algorithm.
    * ASSIGN and alternate returns.
    * Consistency in common block definition. Checks could include common
      block size, variable type matching, and even variable name matching.

  Flint is NOT intended to replace compiler checks. Flint is blind
  to much bad code that any compiler will pick up.

  The following parts of FORTRAN are not handled at all (flagged as bad):
    ASSIGN
    Assigned GOTO
    PAUSE
    ENTRY
    alternate returns
    statement functions (probably will confuse Flint).
    many archaic i/o statements
    most non-standard FORTRAN

  The following statements are ignored (not flagged as bad, but
    not analysed).
    IMPLICIT
    EQUIVALENCE
    FORMAT

  It handles the following FORTRAN extensions without generating a warning.
    INTENT statement
    DO/ENDDO, DOWHILE/ENDDO
    # to start comments
    tabs and full ascii character set.

  Summary of Flags
  ================
  Flint takes a large number of flags, to attempt to keep the error messages
  that it generates down to manageable proportions. The command format is:

   flint [-cdfhirsx2] [-o file] [-l] file ...

    f	 Disable "line checks".
    c	 Allow comments and continuation lines to be interwoven. Normally
 	 Flint flags this as an error.
    r	 Do not warn about seemingly redundant variables.
    d	 Do not insist that variables are always explicitly declared.
    i	 Do not check if a variable has been initialised.
    x	 Allow names longer than 8 characters.
    s	 Load the definitions of specific functions and FORTRAN-IV
 	 standard function.
    u	 Do not worry about unused variables.
    2	 Flint performs two passes.
    h	 Crude treatment of hollerith.
    o	 Generate output file giving subroutine definitions only. The next
 	 command line argument gives the output file name.
    l	 The following file is to be processed in "library mode". This
 	 means that the file is not echoed to flint.log, and that errors
	 detected are to be ignored.

  Basic Workings
  ==============
  Flint maintains two (hash) tables which contain all Flint info about
  variables and subroutines. Almost always, these hash tables are accessed
  through the set_variable and set_routine subroutines. The caller passes in
  whatever it knows about the variable or routine, and the set_? routine passes
  back the accumulated knowledge about the variable or routine. set_variable
  and set_routine are responsible for generating many warnings. 

  Determining Intent
  ==================
  Flint attempts to uncover the intent of subroutine arguments. It gleans
  information from both calls to the routine and the source of the routine
  itself (if available).

  The following rules are used when analysing a call to a routine:
  * Arguments which are passed in as constants, expressions or parameters
    are input! Arguments which are input dummy arguments to the current
    routine are also deemed to be inputs. These rules are reliable.
  * Arguments which are variables which have not been initialised are
    assumed to be output. Also arguments which are output dummy arguments
    to the current routine, and have not yet been initialised, are also
    deemed to be outputs. These rules depend on the accuracy of the
    initialisation checking algorithm, which can be inaccurate. These
    rules are disabled if initialisation checking is disabled.

  The following rules are used when analysing the source of the routine.
  * Dummy arguments which are used as input only are clearly input. This
    rule is reliable.
  * Dummy arguments which are assigned to before they are used, are deemed
    as output. This rule depends on the initialisation checking algorithm,
    and is turned off, if initialisation checking is turned off.
  * Dummy arguments which are used and then assigned to are deemed to
    be input/output. 

  What goes on when dummy arguments are passed to subroutines produces much
  recursive thought. There are some instances where Flint has to discard
  some hard won information, because there are too few flags to describe
  all the contortions that can occur.

  Flags and Hash Tables
  =====================
  Each entry in the "vhash" hash table consists of a name and set of flags.
  These are mostly just variables, but they also contain entries for
  each EXTERNAL and each function or subroutine called (except the
  FORTRAN intrinsics?).

  Each entry in the "rhash" hash table is a subroutine or function definition.
  Each definition consists of the name, flags for the routine, the
  number of subroutine arguments, and an array of flags, one for each argument.
  Entries for FORTRAN intrinsics appear in this table.

  The meaning of most flags (I hope) is fairly clear, but
  F_IN,F_OUT,F_PIN,F_POUT require more attention. For a variable,
  F_IN and F_OUT is set if the variable has been used as source or
  destination of an operation, respectively. F_PIN is set for subroutine
  arguments, indicating that this variable may have been passed in by the
  caller. If Flint determines that this is not so, then this flag is
  turned off. F_POUT indicates that this variable may have been passed
  out by a subroutine. 

  For a subroutine or function, these flags are used only in the
  vhash table. F_IN and F_OUT indicate that the routine
  has been called. F_PIN is always set if it was a dummy argument.
  F_POUT is set if it was passed to a subroutine.

  For a routine argument (in the "rhash" table), F_IN and F_OUT means
  passed in and passed out. An absence of these indicates that Flint does
  not know.

************************************************************************/
#define TRUE		1
#define FALSE		0

#define max(a,b) ((a) > (b) ? (a) : (b) )
#define min(a,b) ((a) < (b) ? (a) : (b) )

#define private static
#include <ctype.h>
#include <stdio.h>

/* Define all the flags. */

#define F_DATA		0x0001		/* Variable in DATA statement */
#define F_ROUTINE	0x0002		/* SUBROUTINE, FUNCTION or EXTERNAL */
#define F_SAVE		0x0004		/* Variable in SAVE statement */
#define F_COMMON	0x0008		/* Variable in COMMON statement */
#define F_OUT		0x0010
#define F_IN		0x0020
#define F_ARG		0x0040		/* Variable or EXTERNAL is a dummy
					   argument */
#define F_INTRINSIC	0x0080		/* Function is a Fortran intrinsic */
#define F_PIN		0x0100
#define F_POUT		0x0200
#define F_PARAMETER     0x0400		/* Variable is in PARAMETER statement */
#define F_ARRAY		0x0800		/* Variable is an array */
#define F_SPECIAL	0x1000		/* An intrinsic function requiring some
					   special handling */
#define F_ACTUAL	0x2000		/* In a rhash flags, this indicates
					   that the source for the routine was
					   actually parsed. */
#define F_INTEGER	0x4000		/* Integer variable or function */
#define F_REAL		0x8000		/* Real */
#define F_DOUBLE       0x10000		/* Double precision */
#define F_COMPLEX      0x20000		/* Complex */
#define F_CHAR	       0x40000		/* Character */
#define F_LOGICAL      0x80000		/* Logical */
#define F_VOID	      0x100000		/* SUBROUTINE */
#define F_GENERIC     0x200000		/* Fortran generic function */

#define IO_MASK    (F_IN|F_OUT)
#define TYPE_MASK  (F_LOGICAL|F_INTEGER|F_REAL|F_DOUBLE|F_COMPLEX|F_CHAR|\
					F_GENERIC|F_VOID)

/* These are or'ed together to form an argument to a routine which 
   handles array and substring indices. */

#define INDICE_1ONLY	1
#define INDICE_WILD	2
#define INDICE_COLON	4
#define INDICE_NULL	8

/* These values are returned by the routine which gets a line from an
   input file, and determines whether its a comment, continuation, etc. */

#define LINE_NORMAL	1
#define LINE_COMMENT	2
#define LINE_CONTINUE	3
#define LINE_END	4
#define LINE_BAD	5

/* These are returned by routines associated with parsing expressions. */

#define TOKEN_LEFT	 0x0001
#define TOKEN_RIGHT	 0x0002
#define TOKEN_LOGICAL	 0x0004
#define TOKEN_RELATIONAL 0x0008
#define TOKEN_ARITH	 0x0010
#define TOKEN_PLUS_MINUS 0x0020
#define TOKEN_NOT	 0x0040

#define BINARY_MASK \
	(TOKEN_LOGICAL|TOKEN_RELATIONAL|TOKEN_ARITH|TOKEN_PLUS_MINUS)
#define UNARY_MASK (TOKEN_PLUS_MINUS|TOKEN_NOT)

#define PROG_SUB_FUNC_STATE	1
#define DECLARATION_STATE	2
#define NORMAL_STATE		3
#define MAXVARLEN	8
#define MAXVAR		128
#define MAXLINE		132
#define MAXTLINE	(80*25)

#define HASHSIZE	701
#define SHASHSIZE	51

typedef struct statement { char *name; int flags,length; struct statement *fwd;
			   void (*parser)();} STATEMENT;

typedef struct symbol {	char *name;
		int flags,narg,*args;
		struct symbol *fwd;} SYMBOL;

/* Declare all the routines that I use. */

private void bug(),error(),clear_hash_table();
private void define_intrinsics(),define_specifics(),define_statements();
private void generate_output(),generate_function(),generate_line();

private void call_statement(),declaration_statement(),close_statement();
private void simple_statement(),data_statement(),do_statement(),dowhile_statement();
private void if_elseif_statement(),ignore_statement(),end_statement();
private void prog_sub_func_statement(),goto_statement(),include_statement();
private void inquire_statement(),open_statement(),parameter_statement();
private void read_write_statement(),rewind_statement(),common_statement();
private void assignment_statement(),blockdata_statement();
private void parse_line(),parse_file();

private char *handle_implied_do_loop(),*handle_dio_list();
private char *handle_variable(),*handle_expression();
private char *handle_indices(),*handle_length();
private char *handle_sub_func_call(),*handle_keywords(),*handle_value();
private char *handle_operator();
private char *get_line(),*get_name();
private char *skip_integer(),*skip_expression(),*skip_enclosed(),*skip_token();
private char *skip_numeric(),*skip_logical(),*skip_string();
private char *usage();
private SYMBOL *get_keyword();
private SYMBOL *create_symbol(),*add_symbol(),*find_symbol();
private int set_variable(),inquire_variable();
private SYMBOL *set_routine(),*inquire_routine();
private int isfunction(),issubstring(),get_arg_intent();
private void banish_hollerith();
char *malloc();

#define issymbol(s) (isalnum(s) || (s) == '_' || (s) == '$')

/* Global variables. */

static SYMBOL *vhash[HASHSIZE], *rhash[HASHSIZE];
static SYMBOL  routine;
static STATEMENT *shash[SHASHSIZE];
static int state,given,library_mode;
static int unused,initialisation,extended,redundant,declare,interweave;
static int lcheck,hollerith,twopass;
static char *unknown = "(Unknown)";
static FILE *log,*msg;
static char errmsg[MAXLINE];

#define ERROR(a) sprintf a;error(errmsg)
/************************************************************************/
main(argc,argv)
int argc;
char *argv[];
{
  char *output_file,*s,*library_flag;
  int i,i0;

/*
  Initialise the hash tables.
*/
  for(i=0;i<HASHSIZE;i++){
    vhash[i] = NULL;
    rhash[i] = NULL;
  }
  given = FALSE;
  routine.name = unknown;
  routine.narg = 0;
  routine.flags = F_VOID|F_ROUTINE;
  routine.fwd = NULL;
  state = PROG_SUB_FUNC_STATE;

/* Load the appropriate hash tables with the definitions of intrinsic and
   hash functions. */

  define_intrinsics();
  define_statements();

/* Open the log and message file. */

  log = fopen("flint.log","w");
  msg = NULL;

/* Handle any flags on the command line. */

  interweave = TRUE;		/* Warn on interwoven comments and
							continuations.	*/
  lcheck = TRUE;		/* Perform simple line checks. 		*/
  unused = TRUE;		/* Warn of unused variables. 		*/
  declare = TRUE;		/* Warn on undeclared variables.	*/
  redundant = TRUE;		/* Warn of redundant variables.		*/
  output_file = NULL;		/* No output file.			*/
  initialisation = TRUE;	/* Check variable initialisation.	*/
  extended = FALSE;		/* Use "strict" name rules.		*/
  twopass = FALSE;		/* Do not do two passes.		*/
  hollerith = FALSE;		/* Do not handle hollerith at all.	*/
  library_flag = "-l";

  for(i=1;i<argc;i++){
    s = argv[i];
    if(*s == '-'){				/* Handle flags. */
      s++;
      i0 = i;
      argv[i] = NULL;
      while(*s)switch(*s++){
        case 'u': unused = FALSE;		break;
	case 'r': redundant = FALSE;		break;
	case 'd': declare = FALSE;		break;
        case 'i': initialisation = FALSE;	break;
        case 'x': extended  = TRUE;		break;
        case 'o': if(i++ < argc){output_file = argv[i]; argv[i] = NULL;}
						break;
        case 's': define_specifics();		break;
	case 'l': argv[i0] = library_flag;	break;
	case 'c': interweave = FALSE;		break;
	case 'f': lcheck = FALSE;		break;
	case '2': twopass = TRUE;		break;
	case 'h': hollerith = TRUE;		break;
	default: fprintf(stderr,"Unrecognised flag %c\n",*(s-1));
						break;
      }

/* Allow output redirection, if the invoking shell does not provide it. */

    }else if(*s == '>'){
      argv[i] = NULL;
      if(i++ < argc){msg = fopen(argv[i],"w"); argv[i] = NULL;}
    }
  }

/* If doing two passes, do the first pass. */

  if(twopass){
    library_mode = TRUE;
    for(i=1;i<argc;i++)
      if(argv[i] != library_flag && argv[i] != NULL)parse_file(argv[i]);
  }

/* Do the final pass. Do not process library files a second time. */

  library_mode = FALSE;
  for(i=1;i<argc;i++){
    if(argv[i] == library_flag){
      library_mode = TRUE;
    }else if(argv[i] != NULL && (!library_mode || !twopass) ){
      parse_file(argv[i]);
      library_mode = FALSE;
    }
  }

/* Close the log file. */

  fclose(log);

/* If the user wanted a interface file, generate it. */

  if(output_file != NULL)generate_output(output_file);
}
/************************************************************************/
private void generate_output(output_file)
char *output_file;
/*
  Generate an output file.
------------------------------------------------------------------------*/
{
  int i;
  FILE *out;
  SYMBOL *p;

  out = fopen(output_file,"w");
  if(out == NULL)bug("Failed to open output file %s",output_file);
  for(i=0;i<HASHSIZE;i++){
    p = rhash[i];
    for(p = rhash[i]; p != NULL; p = p->fwd){
      if(p->flags & F_ACTUAL)generate_function(out,p);
/*      generate_function(out,p); */
    }
  }
  fclose(out);
}
/************************************************************************/
private void generate_function(fh,p)
FILE *fh;
SYMBOL *p;
/*
  Generate a function into the output file.
------------------------------------------------------------------------*/
{
  int i;
  char line[MAXVAR],temp[32];
  static struct {char *leader,*trailer; int mask,target;}
	states[] =     {{"",	     ")",  0,0},
			{"integer ",  "",   F_INTEGER,F_INTEGER},
			{"real ",     "",   F_REAL,F_REAL},
			{"double precision ","",F_DOUBLE,F_DOUBLE},
			{"complex ",  "",   F_COMPLEX,F_COMPLEX},
			{"character ","",   F_CHAR,F_CHAR},
			{"logical ",  "",   F_LOGICAL,F_LOGICAL},
			{"external ", "",   F_ROUTINE,F_ROUTINE},
			{"intent(out) ","", F_OUT,F_OUT},
			{"intent(in) ","",  F_IN,F_IN},
		        {"intent(unknown) ","",F_IN|F_OUT,0}};

#define NSTATES 11

  fprintf(fh,"c***************************************************************\n");
  sprintf(line,"%s %s(",usage(p->flags,temp),p->name);
  states[0].leader = line;

  if(p->narg > 0){
    for(i=0; i < NSTATES; i++)generate_line(fh, states[i].leader,
						states[i].trailer,
						states[i].mask,
						states[i].target,
						p->args,p->narg);
  }else{
    fprintf(fh,"\t%s )\n",line);
  }
  if(!(p->flags & F_VOID))fprintf(fh,"\tintent(out)%s\n",p->name);
  fprintf(fh,"\tend\n");
}
/************************************************************************/
private void generate_line(fh,leader,trailer,mask,target,flags,n)
FILE *fh;
char *leader,*trailer;
int mask,target,flags[],n;
/*
  Output a line to the summary file, in a neat FORTRAN syntax.
------------------------------------------------------------------------*/
{
  char line[MAXLINE];
  int i,nout,used;

  nout = strlen(leader);
  sprintf(line,"\t%s",leader);
  leader = line;
  used = FALSE;

  for(i=0; i < n; i++){
    if( (mask & flags[i]) == target){
      fprintf(fh,"%sa%d",leader,i+1);
      nout += ( i < 9 ? 3 : 4);
      used = TRUE;
      if(nout < 56){
	leader = ",";
      }else{
	nout = 0;
	leader = ",\n     *\t";
      }
    }
  }
  if(used)fprintf(fh,"%s\n",trailer);
}
/************************************************************************/
private void bug(a,b)
char *a,*b;
{
  fprintf(stderr,a,b); fprintf(stderr,"\n");
  perror("Flint");
  exit(0);
}
/************************************************************************/
private void error(text)
char *text;
{
  if(library_mode)return;
  if(log != NULL){
    fprintf(log,"### Warning: %s\n",text);
  }
  if(!given){
    if(msg == NULL)printf("Routine %s.\n",routine.name);
    else	  fprintf(msg,"Routine %s.\n",routine.name);
  }
  if(msg == NULL)printf(" %s\n",text);
  else          fprintf(msg," %s\n",text);
  given = TRUE;
}
/************************************************************************/
private void define_intrinsics()
/*
  Declare all the instrinsic functions.
------------------------------------------------------------------------*/
{
  int n,i;

  static int flag1[1] = {F_REAL|F_DOUBLE|F_IN};
  static int flag2[1] = {F_INTEGER|F_REAL|F_DOUBLE|F_COMPLEX|F_IN};
  static int flag3[1] = {F_REAL|F_DOUBLE|F_COMPLEX|F_IN};
  static int flag4[1] = {F_COMPLEX|F_IN};
  static int flag5[1] = {F_INTEGER|F_IN};
  static int flag6[1] = {F_CHAR|F_IN};
  static int flag7[2] = {F_INTEGER|F_REAL|F_DOUBLE|F_IN,
			 F_INTEGER|F_REAL|F_DOUBLE|F_IN};
  static int flag8[2] = {F_REAL|F_DOUBLE|F_IN,
			 F_REAL|F_DOUBLE|F_IN};
  static int flag9[2] = {F_CHAR|F_IN,
			 F_CHAR|F_IN};
  static int flag_len[1] = {F_CHAR};
#define MAXARG 8
  static int flag_minmax[MAXARG] = {F_INTEGER|F_REAL|F_DOUBLE|F_IN,
				    F_INTEGER|F_REAL|F_DOUBLE|F_IN,
				    F_INTEGER|F_REAL|F_DOUBLE|F_IN,
				    F_INTEGER|F_REAL|F_DOUBLE|F_IN,
				    F_INTEGER|F_REAL|F_DOUBLE|F_IN,
				    F_INTEGER|F_REAL|F_DOUBLE|F_IN,
				    F_INTEGER|F_REAL|F_DOUBLE|F_IN,
				    F_INTEGER|F_REAL|F_DOUBLE|F_IN};

#define def_intrinsic(name,narg,flags,args) {name,\
	flags|F_INTRINSIC|F_ROUTINE,narg,args,NULL}

  static SYMBOL intrinsics[] = {
  def_intrinsic( "aint",  1, F_GENERIC, flag1),
  def_intrinsic( "anint", 1, F_GENERIC, flag1),
  def_intrinsic( "nint",  1, F_INTEGER, flag1),
  def_intrinsic( "tan",   1, F_GENERIC, flag1),
  def_intrinsic( "log10", 1, F_GENERIC, flag1),
  def_intrinsic( "asin",  1, F_GENERIC, flag1),
  def_intrinsic( "acos",  1, F_GENERIC, flag1),
  def_intrinsic( "atan",  1, F_GENERIC, flag1),
  def_intrinsic( "sinh",  1, F_GENERIC, flag1),
  def_intrinsic( "cosh",  1, F_GENERIC, flag1),
  def_intrinsic( "tanh",  1, F_GENERIC, flag1),
  def_intrinsic( "abs",   1, F_SPECIAL|F_GENERIC, flag2),
  def_intrinsic( "real",  1, F_REAL,	flag2),
  def_intrinsic( "dble",  1, F_DOUBLE,	flag2),
  def_intrinsic( "int",   1, F_INTEGER,	flag2),
  def_intrinsic( "cos",   1, F_GENERIC,	flag3),
  def_intrinsic( "sin",   1, F_GENERIC,	flag3),
  def_intrinsic( "exp",   1, F_GENERIC,	flag3),
  def_intrinsic( "log",   1, F_GENERIC,	flag3),
  def_intrinsic( "sqrt",  1, F_GENERIC,	flag3),
  def_intrinsic( "aimag", 1, F_REAL,	flag4),
  def_intrinsic( "conjg", 1, F_COMPLEX,	flag4),
  def_intrinsic( "char",  1, F_CHAR,	flag5),
  def_intrinsic( "ichar", 1, F_INTEGER,	flag6),
  def_intrinsic( "len",   1, F_INTEGER,	flag_len),
  def_intrinsic( "mod",   2, F_GENERIC,	flag7),
  def_intrinsic( "dim",   2, F_GENERIC,	flag7),
  def_intrinsic( "sign",  2, F_GENERIC,	flag7),
  def_intrinsic( "min",   2, F_SPECIAL|F_GENERIC,	flag_minmax),
  def_intrinsic( "max",   2, F_SPECIAL|F_GENERIC,	flag_minmax),
  def_intrinsic( "cmplx", 2, F_SPECIAL|F_COMPLEX,	flag7),
  def_intrinsic( "atan2", 2, F_GENERIC,	flag8),
  def_intrinsic( "index", 2, F_INTEGER,	flag9),
  def_intrinsic( "lle",   2, F_LOGICAL, flag9),
  def_intrinsic( "llt",   2, F_LOGICAL, flag9),
  def_intrinsic( "lge",   2, F_LOGICAL, flag9),
  def_intrinsic( "lgt",   2, F_LOGICAL, flag9)};

/* Add all these intrinsic functions to the standard functions list. */

  n = sizeof(intrinsics)/sizeof(SYMBOL);
  for(i=0; i < n; i++)(void)add_symbol(&intrinsics[i],rhash);
}
/************************************************************************/
private void define_specifics()
/*
  Declare the specific names for the intrinsic functions.
------------------------------------------------------------------------*/
{
  int n,i;

  static int flag_i1[1] =      {F_INTEGER|F_IN};
  static int flag_r1[1] =      {F_REAL|F_IN};
  static int flag_d1[1] =      {F_DOUBLE|F_IN};
  static int flag_c1[1] =      {F_COMPLEX|F_IN};
  static int flag_i2[2] =      {F_INTEGER|F_IN,  F_INTEGER|F_IN};
  static int flag_r2[2] =      {F_REAL   |F_IN,  F_REAL   |F_IN};
  static int flag_d2[2] =      {F_DOUBLE |F_IN,  F_DOUBLE |F_IN};
  static int flag_ix[MAXARG] = {F_INTEGER|F_IN,  F_INTEGER|F_IN,
				F_INTEGER|F_IN,  F_INTEGER|F_IN,
				F_INTEGER|F_IN,  F_INTEGER|F_IN,
				F_INTEGER|F_IN,  F_INTEGER|F_IN};
  static int flag_rx[MAXARG] = {F_REAL   |F_IN,  F_REAL   |F_IN,
				F_REAL   |F_IN,  F_REAL   |F_IN,
				F_REAL   |F_IN,  F_REAL   |F_IN,
				F_REAL   |F_IN,  F_REAL   |F_IN};
  static int flag_dx[MAXARG] = {F_DOUBLE |F_IN,  F_DOUBLE |F_IN,
				F_DOUBLE |F_IN,  F_DOUBLE |F_IN,
				F_DOUBLE |F_IN,  F_DOUBLE |F_IN,
				F_DOUBLE |F_IN,  F_DOUBLE |F_IN};

  static SYMBOL intrinsics[] = {
  def_intrinsic( "dsqrt",   1,	F_DOUBLE,  flag_d1),
  def_intrinsic( "csqrt",   1,	F_COMPLEX, flag_c1),
  def_intrinsic( "alog",    1,	F_REAL,    flag_r1),
  def_intrinsic( "dlog",    1,  F_DOUBLE,  flag_d1),
  def_intrinsic( "clog",    1,  F_COMPLEX, flag_c1),
  def_intrinsic( "alog10",  1,  F_REAL,    flag_r1),
  def_intrinsic( "dlog10",  1,  F_DOUBLE,  flag_d1),
  def_intrinsic( "dexp",    1,  F_DOUBLE,  flag_d1),
  def_intrinsic( "cexp",    1,  F_COMPLEX, flag_c1),
  def_intrinsic( "dsin",    1,  F_DOUBLE,  flag_d1),
  def_intrinsic( "csin",    1,  F_COMPLEX, flag_c1),
  def_intrinsic( "dcos",    1,  F_DOUBLE,  flag_d1),
  def_intrinsic( "ccos",    1,  F_COMPLEX, flag_c1),
  def_intrinsic( "dtan",    1,  F_DOUBLE,  flag_d1),
  def_intrinsic( "dasin",   1,  F_DOUBLE,  flag_d1),
  def_intrinsic( "dacos",   1,  F_DOUBLE,  flag_d1),
  def_intrinsic( "datan",   1,  F_DOUBLE,  flag_d1),
  def_intrinsic( "datan2",  2,  F_DOUBLE,  flag_d2),
  def_intrinsic( "dsinh",   1,  F_DOUBLE,  flag_d1),
  def_intrinsic( "dcosh",   1,  F_DOUBLE,  flag_d1),
  def_intrinsic( "dtanh",   1,  F_DOUBLE,  flag_d1),
  def_intrinsic( "dabs",    1,  F_DOUBLE,  flag_d1),
  def_intrinsic( "cabs",    1,  F_REAL,    flag_c1),
  def_intrinsic( "iabs",    1,  F_INTEGER, flag_i1),
  def_intrinsic( "idint",   1,  F_INTEGER, flag_d1),
  def_intrinsic( "dint",    1,  F_DOUBLE,  flag_d1),
  def_intrinsic( "idnint",  1,  F_INTEGER, flag_d1),
  def_intrinsic( "dnint",   1,  F_DOUBLE,  flag_d1),
  def_intrinsic( "sngl",    1,  F_REAL,    flag_d1),
  def_intrinsic( "ifix",    1,  F_INTEGER, flag_r1),
  def_intrinsic( "float",   1,  F_REAL,    flag_i1),
  def_intrinsic( "amax1",   1,  F_REAL|F_SPECIAL,    flag_rx),
  def_intrinsic( "dmax1",   1,  F_DOUBLE|F_SPECIAL,  flag_dx),
  def_intrinsic( "max0",    1,  F_INTEGER|F_SPECIAL, flag_ix),
  def_intrinsic( "max1",    1,  F_INTEGER|F_SPECIAL, flag_rx),
  def_intrinsic( "amax0",   1,  F_REAL|F_SPECIAL,    flag_ix),
  def_intrinsic( "amin1",   1,  F_REAL|F_SPECIAL,    flag_rx),
  def_intrinsic( "dmin1",   1,  F_DOUBLE|F_SPECIAL,  flag_dx),
  def_intrinsic( "min0",    1,  F_INTEGER|F_SPECIAL, flag_ix),
  def_intrinsic( "min1",    1,  F_INTEGER|F_SPECIAL, flag_rx),
  def_intrinsic( "amin0",   1,  F_REAL|F_SPECIAL,    flag_ix),
  def_intrinsic( "ddim",    2,  F_DOUBLE,  flag_d2),
  def_intrinsic( "idim",    2,  F_INTEGER, flag_i2),
  def_intrinsic( "amod",    2,  F_REAL,    flag_r2),
  def_intrinsic( "dmod",    2,  F_DOUBLE,  flag_d2),
  def_intrinsic( "dsign",   2,  F_DOUBLE,  flag_d2),
  def_intrinsic( "isign",   2,  F_INTEGER, flag_i2)};


/* Add all these specific functions to the standard functions list. */

  n = sizeof(intrinsics)/sizeof(SYMBOL);
  for(i=0; i < n; i++)(void)add_symbol(&intrinsics[i],rhash);
}
/************************************************************************/
private void define_statements()
/*
  Fill in the hash table used to store the valid FORTRAN statements.
------------------------------------------------------------------------*/
{
#define def_statement(name,routine,flag) {name,flag,sizeof(name)-1,NULL,routine}

  char *s;
  int i,hashval;
  static STATEMENT fortstat[]={
    def_statement("backspace(",rewind_statement,0),
    def_statement("blockdata",blockdata_statement,0),
    def_statement("byte",declaration_statement,F_SPECIAL|F_INTEGER),
    def_statement("call",call_statement,0),
    def_statement("character",declaration_statement,F_CHAR),
    def_statement("close(",close_statement,0),
    def_statement("common",common_statement,F_COMMON),
    def_statement("complex",declaration_statement,F_COMPLEX),
    def_statement("continue",simple_statement,0),
    def_statement("data",data_statement,0),
    def_statement("do",do_statement,0),
    def_statement("doubleprecision",declaration_statement,F_DOUBLE),
    def_statement("dowhile(",dowhile_statement,0),
    def_statement("dimension",declaration_statement,F_ARRAY),
    def_statement("else",simple_statement,0),
    def_statement("elseif(",if_elseif_statement,0),
    def_statement("end",end_statement,0),
    def_statement("enddo",simple_statement,0),
    def_statement("endif",simple_statement,0),
    def_statement("endfile(",rewind_statement,0),
    def_statement("equivalence(",ignore_statement,0),
    def_statement("external",declaration_statement,F_ROUTINE),
    def_statement("format(",ignore_statement,0),
    def_statement("function",prog_sub_func_statement,0),
    def_statement("goto",goto_statement,0),
    def_statement("if(",if_elseif_statement,0),
    def_statement("implicit",ignore_statement,0),
    def_statement("include\'",include_statement,0),
    def_statement("inquire(",inquire_statement,0),
    def_statement("integer",declaration_statement,F_INTEGER),
    def_statement("intent(in)",declaration_statement,F_IN),
    def_statement("intent(out)",declaration_statement,F_OUT),
    def_statement("intent(unknown)",declaration_statement,F_POUT),
    def_statement("intrinsic",declaration_statement,F_ROUTINE),
    def_statement("logical",declaration_statement,F_LOGICAL),
    def_statement("open(",open_statement,0),
    def_statement("parameter",parameter_statement,0),
    def_statement("program",prog_sub_func_statement,F_VOID),
    def_statement("read(",read_write_statement,F_OUT),
    def_statement("real",declaration_statement,F_REAL),
    def_statement("return",simple_statement,0),
    def_statement("rewind(",rewind_statement,0),
    def_statement("save",declaration_statement,F_SAVE),
    def_statement("subroutine",prog_sub_func_statement,F_VOID),
    def_statement("stop",simple_statement,0),
    def_statement("write(",read_write_statement,F_IN)};

#define N_STATEMENTS (sizeof(fortstat)/sizeof(STATEMENT))

  for(i=0; i < SHASHSIZE; i++)shash[i] = NULL;

  for(i=0; i < N_STATEMENTS; i++){
    s = fortstat[i].name;
    hashval = (*s - 'a') + (*(s+1) - 'a');
    fortstat[i].fwd = shash[hashval];
    shash[hashval] = &fortstat[i];
  }
}
/************************************************************************/
private void parse_file(name)
char *name;
/*
  Process this particular file. Open the file, then loop through it,
  reading full lines of FORTRAN, then call the routine to parse the
  line.
------------------------------------------------------------------------*/
{
  char line[MAXLINE],tline[MAXTLINE],*in,*out;
  FILE *stream;
  int lines,flag,echo,quotes,column;

  echo = (log != NULL && !library_mode);
  stream = fopen(name,"r");
  if(stream == NULL){ERROR((errmsg,"Error opening %s",name));}
  if(stream == NULL)return;

  in = get_line(line,MAXLINE,stream,&flag,&column);
  while(flag != LINE_END){

/* Loop around getting an input line, append it to the total line, converting
   to lower case, and stripping out blanks as we go. */

    if(flag == LINE_NORMAL){
      lines = 0;
      out = tline;
      do{
	if(echo)fputs(line,log);
        if(hollerith)banish_hollerith(in);
	if(lines++ < 20){
	  quotes = 0;
	  while(*in){
	    if(*in == '\t')column = 8*(column/8) + 7;
	    else if(isspace(*in));
	    else if(isupper(*in))*out++ = *in - 'A' + 'a';
	    else if(*in == '\'') {*out++ = *in; quotes++;}
	    else                 *out++ = *in;
	    in++;
	    column++;
	  }
	}
	column--;		/* Ignore the \n at the end of the line. */
	if(quotes % 2 && lcheck)error("Odd number of quotes on line");
	if(column > 72 && lcheck)error("Line longer than 72 characters");
	in = get_line(line,MAXLINE,stream,&flag,&column);
	while(!interweave && flag == LINE_COMMENT)
		in = get_line(line,MAXLINE,stream,&flag,&column);
      }while(flag == LINE_CONTINUE);
      *out = '\0';
      if(lines > 20)error("Too many continuations -- line ignored");
      else parse_line(tline);

/* Comments and spurious continuation lines. */

    }else if(flag != LINE_END){
      if(flag == LINE_CONTINUE)error("Spurious continuation line ignored");
      if(flag == LINE_BAD)     error("Badly formed line ignored");
      if(echo)fputs(line,log);
      in = get_line(line,MAXLINE,stream,&flag,&column);
    }
  }
  fclose(stream);
}
/************************************************************************/
private char *get_line(line,maxline,stream,flag,colnum)
char *line;
FILE *stream;
int maxline,*flag,*colnum;
/*
  Get a line of FORTRAN, skip over leading blanks or statement labels,
  and determine whether its a continuation line of not. Ignore comments.
------------------------------------------------------------------------*/
{
  char *s,*p;
  int column;

/* Get a line, and return immediately if its an eof. */

  *flag = LINE_END;
  s = fgets(line,maxline,stream);
  if(s == NULL)return(s);

/* Skip over leading blank characters. */

  while(*s == ' ')s++;
  column = s - line + 1;

/* Determine whether it is a continuation, comment or not, and skip over and
   statement label that is present. */

  if(column == 6){
    *flag = LINE_CONTINUE;
    s++;
  }else if((column == 1 && (*s=='C' || *s=='c' || *s=='*' || *s=='#'))){
    *flag = LINE_COMMENT;
  }else{
    while(column < 6 && isdigit(*s)){column++; s++;}
    while(isspace(*s)){
      if(*s == '\t')column = 8*((column-1)/8+1) + 1;
      else column++;
      s++;
    }
    if(!*s)			       *flag = LINE_COMMENT;
    else if(column >= 7 && isalpha(*s))*flag = LINE_NORMAL;
    else 			       *flag = LINE_BAD;
  }

/* If its a normal or continuation line, remove trailing white chars.
   There is at least one non-white char in the line. */

  if(*flag == LINE_NORMAL || *flag == LINE_CONTINUE){
    p = s + strlen(s);
    while(isspace(*(p-1)))p--;
    *p++ = '\n'; *p++ = '\0';
  }
  *colnum = column - 1;
  return(s);
}
/************************************************************************/
private void parse_line(s)
char *s;
/*
  Determine what sort of FORTRAN statement this line represents,
  then call the appropriate routine to proccess it.
------------------------------------------------------------------------*/
{
  int isexp,hashval;
  char *sd;
  STATEMENT *p;

/* Check to see if this is an assignment statement. */

  isexp = FALSE;
  sd = s;
  if(isalpha(*sd)){
    while(issymbol(*sd))sd++;
    if(*sd == '(')sd = skip_expression(sd);
    if(*sd == '='){
      sd = skip_expression(sd+1);
      isexp = !(*sd);
    }
  }

/* If its an assignment statement, process it. */

  if(isexp){
    assignment_statement(s);

/* Otherwise determine the statements hash code, look it up in the
   hash table, and then process it. */

  }else{
    hashval = max(min((*s - 'a') + (*(s+1) - 'a'), SHASHSIZE-1),0);
    for(p=shash[hashval]; p != NULL; p = p->fwd)
      if(!strncmp(s,p->name,p->length))break;
    if(p == NULL)error("Unrecognised statement");
    else         (*(p->parser))(s,p);
  }
}
/************************************************************************/
private void common_statement(s,f)
char *s;
STATEMENT *f;
/*
  Handle a COMMON statement
------------------------------------------------------------------------*/
{
  char name[MAXVAR];
  STATEMENT statement;

  s += f->length;
  if( *s == '/'){
    s = get_name(s+1,name);
    if( *s == '/')s++;
    else error("Bad COMMON statement");
  }
  statement.name = f->name;
  statement.length = 0;
  statement.flags = f->flags;
  declaration_statement(s,&statement);
}
/************************************************************************/
private void declaration_statement(s,f)
char *s;
STATEMENT *f;
/*
	Handle declaration and COMMON statements.
------------------------------------------------------------------------*/
{
  int flags;
  char variable[MAXVAR];
  STATEMENT statement;

  s += f->length;
  if(!extended &&
    ((f->flags & F_SPECIAL) || ((*s == '*') && !(f->flags & F_CHAR))))
    error("Non-ANSI declaration statement");
  if(*s == '*')s = handle_length(s);

/* Check if its a function declaration. */

  if(isfunction(s,f)){
    statement.name = f->name;
    statement.flags = f->flags;
    statement.length = 8;
    prog_sub_func_statement(s,&statement);
  }else while(*s){
    s = get_name(s,variable);
    flags = f->flags;
    if( *s == '('){
      s = handle_indices(s,INDICE_WILD|INDICE_COLON);
      flags |= F_ARRAY;
    }
    if(*variable)
      (void)set_variable(variable,flags);
    else{
      error("Bad variable name");
      break;
    }

/* Handle whatever is beyond the variable name. */

    if(f->flags & F_CHAR)s = handle_length(s);
    if( *s == ',')
      s++;
    else if( *s != '\0'){
      error("Unexpected characters");
      break;
    }
  }
}
/************************************************************************/
private int issubstring(s)
char *s;
/*
  Determine if the following bracketting looks like substring specs.
------------------------------------------------------------------------*/
{
  int nesting;
  if(*s++ != '(')return(FALSE);
  nesting = 0;
  while( *s && (nesting || (*s != ':' && *s != ')'))){
    if(*s == '(')nesting++;
    else if(*s == ')')nesting--;
    else if(*s == '\''){
      s++;
      while(*s && (*s != '\''))s++;
      if(!*s)s--;
    }
    s++;
  }
  return(*s == ':');
}
/************************************************************************/
private int isfunction(s,f)
char *s;
STATEMENT *f;
/*
  Determine if we are looking at a FUNCTION declaration.
------------------------------------------------------------------------*/
{
  if( (state != PROG_SUB_FUNC_STATE) || !(f->flags & TYPE_MASK)
      || strncmp("function",s,8)     || !isalpha(*(s+8)) )return(FALSE);

  s += 9;
  while(issymbol(*s))s++;
  if( *s++ != '(')return(FALSE);
  if(isalpha(*s) || *s == ')' )return(TRUE);
  return(FALSE);
}
/************************************************************************/
private void parameter_statement(s,f)
char *s;
STATEMENT *f;
/*
  Handle the parameter statement.
------------------------------------------------------------------------*/
{
  char name[MAXVAR];
  int type,dec;

  s += f->length;
  dec = (*s != '(');
  if(dec)s--;
  do{
    s = get_name(s+1,name);
    if(*name && *s++ == '='){
      s = handle_expression(s,&type);
      if(type)(void)set_variable(name,(dec?type|F_PARAMETER:F_PARAMETER));
    }
  }while(*s == ',');
  if(!dec && *s == ')')s++;
  if(*s)error("Bad PARAMETER statement");
  else if(dec)error("Non-ANSI PARAMETER statement");
}
/************************************************************************/
private void do_statement(s,f)
char *s;
STATEMENT *f;
/*
  Handle a DO statement (either the normal or VAX forms).
------------------------------------------------------------------------*/
{
  char *sd,name[MAXVAR];
  int type;

  s += f->length;
  if(isdigit(*s))s = skip_integer(s);
  sd = s;
  s = handle_variable(s,name);
  if(name[0] && *s == '='){
    type = set_variable(name,F_OUT|F_IN);
    if(!(type & F_INTEGER))error("DO loop variable is not an integer");
    do{
      sd = s+1;
      s = handle_expression(sd,&type);
      if(!(type & F_INTEGER))error("DO loop expression is not integer valued");
    }while(s != sd && *s == ',');
  }
  if(*s)error("Bad DO statement");
}
/************************************************************************/
private void open_statement(s,f)
char *s;
STATEMENT *f;
/*
  Handle the OPEN statement.
------------------------------------------------------------------------*/
{
#define def_key(name,flags) {name,flags,0,NULL,NULL}
  static char *defaults[]={"unit"};
  static SYMBOL keys[]={
    def_key("access",	F_CHAR|F_IN),
    def_key("blank",	F_CHAR|F_IN),
    def_key("err",	F_INTEGER|F_IN),
    def_key("file",	F_CHAR|F_IN),
    def_key("form",	F_CHAR|F_IN),
    def_key("iostat",	F_INTEGER|F_OUT),
    def_key("recl",	F_INTEGER|F_IN),
    def_key("status",	F_CHAR|F_IN),
    def_key("unit",	F_INTEGER|F_IN)};
  s = handle_keywords(s + f->length - 1,
			keys, sizeof(keys)/sizeof(SYMBOL),
			defaults,sizeof(defaults)/sizeof(char *));
  if(*s)error("Bad OPEN statement");
}
/************************************************************************/
private void close_statement(s,f)
char *s;
STATEMENT *f;
/*
  Handle a CLOSE statement.
------------------------------------------------------------------------*/
{
  static char *defaults[]={"unit","err","iostat"};
  static SYMBOL keys[]={
    def_key("err",	F_INTEGER|F_IN),
    def_key("iostat",	F_INTEGER|F_OUT),
    def_key("status",	F_CHAR|F_IN),
    def_key("unit",	F_INTEGER|F_IN)};

  s = handle_keywords(s + f->length - 1,
			keys, sizeof(keys)/sizeof(SYMBOL),
			defaults,sizeof(defaults)/sizeof(char *));
  if(*s)error("Bad CLOSE statement");
}
/************************************************************************/
private void rewind_statement(s,f)
char *s;
STATEMENT *f;
/*
  Handle a REWIND, BACKSPACE and ENDFILE statement.
------------------------------------------------------------------------*/
{
  static char *defaults[]={"unit","err","iostat"};
  static SYMBOL keys[]={
    def_key("err",	F_INTEGER|F_IN),
    def_key("iostat",	F_INTEGER|F_OUT),
    def_key("unit",	F_INTEGER|F_IN)};

  s = handle_keywords(s + f->length - 1,
			keys, sizeof(keys)/sizeof(SYMBOL),
			defaults,sizeof(defaults)/sizeof(char *));
  if(*s){ERROR((errmsg,"Bad %s statement",f->name));}
}
/************************************************************************/
private void inquire_statement(s,f)
char *s;
STATEMENT *f;
/*
  Handle a CLOSE statement.
------------------------------------------------------------------------*/
{
  static char *defaults[]={"unit"};
  static SYMBOL keys[]={
    def_key("access",	F_CHAR|F_OUT),
    def_key("blank",	F_CHAR|F_OUT),
    def_key("direct",	F_CHAR|F_OUT),
    def_key("err",	F_INTEGER|F_OUT),
    def_key("exist",	F_LOGICAL|F_OUT),
    def_key("file",	F_CHAR|F_IN),
    def_key("form",	F_CHAR|F_OUT),
    def_key("formatted",F_CHAR|F_OUT),
    def_key("iostat",	F_INTEGER|F_OUT),
    def_key("name",	F_CHAR|F_OUT),
    def_key("named",	F_LOGICAL|F_OUT),
    def_key("nextrec",	F_INTEGER|F_OUT),
    def_key("number",	F_INTEGER|F_OUT),
    def_key("opened",	F_LOGICAL|F_OUT),
    def_key("recl",	F_INTEGER|F_OUT),
    def_key("sequential", F_CHAR|F_OUT),
    def_key("unformatted",F_CHAR|F_OUT),
    def_key("unit",	F_INTEGER|F_IN)};

  s = handle_keywords(s + f->length - 1,
			keys, sizeof(keys)/sizeof(SYMBOL),
			defaults,sizeof(defaults)/sizeof(char *));
  if(*s)error("Bad INQUIRE statement");
}
/************************************************************************/
private void read_write_statement(s,f)
char *s;
STATEMENT *f;
/*
  Handle either a READ or WRITE statement.
------------------------------------------------------------------------*/
{
  int n;
  SYMBOL *p;

  static char *defaults[]={"unit","fmt","iostat","err","end"};
  static SYMBOL keys[]={
    def_key("end",	F_INTEGER|F_IN),
    def_key("err",	F_INTEGER|F_IN),
    def_key("fmt",	F_CHAR|F_INTEGER|F_IN),
    def_key("iostat",	F_INTEGER|F_OUT),
    def_key("rec",	F_INTEGER|F_IN),
    def_key("unit",	F_CHAR|F_INTEGER)};

  n = sizeof(keys)/sizeof(SYMBOL);
  p = &keys[n-1];
  p->flags = (TYPE_MASK & p->flags) | (IO_MASK ^ f->flags);
  s = handle_keywords(s + f->length - 1,
			keys, sizeof(keys)/sizeof(SYMBOL),
			defaults,sizeof(defaults)/sizeof(char *));

/* Handle the variables following READ/WRITE statement. This does not
   handle implied do loops. */

  s = handle_dio_list(s,f->flags);
  if(*s)error("Bad READ or WRITE statement");
}
/************************************************************************/
private void data_statement(s,f)
char *s;
STATEMENT *f;
/*
  Handle a DATA statement.
------------------------------------------------------------------------*/
{
  char *sd;

  s += f->length;
  while( *s){
    sd = s;
    s = handle_dio_list(s,F_DATA);
    if(sd == s){
      error("Bad DATA statement");
      break;
    }else{
      if(*s == '/'){
	s++;
	while(*s != '/'  && *s ){if(*s == '\'')s = skip_string(s); else s++;}
      }
      if(*s == '/')s++;
      if(*s == ',')s++;
    }
  }
}
/************************************************************************/
private void call_statement(s,f)
char *s;
STATEMENT *f;
/*
  Handle the call statement.
------------------------------------------------------------------------*/
{
  int type;
  s = handle_sub_func_call(s+f->length,F_VOID,&type);
  if(*s)error("Bad CALL syntax");
}  
/************************************************************************/
private void assignment_statement(s)
char *s;
/*
  Handle an assignment statement.
------------------------------------------------------------------------*/
{
  char variable[MAXVAR];
  int ok,type,flags;

  ok = FALSE;
  s = handle_variable(s,variable);
  if(*variable && *s == '='){
    s = handle_expression(s+1,&type);
    ok = !(*s);
  }

  if(! ok ) error("Bad assignment statement");
  else{
    flags = set_variable(variable,F_OUT);
    switch(type){
     case F_REAL:
     case F_INTEGER:
     case F_COMPLEX:
     case F_DOUBLE:	flags &= F_REAL|F_INTEGER|F_COMPLEX|F_DOUBLE; break;
     case F_LOGICAL:	flags &= F_LOGICAL;			      break;
     case F_CHAR:	flags &= F_CHAR;			      break;
    }
    if(!flags)error("Assignment type mismatch");
  }
}
/************************************************************************/
private void end_statement(s,f)
char *s;
STATEMENT *f;
/*
  Handle the END statement. Find all the variables that have not been used.
------------------------------------------------------------------------*/
{
  SYMBOL *p;
  int i,flags;

/* Check that this is a well formed END statement. */

  s += f->length;
  if(*s)error("Bad END statement");

/* See which variables have not been used or may be redundant. */

  if(unused || redundant){
    for(i=0;i<HASHSIZE;i++){
      p = vhash[i];
      while(p != NULL){
        if( !(p->flags & (F_COMMON|F_PARAMETER))){
	  if( unused && !(p->flags & (IO_MASK|F_POUT)) )
	    {ERROR((errmsg,"Variable %s was never used.",p->name));}
	  else if( redundant && (p->flags & F_OUT) && !(p->flags & (F_IN|F_POUT|F_ARG)) )
	    {ERROR((errmsg,"Variable %s may be redundant.",p->name));}
        }
        p = p->fwd;
      }
    }
  }

/* Send in what we seem to think the definition of the routine is. */

  if(routine.name != unknown){
    routine.flags |= (library_mode ? 0 : F_ACTUAL);
    for(p=routine.fwd;  p != NULL; p = p->fwd){
      flags = set_variable(p->name,0);
      if(!(flags & F_PIN))flags &= (~F_IN);
      else if(flags & F_POUT)flags &= (~IO_MASK);
      p->flags = ((TYPE_MASK|IO_MASK|F_ROUTINE) & flags);
    }
    (void)set_routine(&routine);
  }

/* Now delete the table of symbols and externals. */

  state = PROG_SUB_FUNC_STATE;
  given = FALSE;
  routine.name = unknown;
  routine.narg = 0;
  clear_hash_table(&(routine.fwd),1);
  clear_hash_table(vhash,HASHSIZE);
}
/************************************************************************/
private void dowhile_statement(s,f)
char *s;
STATEMENT *f;
/*
  Handle the dowhile statement.
------------------------------------------------------------------------*/
{
  int type;
  s = handle_expression(s+f->length-1,&type);
  if(!(type&F_LOGICAL))error("DOWHILE expression is not logical valued");
  if(*s) error("Failed to find end of expression");
}
/************************************************************************/
private void if_elseif_statement(s,f)
char *s;
STATEMENT *f;
/*
  Handle the IF statement.
------------------------------------------------------------------------*/
{
  int type;
  s = handle_expression(s + f->length - 1,&type);

/* Ignore the labels of an arithmetic IF statement. */

  if(*(f->name) == 'i' && isdigit(*s));

/* Handle logical IF or ELSEIF statement. */

  else{
    if(!(type&F_LOGICAL))error("IF or ELSEIF expression is not logical valued");
    if(!strcmp(s,"then"));
    else if(!(*s) || *(f->name) == 'e')error("Bad IF or ELSEIF statement");
    else parse_line(s);
  }
}
/************************************************************************/
private void goto_statement(s,f)
char *s;
STATEMENT *f;
/*
  Handle the goto statement.
------------------------------------------------------------------------*/
{
  int type;

  s += f->length;
  if(*s == '('){
    s = handle_indices(s,0);
    if(*s == ',')s++;
    s = handle_expression(s,&type);
  }else{
    s = skip_integer(s);
  }
  if(*s)error("Bad GOTO statement");
}
/************************************************************************/
private void blockdata_statement(s,f)
char *s;
STATEMENT *f;
/*----------------------------------------------------------------------*/
{
  state = DECLARATION_STATE;
  if( *(s+f->length) )error("Extra characters after BLOCK DATA statement");
}
/************************************************************************/
private void prog_sub_func_statement(s,f)
char *s;
STATEMENT *f;
/*
  Handle a PROGRAM, SUBROUTINE or FUNCTION statement.
------------------------------------------------------------------------*/
{
  char name[MAXVAR];
  SYMBOL *p;

  state = DECLARATION_STATE;
  s = get_name(s + f->length,name);
  if(*name){
    routine.name = malloc(strlen(name)+1);
    strcpy(routine.name,name);
    routine.narg = 0;
    routine.flags = f->flags | F_ROUTINE;
    if(!(F_VOID & f->flags))(void)set_variable(routine.name,f->flags|F_ARG);
    p = &routine;
    if(*s == '(' && *(f->name) != 'p'){
      do{
        s = get_name(s+1,name);
        if(name[0]){
	  p->fwd  = (SYMBOL *)malloc(sizeof(SYMBOL));
	  p = p->fwd;
          p->name = malloc(strlen(name)+1);
          strcpy(p->name,name);
          (void)set_variable(name,F_PIN|F_ARG);
          p->fwd = NULL;
	  p->narg = 0;
          routine.narg++;
        }
      }while(*s == ',');
    }
    if(*s == ')')s++;
  }
  if(*s) error("Bad SUBROUTINE or FUNCTION statement");
}
/************************************************************************/
private void include_statement(s,f)
char *s;
STATEMENT *f;
/*
  Handle the INCLUDE statement.
------------------------------------------------------------------------*/
{
  char file[MAXVAR];
  int ok,n;

  ok = FALSE;
  s += f->length-1;
  if(*s++ == '\''){
    n = strlen(s)-1;
    if(n && *(s+n)=='\''){
      strncpy(file,s,n);
      file[n] = '\0';
      parse_file(file);
      ok = TRUE;
    }
  }
  if(!ok)error("Bad INCLUDE statement");
}    
/************************************************************************/
private void simple_statement(s,f)
char *s;
STATEMENT *f;
/*
  Handle a statement that consists of a single word (e.g. RETURN, STOP,
  etc). Check that the end-of-line occurs in the place that its expected.
------------------------------------------------------------------------*/
{
  s += f->length;
  if(*s){ERROR((errmsg,"Bad %s statement",f->name));}
}
/************************************************************************/
private void ignore_statement(s,f)
char *s;
STATEMENT *f;
/*
  Some statement that I do not particularly care about. Just ignore it.
------------------------------------------------------------------------*/
{}
/************************************************************************/
private char *skip_expression(s)
char *s;
/*
  Skip over an expression.
------------------------------------------------------------------------*/
{
  int expecting_value,Token;
  char *sd;

  expecting_value = TRUE;

  sd = s;
  while(*s){
    s = skip_token(s,&Token);
    if(expecting_value){
      expecting_value = ( Token & UNARY_MASK );
      if(!(Token & (UNARY_MASK|TYPE_MASK)))break;
    }else{
      expecting_value = ( Token & BINARY_MASK );
      if(!(Token & BINARY_MASK) )break;
    }
    sd = s;
  }
  return(sd);
}
/************************************************************************/
private char *skip_token(s,Token)
char *s;
int *Token;
/*
  Get a token from the expression stream. Handle variables and function
  calls.
------------------------------------------------------------------------*/
{
  *Token = 0;

/* Variable, array or function call. */

  if(isalpha(*s)){
    while(issymbol(*s))s++;
    if(*s == '(')s = skip_enclosed(s);
    if(*s == '(' && issubstring(s)) s = skip_enclosed(s);
    *Token = TYPE_MASK;

/* Arithmetic and character operators. */

  }else if(*s == '+' || *s == '-'){
    s++;
    *Token = TOKEN_PLUS_MINUS;
  }else if(*s == '*' || *s == '/'){
    if(*(s+1) == *s)s += 2;
    else	    s += 1;
    *Token = TOKEN_ARITH;

/* Logical or relational operator, or logical value. Use a linear
   search of an array. The array is ordered in roughly decreasing
   frequency of occurence of the operators. */

  }else if(*s == '.' && isalpha(*(s+1))){
    s = skip_logical(s,Token);

/* Numeric (excluding complex values). */

  }else if(isdigit(*s) || (*s == '.' && isdigit(*(s+1)))){
    s = skip_numeric(s,Token);

/* Complex value or left bracket. */

  }else if(*s == '('){
    s = skip_enclosed(s);
    *Token = TYPE_MASK;

/* Character string. */

  }else if(*s == '\''){
    s = skip_string(s);
    *Token = F_CHAR;
  }
  return(s);
}
/************************************************************************/
private char *skip_string(s)
char *s;
/*
  Skip over a string.
------------------------------------------------------------------------*/
{
  while(*s == '\''){
    for(s++; *s && *s != '\''; s++);
    if(*s == '\'')s++;
  }
  return(s);
}
/************************************************************************/
private char *skip_integer(s)
char *s;
/*
  Span across an integer.
------------------------------------------------------------------------*/
{
  while(isdigit(*s))s++;
  return(s);
}
/************************************************************************/
private char *skip_numeric(s,typed)
char *s;
int *typed;
/*
  Skip across an integer, real or double precision number. Determine the
  type as we go. Be careful of periods, otherwise we could screw us
  situations like "1..eq.variable" or "1.eq.variable".
------------------------------------------------------------------------*/
{
  *typed = 0;
  if(!isdigit(*s) && !(*s == '.' && isdigit(*(s+1))))return(s);
  *typed = F_INTEGER;
  if(isdigit(*s))while(isdigit(*s))s++;
  if(*s == '.'){
    if(isalpha(*(s+1)) && isalpha(*(s+2)))return(s);
    s++;
    *typed = F_REAL;
    while(isdigit(*s))s++;
  }
  if(*s == 'd')*typed = F_DOUBLE;
  else if(*s == 'e')*typed = F_REAL;
  else return(s);
  s++;
  if(*s == '+' || *s == '-')s++;
  while(isdigit(*s))s++;
  return(s);
}
/************************************************************************/
private char *skip_logical(s,Token)
char *s;
int *Token;
/*
  Determine what the logical value, relational operator or logical
  operator in the input string is.
------------------------------------------------------------------------*/
{
  int j,length;
  char *sd;
  static struct OPERATORS {char *op; int length,token;}
	operators[16]= {{".and.",  5, TOKEN_LOGICAL},
			{".eq.",   4, TOKEN_RELATIONAL},
			{".eqv.",  5, TOKEN_LOGICAL},
			{".false.",7, F_LOGICAL},
			{".ge.",   4, TOKEN_RELATIONAL},
			{".gt.",   4, TOKEN_RELATIONAL},
			{".le.",   4, TOKEN_RELATIONAL},
			{".lt.",   4, TOKEN_RELATIONAL},
			{".ne.",   4, TOKEN_RELATIONAL},
			{".neqv.", 6, TOKEN_LOGICAL},
			{".not.",  5, TOKEN_NOT},
			{".or.",   4, TOKEN_LOGICAL},
			{".true.", 6, F_LOGICAL},
			{"~~~~",   4, 0},
			{"~~~~",   4, 0},
			{"~~~~",   4, 0}};
  sd = s;
  *Token = 0;
  s++;
  while(isalpha(*s))s++;
  if(*s++ != '.')return(sd);
  length = s - sd;
  if(length < 4)return(sd);
  j = 0;
  if(strncmp(sd,operators[j+8].op,4) >= 0)j += 8;
  if(strncmp(sd,operators[j+4].op,4) >= 0)j += 4;
  if(strncmp(sd,operators[j+2].op,4) >= 0)j += 2;
  if(strncmp(sd,operators[j+1].op,4) >= 0)j += 1;
  if(length != operators[j].length || strncmp(sd,operators[j].op,length))
    return(sd);
  *Token = operators[j].token;
  return(s);
}
/************************************************************************/
private char *skip_enclosed(s)
char *s;
/*
  Skip over a something (?) which is enclosed in brackets. So we keep
  track of bracket nesting and quotes.
------------------------------------------------------------------------*/
{
  int nesting;
  nesting = 0;
  do{
    if(*s == '(')nesting++;
    else if(*s == ')')nesting--;
    else if(*s == '\'')s = skip_string(s) - 1;
    s++;
  }while(*s && nesting);
  return(s);
}
/************************************************************************/
private void banish_hollerith(s)
char *s;
/*
  This crudely filters out Hollerith variables. This replaces the Hollerith
  value with an integer which has the same number of characters. A hollerith
  value split between two lines is not handled correctly. Also this
  expects holleriths to be treated as integers, whereas they might be
  treated as reals or double precisions. This will not cause problems except
  in subroutine argument type checking.
------------------------------------------------------------------------*/
{
  int special,i,n;
  char *s0;

  special = FALSE;
  do{
    if(isspace(*s));
    else if(*s == '(' || *s == '=' || *s == ',')special = TRUE;
    else if(isdigit(*s) && special){		  /* Possible hollerith. */
      s0 = skip_integer(s);
      if(*s0 == 'H' || *s0 == 'h'){		  /* Found a hollerith. */
	n = 0;
	while(isdigit(*s))n = 10*n + *s++ - '0';  /* Determine its length. */
	n++;
	for(i=0; i < n && *s0; i++)*s0++ = '0';	  /* Replace with an int. */
      }
      s = s0 - 1;
      special = FALSE;
    }else if(*s == '\''){
      s = skip_string(s) - 1;
      special = FALSE;
    }else special = FALSE;
    s++;
  }while(*s);
}
/************************************************************************/
private int get_arg_intent(name)
char *name;
/*
  This looks up the intent of an argument of the current subroutine or
  function.
------------------------------------------------------------------------*/
{
  SYMBOL *r,*p;
  int i;

/* Check if this subroutine "argument" is actually the value returned by
   a function. */

  if(!strcmp(routine.name,name))return(F_OUT);

/* Get the description of the current routine. If there is no info, just
   return immediately. */

  r = inquire_routine(routine.name);
  if(!r)return(0);

/* If the number of arguments does not agree, something is screwy, and its
   best to play it safe but admitting nothing. */

  if(r->narg != routine.narg) return(0);

/* Determine which argument we are looking for. */

  p = routine.fwd;
  for(i=0;i < routine.narg; i++){
    if(!strcmp(p->name,name))break;
    p = p->fwd;
  }

/* Did we find something. If so, return the good oil. */

  if(!p) return(0);
  return(*(r->args + i) & IO_MASK);
}
/************************************************************************/
private char *get_name(s,name)
char *s,*name;
/*
  Take as much of the input string as forms a valid FORTRAN variable name.
------------------------------------------------------------------------*/
{
  if(isalpha(*s))while(issymbol(*s))*name++ = *s++;
  *name = '\0';
  return(s);
}
/************************************************************************/
private SYMBOL *get_keyword(name,keys,nkeys)
char *name;
int nkeys;
SYMBOL keys[];
/*
  Perform a binary search to locate a keyword in an array of keywords.
------------------------------------------------------------------------*/
{
  int j,k,m,x;

  if(name==NULL)return(NULL);
  k = 0; m = nkeys - 1;
  while( k <= m){
    j = (k+m)/2;
    x = strcmp(name,(keys[j]).name);
    if( x == 0 )return(&keys[j]);
    if( x < 0 )m = j - 1;
    else       k = j + 1;
  }
  return(NULL);
}
/************************************************************************/
private char *handle_keywords(s,keys,nkeys,defaults,ndefaults)
char *s;
int nkeys,ndefaults;
SYMBOL keys[];
char *defaults[];
/*
  Handle keyword-driven commands such as OPEN, CLOSE, INQUIRE,
  READ and WRITE.
------------------------------------------------------------------------*/
{
  char variable[MAXVAR],*name,*sd;
  int n,flags,type;
  SYMBOL *p;

  n = 0;
  if(*s != '(')error("Bad OPEN, CLOSE, READ, WRITE or INQUIRE statement");
  else do{
    sd = s + 1;
    s = get_name(sd,variable);
    name = variable;
    if(*s++ != '='){
      s = sd;
      if(n < ndefaults)	name = defaults[n];
    }
    n++;
    p = get_keyword(name,keys,nkeys);
    if( p == NULL ){
      ERROR((errmsg,"Keyword %s has been ignored.",name));
      s = skip_expression(s);
    }else if(*s =='*'){
      s++;
      if( strcmp("unit",name) && strcmp("fmt",name))
        {ERROR((errmsg,"Bad syntax for keyword %s.",name));}
    }else if(!strcmp("unit",name) && ( p->flags & F_CHAR) ){
      if(isdigit(*s)){
        s = skip_integer(s);
      }else{
        s = handle_variable(s,variable);
	if(!variable[0]){
	  ERROR((errmsg,"Bad syntax for keyword %s.",p->name));
	  s = skip_expression(s);
        }else if(p->flags & F_IN){
	  (void)set_variable(variable,F_IN);
        }else{
	  flags = set_variable(variable,0);
          if(flags & F_INTEGER)(void)set_variable(variable,F_IN);
	  else if(flags & F_CHAR)(void)set_variable(variable,F_OUT);
	  else {ERROR((errmsg,"Incompatible data type: unit=%s.",variable));}
        }
      }
    }else if(p->flags & F_OUT){
      s = handle_variable(s,variable);
      if(!variable[0]){
	ERROR((errmsg,"Bad syntax for keyword %s.",p->name));
	s = skip_expression(s);
      }else if(!(TYPE_MASK & p->flags & set_variable(variable,F_OUT))){
        ERROR((errmsg,"%s=%s: Incompatible type",p->name,variable));
      }
    }else{
      s = handle_expression(s,&type);
      if(!(type & p->flags))
	{ERROR((errmsg,"Keyword %s: Incompatible type",p->name));}
    }
  }while(*s == ',');
  if(*s == ')')s++;
  return(s);
}
/************************************************************************/
private char *handle_expression(s,typed)
char *s;
int *typed;
/*
  This analyses an expression to determine its data type (logical, real,
  etc). It also flags (as used) the variables used in the expression.
  This routine is also responsible for determining whether a symbol
  represents a function or an array.
  The routine does not to a complete check of the correctness of the
  expression, but if the expression is well formed, then the type
  given is correct.
------------------------------------------------------------------------*/
{
  int nesting,expecting_value,Token,flags;
  char *sd,*s0;

  expecting_value = TRUE;
  nesting = 0;
  flags = 0;

/* Build up a mask of all the operators and variable types used. */

  sd = s;
  s0 = s;
  while(*s){
    if(expecting_value){
      s = handle_value(s,&Token);
      expecting_value = ( Token & (UNARY_MASK|TOKEN_LEFT) );
      if(Token & TOKEN_LEFT)nesting++;
      else if(!(Token & (UNARY_MASK|TYPE_MASK)))break;
    }else{
      s = handle_operator(s,&Token);
      expecting_value = ( Token & BINARY_MASK );
      if((Token & TOKEN_RIGHT) && nesting)nesting--;
      else if(!(Token & BINARY_MASK) )break;
    }
    sd = s;
    flags |= Token;
  }

/* The nesting should be zero, and we should be expecting an operator. */

  if(nesting || expecting_value)sd = s0;

/* Now analyse the mask to determine the type of the expression. */

  if(flags & (TOKEN_RELATIONAL|TOKEN_LOGICAL|F_LOGICAL))*typed = F_LOGICAL;
  else if(flags & F_COMPLEX)*typed = F_COMPLEX;
  else if(flags & F_DOUBLE) *typed = F_DOUBLE;
  else if(flags & F_REAL)   *typed = F_REAL;
  else if(flags & F_INTEGER)*typed = F_INTEGER;
  else if(flags & F_CHAR)   *typed = F_CHAR;
  else *typed = 0;

  return(sd);
}
/************************************************************************/
private char *handle_value(s,Token)
char *s;
int *Token;
/*
  Scan through the input, getting the next value from it.
------------------------------------------------------------------------*/
{
  int flag;
  char *sd,name[MAXVAR];

  sd = s;
  *Token = 0;

/* Variable, array or function call. */

  if(isalpha(*s)){
    s = handle_variable(sd,name);
    if(*name){
      *Token = set_variable(name,F_IN);
    }else{
      s = handle_sub_func_call(sd,0,Token);
    }
    if(s == sd)	*Token = 0;
    else	*Token &= TYPE_MASK;

/* Unary plus or minus. */

  }else if(*s == '+' || *s == '-'){
    s++;
    *Token = TOKEN_PLUS_MINUS;

/* Logical value or .not. */

  }else if(*s == '.' && isalpha(*(s+1))){
    s = skip_logical(s,Token);

/* Numeric (excluding complex values). */

  }else if(isdigit(*s) || (*s == '.' && isdigit(*(s+1)))){
    s = skip_numeric(s,Token);

/* Complex value or left bracket. */

  }else if(*s == '('){
    sd = ++s;
    *Token = TOKEN_LEFT;
    s = skip_numeric(s,&flag);
    if(*s++ != ',' || !flag )return(sd);
    s = skip_numeric(s,&flag);
    if(*s++ != ')' || !flag )return(sd);
    *Token = F_COMPLEX;

/* Character string. */

  }else if(*s == '\''){
    s = skip_string(s);
    *Token = F_CHAR;
  }
  return(s);
}
/************************************************************************/
private char *handle_operator(s,Token)
char *s;
int *Token;
/*
  Scan through the input, getting the next operator from it.
------------------------------------------------------------------------*/
{
  *Token = 0;

/* Arithmetic and character operators. */

  if(*s == '+' || *s == '-'){
    s++;
    *Token = TOKEN_PLUS_MINUS;
  }else if(*s == '*' || *s == '/'){
    if(*(s+1) == *s)s += 2;
    else	    s += 1;
    *Token = TOKEN_ARITH;

/* Logical or relational operator. */

  }else if(*s == '.'){
    s = skip_logical(s,Token);

/* Right bracket. */

  }else if(*s == ')'){
    s++;
    *Token = TOKEN_RIGHT;
  }
  return(s);
}
/************************************************************************/
private char *handle_dio_list(s,flags)
char *s;
int flags;
/*
  This parses a string of variable names or implied do loops, and sets the
  the flags of the appropriate variables.
------------------------------------------------------------------------*/
{
  int type,do_loop;
  char *sd,*s0,name[MAXVAR];

/* Loop over all variables in the list. */

  s--;
  do{
    sd = s + 1;

/* Explore a bit to see if we have an implied do loop. */

    if( *sd == '('){
      if(flags & F_IN){
	s0 = skip_expression(sd+1);
	do_loop = ( (s0 != sd) && (*sd != ')') );
      }else{
	do_loop = TRUE;
      }
    }else do_loop = FALSE;

/* Handle an implied do loop, an expression or a variable. */

    if(do_loop){
      s = handle_implied_do_loop(sd,flags);
    }else if(flags & F_IN){
      s = handle_expression(sd,&type);
    }else{
      s = handle_variable(sd,name);
      if(*name)(void)set_variable(name,flags);
    }
  }while(s != sd && *s == ',');
  return(s);
}
/************************************************************************/
private char *handle_implied_do_loop(s,flags)
char *s;
int flags;
/*
  Handle an implied do loop.
------------------------------------------------------------------------*/
{
  char *ss,*se,*s0;
  char name[MAXVAR];
  int type;

/* Remember the begining, but find the end of the variable list. */

  ss = s;
  do{
    s0 = s + 1;
    s = skip_expression(s0);
  }while(s != s0 && *s == ',');
  if(*s != '=')return(ss);
  if(handle_variable(s0,name) != s)return(ss);
  if(!*name)return(ss);
  if(!(set_variable(name,F_IN|F_OUT) & F_INTEGER))
    error("Non-integer implied do loop index");

/* How handle the do loop limit expressions. */

  do{
    se = s + 1;
    s = handle_expression(se,&type);
    if(type != F_INTEGER)error("Non-integer implied do loop expression");
  }while(s != se && *s == ',');
  if(*s != ')')return(ss);
  se = s + 1;

/* Handle the variables, etc, within the implied do loop, and return
   a pointer to the begining or end, depending on success. */

  *(s0-1) = '\0';
  s = handle_dio_list(ss+1,flags);
  return(*s ? ss : se);
}
/************************************************************************/
private char *handle_length(s)
char *s;
/*
  Skip over a string length specification.
------------------------------------------------------------------------*/
{
  if(*s != '*')return(s);
  else if(isdigit(*(s+1)))return(skip_integer(s+1));
  else return(handle_indices(s+1,INDICE_1ONLY|INDICE_WILD));
}
/************************************************************************/
private char *handle_indices(s,flags)
char *s;
int flags;
/*
  This analyses and skips over array index and substring specifications.
------------------------------------------------------------------------*/
{
  int type;
  char *s0;

  s0 = s;
  if(*s != '(')return(s0);
  do{
    s++;
    if((flags & INDICE_WILD) && (*s == '*')){
      flags |= INDICE_1ONLY;
      s++;
    }else{
      s = handle_expression(s,&type);
      if(type && type != F_INTEGER)
	error("Non-integer array or string subscript");
      if((type || (flags & INDICE_NULL)) && (flags & INDICE_COLON)
        && *s == ':'){
	s = handle_expression(s+1,&type);
	if(type && type != F_INTEGER)
	  error("Non-integer array or string subscript");
      }
    }
  }while(*s == ',' && !(flags & INDICE_1ONLY));
  if(*s++ != ')')s = s0;
  return(s);
}
/************************************************************************/
private char *handle_variable(s,variable)
char *s,*variable;
/*
  This looks at the next bit of the code, to see if it represents a simple
  variable (possibly with indices, if the variable represents an array).
  Be careful! It might represent a function call. We can only tell this if
  it is not an array, yet is followed by a bracket. Character variables
  are trickier, because they can be followed by substring specs.
------------------------------------------------------------------------*/
{
  char *sd;
  int flags;

  sd = get_name(s,variable);
  if(*variable){
    if(*sd == '(')flags = inquire_variable(variable);
    else          flags = 0;
    if( !(flags & F_ROUTINE)){
      if( *sd == '(' && (flags & F_ARRAY) )sd = handle_indices(sd,0);
      if( *sd == '(' && (flags & F_CHAR) && issubstring(sd) )
		sd = handle_indices(sd,INDICE_1ONLY|INDICE_COLON|INDICE_NULL);
    }
    if( *sd == '(' ) sd = s;
  }
  if(s == sd)*variable = '\0';
  return(sd);
}
/************************************************************************/
private char *handle_sub_func_call(s,rflags,type)
char *s;
int *type,rflags;
/*
  Handle a subroutine or function call.
------------------------------------------------------------------------*/
{
  char rname[MAXVAR],name[MAXVAR],*s1,*s2,*vname;
  int flags,*fa,mask,nargs;
  SYMBOL route,*p,*q;

  s1 = get_name(s,rname);
  route.name = rname;
  route.narg = 0;
  route.flags = rflags;
  route.fwd = NULL;
  p = &route;
  if(*s1 == '('){
    do{
      s2 = s1 + 1;

/* If its a simple variable name, determine (as best we can) whether it is
   passed in or out. */

      s1 = handle_variable(s2,name);
      if(*name && (*s1 == ')' || *s1 == ',')){
        vname = malloc(strlen(name)+1);
        strcpy(vname,name);
        flags = set_variable(name,0);

/* If the variable is a parameter, then it must be input. */

	if(flags & F_PARAMETER){
	  flags = (flags & TYPE_MASK) | F_IN;

/* If the variable is a subroutine arg, then scavenge info about it from
   any intent info available for this routine. */

	}else if( (flags & F_ARG) && !(flags & (F_OUT|F_ROUTINE)) ){
	  flags &= TYPE_MASK;
	  switch(get_arg_intent(name)){
	    case F_IN:	flags |= F_IN;				break;
	    case F_OUT: flags |= (initialisation ? F_OUT : 0);	break;
	    default:						break;
	  }

/* If the variable has not been initialised, then it must be output
   by the routine. Skip this if we are not performing "initialisation"
   checking. */

	}else if(initialisation &&
		!(flags & (F_OUT|F_COMMON|F_SAVE|F_DATA|F_PIN|F_ROUTINE))){
	  flags = (flags & TYPE_MASK) | F_OUT;

/* Otherwise we have not a clue whether its input or output. */

        }else{ 
	  flags &= (TYPE_MASK | F_ROUTINE);
	}

/* Expressions are always input. Determine the type of the expression. */

      }else{
        s1 = handle_expression(s2,&flags);
        flags |= F_IN;
        vname = NULL;
      }
      if(s2 != s1){
        p->fwd = (SYMBOL *)malloc(sizeof(SYMBOL));
        p = p->fwd;
        p->fwd = NULL;
	p->narg = 0;
        p->name = vname;
        p->flags = flags;
        route.narg++;
      }
    }while(*s1 == ',');
    if(*s1 == ')')s1++;
    else error("Bad CALL or function call syntax");
  }

/* Compare this with previous usage of this subroutine. */

  q = set_routine(&route);

/* Determine the type of the output of the function call. */

  *type = (q->flags & TYPE_MASK);
  if( (q->flags & F_GENERIC) && route.narg > 0 )
	*type = ( (route.fwd)->flags & TYPE_MASK);

/* Now indicate the use of simple variables passed to the subroutine.
   If we cannot determine what the use of the variable was,
   assume it was used both as input and output. */

  for(fa = q->args, p=route.fwd, nargs=q->narg; p != NULL && nargs > 0;
	p = p->fwd, fa++, nargs--){
    if(p->name != NULL){
      mask = IO_MASK & *fa;
      if(!mask) mask = F_POUT;
      (void)set_variable(p->name,mask);
    }
  }

/* Delete the things that this routine has allocated. */

  clear_hash_table(&(route.fwd),1);
  return(s1);
}
/************************************************************************/
private SYMBOL *inquire_routine(name)
char *name;
/*
  Find info about a particular routine.
------------------------------------------------------------------------*/
{
  return(find_symbol(name,rhash));
}
/************************************************************************/
private SYMBOL *set_routine(route)
SYMBOL *route;
/*
  Define a subroutine or function. First this looks up to see if the
  function has been defined before. If so, it compares the definitions
  and reports on any anomolies.
------------------------------------------------------------------------*/
{
  int *fa, old_io, new_io, i, init,flags,frozen;
  SYMBOL *p,*q;
  char old_use[32],new_use[32];

/* Get an entry for this subroutine. It seems I have to search everywhere. */

  flags = route->flags;
  init = FALSE;
  p = find_symbol(route->name,vhash);
  if(p != NULL && (p->flags & F_ROUTINE) && (p->flags & F_ARG)){
    if(!(p->flags & F_ROUTINE)){
      ERROR((errmsg,"Routine %s does not appear in an external statement",
	     route->name));
      p->flags |= F_ROUTINE;
    }
    init = !(p->flags & (F_IN|F_OUT));
  }else p = find_symbol(route->name,rhash);
  if(p == NULL){
    p = create_symbol(route->name,rhash);
    init = TRUE;
  }
/* It might be the min,max,cmplx or abs function. These need special
   processing, as they do not fall into the scheme of things. */

  if(p->flags & F_SPECIAL){
    if(!strcmp("abs",p->name)){
      if(route->narg >= 1){
	flags = TYPE_MASK & route->fwd->flags;
	if(flags == F_COMPLEX)flags = F_REAL;
	p->flags = (p->flags & ~TYPE_MASK)|flags;
      }
    }else if(!strcmp("cmplx",p->name)) p->narg = min(max(route->narg,1),2);
    else			       p->narg = min(max(route->narg,2),MAXARG);
  }

/* If its not an intrinsic, indicate that the function has been used. */

  if(!(p->flags & F_INTRINSIC)){
    flags = set_variable(route->name,route->flags|F_ROUTINE|F_IN|F_OUT);
  }else{
    flags |= p->flags;
  }

/* Copy the new definition if needed. */

  if(init){
    p->flags = flags;
    p->narg = route->narg;
    if(p->narg > 0){
      p->args = (int *)malloc(sizeof(int)*(route->narg));
      fa = p->args;
      q = route->fwd;
      for(i=0;i<route->narg;i++){
        *fa++ = q->flags;
        q = q->fwd;
      }
    }
  }

/* See if this definition is frozen. */

  frozen = p->flags & F_INTRINSIC ;

/* Compare old and new definitions, and update them. */

  if(p->narg != route->narg)
    {ERROR((errmsg,"Routine %s; previously %d args; now %d args.",
      p->name,p->narg,route->narg));}
  if( !frozen )p->narg = min( route->narg, p->narg );

  if( !(p->flags & flags & TYPE_MASK) )
    {ERROR((errmsg,"Routine %s; previously %s; now %s.",
	p->name,usage(p->flags,old_use),usage(flags,new_use)));}
  if(!frozen)p->flags |= flags;

  fa = p->args;
  q = route->fwd;
  for(i=0; i < p->narg; i++){
    old_io = *fa & IO_MASK;   new_io = q->flags & IO_MASK;
    if( !(*fa & q->flags & (TYPE_MASK|F_ROUTINE)) ||
         ( old_io && new_io && (old_io != new_io)) ){
      if(q->name == NULL){
	ERROR((errmsg,"Arg %d of %s; previously %s; now %s.",i+1,
	  p->name,usage(*fa,old_use),usage(q->flags,new_use)));
      }else{
	ERROR((errmsg,"Arg %d (%s) of %s; previously %s; now %s.",i+1,q->name,
	  p->name,usage(*fa,old_use),usage(q->flags,new_use)));
      }
    }
    if(!frozen)
	*fa = (q->flags & (TYPE_MASK|F_ROUTINE))|( new_io ? new_io : old_io );
    fa++;
    q = q->fwd;
  }
  return(p);	  
}
/************************************************************************/
private char *usage(flags,line)
int flags;
char *line;
/*
  Format the particular usage of this variable into something nice and neat.
------------------------------------------------------------------------*/
{
  char *type,*io,*spacer,*null;

  null = "";

/* Handle functions and externals as special cases. */

  if(flags & F_ROUTINE){
    io = null;
    switch(flags & TYPE_MASK){
      case F_LOGICAL:	type = "logical function";	break;
      case F_INTEGER:	type = "integer function";	break;
      case F_REAL:	type = "real function";		break;
      case F_DOUBLE:	type = "double function";	break;
      case F_COMPLEX:	type = "complex function";	break;
      case F_CHAR:	type = "character function";	break;
      case F_VOID:	type = "subroutine";		break;
      case F_GENERIC:	type = "intrinsic function";	break;
      default:		type = "external";		break;
    }
  }else{
    switch(flags & TYPE_MASK){
      case F_LOGICAL:	type = "logical";		break;
      case F_INTEGER:	type = "integer";		break;
      case F_REAL:	type = "real";			break;
      case F_DOUBLE:	type = "double";		break;
      case F_COMPLEX:	type = "complex";		break;
      case F_CHAR:	type = "character";		break;
      default:		type = null;			break;
    }
    switch(flags & IO_MASK){
      case F_IN:	io = "input";			break;
      case F_OUT:	io = "output";			break;
      case IO_MASK:	io = "input/output";		break;
      default:		io = null;			break;
    }
  }
  if(io == null && type == null)     spacer = "unknown";
  else if(io != null && type != null)spacer = " ";
  else				     spacer = null;

  strcpy(line,io);
  strcat(line,spacer);
  strcat(line,type);
  return(line);
}
/************************************************************************/
private int inquire_variable(name)
char *name;
/*
  Look at the characteristics of a variable which should be visible in the
  current routine.
------------------------------------------------------------------------*/
{
  SYMBOL *p;
  p = find_symbol(name,vhash);
  if(p == NULL) return(0);
  return(p->flags);
}
/************************************************************************/
private int set_variable(name,flags)
char *name;
int flags;
/*
	Set flags of a particular variable.
------------------------------------------------------------------------*/
{
  char *s;
  SYMBOL *p;

  p = find_symbol(name,vhash);

/* Create a symbol if necessary, and check that it is valid. */

  if(p == NULL){
    p = create_symbol(name,vhash);
    if(!extended){
      if(strlen(name) > MAXVARLEN){
        ERROR((errmsg,"Name %s is longer than %d characters.",name,MAXVARLEN));
      }
      s = name;
      while(*s && *s != '_' && *s != '$')s++;
      if(*s){ERROR((errmsg,"Name %s contains nonstandard characters",name));}
    }
  }

/* If a variable has already been used as output, ignore the possibility
   that its possibly been assigned by a subroutine. There is something
   more devious about this, but I cannot remember what. */

  if((p->flags & IO_MASK) == IO_MASK)flags &= ~F_POUT;

/* Check for case of undefined variable. */

  p->flags |= flags;
  if( !(p->flags & TYPE_MASK) &&
      (  ( !(p->flags & F_ROUTINE) && !(flags & F_ARG) ) ||
         (  (p->flags & F_ROUTINE) &&  (p->flags & IO_MASK) )
      )
    ){
    if(declare){
    ERROR((errmsg,"Variable or function %s was not declared.",p->name));}
    p->flags |= (( *(p->name) >= 'i' && *(p->name) <= 'n' ) ? F_INTEGER :
								F_REAL );
  }

/* Check for a variable which was used before it was initialised. */

  if( initialisation && (p->flags & F_IN) &&
    !(p->flags & (F_DATA|F_SAVE|F_COMMON|F_OUT|F_PARAMETER|F_PIN|F_POUT))){
    ERROR((errmsg,"Variable %s has not been initialised.",name));
    p->flags |= F_OUT;
  }

/* If it is assigned to before it is used as input, then it cannot
   possibly be input from the calling routine. Also this does not make
   sense if the variable appears in a DATA statement. */

  if( initialisation && (p->flags & F_OUT) &&
    !(p->flags & (F_POUT|F_IN))){
      if(p->flags & F_DATA)
        {ERROR((errmsg,"Data statement for %s seems useless.",p->name));}
      p->flags &= ~F_PIN;
    }

  return(p->flags);
}
/************************************************************************/
private void clear_hash_table(hash,nhash)
SYMBOL *hash[];
int nhash;
/*
    A subroutine has ended. Go through the variable table, deleting each
    variable entry, and giving warnings about unused variables.
------------------------------------------------------------------------*/
{
  int i;
  SYMBOL *p,*q;
  for(i=0; i<nhash; i++){
    p = hash[i];
    while( p != NULL ){
      q = p->fwd;
      if(p->name != NULL )free((char *)p->name);
      if(p->narg > 0)free((char *)(p->args));
      free((char *)p);
      p = q;
    }
    hash[i] = NULL;
  }
}
/************************************************************************/
private SYMBOL *create_symbol(name,hash)
char *name;
SYMBOL *hash[];
{
  char *s;
  int hashval,length;
  SYMBOL *p;

/* Copy all the data. */

  p = (SYMBOL *)malloc(sizeof(SYMBOL));
  p->flags = 0;
  p->narg  = 0;
  p->args  = NULL;
  length = strlen(name);
  p->name  = malloc(length+1);
  strcpy(p->name,name);

/* Now add the entry to the hash table. */
  
  s = name;
  hashval = 0;
  while(*s != '\0') hashval += *s++;
  hashval %= HASHSIZE;
  p->fwd = hash[hashval];
  hash[hashval] = p;
  return(p);
}
/************************************************************************/
private SYMBOL *add_symbol(p,hash)
SYMBOL *p,*hash[];
{
  char *s;
  int hashval;

/* Now add the entry to the hash table. */
  
  s = p->name;
  hashval = 0;
  while(*s != '\0') hashval += *s++;
  hashval %= HASHSIZE;
  p->fwd = hash[hashval];
  hash[hashval] = p;
  return(p);
}
/************************************************************************/
private SYMBOL *find_symbol(name,hash)
char *name;
SYMBOL *hash[];
/*
	This tries to find a symbol in the given hash table.
------------------------------------------------------------------------*/
{
  int hashval;
  char *s;
  SYMBOL *p;

  s = name;
  hashval = 0;
  while(*s != '\0') hashval += *s++;
  hashval %= HASHSIZE;

  for(p = hash[hashval]; p != NULL ; p = p->fwd){
    if(!strcmp(p->name,name))break;
  }

  return(p);
}
