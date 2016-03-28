/*
	GaussFit - A System for Least Squares and Robust Estimation

	Source Code Copyright (C) 1987 by William H. Jefferys,
	Michael J. Fitzpatrick and Barbara E. McArthur
	All Rights Reserved.
*/

#include <stdio.h>
#include <ctype.h>
#include "def.h"
#include "symtab.h"

#include "array.h"
#include "datum.h"
#include "simpledefs.h"
#include "protoplasm.h"
#include "alloc.h"

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



struct nlist *symtab ;          /* pointer to beginning of */

/* symbol table            */

/*****  reserved keywords *****/
struct sysw  syswords[] = {
	{ "if",				IF 			},
	{ "else",			ELSE 		},
	{ "elseif",			ELSEIF 		},
	{ "break",			BREAK 		},
	{ "continue",    	CONTINUE 	},
	{ "return",			RETURN 		},
	{ "include",		INCLUDE 	},
	{ "while",			WHILE 		},
	{ "for",			FOR 		}, 
	{ "until",			UNTIL 		}, 
	{ "do",				DO 			},
	{ "parameter",		PARAMETER 	}, 
	{ "observation",	OBSERVATION }, 
	{ "data",			DATUM 		}, 
	{ "variable",		VARIABL  	}, 
	{ "loop",			LOOP 		},
	{ "constant",		CONSTANT 	},
	{ "environment",	ENVIR		}
};

#define NUM_SYSWORDS   sizeof(syswords)/sizeof(struct sysw)

int var_decls[] = { 
	PARAMETER, OBSERVATION, DATUM, VARIABL, CONSTANT };
#define NUM_DECL_TYPES sizeof(var_decls)/sizeof(int)

extern int bracket_level;
extern int saw_param;
extern int saw_eql;
extern int par_type;


struct tlist *tlist, *start;





/*
** INSERT - put a new name into the symbol table.
**    input   -   symbol name and token value
**    output  -   nothing.  Blindly inserts anything
*/

struct nlist *insert(s, val)
char *s;		/* symbol name */
int val;		/* token value */
{
	unsigned long size;
	struct nlist *np;

#ifdef MLDEBUG
	xprintf(stderr,"insert():  s = :%s:\n",s);
#endif

	if ((np=lookup(s)) == NULL) {      /* insert in symtab */
insert_:	
		np = entry_alloc();
		if (np == NULL) {
			xprintf(stderr,
			"Memory allocation fault installing %s in symtab\n",s);
			return(NULL);
		}
		np->next = symtab;
		np->sname = wordfill(s); 
		if (val == NM_PARM) {
			np->tokval = IDENTIFIER;
			np->par_type = VARIABL;
			np->blevel = 1;
		}
		else if (val == NM_FUNC) {
			np->tokval = val;
			np->par_type = par_type;
			np->blevel = 0;
		}
		else if (val == INDEX) {
			np->tokval = IDENTIFIER;
			np->par_type = INDEX;
			np->blevel = 0;
		}
		else {
			np->tokval = val;
			np->par_type = par_type;
			np->blevel = bracket_level;
		}

		/*
		xprintf(stdout, "insert %8s %4d : %4d %4d %4d (%4d)\n", s, val,
			np->tokval, np->par_type, np->blevel, bracket_level);
		*/
		symtab = np;
		return(np);
	} 
	else {
		/* already in symbol table */
		if(np->blevel != bracket_level)
			goto insert_;
		else
			return(np);
	}

}





/*  
**  LOOKUP - check the symbol table for a given entry 's'
**     input    - symbol name in 's'
**     output   - token value
*/

struct nlist *lookup(s)
char *s;		/* sumbol name */
{
	struct nlist *np;

	for(np=symtab; np != NULL; np = np->next)

	{ 
		if(strcmp(s,np->sname) == 0)
			return(np);
			}
	return(NULL);
}






/* 
**   IS_RESERVED - check sysword[] to see if input is a reserved word.
**	returns value field of struct.   -1 is returned if not a sysword.
*/

is_reserved(s)
char *s;
{
	register int i;

	for(i=0;i<NUM_SYSWORDS;i++) {
		if(strcmp(syswords[i].word,s) == 0)
			return (syswords[i].value) ;
	}
	return( -1 );
}




/*
** ENTRY_ALLOC - Allocate space for the structure for a new entry in the
** symbol table
*/

struct nlist *entry_alloc()
{
	return( (struct nlist *) MemAlloc("entry_alloc", (long)sizeof(struct nlist)));
}





/*
**  TLIST_ALLOC - allocate a structure for the token list.
*/

struct tlist *tlist_alloc()
{
	return((struct tlist *)MemAlloc("tlist", (long)sizeof(struct tlist)));
}






/*
**  INIT_TABLE - Initialize the symbol table by placing all reserved words
**  in it and setting pointers to 1st and last
*/

init_table()
{
	register int i;
	struct nlist *np;

	if((np=entry_alloc()) == NULL) {
		xprintf(stderr,"Memory fault initializing symbol table\n");
		fatalerror("","");
	}
	symtab = np;		/* insert IF    */
	np->sname = wordfill("if");
	np->tokval = IF;
	np->blevel = 0;
	np->next = NULL;
	
	for(i=1;i<NUM_SYSWORDS;i++)		/* do the rest of em */
		insert(syswords[i].word,syswords[i].value);
}




/*
**    DELTAB - delete entries from the symbol table based on the current
**    bracket_level.  Called after each '}' encountered to remove local
**    variables from the list.
*/

deltab(blevel)
int blevel;
{
	struct nlist *np;

	
	if(symtab->blevel < blevel) return;
	for(np=symtab; np->blevel >= blevel ; np = symtab) {
		symtab = np->next;
		freemem("deltab", (char *)np);
	}
}



/*  INIT_TOKEN_LIST  -  For error recovery purposes I want to keep a list
**  of tokens seen so that I can hopefully figure out what happened.  TLIST
**  is a doubly linked circular list of tokens.  I made it circular so I
**  can recycle the tokens.
*/

struct tlist *init_token_list()
{
	register int i;
	struct tlist *tlist_alloc(), *tp;

	start = tlist_alloc();		/* get first struct              */
	start->symbol = NULL;
	start->token = -1;
	tp = start;		/* move to next struct		 */
	
	for(i=0; i<TLIST_SIZE; i++) {
		tp->next = tlist_alloc();	/* make another one	 */
		tp->next->back = tp;		/* doubly link it	 */
		tp = tp->next;
		tp->symbol = NULL;
		tp->token = -1;
	}
	tp->next = start;		/* link back to start 		 */
	start->back = tp;		/* now circular			 */
	
	return((struct tlist *)start);	/* send it back			 */
}



/*
**   DUMP_SYMTAB  -  Print out symtab contents for debugging.
*/

dump_symtab(BLEV)
int BLEV;
{
	struct nlist *np;

	xprintf(stderr,"\n\tSYMBOL TABLE BLEV = %d\n",BLEV);
	for(np=symtab;np!=NULL;np=np->next) {
	    if(is_reserved(np->sname)== -1) {
		xprintf(stderr,"blevel=%d\tpar_type=%d\ttoken=%d\tname=:%s:\n",
			np->blevel,np->par_type,np->tokval,np->sname);
	     }
	}
	xprintf(stderr, "\n");
}




/* 
**  IS_DECLARATION_SYSWORD -  Check to see if token value is a type of 
**  declaration.
*/

is_declaration_sysword(val)
int val;
{
	register int i;

	for(i=0; i<NUM_DECL_TYPES; i++) {
		if(val == var_decls[i]) return(1);
	}
	return(0);
}



/*
**  IS_VALID_LHS - Check to see if data type is legal for LHS of equation.
*/

is_valid_lhs(val) int val; { return( (val == VARIABL? 1 : 0) ); }



