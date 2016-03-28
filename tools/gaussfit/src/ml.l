comment		"/*"

EOS		";"
D		[0-9]
H		[0-9a-fA-F]
A		[a-zA-Z]
W		[ \t]*
include		"#include"
t0		"#t0"
t1		"#t1"
t2		"#t2"
t3		"#t3"
t4		"#t4"


%%


[ \t]+		/* groups of blanks and tabs, while significant as delimiters,
		 * are otherwise ignored.
		 */ ;


"\\"[ \t]*\n	{	/* trailing '\' completely absorbed */
			line_number++;
			fl_stack[cur_fd]->line_number++;
		}


"<="		{  /* LOGICAL relational operator token */
		   Echo ; return (process_token(yytext,YOP_LE));  
		}
">="		{  /* LOGICAL relational operator token */
		   Echo ; return (process_token(yytext,YOP_GE));  
		}
"<"		{  /* LOGICAL relational operator token */
		   Echo ; return (process_token(yytext,YOP_LT));  
		}
">"		{  /* LOGICAL relational operator token */
		   Echo ; return (process_token(yytext,YOP_GT));  
		}
"=="		{  /* LOGICAL relational operator token */
		   Echo ; return (process_token(yytext,YOP_EQ));  
		}
"!="		{  /* LOGICAL relational operator token */
		   Echo ; return (process_token(yytext,YOP_NE));  
		}
"||"		{  /* LOGICAL relational operator token */
		   Echo ; return (process_token(yytext,YOP_OR));  
		}
"&&"		{  /* LOGICAL relational operator token */
		   Echo ; return (process_token(yytext,YOP_AND));  
		}
"!"		{  /* LOGICAL relational operator token */
		   Echo ; return (process_token(yytext,YOP_NOT));  
		}

\+		{  /* ARITHMETIC operator token */
		   Echo;  process_token(yytext,OP); return(YOP_PLUS);	
		}
\-		{  /* ARITHMETIC operator token */
		   Echo;  process_token(yytext,OP); return(YOP_MINUS);	
		}
\*		{  /* ARITHMETIC operator token */
		   Echo;  process_token(yytext,OP); return(YOP_MUL);	
		}
\/		{  /* ARITHMETIC operator token */
		   Echo;  process_token(yytext,OP); return(YOP_DIV);	
		}
\^		{  /* ARITHMETIC operator token */
		   Echo;  process_token(yytext,OP); return(YOP_EXP);	
		}
\=		{  /* STATEMENT operator token */
		   Echo;  process_token(yytext,OP); return(EQUALS);	
		}
\{ 		{  /* Left Bracket: increment level number and check
                   **               for new function entrance
		   */
		   Echo;  ++bracket_level;
			return (process_token(yytext,Y_LC));  
		}
\} 		{  /*Right Bracket: decrement level number and check
                   **                for new function entrance
		   */
		   Echo; 
		   deltab(bracket_level);  /* clean up symbol table */
		   return (process_token(yytext,Y_RC));  
		}
\[ 		{  /* Left square bracket */
		   Echo ; return (process_token(yytext,Y_LB));  
		}
\] 		{  /* Right square bracket */
		   Echo ; return (process_token(yytext,Y_RB));  
		}
\( 		{  /* Left parenthesis - increment level */
		   Echo ; paren_level++ ; return (process_token(yytext,Y_LP));
		}
\) 		{  /* Right parenthesis - decrement level */
		   Echo ; paren_level-- ; return (process_token(yytext,Y_RP));
		}
\; 		{  /* Semicolon */
		   Echo ; 
			return (process_token(yytext,Y_SMC));  
		}
\: 		{  /* Colon */
		   Echo ; return (process_token(yytext,Y_CLN));  
		}
\, 		{  /* Comma - used in lists, multiple declarations */
		   Echo ; return (process_token(yytext,Y_COM));  
		}

{D}+(([bB])|({H}*[xX]))?  { 	/* read an integer and convert to float */
			char *p;
		   
		   Echo ;
			pushtxt(wordfill(yytext));
			yylval = (double) atoi (yytext); /* set value */
			return (process_token(yytext,CONST));
		}
(({D}+)|(({D}*"."{D}+)|({D}+"."{D}*)))([eEdD][+-]?{D}+)? {
			char *malloc(), *p;
		   Echo ;
			pushtxt(wordfill(yytext));
			yylval = atof (yytext); /* set value */
		 	return (process_token(yytext,CONST));
		}

\n		{   /* keep track of line number for runtime diags */
		    line_number++;
		    fl_stack[cur_fd]->line_number++;
		}

{include}	{    /*  Screw it - I'll do it later */
			char c, fname[20], *malloc();
			register int i;

			/*  Get the filename to include */
			eat_ws();
			do { c = input(); } while (c != '\"' && c != '\n');
			if (c == '\n') {
				fprintf(stderr,
"\"%s\", line %d: Missing quotes after #include.\n",fl_stack[cur_fd]->fname,
				fl_stack[cur_fd]->line_number);
				exit(0);
			}
			i=(-1);
			do { 
				i++;
				fname[i] = input(); 
			} while (fname[i] != '\"' && fname[i] != '\n');
			/*--i;*/
			if (fname[i] == '\n') {
				fprintf(stderr,
"\"%s\", line %d: Unmatched quote after #include.\n",fl_stack[cur_fd]->fname,
				fl_stack[cur_fd]->line_number);
				exit(0);
			}
			fname[i] = '\0';
			if((fl_list[numincls].file=fopen(fname,"r")) == NULL) {
			    fprintf(stderr,
			      "compile:  error opening include file  '%s\n'",
			      fname);
			    exit(0);
			}
			fl_list[numincls].fname = wordfill(fname);
			fl_list[numincls].line_number = 1;
			fl_stack[++cur_fd] = fl_list + numincls;
			numincls++;
		}

{comment}	{	/* Ignore a comment. */
		       char c1,c2;
			int open_comment_line;

			/* Search till you find a comment terminator */
			open_comment_line = fl_stack[cur_fd]->line_number;
		       c1 = input(); c2=input();
		       while(1) {	/* eat the comment */
			   if(c1 == '\n') { 
				line_number++; 
				fl_stack[cur_fd]->line_number++;
			   } else if (c1 == 0) {
				fprintf(stderr,
"\"%s\", line %d: Unterminated comment.\n",fl_stack[cur_fd]->fname,
				open_comment_line);
				exit(0);
			   }
		       	   if(c1 == '*' && c2 == '/') break;
		       	   c1 = c2; 
			   c2=input();
		       }	
		}

{t0}		{  /* DEBUG - runtime: Level 0   */
		   Echo ;    print_instr(opTrace0,""); eat_ws();	
		}
{t1}		{  /* DEBUG - runtime: Level 1   */
		   Echo ;    print_instr(opTrace1,""); eat_ws();	
		}
{t2}		{  /* DEBUG - runtime: Level 2   */
		   Echo ;    print_instr(opTrace2,""); eat_ws();	
		}
{t3}		{  /* DEBUG - runtime: Level 3   */
		   Echo ;    print_instr(opTrace3,""); eat_ws();	
		}
{t4}		{  /* DEBUG - runtime: Level 4   */
		   Echo ;    print_instr(opTrace4,""); eat_ws();	
		}

[a-zA-Z_][a-zA-Z0-9_]* {

		    /*
		    **  Decide what type of token it is based on
		    **  symbol table information or context.
		    */

			int i,j;
			char *malloc(), *p;
			extern int is_reserved();
			extern struct nlist *insert(), *lookup();

			Echo;
			
			if (-1 != (i = is_reserved(yytext))) {
				return process_token(yytext,i);
			} else {
				p = wordfill(yytext);
				pushtxt(p);
				return process_token(yytext,IDENTIFIER);
			}
		}

.	{  /* all else is trash */
		fprintf(stderr, 
		"\"%s\", line %d: Unrecognized input.\n",
		fl_stack[cur_fd]->fname,fl_stack[cur_fd]->line_number);
		exit(0);
	}
%%



/* 
**  EAT_WS - Skip over any white space: blanks, tabs newlines 
*/
eat_ws()
{
	int c;

	c = input();
	while(c == ' ' || c == '\t' || c == '\n' || c == '\r') { 
		if(c == '\n')  {    /* Process line numbers */
			line_number++;
			fl_stack[cur_fd]->line_number++;
		}
		c = input();
	}
	unput(c);   /* put back last character */
}


/* 
**  OUTPUT - override LEX output routine to print a character to the output
**  file.
*/

output(c) 
char c;
{
     extern FILE *fd_out;
     putc(c,fd_out);
}


/*
**  INPUT - override LEX input routine to read input from the current file.
**  Still manages LEX pointers for fallback chars.
*/

input() {
	extern int cur_fd, colnum;
        char cdbug;

	if(fl_stack[cur_fd]->file == NULL) {
          fprintf(stderr,"NULL file descriptor in input: cur_fd = %d\n",cur_fd);
          fflush(stderr);
	}                                                   

	if(yysptr > yysbuf) {  
		--yysptr;			
		yytchar = U(*yysptr);
	} else {
		yytchar = getc(fl_stack[cur_fd]->file);
	}
	if(yytchar == 10)  { yylineno++;   }
	if(yytchar == EOF) { yytchar = 0;  }           

/**********************************************************************	
   The above is a more readable form of this, which is what is supplied
   with LEX.

   (((yytchar=yysptr>yysbuf?U(*--yysptr):getc(fd_list[cur_fd]))		
    ==10?(yylineno++,yytchar):yytchar)					
    ==EOF?0:yytchar); 
 **********************************************************************/							
    if(yytchar == '#') {    /* INPUT() debug stuff  */
           cdbug = getc(fl_stack[cur_fd]->file);
           if (cdbug == '#') yytchar = 0;
           else ungetc(cdbug,fl_stack[cur_fd]->file);
     }               


    return(yytchar);
}


/*
**  UNPUT - override LEX unput routine.  Returns the character to the 
**  current input stream while adjusting LEX pointers.  Like ungetc().
*/

unput(c)                  
int c;
{
	yytchar= (c);
	if(yytchar=='\n')
		yylineno--;
        *yysptr = yytchar;
	yysptr++;
}





/* 
**  NEXTCHAR() - Return the next character after white space  - doesn't
**  disturb input stream.
*/

nextchar()	
{
	int buf[100];
	int i, c, j;
                                  
	i=0;
	while(1) {
            buf[i] = input();
            i++;  
            if(!white_space(buf[i-1])) break;
	}   
        --i;
        c = buf[i];	
	for(j=i;j>=0;j--) unput(buf[j]); 
  
	return(c);
}


/*
**  WHITE_SPACE() - is character white space??
*/

white_space(c) int c; { return(Index(",; \r\t\n",c)==NULL ? 0 : 1 ); }

/*
**  TYPE_DECL() - Is token a variable declaration type??
*/

type_decl(VAL) int VAL; { return(((VAL>=PARAMETER&&VAL<=VARIABL)?1:0)); }



/*
** Index() - Same as in UNIX: If character 'c' is in the string 'sp' then
** a pointer starting at 'c' is returned.
*/

char *Index(sp,c)
char *sp, c;
{     
	do {
		if(*sp == c)  return(sp);
        	sp++;
	} while(*sp != '\0');
	return(NULL);
}

