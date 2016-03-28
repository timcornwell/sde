/*
**	GAUSS - A System for Least Squares and Robust Estimation
**
**	Source Code Copyright (C) 1987 by William H. Jefferys,
**	Michael J. Fitzpatrick and Barbara E. McArthur
**	All Rights Reserved.
*/

# define U(x) x
# define BEGIN yybgin = yysvec + 1 +
# define INITIAL 0
# define YYLERR yysvec
# define YYSTATE (yyestate-yysvec-1)
# define YYOPTIM 1
# define YYLMAX 200
# define yymore() (yymorfg=1)
# define ECHO xprintf(stdout, yyout, "%s",yytext)
# define REJECT { nstr = yyreject(); goto yyfussy;}
int yyleng; extern char yytext[];
int yymorfg;
extern char *yysptr, yysbuf[];
int yytchar;
extern FILE *yyin, *yyout;
extern int yylineno;
struct yysvf { 
	struct yywork *yystoff;
	struct yysvf *yyother;
	int *yystops;};
struct yysvf *yyestate;
extern struct yysvf yysvec[], *yybgin;
# define YYNEWLINE 10
yylex(){
int nstr; extern int yyprevious;
while((nstr = yylook()) >= 0)
yyfussy: switch(nstr){
case 0:
if(yywrap()) return(0); break;
case 1:
	/* groups of blanks and tabs, while significant as delimiters,
		 * are otherwise ignored.
		 */ ;
break;
case 2:
{	/* trailing '\' completely absorbed */
			line_number++;
			fl_stack[cur_fd]->line_number++;
		}
break;
case 3:
	{  /* LOGICAL relational operator token */
		   Echo ; return (process_token(yytext,YOP_LE));  
		}
break;
case 4:
	{  /* LOGICAL relational operator token */
		   Echo ; return (process_token(yytext,YOP_GE));  
		}
break;
case 5:
	{  /* LOGICAL relational operator token */
		   Echo ; return (process_token(yytext,YOP_LT));  
		}
break;
case 6:
	{  /* LOGICAL relational operator token */
		   Echo ; return (process_token(yytext,YOP_GT));  
		}
break;
case 7:
	{  /* LOGICAL relational operator token */
		   Echo ; return (process_token(yytext,YOP_EQ));  
		}
break;
case 8:
	{  /* LOGICAL relational operator token */
		   Echo ; return (process_token(yytext,YOP_NE));  
		}
break;
case 9:
	{  /* LOGICAL relational operator token */
		   Echo ; return (process_token(yytext,YOP_OR));  
		}
break;
case 10:
	{  /* LOGICAL relational operator token */
		   Echo ; return (process_token(yytext,YOP_AND));  
		}
break;
case 11:
	{  /* LOGICAL relational operator token */
		   Echo ; return (process_token(yytext,YOP_NOT));  
		}
break;
case 12:
	{  /* ARITHMETIC operator token */
		   Echo;  process_token(yytext,OP); return(YOP_PLUS);	
		}
break;
case 13:
	{  /* ARITHMETIC operator token */
		   Echo;  process_token(yytext,OP); return(YOP_MINUS);	
		}
break;
case 14:
	{  /* ARITHMETIC operator token */
		   Echo;  process_token(yytext,OP); return(YOP_MUL);	
		}
break;
case 15:
	{  /* ARITHMETIC operator token */
		   Echo;  process_token(yytext,OP); return(YOP_DIV);	
		}
break;
case 16:
	{  /* ARITHMETIC operator token */
		   Echo;  process_token(yytext,OP); return(YOP_EXP);	
		}
break;
case 17:
	{  /* STATEMENT operator token */
		   Echo;  process_token(yytext,OP); return(EQUALS);	
		}
break;
case 18:
		{  /* Left Bracket: increment level number and check
                   **               for new function entrance
		   */
		   Echo;  ++bracket_level;
			return (process_token(yytext,Y_LC));  
		}
break;
case 19:
		{  /*Right Bracket: decrement level number and check
                   **                for new function entrance
		   */
		   Echo; 
		   deltab(bracket_level);  /* clean up symbol table */
		   return (process_token(yytext,Y_RC));  
		}
break;
case 20:
		{  /* Left square bracket */
		   Echo ; return (process_token(yytext,Y_LB));  
		}
break;
case 21:
		{  /* Right square bracket */
		   Echo ; return (process_token(yytext,Y_RB));  
		}
break;
case 22:
		{  /* Left parenthesis - increment level */
		   Echo ; paren_level++ ; return (process_token(yytext,Y_LP));
		}
break;
case 23:
		{  /* Right parenthesis - decrement level */
		   Echo ; paren_level-- ; return (process_token(yytext,Y_RP));
		}
break;
case 24:
		{  /* Semicolon */
		   Echo ; 
			return (process_token(yytext,Y_SMC));  
		}
break;
case 25:
		{  /* Colon */
		   Echo ; return (process_token(yytext,Y_CLN));  
		}
break;
case 26:
		{  /* Comma - used in lists, multiple declarations */
		   Echo ; return (process_token(yytext,Y_COM));  
		}
break;
case 27:
 { 	/* read an integer and convert to float */
			char *p;
		   
		   Echo ;
			pushtxt(wordfill(yytext));
			yylval = (double) atoi (yytext); /* set value */
			return (process_token(yytext,CONST));
		}
break;
case 28:
{
			char *p;
		   Echo ;
			pushtxt(wordfill(yytext));
			yylval = atof (yytext); /* set value */
		 	return (process_token(yytext,CONST));
		}
break;
case 29:
	{   /* keep track of line number for runtime diags */
		    line_number++;
		    fl_stack[cur_fd]->line_number++;
		}
break;
case 30:
{
			char c, fname[20];
			register int i;

			/*  Get the filename to include */
			eat_ws();
			do { c = input(); } while (c != '\"' && c != '\n');
			if (c == '\n') {
				xprintf(stderr,
"\"%s\", line %d: Missing quotes after #include.\n",fl_stack[cur_fd]->fname,
				fl_stack[cur_fd]->line_number);
				fatalerror("","");
			}
			i=(-1);
			do { 
				i++;
				fname[i] = input(); 
			} while (fname[i] != '\"' && fname[i] != '\n');
			/*--i;*/
			if (fname[i] == '\n') {
				xprintf(stderr,
"\"%s\", line %d: Unmatched quote after #include.\n",fl_stack[cur_fd]->fname,
				fl_stack[cur_fd]->line_number);
				fatalerror("","");
			}
			fname[i] = '\0';
			if((fl_list[numincls].file=fopen(fname,"r")) == NULL) {
			    xprintf(stderr,
			      "compile:  error opening include file  '%s\n'",
			      fname);
				fatalerror("","");
			}
			fl_list[numincls].fname = wordfill(fname);
			fl_list[numincls].line_number = 1;
			fl_stack[++cur_fd] = fl_list + numincls;
			numincls++;
			
		}
break;
case 31:
{	/* Ignore a comment. */
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
				xprintf(stderr,
"\"%s\", line %d: Unterminated comment.\n",fl_stack[cur_fd]->fname,
				open_comment_line);
				fatalerror("","");
			   }
		       	   if(c1 == '*' && c2 == '/') break;
		       	   c1 = c2; 
			   c2=input();
		       }	
		}
break;
case 32:
	{  /* DEBUG - runtime: Level 0   */
		   Echo ;    print_instr(opTrace0,""); eat_ws();	
		}
break;
case 33:
	{  /* DEBUG - runtime: Level 1   */
		   Echo ;    print_instr(opTrace1,""); eat_ws();	
		}
break;
case 34:
	{  /* DEBUG - runtime: Level 2   */
		   Echo ;    print_instr(opTrace2,""); eat_ws();	
		}
break;
case 35:
	{  /* DEBUG - runtime: Level 3   */
		   Echo ;    print_instr(opTrace3,""); eat_ws();	
		}
break;
case 36:
	{  /* DEBUG - runtime: Level 4   */
		   Echo ;    print_instr(opTrace4,""); eat_ws();	
		}
break;
case 37:
{

		    /*
		    **  Decide what type of token it is based on
		    **  symbol table information or context.
		    */

			int i,j;
			char *p;
			extern int is_reserved();

			Echo;
			
			if (-1 != (i = is_reserved(yytext))) {
				return process_token(yytext,i);
			} else {
				p = wordfill(yytext);
				pushtxt(p);
				return process_token(yytext,IDENTIFIER);
			}
		}
break;
case 38:
{  /* all else is trash */
		xprintf(stderr, 
		"\"%s\", line %d: Unrecognized input.\n",
		fl_stack[cur_fd]->fname,fl_stack[cur_fd]->line_number);
		fatalerror("","");
	}
break;
case -1:
break;
default:
xprintf(yyout,"bad switch yylook %d",nstr);
} return(0); }
/* end of yylex */



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

int yyvstop[] ={
0,

38,
0,

1,
38,
0,

29,
0,

11,
38,
0,

38,
0,

38,
0,

22,
38,
0,

23,
38,
0,

14,
38,
0,

12,
38,
0,

26,
38,
0,

13,
38,
0,

38,
0,

15,
38,
0,

27,
28,
38,
0,

25,
38,
0,

24,
38,
0,

5,
38,
0,

17,
38,
0,

6,
38,
0,

37,
38,
0,

20,
38,
0,

38,
0,

21,
38,
0,

16,
38,
0,

18,
38,
0,

38,
0,

19,
38,
0,

1,
0,

8,
0,

10,
0,

28,
0,

31,
0,

28,
0,

27,
28,
0,

27,
0,

27,
0,

3,
0,

7,
0,

4,
0,

37,
0,

2,
0,

9,
0,

32,
0,

33,
0,

34,
0,

35,
0,

36,
0,

28,
0,

28,
0,

30,
0,
0};
# define YYTYPE char
struct yywork { YYTYPE verify, advance; } yycrank[] ={
0,0,	0,0,	1,3,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	1,4,	1,5,	
4,31,	0,0,	25,48,	25,49,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	1,6,	4,31,	
1,7,	25,48,	0,0,	1,8,	
8,35,	1,9,	1,10,	1,11,	
1,12,	1,13,	1,14,	1,15,	
1,16,	1,17,	16,37,	34,52,	
34,53,	34,54,	34,55,	34,56,	
0,0,	0,0,	0,0,	1,18,	
1,19,	1,20,	1,21,	1,22,	
6,32,	20,44,	1,23,	1,23,	
21,45,	1,23,	22,46,	2,6,	
1,23,	2,7,	0,0,	0,0,	
2,8,	0,0,	2,9,	2,10,	
2,11,	0,0,	2,13,	2,14,	
2,15,	2,16,	0,0,	36,57,	
36,57,	1,23,	0,0,	0,0,	
1,24,	1,25,	1,26,	1,27,	
2,18,	2,19,	2,20,	2,21,	
2,22,	51,60,	63,64,	64,65,	
0,0,	0,0,	7,33,	15,36,	
15,36,	15,36,	15,36,	15,36,	
15,36,	15,36,	15,36,	15,36,	
15,36,	7,34,	33,51,	36,57,	
36,57,	60,62,	62,63,	0,0,	
1,28,	1,29,	1,30,	29,50,	
0,0,	2,24,	2,25,	2,26,	
2,27,	17,38,	0,0,	17,39,	
17,39,	17,39,	17,39,	17,39,	
17,39,	17,39,	17,39,	17,39,	
17,39,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
17,40,	17,41,	17,40,	17,42,	
17,42,	17,40,	0,0,	0,0,	
0,0,	2,28,	2,29,	2,30,	
58,61,	58,61,	58,61,	58,61,	
58,61,	58,61,	58,61,	58,61,	
58,61,	58,61,	0,0,	17,43,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
17,40,	17,41,	17,40,	17,42,	
17,42,	17,40,	0,0,	23,47,	
23,47,	23,47,	23,47,	23,47,	
23,47,	23,47,	23,47,	23,47,	
23,47,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	17,43,	
23,47,	23,47,	23,47,	23,47,	
23,47,	23,47,	23,47,	23,47,	
23,47,	23,47,	23,47,	23,47,	
23,47,	23,47,	23,47,	23,47,	
23,47,	23,47,	23,47,	23,47,	
23,47,	23,47,	23,47,	23,47,	
23,47,	23,47,	0,0,	0,0,	
0,0,	0,0,	23,47,	0,0,	
23,47,	23,47,	23,47,	23,47,	
23,47,	23,47,	23,47,	23,47,	
23,47,	23,47,	23,47,	23,47,	
23,47,	23,47,	23,47,	23,47,	
23,47,	23,47,	23,47,	23,47,	
23,47,	23,47,	23,47,	23,47,	
23,47,	23,47,	38,38,	38,38,	
38,38,	38,38,	38,38,	38,38,	
38,38,	38,38,	38,38,	38,38,	
40,40,	40,40,	40,40,	40,40,	
40,40,	40,40,	40,40,	40,40,	
40,40,	40,40,	38,57,	38,57,	
0,0,	0,0,	0,0,	0,0,	
0,0,	40,40,	40,40,	40,40,	
40,40,	40,40,	40,40,	42,58,	
0,0,	42,58,	0,0,	0,0,	
42,59,	42,59,	42,59,	42,59,	
42,59,	42,59,	42,59,	42,59,	
42,59,	42,59,	0,0,	0,0,	
40,43,	0,0,	38,57,	38,57,	
0,0,	0,0,	0,0,	0,0,	
0,0,	40,40,	40,40,	40,40,	
40,40,	40,40,	40,40,	57,58,	
0,0,	57,58,	0,0,	0,0,	
57,61,	57,61,	57,61,	57,61,	
57,61,	57,61,	57,61,	57,61,	
57,61,	57,61,	0,0,	0,0,	
40,43,	59,59,	59,59,	59,59,	
59,59,	59,59,	59,59,	59,59,	
59,59,	59,59,	59,59,	0,0,	
0,0};
struct yysvf yysvec[] ={
0,	0,	0,
yycrank+-1,	0,		0,	
yycrank+-38,	yysvec+1,	0,	
yycrank+0,	0,		yyvstop+1,
yycrank+3,	0,		yyvstop+3,
yycrank+0,	0,		yyvstop+6,
yycrank+3,	0,		yyvstop+8,
yycrank+1,	0,		yyvstop+11,
yycrank+2,	0,		yyvstop+13,
yycrank+0,	0,		yyvstop+15,
yycrank+0,	0,		yyvstop+18,
yycrank+0,	0,		yyvstop+21,
yycrank+0,	0,		yyvstop+24,
yycrank+0,	0,		yyvstop+27,
yycrank+0,	0,		yyvstop+30,
yycrank+59,	0,		yyvstop+33,
yycrank+8,	0,		yyvstop+35,
yycrank+87,	0,		yyvstop+38,
yycrank+0,	0,		yyvstop+42,
yycrank+0,	0,		yyvstop+45,
yycrank+4,	0,		yyvstop+48,
yycrank+7,	0,		yyvstop+51,
yycrank+9,	0,		yyvstop+54,
yycrank+143,	0,		yyvstop+57,
yycrank+0,	0,		yyvstop+60,
yycrank+5,	0,		yyvstop+63,
yycrank+0,	0,		yyvstop+65,
yycrank+0,	0,		yyvstop+68,
yycrank+0,	0,		yyvstop+71,
yycrank+3,	0,		yyvstop+74,
yycrank+0,	0,		yyvstop+76,
yycrank+0,	yysvec+4,	yyvstop+79,
yycrank+0,	0,		yyvstop+81,
yycrank+8,	0,		0,	
yycrank+3,	0,		0,	
yycrank+0,	0,		yyvstop+83,
yycrank+19,	yysvec+15,	yyvstop+85,
yycrank+0,	0,		yyvstop+87,
yycrank+218,	0,		yyvstop+89,
yycrank+0,	yysvec+17,	yyvstop+91,
yycrank+228,	0,		0,	
yycrank+0,	yysvec+40,	yyvstop+94,
yycrank+256,	yysvec+40,	0,	
yycrank+0,	0,		yyvstop+96,
yycrank+0,	0,		yyvstop+98,
yycrank+0,	0,		yyvstop+100,
yycrank+0,	0,		yyvstop+102,
yycrank+0,	yysvec+23,	yyvstop+104,
yycrank+0,	yysvec+25,	0,	
yycrank+0,	0,		yyvstop+106,
yycrank+0,	0,		yyvstop+108,
yycrank+2,	0,		0,	
yycrank+0,	0,		yyvstop+110,
yycrank+0,	0,		yyvstop+112,
yycrank+0,	0,		yyvstop+114,
yycrank+0,	0,		yyvstop+116,
yycrank+0,	0,		yyvstop+118,
yycrank+288,	0,		0,	
yycrank+116,	0,		0,	
yycrank+301,	yysvec+40,	yyvstop+120,
yycrank+13,	0,		0,	
yycrank+0,	yysvec+58,	yyvstop+122,
yycrank+5,	0,		0,	
yycrank+2,	0,		0,	
yycrank+2,	0,		0,	
yycrank+0,	0,		yyvstop+124,
0,	0,	0};
struct yywork *yytop = yycrank+358;
struct yysvf *yybgin = yysvec+1;
char yymatch[] ={
00  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,011 ,012 ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
011 ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,'+' ,01  ,'+' ,01  ,01  ,
'0' ,'0' ,'0' ,'0' ,'0' ,'0' ,'0' ,'0' ,
'0' ,'0' ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,'A' ,'B' ,'A' ,'D' ,'D' ,'A' ,'G' ,
'G' ,'G' ,'G' ,'G' ,'G' ,'G' ,'G' ,'G' ,
'G' ,'G' ,'G' ,'G' ,'G' ,'G' ,'G' ,'G' ,
'X' ,'G' ,'G' ,01  ,01  ,01  ,01  ,'G' ,
01  ,'A' ,'B' ,'A' ,'D' ,'D' ,'A' ,'G' ,
'G' ,'G' ,'G' ,'G' ,'G' ,'G' ,'G' ,'G' ,
'G' ,'G' ,'G' ,'G' ,'G' ,'G' ,'G' ,'G' ,
'X' ,'G' ,'G' ,01  ,01  ,01  ,01  ,01  ,
0};
char yyextra[] ={
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0};
/*	ncform	4.1	83/08/11	*/

int yylineno =1;
# define YYU(x) x
char yytext[YYLMAX];
struct yysvf *yylstate [YYLMAX], **yylsp, **yyolsp;
char yysbuf[YYLMAX];
char *yysptr = yysbuf;
int *yyfnd;
extern struct yysvf *yyestate;
int yyprevious = YYNEWLINE;
yylook(){
	register struct yysvf *yystate, **lsp;
	register struct yywork *yyt;
	struct yysvf *yyz;
	int yych;
	struct yywork *yyr;
# ifdef LEXDEBUG
	int debug;
# endif
	char *yylastch;
	/* start off machines */
# ifdef LEXDEBUG
	debug = 0;
# endif
	if (!yymorfg)
		yylastch = yytext;
	else {
		yymorfg=0;
		yylastch = yytext+yyleng;
		}
	for(;;){
		lsp = yylstate;
		yyestate = yystate = yybgin;
		if (yyprevious==YYNEWLINE) yystate++;
		for (;;){
# ifdef LEXDEBUG
			if(debug)xprintf(yyout,"state %d\n",yystate-yysvec-1);
# endif
			yyt = yystate->yystoff;
			if(yyt == yycrank){		/* may not be any transitions */
				yyz = yystate->yyother;
				if(yyz == 0)break;
				if(yyz->yystoff == yycrank)break;
				}
			*yylastch++ = yych = input();
		tryagain:
# ifdef LEXDEBUG
			if(debug){
				xprintf(yyout,"char ");
				allprint(yych);
				}
# endif
			yyr = yyt;
			if ( (int)yyt > (int)yycrank){
				yyt = yyr + yych;
				if (yyt <= yytop && yyt->verify+yysvec == yystate){
					if(yyt->advance+yysvec == YYLERR)	/* error transitions */
						{unput(*--yylastch);break;}
					*lsp++ = yystate = yyt->advance+yysvec;
					goto contin;
					}
				}
# ifdef YYOPTIM
			else if((int)yyt < (int)yycrank) {		/* r < yycrank */
				yyt = yyr = yycrank+(yycrank-yyt);
# ifdef LEXDEBUG
				if(debug)xprintf(yyout,"compressed state\n");
# endif
				yyt = yyt + yych;
				if(yyt <= yytop && yyt->verify+yysvec == yystate){
					if(yyt->advance+yysvec == YYLERR)	/* error transitions */
						{unput(*--yylastch);break;}
					*lsp++ = yystate = yyt->advance+yysvec;
					goto contin;
					}
				yyt = yyr + YYU(yymatch[yych]);
# ifdef LEXDEBUG
				if(debug){
					xprintf(yyout,"try fall back character ");
					allprint(YYU(yymatch[yych]));
					}
# endif
				if(yyt <= yytop && yyt->verify+yysvec == yystate){
					if(yyt->advance+yysvec == YYLERR)	/* error transition */
						{unput(*--yylastch);break;}
					*lsp++ = yystate = yyt->advance+yysvec;
					goto contin;
					}
				}
			if ((yystate = yystate->yyother) && (yyt= yystate->yystoff) != yycrank){
# ifdef LEXDEBUG
				if(debug)xprintf(yyout,"fall back to state %d\n",yystate-yysvec-1);
# endif
				goto tryagain;
				}
# endif
			else
				{unput(*--yylastch);break;}
		contin:
# ifdef LEXDEBUG
			if(debug){
				xprintf(yyout,"state %d char ",yystate-yysvec-1);
				allprint(yych);
				}
# endif
			;
			}
# ifdef LEXDEBUG
		if(debug){
			xprintf(yyout,"stopped at %d with ",*(lsp-1)-yysvec-1);
			allprint(yych);
			}
# endif
		while (lsp-- > yylstate){
			*yylastch-- = 0;
			if (*lsp != 0 && (yyfnd= (*lsp)->yystops) && *yyfnd > 0){
				yyolsp = lsp;
				if(yyextra[*yyfnd]){		/* must backup */
					while(yyback((*lsp)->yystops,-*yyfnd) != 1 && lsp > yylstate){
						lsp--;
						unput(*yylastch--);
						}
					}
				yyprevious = YYU(*yylastch);
				yylsp = lsp;
				yyleng = yylastch-yytext+1;
				yytext[yyleng] = 0;
# ifdef LEXDEBUG
				if(debug){
					xprintf(yyout,"\nmatch ");
					sprint(yytext);
					xprintf(yyout," action %d\n",*yyfnd);
					}
# endif
				return(*yyfnd++);
				}
			unput(*yylastch);
			}
		if (yytext[0] == 0  /* && feof(yyin) */)
			{
			yysptr=yysbuf;
			return(0);
			}
		yyprevious = yytext[0] = input();
		if (yyprevious>0)
			output(yyprevious);
		yylastch=yytext;
# ifdef LEXDEBUG
# endif
		}
	}
yyback(p, m)
	int *p;
{
if (p==0) return(0);
while (*p)
	{
	if (*p++ == m)
		return(1);
	}
return(0);
}
	/* the following are only used in the lex library */
yyinput(){
	return(input());
	}
yyoutput(c)
  int c; {
	output(c);
	}
yyunput(c)
   int c; {
	unput(c);
	}
