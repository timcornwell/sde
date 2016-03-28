/*
	GaussFit - A System	for Least Squares and Robust Estimation

	Source Code Copyright (C) 1987 by William H. Jefferys,
	Michael J. Fitzpatrick and Barbara E. McArthur
	All Rights Reserved.
*/

/* ImitationMIDAS table routines */

#include <stdio.h>
#include <math.h>
#include "strings.h"
#include <ctype.h>
#include "house.h"
#include "defines.h"
#include "array.h"
#include "datum.h"
#include "simpledefs.h"
#include "protoplasm.h"
#include "alloc.h"


char *coltypestr[2] = { "double", "char" };
INFO now;

printchars(fd,table,i,j)
	FILE *fd;
	MIDAS *table;
	long i,j;
{
	rowcolrangecheck(table,i,j,1);
	xprintf(fd,"%s\t",table->rows[i][j].s);
}

loaddouble(table,i,j,word)
	MIDAS *table;
	int i,j;
	char *word;
{
	int num;
	double dnum;

	rowcolrangecheck(table,i,j,1);
	if (strcmp(word,"#N/A") == 0) {  /* if the value is undefined */
		dnum = UND; /* set	double to undefined */
	} else { 
		num = sscanf(word,"%lf",&dnum); /* read	double num from word string */

		/* check argument	return	for function */
		if (num == EOF)
			fatalerror2("Midas Read Error. Attempt to read	double value past end of line number: %d, file: %s.\n",i,table->fname);
		if (num == 0)
			fatalerror("Midas Read Error. No value exists in file: % s.\n",table->fname);
	}
	table->rows[i][j].d = dnum; /*load double number into next position in midasrow */
}

long
strsum(str) 
	char *str;
{
	long sum;
	for (sum = 0; *str;) sum += *str++;
	return sum;
}

MIDAS *midasnew() {
	MIDAS *table;
	
	table = (MIDAS*)MemAlloc("midasnew", sizeof(MIDAS));
	table->maxcols = MIDASCOLS;
	table->maxrows = MIDASROWS;
	table->numcols = 0;
	table->numrows = 0;
	strcpy(table->fname,"Untitled");
	table->cols = (MidasColumn*)MemAlloc("midasnewcol", sizeof(MidasColumn) * table->maxcols);
	table->rows = (MidasElem**)MemAlloc("midasnewrow", sizeof(MidasElem*) * table->maxrows);
	return table;
}

midasfree(table)
	MIDAS *table;
{
	int i,j;
	
	/* free all strings */
	for (i=0; i<table->numcols; ++i) {
		if (table->cols[i].type == midasString) {
			for (j=0; j<table->numrows; ++j) {
				freemem("midasfree",(char*)table->rows[j][i].s);
				table->rows[j][i].s = NULL;
			}
		}
	}
	
	/* free array of column descriptors */
	freemem("midasfree",(char*)table->cols);
	table->cols = NULL;
	
	/* free each row */
	for (j=0; j<table->numrows; ++j) {
		freemem("midasfree",(char*)table->rows[j]);
		table->rows[j] = NULL;
	}
	
	/*free pointers to rows */
	freemem("midasfree",(char*)table->rows);
	table->rows = NULL;
	
}

midasaddrow(table) 
	MIDAS *table;
{
	int i, row;

	row = table->numrows;
	if (++table->numrows >= table->maxrows) {
		table->maxrows += table->maxrows>>2;
		table->rows = (MidasElem**)Reallocate("midasaddrow", sizeof(MidasElem*) * table->maxrows, (char*)table->rows);
	}
	table->rows[row] = (MidasElem*)MemAlloc("midasaddrow2", sizeof(MidasElem) * table->maxcols);
	for (i=0; i<table->numcols; ++i) {
		if (table->cols[i].type == midasDouble) {
			table->rows[row][i].d = 0.;
		} else {
			table->rows[row][i].s = wordalloc("?");
		}
	}
}

midasaddcol(table, name, type) 
	MIDAS *table;
	char *name;
	short type;
{
	int i, col;
	
	col = table->numcols;
	if (++table->numcols >= table->maxcols) {
		table->maxcols += table->maxcols>>2;
		table->cols = (MidasColumn*)Reallocate("midasaddcol", sizeof(MidasColumn) * table->maxcols, (char*)table->cols);
		for (i=0; i<table->numrows; ++i) {
			table->rows[i] = (MidasElem*)Reallocate("midasaddcol2", sizeof(MidasElem) * table->maxcols, (char*)table->rows[i]);
		}
	}
	strcpy(table->cols[col].name, name);
	table->cols[col].namesum = strsum(name); /* name checksum */
	
	table->cols[col].type = type;
	for (i=0; i<table->numrows; ++i) {
		if (type == midasDouble) table->rows[i][col].d = 0.;
		else table->rows[i][col].s = wordalloc("?");
	}
}

midassize(table, rows, cols) 
	MIDAS *table;
	short rows, cols;
{
	short i,j;

	table->numrows = rows;
	table->numcols = cols;
	
	table->maxrows = rows + MIDASROWS;
	table->rows = (MidasElem**)Reallocate("midassize1", sizeof(MidasElem*) * table->maxrows, (char*)table->rows);
	table->maxcols = cols + MIDASCOLS;
	table->cols = (MidasColumn*)Reallocate("midassize2", sizeof(MidasColumn) * table->maxcols, (char*)table->cols);
	for (i=0; i<table->numrows; ++i) {
		table->rows[i] = (MidasElem*)MemAlloc("midassize3", sizeof(MidasElem) * table->maxcols);
	}
	for (j=0; j<table->numcols; ++j) {
		strcpy(table->cols[j].name, "?");
		table->cols[j].type = midasDouble;
		for (i=0; i<table->numrows; ++i) {
			table->rows[i][j].d = 0.;
		}
	}
}

midaschangecoltype(table, col, type) 
	MIDAS *table;
	int col;
	short type;
{	
	int i;

	if (type != table->cols[col].type) {
		table->cols[col].type = type;
		for (i=0; i<table->numrows; ++i) {
			if (type == midasDouble) {
				freemem("coltypechange",(char*)table->rows[i][col].s);
				table->rows[i][col].d = 0.;
			} else {
				table->rows[i][col].s = wordalloc("?");
			}
		}
	}
}

gettableheader(fd, table) /* read in colnames and coltypes from table file */
	FILE *fd;
	MIDAS *table;
{
	char line[MAXLIN], name[LSTRLEN], *lineptr;
	int i;
	
	if (getline(line,MAXLIN-1,fd) == 0) { /* get line */
		fatalerror("Midas Read Error. Unable to read first line of file: %s.\n",table->fname);
	}
	lineptr = line;
	for (i=0; lineptr = getword(lineptr,name); ++i) { /* get name from line, remove it from line */
		midasaddcol(table, name, midasDouble);
	}
	
	if (getline(line,MAXLIN-1,fd) == 0) { /* get line */
		fatalerror("Midas Read Error. Unable to read first line of file: %s.\n",table->fname);
	}
	lineptr = line;
	for (i=0; lineptr = getword(lineptr,name); ++i) { /* get name from line, remove it from line */
		if (strcmp("double", name)==0) {
			table->cols[i].type = midasDouble;
		} else if (strcmp("char", name)==0) {
			table->cols[i].type = midasString;
		}
	}
}

midasread(fd,table) /*readMIDAS table from a file */
	FILE *fd;
	MIDAS *table;
{

	int i,j;
	char *getword();
	char line[MAXLIN],*lineptr, word[LSTRLEN], **charalloc();

	gettableheader(fd, table); /* get column names and types*/
	for (i=0; getline(line,MAXLIN-1,fd) != 0; ++i) { /* get line */
		now.row = i;
		lineptr = line;
		midasaddrow(table);
		for (j=0; j<table->numcols && (lineptr = getword(lineptr,word)); ++j) {
			now.col = j;
			if (table->cols[j].type == midasDouble) {
				loaddouble(table, i, j, word);
			} else {
				table->rows[i][j].s = wordalloc(word);
			}
		}
	}
	fflush(fd); /* flush the file before closing */
	fclose(fd);
}	

MIDAS *midasopen(fname) /* open midas file, keyword name in environment file has filename */
	char *fname;
{
	FILE *fd;
	MIDAS *table;
	long flen;
	char *text;
	if(!(fd = fopen(fname,"r"))) { /* try to open the file */
		fatalerror("Data or Parameter File %s Does Not Exist\n",fname);
	}
	
	table = midasnew();
	strcpy(table->fname,fname); /* copy file name	into table header */
	strcpy(now.fname,fname); /* copy file name	into table header */
	now.col = 0;
	now.row = 0;
	midasread(fd,table); /* read data	into table */
	return table; /*	return table pointer */
}

MIDAS *midaswrite(table,outname,closeit) /* close midas table*/
	MIDAS *table;
	char *outname;
	short closeit;
{
	FILE *fd;
	int i,j;

	if(table) {  /* see if it's open */
		if(!(fd=fopen(outname,"w"))) {  /* open file for writing */
			fatalerror("Can't open output MIDAS table %s\n",table->fname);
		}
		
		for(i=0;i<table->numcols;i++) {  /*print out column headers */
			xprintf(fd,"%s\t",table->cols[i].name);
		}
		xprintf(fd,"\n");
		
		for(i=0;i<table->numcols;i++) {  /*print out column type */
			xprintf(fd,"%s\t",coltypestr[table->cols[i].type]);
		}
		xprintf(fd,"\n");
		
		for(i=0;i<table->numrows;i++) {  /* do for each row */
			for(j=0;j<table->numcols;j++) { /* print out each entry in the row */
				if (table->cols[j].type == midasDouble) {
					printdouble(fd,table->rows[i][j].d,1);
				} else if (table->cols[j].type == midasString) {
					printchars(fd,table,i,j);
				}
			}
			xprintf(fd,"\n");
		}
		
		fflush(fd); /* flush the file before closing */
		fclose(fd); /* close file */
		if (closeit) {
			if (strcmp(outname,table->fname) == 0) {
				midasfree(table);
			}
			return NULL;
		}
		return table;
	} else {
		fatalerror("Could not close data or paramter file that was never opened: %s\n",table->fname);
	}
}

rowcolrangecheck(table, i, j, fatal)
	MIDAS *table;
	long i,j;
	short fatal;
{
	if (i<0 || j<0) {	/* negative row or col always fatal */
		fatalerror("Midas internal error, row or col negative\n","");
	} else if (i>=table->numrows || j>=table->numcols) {
		if (fatal) {
			xprintf(stdout, "i,j %ld %ld   of %d %d\n",i,j,table->numrows,table->numcols);
			/*//Debugger();*/
			fatalerror("Midas internal error, row or col out of range\n","");
		}
		return 1;
	}
	return 0;
}

int
findcolumn(table, name)
	MIDAS *table;
	char *name;
{
	int i;
	long namesum;
	
	namesum = strsum(name);		/* try to save alot of strcmp()'s */
	for (i=0; i<table->numcols; ++i) {
		if (table->cols[i].namesum == namesum) {
			if (strcmp(name, table->cols[i].name)==0) return i;
		}
	}
	return -1;
}

int
findcolumnsum(table, name, namesum)
	MIDAS *table;
	char *name;
	long namesum;
{
	int i;
	
	for (i=0; i<table->numcols; ++i) {
		if (table->cols[i].namesum == namesum) {
			if (strcmp(name, table->cols[i].name)==0) return i;
		}
	}
	return -1;
}

putmidasvalcol(table,row,col,dval)
	MIDAS *table;
	int row,col;
	double dval;
{
	double *rowalloc();

	if(row >= table->numrows) {  /* if row does not exist */
		midasaddrow(table);
	}
	rowcolrangecheck(table, row,col, 1);
	table->rows[row][col].d = dval; /* insert value into table at [row][col] */
}

putmidasstr(table,name,row,cval) /* put a value	int o the table at column "name" and row */
	MIDAS *table;
	char *name;
	int row;
	char *cval;
{
	int col;

	if ((col = findcolumn(table, name)) == -1) {
		midasaddcol(table, name, midasDouble);
		col = table->numcols - 1;
	}
	putmidasstrcol(table, row, col, cval);
}

putmidasstrcol(table,row,col,cval)
	MIDAS *table;
	int row,col;
	char *cval;
{
	double *rowalloc();

	if(row >= table->numrows) {  /* if row does not exist */
		midasaddrow(table);
	}
	rowcolrangecheck(table, row,col, 1);
	table->rows[row][col].s = wordalloc(cval); /* insert value into table at [row][col] */
}

putmidasval(table,name,row,dval) /* put a value	int o the table at column "name" and row */
	MIDAS *table;
	char *name;
	int row;
	double dval;
{
	int col;

	if ((col = findcolumn(table, name)) == -1) {
		midasaddcol(table, name, midasDouble);
		col = table->numcols - 1;
	}
	putmidasvalcol(table, row, col, dval);
}

/* get a value from the table index by column "name and row */
double 
getmidasval(table,name,row) 
	MIDAS *table;
	char *name;
	int row;
{
	int col;
	
	if ((col = findcolumn(table, name)) == -1) {
		return (double)UND;
	}
	rowcolrangecheck(table, row, col, 1);
	return table->rows[row][col].d; /* return value found */
}


/* get a value from the table index by column "name and row */
char * 
getmidasstr(table,name,row) 
	MIDAS *table;
	char *name;
	int row;
{
	int col;
	
	if ((col = findcolumn(table, name)) == -1) {
		return NULL;
	}
	rowcolrangecheck(table, row, col, 1);
	return table->rows[row][col].s; /* return value found */
}



char *getword(line,word) /* get the next word from line and remove it from line */
char *line, *word;
{
	int i ,status,numtabs = 0;
	;

	for (i=0;i<(LSTRLEN-1);i++) /*	for the length of the word */
		word[i] = ' '; /* set the word to blank */

	if ((*line == '\n') || (*line == '\0'))	return NULL; /*	if end of line or null	return */
	while (status = isspace(*line)) 
	{	
		if (*line == '\t')
			numtabs++;
		++line; /* skip blanks or tabs */
	}

	if ((*line == '\n') || (*line == '\0'))	return NULL; /*	if end of line or null	return */

	if (numtabs >= 2)
	{
		warningerror("Data missing in file: %s,\n On line: %d col: %d\n",now.fname,(now.row)+1,(now.col)+1);
		*word++ = '0';
	}
	else
	{
		while (!(isspace(*line)) && (*line != '\0')) /*	while not blanks or tabs */
		{
			if ((*line == '\n') || (*line == '\0'))	return NULL;
			*word++ = *line++; /* fill word from line */
		}
	} 

	*word++='\0';
	return line; /*	return incremented line */
}


char *wordalloc(word1)
	char *word1;
{
	int num;
	if ((num=strcmp(word1,"#N/A")) == 0) /*	if it is undefined */
		return "UND";
	else return wordfill(word1);
}

getline(s, lim,fd)
char s[];
int lim;
FILE *fd;
{
	int c,i;
	int noblanks;

	i = 0;
	noblanks = 0;
	while (--lim >0 && (c=fgetc(fd)) != EOF && c != '\n')
	{
		s[i++] = c;
		if(!(isspace(c)))
			noblanks = 1;
	}
	if (c == '\n' || c == EOF) {
		s[i++] = '\n';
	}
	if (c == EOF) {
		i--;
		ungetc(c,fd);
	}
	s[i] = '\0';
	if(noblanks)
		return(i);
	else
		return 0;
}


copytable(table,outtable) /* copies table file to a new table file */
MIDAS *table;
char *outtable;
{
	midaswrite(table,outtable,1);
}

getnumrows(table) /*	return number of rows in table */
	MIDAS *table;
{
	return (table->numrows);
}

getnumcols(table) /*	return number of cols in table */
	MIDAS *table;
{
	return (table->numcols);
}

char *getcolname(col,table) /* get name of specific column in midastable */
	MIDAS *table;
	int col;
{
	if (col <= getnumcols(table))
		return table->cols[col].name;
	return NULL;
}

changecolname(table,oldname,newname) /* change name of column in table */
	MIDAS *table;
	char *oldname, *newname;
{
	int col;
	if ((col=findcolumn(table, oldname)) != -1) {
		strcpy(table->cols[col].name, newname);
		table->cols[col].namesum = strsum(newname);
		return 1;
	}
	return 0; /* return 0 for failure */
}

setundef(table,colname,row,type) /* set value to undefined */
	MIDAS *table;
	int row;
	char *colname,*type;
{
	int j,num;
	char *cval;
	double dval;
	
	if (row < 0)
		fatalerror("Setundef Error Request for negative rownumber--MIDAS file %s\n",table->fname);
	if (type[0] == 'd') {
		dval = (double)UND;
		putmidasval(table,colname,row,dval);
		return 1;
	} else if (type[0] == 'c') {
		putmidasstr(table,colname,row,"UND");
		return 1;
	}
	return 0; /* return 0 for failure */
}

