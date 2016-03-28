
/*
	GaussFit - A System for Least Squares and Robust Estimation

	Source Code Copyright (C) 1987 by William H. Jefferys,
	Michael J. Fitzpatrick and Barbara E. McArthur
	All Rights Reserved.
*/

/* imitation FITS file routines */

#include "defines.h"
#include "array.h"
#include "datum.h"
#include "simpledefs.h"
#include "protoplasm.h"
#include "alloc.h"
#include <stdio.h>
#include "strings.h"


strrpos(s,t)  /* strrpos not defined for non-MAC machines finds rightmost 
					instance of character t in string s. 
					returns -1 if t not in s */
	char *s, t;
{
	int i,last;

	last = -1; /* failure result */
	for(i=0;s[i]!='\0';i++) /* search string */
		if(s[i]==t) /* update if character found */
			last = i;
	return last;
}

FITS *fitsopen(fname) /* open a fits file named fname */	
	char *fname;
{
	FILE *fd;
	FITS *table;

	if(!(fd = fopen(fname,"r"))) /* open the file to see if it exists */
		fatalerror("Environment file %s does not exist\n",fname);
	if(!(table = (FITS *)MemAlloc("fitsopen", (long)sizeof(FITS)))) /* if it exists, allocate memory for hte file header */
		fatalerror("Memory Manager Error--FITS file header\n","");
	if(!(table->value = (ENTRE *)MemAlloc("fitsrows", FITSROWS*(long)sizeof(ENTRE)))) /* allocate space for file entries */
		fatalerror("Memory Manager Error--FITS file entry space\n","");
	strncpy(table->filename,fname,63); /* copy filename into table */
	fitsread(fd,table); /* read file into table */
	fflush(fd); /* flush the file before closing */
	fclose(fd); /* close file */
	return table; /* return pointer to information read */
}

FITS *fitsclose(table,dispose) /* close fits file "table" */
FITS *table;
int dispose;
{
	FILE *fd;

	if(!(fd = fopen(table->filename,"w"))) /* open file for writing */
		fatalerror("Can't Write out environment file %s\n",table->filename);
	fitswrite(fd,table); /* write out information */
	if (dispose) {
		freemem("fitstabval", (char*)table->value); /* deallocate table space */
		freemem("fitstable", (char*)table);
		table = NULL;
	}
	fflush(fd); /* flush the file before closing */
	fclose(fd);
	return table;
}


fitsread(fd,table) /* read FITS file into allocated table space */
FILE *fd;
FITS *table;
{
	char line[256];
	char key[KEYLEN];
	char str[FSTRLEN];
	int type;

	table->nrows = 0; /* preset rows read to zero */
	while(fgets(line,255,fd) && (table->nrows<FITSROWS)) /* get a new line. Error if table space exhausted */	
	{
		/*sscanf(line,"%8s = %22[^&]",key,str);*/ /* get keyword and untranslated entry */
		sscanf(line,"%8s = %22s",key,str); /* get keyword and untranslated entry */
		if(strcmp(key,"END")==0) /* finish if keyword = END */
			break;
		strncpy(table->value[table->nrows].name,key,KEYLEN); /* copy keyword into entry */

		convertstr(str,(OBJECT *)table->value[table->nrows].value.sval,
			&type); /* cconvert value into proper form, enter into table */
		table->value[table->nrows].type = type; /* enter type into table */
		table->nrows++; /* increment number of rows */
	}
}

fitswrite(fd,table) /* write out fits table */
FILE *fd;
FITS *table;
{
	int i;

	for(i=0;i<table->nrows;i++) /* do for each row in table */
	{
		xprintf(fd,"%-8s=\t",table->value[i].name); /* print keyword */
		switch(table->value[i].type) /* print value depending on its type */
		{
			case(STRING): /* string value */
			xprintf(fd,"'%s'\n",table->value[i].value.sval);
			break;
			case(REAL): /* real value */
			printdouble(fd,table->value[i].value.dval,1);
			xprintf(fd,"\n");
			break;
		}
	}
	xprintf(fd,"END\n"); /* print END keyword */
}

char *getfitsstr(table,s) /* get string from table corresponding to keyword s */
FITS *table;
char *s;
{
	if(table)  /* see if table is open */
	{
		int i;

		for(i=0;i<table->nrows;i++) /* scan table rows */
		{
			if(strcmp(table->value[i].name,s)==0 
			    && table->value[i].type==(int)STRING) /* find keyword, check that it's a string */	
			{
				return table->value[i].value.sval; /* if found, return pointer to string */
			}
		}
		return NULL; /* if not found, return NULL pointer */
	}
	fatalerror("No environment file is defined\n","");
}

putfitsstr(table,s,t) /* put  a string t i fits table at keyword X */
FITS *table;
char *s, *t;
{
	if(table)  /* see if table is open */
	{
		int i;

		for(i=0;i<table->nrows;i++) /* scan table rows */
			if(strcmp(table->value[i].name,s)==0) /* find first instance of keyword */
			{
				table->value[i].type=(int)STRING;/* change it to type STRING */
				strncpy(table->value[i].value.sval,t,FSTRLEN-2);/* copy string into table */
				return;
			}
		if(table->nrows<FITSROWS)/* keyword not found. enter new keyword */
		{
			table->value[i].type=(int)STRING; /*insert entry */
			strncpy(table->value[i].value.sval,t,FSTRLEN-2);
			strncpy(table->value[table->nrows].name,s,8);
			table->nrows++; /* increment rows */
			return;
		}
		else
			fatalerror("Out of environment table space\n","");
	}
	fatalerror("Environment file is undefined.  Cannot put a string.\n","");
}

double getfitsval(table,s) /* get a double value from table, keyword s */
FITS *table;
char *s;
{
	int i;
	if(table)  /* see if table is open */
	{
		for(i=0;i<table->nrows;i++) /* scan table rows */
			if(strcmp(table->value[i].name,s)==0) /* if found, check that it's real */
				if(table->value[i].type == (int)REAL)
					return table->value[i].value.dval; /* return value found */
		return 0.0; /* otherwise return 0.0 */
	}
	fatalerror("Environemnt file is Undefined. Cannot get double value.\n","");
}

putfitsval(table,s,val) /* put double val into table at keyword s */
FITS *table;
char *s;
double val;
{
	int i;
	if(table)  /* see if table is open */
	{
		for(i=0;i<table->nrows;i++) /* scan table rows */
			if(strcmp(table->value[i].name,s)==0) /* if keyword found */
			{
				table->value[i].type = (int)REAL; /* insert real at this point */
				table->value[i].value.dval = val; /* insert value */
				return;
			}
		if(table->nrows<FITSROWS) /* keyword not found, enter new keyword */
		{
			table->value[table->nrows].type = (int)REAL; /* insert data at last position */
			table->value[table->nrows].value.dval = val;
			strncpy(table->value[table->nrows].name,s,8);
			table->nrows++; /* increment rows */
			return;
		}
		else
			fatalerror("Out of environment file space\n","");
	}
	fatalerror("Environment file is undefined. Cannot put a double value.\n","");
}


convertstr(s,x,type) /* given string, determine its type and convert it */
char *s;
OBJECT *x;
int *type;
{
	if(s[0]=='\'') /* if first char is ', it's a string */
	{
		int i;

		strncpy(x->sval,&s[1],FSTRLEN-1); /* copy string out */
		if((i=strrpos(x->sval,'\'')) >= 0) /* turn final ' to a '\0' */
			x->sval[i] = '\0';
		else
			fatalerror("Illegally terminated environment string\n","");
		*type = (int)STRING; /* return type STRING */
	}
	else /* it's real */
	{
		sscanf(s,"%lf",&x->dval); /* convert to real */
		*type = (int)REAL; /* return type REAL */
	}
}


