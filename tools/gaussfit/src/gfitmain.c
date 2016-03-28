/*
	GaussFit - A System for Least Squares and Robust Estimation

	Source Code Copyright (C) 1987 by William H. Jefferys,
	Michael J. Fitzpatrick and Barbara E. McArthur
	All Rights Reserved.
*/

/*
**	MAIN - main routine for Gauss.
**	    
**
**
**	Programming begun 09/11/1986  by Mike Fitzpatrick
**
**	MODIFICATION HISTORY:
**
*/

#include "defines.h"
#include <stdio.h>
#include <string.h>
#include <varargs.h>
#include <ctype.h>
#include "array.h"
#include "datum.h"
#include "simpledefs.h"
#include "protoplasm.h"
#include "alloc.h"


extern int cur_fd;         /* information used by compiler */
extern int line_number;
int recopyfilecmd = 0;

#ifndef OBJECTS


main(argc,argv) /* main routine for other computers */
	/* arguments are model and environment file names */
	int argc;
	char *argv[];
{
	char ofname[25], ifname[25];
	int errstat = 0;

	prolog();
	recopyfilecmd = 0;
	if (argc >= 2 && argv[1][0] == '-') {
		if (argv[1][1] == 'c') {
			recopyfilecmd = 1;
		}
		argc--;
		argv[1] = argv[2];
		argv[2] = argv[3];
	}
	if(argc == 2) { /* only model file was supplied */
		getfilename("\nEnter the environment filename: ",ofname); /* get environment file name */
		strcpy(ifname,argv[1]);
	} 
	else if(argc == 3) { /* all parameters were supplied */
		strcpy(ifname,argv[1]);
		strcpy(ofname,argv[2]);
	} 
	else { /* no parameters were supplied */
		getfilename("\nEnter the model filename: ",ifname); /* get model filename */
		getfilename("\nEnter the environment filename: ",ofname); /* get environment filename */
	}

	gaussmain(ifname,ofname); /* execute program */

}

getfilename(prompt,s) /* get a filename */
char *prompt;
char *s;
{
	char str[96];
	do /* repeat until a legal name is supplied */
	{
		xprintf(stdout, prompt); /* print the prompt */
		fgets(str, 81, stdin);
		sscanf(str, "%s",s); /* get the response */
		xprintf(stdout,  "%s\n",s); /* print the response */
	} 
	while (s[0] == 0); /* condition is that non-null string has been entered */
}

long 
newlineconvert(char *str, int *newlines) {
	char *str0;
	*newlines = 0;
	for (str0 = str; *str; str++) {
		if (*str == '\n' || *str == '\r') {
			*str = '\r';
			(*newlines) ++;
		}
	}
	return str - str0;
}

void
xprintf(FILE *file, char *format, ...)
{
	void *args;
	
	args = (char*)(&format) + sizeof(char*);
	vfprintf(file, format, args);
}

xfflush(FILE *file) {
	fflush(file);
}

#else

#include "CEditPane.h"
#include "CStdOutDoc.h"

long 
newlineconvert(char *str, int *newlines) {
	char *str0;
	*newlines = 0;
	for (str0 = str; *str; str++) {
		if (*str == '\n' || *str == '\r') {
			*str = '\r';
			(*newlines) ++;
		}
	}
	return str - str0;
}

int numlines = 0;
short spanh, spanv;
extern CStdoutDoc *gOutDoc;

void
xprintf(FILE *file, char *format, ...)
{
	void *args;
	long selend, len, selbeg0, selend0;
	CEditPane *outpane;
	char str[256];
	int newlines;
	
	args = (char*)(&format) + sizeof(char*);
	if (file == stderr || file == stdout) {
		outpane = (CEditPane *)(gOutDoc->itsMainPane);
		selend = outpane->GetLength();
		outpane->GetSelection(&selbeg0, &selend0);
		if (selend != selbeg0 || selend != selend0) {
			outpane->SetSelection(selend, selend, TRUE);
		}
		vsprintf(str, format, args);
		len = newlineconvert(str, &newlines);
		numlines += newlines;
		outpane->InsertTextPtr(str, len, FALSE);
		outpane->GetFrameSpan(&spanh, &spanv);
		if (numlines > spanv) {
			LongPt p;
			numlines = 0;
			p.v = outpane->FindLine(selend);
			p.h = 0;
			outpane->ScrollTo(&p,TRUE);
		}
	} else {
		vfprintf(file, format, args);
	}
}

xfflush(FILE *file) {
	if (file == stderr || file == stdout) {
	} else {
		fflush(file);
	}
}

#endif
