/*
	National Radio Astronomy Observatory, Socorro, NM 87801
	Software Development Environment (SDE)
	(C)opyright 1989 by Associated Universities, Inc.; all rights reserved
        @(#)fil.c	1.5 1/12/95
*/

/*

                     *****************************
                     *       FIL routines        *
                     *****************************

	These routines provide unformatted io facilities to
FORTRAN. They call c-routines which may or may not be standard.


					Tim Cornwell 7/15/88

Audit Trail:
        The file id passed to Fortran is a pointer into an array
        in which the real pointers are kept.
                                        T.J. Cornwell 1 June 1989

        Elim empty declarations (";" statements) to elim warnings.
					M. Stupar	28 Dec 1994
*/

/* The following is the maximum number of files which can be open at any one
time */

#define MAXFILES 2048

#include <stdio.h>

static FILE *fileids[MAXFILES];
static int currentfileno;


#if COMP_CRAY
#define filcinit_ FILCINIT
#define filcopen_ FILCOPEN
#define filcclos_ FILCCLOS
#define filcwrit_ FILCWRIT
#define filcread_ FILCREAD
#define filcsync_ FILCSYNC
#endif
#if COMP_IBM || COMP_HP
#define filcinit_ filcinit
#define filcopen_ filcopen
#define filcclos_ filcclos
#define filcwrit_ filcwrit
#define filcread_ filcread
#define filcsync_ filcsync
#endif

/* Initialize file system */

void filcinit_()
{
    int i;
    currentfileno=0;
    for (i=0;i<MAXFILES;i++) {
       fileids[i]=(FILE*)0;
    }
}

/* Open a file for io */

void filcopen_(filename, iflen, iomode, iilen, id)
char *filename;
char *iomode;
int *id;
int *iflen;
int *iilen;
{
   int flen = *iflen;
   int ilen = *iilen;
   extern char *malloc();
   char *int_filename;			/* internal filename */
   char *int_iomode;			/* internal iomode */
   FILE *int_file;
   int i = 0;

   int_filename = malloc(flen+1);
   i=0;
   while (++i<=flen)			/* copy filename */
     int_filename[i-1] = filename[i-1];
     int_filename[flen] = '\0';

   int_iomode = malloc(ilen+1);
   i=0;
   while (++i<=ilen)			/* copy iomode */
     int_iomode[i-1] = iomode[i-1];
     int_iomode[ilen] = '\0';

   int_file = fopen(int_filename, int_iomode);
   if(int_file) {
      while(fileids[currentfileno++]&&currentfileno < MAXFILES);
      if(currentfileno<MAXFILES) {
        fileids[currentfileno] = int_file;
        *id = currentfileno;
      }
      else {
        *id = 0;
      }
   }
   else {
     *id = 0;
   }
   free(int_filename);
   free(int_iomode);
}

/* Close a file for io */

void filcclos_(id)
int *id;
{
  FILE *int_file;
  int_file = fileids[*id];
  fclose(int_file);
  fileids[*id] = NULL;
}

/* Read from a file */

void filcread_(ptr, nbytes, id, nio)
char *ptr;
int *nbytes;
int *id;
int *nio;
{
   FILE *file;
   file = fileids[*id];
   if(file) {
      *nio = fread(ptr, sizeof(*ptr), *nbytes, file);
   }
   else {
      *nio = 0;
   };
}

/* Write to a file */

void filcwrit_(ptr, nbytes, id, nio)
char *ptr;
int *nbytes;
int *id;
int *nio;
{
   FILE *file;
   file = fileids[*id];
   if(file) {
      *nio = fwrite(ptr, sizeof(*ptr), *nbytes, file);
      if(*nio!=*nbytes) {
         perror("filcwrit");
      }
   }
   else {
      *nio = 0;
   }
}
