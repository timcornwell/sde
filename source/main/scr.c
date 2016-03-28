/*
	National Radio Astronomy Observatory, Socorro, NM 87801
	Software Development Environment (SDE)
*/

/*
		**********************************
		* Scratch space allocation (SCR) *
		**********************************

These routines allocate and deallocate scratch space. These routines are
machine-dependent. 

   CONVEX: If the required space is large then the memory is mapped to a 
special disk file. Otherwise a simple malloc is used. The scratch files
have logical names SDESCRn n=1,32, and the default is SDESCR. These
must point t directory o names like /scr/SDE.
   SPARC: Same as CONVEX
   IBM6000: Allocate a large amount of memory using malloc and then parcel
it out as required. This is only needed until malloc is fixed.
   OTHERWISE: A simple malloc is used.

Audit trail:
	Changed size limit to 8Mbytes, also inserted check to ensure that
	no more than 32 files are allocated
					T.J. Cornwell	August 23 1989
	Added Sun support
					T.J. Cornwell	September 19 1989
	Changed Sun limit to 4 Mbytes
					T.J. Cornwell	December 20 1989
	Changed to host-dependent limit
					T.J. Cornwell   August 10 1990
        Added IBM6000 support
					T.J. Cornwell   March 27 1991
        Now defaults to T1 (i.e. ./ usually) so the user sees the SDESCR
        files
					T.J. Cornwell   April 6 1992
        Added ALPHA support to be same as SPARC/CONVEX
					J.D. Ellithrope October 19 1994
*/


#if COMP_CONVEX || COMP_SUN || COMP_ALPHA
#include <strings.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <sys/file.h>
#include <stdio.h>
#define NSCR 32
int size_limit = 0x0800000;	/* Size limit of 8 Mbytes */

int nscr = 0;
struct scr {
  char *file;
  char *addr;
} scrtab[NSCR];

#else
#endif
#include <stdio.h>

char *baseptr, *currptr;

#include <stdio.h>

/* Initialize scr system */ 

void scrinit()
{
#if COMP_CONVEX || COMP_SUN || COMP_ALPHA
  unsigned int i;
  char envname[10];
  char *scrname, *scrlim;
  char *malloc();
  char *getenv();
  nscr = 0;
  if((scrlim = getenv("SDEMEM"))!=0) {
     sscanf(scrlim,"%d",&size_limit);
     size_limit *= 0x100000;
   }
  for(i=0;i<NSCR;i++) {
    sprintf(envname, "SDESCR%-u", i);
    if((scrname = getenv(envname))==0) {
       if((scrname = getenv("T1"))==0) {
          scrname = getenv("SDESCR");
       }
    }
    scrtab[i].file = malloc(64);
    sprintf(scrtab[i].file,"%s/SDESCRXXXXXX",scrname);
  };
#else
#if SDESTATIC
  char envname[10];
  char *malloc();
  char *scrlim;
  char *getenv();
  if((scrlim = getenv("SDEMEM"))!=0) {
     sscanf(scrlim,"%d",&size_limit);
     size_limit *= 0x100000;
   } 
   size_limit = 24;
   size_limit *= 0x100000;
   baseptr=(char*)malloc((size_t)size_limit);
   currptr=baseptr;
#endif
#endif
}

/* Clean-up scr system */

void screxit()

{
#if COMP_CONVEX || COMP_SUN || COMP_ALPHA
  int i;
  for (i=0;i<nscr;i++)
    if (scrtab[i].addr) unlink(scrtab[i].file);
#else
#if SDESTATIC
    free(baseptr);
#endif
#endif
}

/* Allocate space */

char *scralloc(size)
int size;
{
  extern char *malloc();
#if COMP_CONVEX || COMP_SUN || COMP_ALPHA
  extern int ftruncate();
  if (nscr < NSCR && size >= size_limit) {
     int fd;
     mktemp(scrtab[nscr].file);
     fd = open (scrtab[nscr].file, O_RDWR | O_CREAT, 0777);
     if(fd==-1) {
        perror("open failed in scralloc");
        return((char *)0x0);
     }
     else {
        caddr_t iaddr = (caddr_t) 0x0;
        off_t offset = (off_t) 0x0;
        if(ftruncate(fd, size)) {
           perror("ftruncate failed in scralloc");
           return((char *)0x0);
        }
        else {
#if COMP_CONVEX
           unsigned int len = (unsigned int)size;
           unsigned int prot = PROT_READ | PROT_WRITE;
           unsigned int share = MAP_FILE | MAP_EXTEND;
           scrtab[nscr].addr = (char *)mmap(iaddr, &len, prot, share, fd, 
              offset);
#else
           int prot = PROT_READ | PROT_WRITE;
           int share = MAP_SHARED;
           scrtab[nscr].addr = (char *)mmap(iaddr, size, prot, share, fd, 
              offset);
#endif
           if((int)scrtab[nscr].addr==-1) {
              perror("mmap failed in scralloc");
              return((char *)0x0);
   	   }
           else {
              return(scrtab[nscr++].addr);
	    }
         }
     }
  }
  else {
     return(malloc(size));
  };
#else
#if SDESTATIC
  char *retptr;
  retptr=currptr;
  currptr+=size;
  return(retptr);
#else
  return(malloc(size));
#endif
#endif
}

/* Deallocate scratch space */
void scrfree(addr, size)
char *addr;
int size;
{
#if COMP_CONVEX || COMP_SUN || COMP_ALPHA
  int i;
  if (size>=size_limit) {
#if COMP_CONVEX
     munmap((caddr_t)addr); 
#else
     munmap((caddr_t)addr,size);
#endif
    for (i=0;i<nscr;i++) {
        if(addr==scrtab[i].addr) {
           unlink(scrtab[i].file);
           scrtab[i].addr = (char *) 0x0;
        };
     }
  }
  else {
     free(addr);
  }
#else
  free(addr);
#endif
}

