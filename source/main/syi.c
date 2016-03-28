/*
	National Radio Astronomy Observatory, Socorro, NM 87801
	Software Development Environment (SDE)
*/

/*

   Various system level commands which are called by sys.f routines

*/
/*
   Audit:

   Elim empty statements (";" statements) to elim warnings.
					M. Stupar	28 Dec 1994
*/

#define NAMELEN 64

#include <stdio.h>

#ifndef COMP_SUN40
#include <time.h>
#endif
#ifdef COMP_HP
#include <sys/times.h>
#include <unistd.h>
#endif
#include <sys/time.h>
#include <sys/resource.h>

extern char *malloc();

#if COMP_CRAY
#define systemc_ SYSTEMC
#define syshostc_ SYSHOSTC
#define sysgtenc_ SYSGTENC
#define sysexit_ SYSEXIT
#define sysutimec_ SYSUTIMEC
#define syscwdc_ SYSCWDC
#define sysdatetc_ SYSDATETC
#define sysdatecc_ SYSDATECC
#define sysecputc_ SYSECPUTC
#endif

#if COMP_IBM || COMP_HP
#define _BSD
#define BSD_
#define systemc_ systemc
#define syshostc_ syshostc
#define sysexit_ sysexit
#define sysgtenc_ sysgtenc
#define syscwdc_ syscwdc
#define sysutimec_ sysutimec
#define sysdatetc_ sysdatetc
#define sysdatecc_ sysdatecc
#define sysecputc_ sysecputc
#endif

/* We need this so the IBM won't call the fortran version */

int sslen(str)   /* Constrained to be less than NAMELEN */
char *str;
{
        int i;
        for (i=0;(str[i]!='\0')&&(i<NAMELEN);i++);
        return(i);
}
int sslen0(str)   /* Unconstrained */
char *str;
{
        int i;
        for (i=0;(str[i]!='\0');i++);
        return(i);
}

void sysexit_(estatus)
int *estatus;
{
        exit(&estatus);
}

int systemc_(command, nchar)
char *command;
int *nchar;
{
        int i, istat;
        char *int_command;

        int_command = (char *) malloc(*nchar+1);
        for (i=0;i<*nchar;i++) int_command[i] = command[i];
        int_command[*nchar] = '\0';
	istat = system(int_command);
	free(int_command);
	return(istat);
}

void sysgtenc_(name1, n1, name2, n2)
char *name1, *name2;
int *n1, *n2;
{
        int i, l2;
        char *int_name1, *int_name2,  *getenv();

        int_name1 = (char *) malloc((*n1)+1);
        for (i=0;i<*n1;i++) int_name1[i] = name1[i];
        int_name1[*n1] = '\0';
	int_name2 = getenv(int_name1);
	free(int_name1);
        if(int_name2) {
	   l2 = sslen0(int_name2);
	   if (l2 > *n2) {
	       *n2 = -1;
	   }
	   else {
	       *n2 = l2;
	       for (i=0;i<*n2;i++) name2[i] = int_name2[i];
	   }
        }
        else {
           *n2 = 0;
        };
}

void syshostc_(name, n)
char *name;
int *n;
{
        static char int_name[NAMELEN] = "Unknown machine\0";
        int len = NAMELEN;
        gethostname(int_name, len);
        strcpy(name, int_name);
        *n=sslen(name);
}

void sysdatetc_(time, n)
char *time;
int *n;
{
        struct timeval tp;
        struct timezone tzp;
        gettimeofday(&tp, &tzp);
#if COMP_SUN40
        strcpy(time,ctime(&tp.tv_sec));
#else
        strcpy(time,ctime((time_t*)&tp.tv_sec));
#endif
        *n=sslen(time);
}

void sysdatecc_(y, m, d)
int *y, *m, *d;
{
        struct timeval tp;
        struct timezone tzp;
        struct tm *tm;
        gettimeofday(&tp, &tzp);
#if COMP_SUN40
        tm=localtime(&tp.tv_sec);
#else
        tm=localtime((time_t*)&tp.tv_sec);
#endif
        *y=tm->tm_year;
        *m=tm->tm_mon;
        *d=tm->tm_mday;
}

void sysutimec_(time, n)
char *time;
int *n;
{
        float lu, ls;
#if COMP_HP
        struct tms rusage;
        times( &rusage);
        lu = (float)(rusage.tms_utime)/( float)(sysconf(_SC_CLK_TCK));
        ls = (float)(rusage.tms_stime)/( float)(sysconf(_SC_CLK_TCK));
#else        
        struct rusage rusage;
        getrusage(RUSAGE_SELF, &rusage);
        lu = (float)(rusage.ru_utime.tv_sec) + 0.000001 * 
             (float)(rusage.ru_utime.tv_usec);
        ls = (float)(rusage.ru_stime.tv_sec) + 0.000001 * 
             (float)(rusage.ru_stime.tv_usec);
#endif
        sprintf(time, "User: %.3f System: %.3f", lu, ls);
        *n=sslen(time);
}

void sysecputc_(x)
float *x;
{
        float lu, ls;
#if COMP_HP
        struct tms rusage;
        times( &rusage);
        lu = (float)(rusage.tms_utime)/( float)(sysconf(_SC_CLK_TCK));
        ls = (float)(rusage.tms_stime)/( float)(sysconf(_SC_CLK_TCK));
#else        
        struct rusage rusage;
        getrusage(RUSAGE_SELF, &rusage);
        lu = (float)(rusage.ru_utime.tv_sec) + 0.000001 * 
             (float)(rusage.ru_utime.tv_usec);
        ls = (float)(rusage.ru_stime.tv_sec) + 0.000001 * 
             (float)(rusage.ru_stime.tv_usec);
#endif
        *x=lu+ls;
}

void syscwdc_(name, n)
char *name;
int *n;
{
        char int_name[NAMELEN];
#if COMP_HP
        getcwd(int_name,NAMELEN);
#else
        getwd(int_name);
#endif
        strcpy(name, int_name);
        *n=sslen(name);
}

#if COMP_HP
void getarg(i, arg)
int *i;
char *arg;
{
   arg[0] = '\0';
} 
#endif
