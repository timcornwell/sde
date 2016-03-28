/*
	National Radio Astronomy Observatory, Socorro, NM 87801
	Software Development Environment (SDE)
*/

/*
   Audit:

   Elim empty statements (";" statements) to elim warnings.
					M. Stupar	28 Dec 1994
*/

/*

   Convert strings to variables
   
*/

#if COMP_CRAY
#define stratof_ STRATOF
#define stratod_ STRATOD
#define stratoi_ STRATOI
#endif
#if COMP_IBM || COMP_HP
#define stratof_ stratof
#define stratod_ stratod
#define stratoi_ stratoi
#endif

void stratof_(name, n, x)
char *name;
int *n;
float *x;
{
        int i;
        char *int_name,  *malloc();

        int_name = (char *) malloc((*n)+1);
        for (i=0;i<*n;i++) int_name[i] = name[i];
        int_name[*n] = '\0';
        sscanf(int_name, "%f", x);
        free(int_name);
}

/* Bugfix on May 11 '93 -- DSB  Remove intermediate variable */ 
void stratod_(name, n, x)
char *name;
int *n;
double *x;
{
        int i;
        char *int_name, *malloc();

        int_name = (char *) malloc((*n)+1);
        for (i=0;i<*n;i++) int_name[i] = name[i];
        int_name[*n] = '\0';
        sscanf(int_name, "%lf", x);
        free(int_name);
}

void stratoi_(name, n, x)
char *name;
int *n;
int *x;
{
        int i;
        char *int_name,  *malloc();

        int_name = (char *) malloc((*n)+1);
        for (i=0;i<*n;i++) int_name[i] = name[i];
        int_name[*n] = '\0';
        sscanf (int_name, "%d", x);
        free(int_name);
}


