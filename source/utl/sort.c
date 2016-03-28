/*
C     
C     National Radio Astronomy Observatory, Socorro, NM 87801
C     Software Development Environment (SDE)
C++
C @(#)sort.c	1.3    11/16/92
C     

  A Fortran interface to the Unix Runtime library function QSORT(3)

  A number of different routines are provided for the different data types
  real, integer, double, and integer*2.

  Each routine is passed the array to be sorted and returns an integer*4
  array indicating how to sort the array. This can then be passed to another
  routine to actually sort the array and other associated arrays
  can also be sorted using the same index.

  Audit trail:
  		New Routine.
	                                        R.G. Marson 12 Feb 1990
	   	Add names for IBM, etc.
						T.J. Cornwell July 18, 1992
                Added direction switch
                                                D.S. Briggs Nov 16 1992
C
C----------------------------------------------------------------------
*/

#if COMP_CRAY
#define sortr_ SORTR
#define sortd_ SORTD
#define sorti_ SORTI
#define sortj_ SORTJ
#define sortr_ SORTR
#endif
#if COMP_IBM || COMP_HP
#define sortr_ sortr
#define sortd_ sortd
#define sorti_ sorti
#define sortj_ sortj
#define sortr_ sortr
#endif

/* REAL*4 arrays */
typedef struct
{
  float data;
  long index;
} r_record;

int r_compare_asc(i,j)
r_record *i,*j;
{
  if (i->data > j->data)
    return(1);
  else if (i->data == j->data)
    return(0);
  else
    return(-1);
}

int r_compare_dsc(i,j)
r_record *i,*j;
{
  if (i->data > j->data)
    return(-1);
  else if (i->data == j->data)
    return(0);
  else
    return(1);
}

void sortr_(rarray, size, dir, index)
long *size;
long *dir;
float *rarray;
long *index;
{
  r_record *array;
  long i;

  array = (r_record *) scralloc((int) (*size * sizeof(r_record)) );
  for (i=0; i<*size;i++)
    {
      array[i].data = rarray[i];
      array[i].index = i+1;
    }
  if (*dir>0)
    qsort(array, *size, sizeof(r_record), r_compare_asc);
  else
    qsort(array, *size, sizeof(r_record), r_compare_dsc);
  for (i=0; i<*size; i++)
    index[i] = array[i].index;
  scrfree(array, *size * sizeof(r_record));
}
/* REAL*8 (DOUBLE) arrays */
typedef struct
{
  double data;
  long index;
} d_record;

int d_compare_asc(i,j)
d_record *i,*j;
{
  if (i->data > j->data)
    return(1);
  else if (i->data == j->data)
    return(0);
  else
    return(-1);
}
int d_compare_dsc(i,j)
d_record *i,*j;
{
  if (i->data > j->data)
    return(-1);
  else if (i->data == j->data)
    return(0);
  else
    return(1);
}

void sortd_(darray, size, dir, index)
long *size;
long *dir;
double *darray;
long *index;
{
  d_record *array;
  long i;

  array = (d_record *) scralloc((int) (*size * sizeof(d_record)) );
  for (i=0; i<*size;i++)
    {
      array[i].data = darray[i];
      array[i].index = i+1;
    }
  if (*dir>0)
    qsort(array, *size, sizeof(d_record), d_compare_asc);
  else
    qsort(array, *size, sizeof(d_record), d_compare_dsc);
  for (i=0; i<*size; i++)
    index[i] = array[i].index;
  scrfree(array, *size * sizeof(d_record));
}
/* INTEGER*4 arrays */
typedef struct
{
  long data;
  long index;
} i_record;

int i_compare_asc(i,j)
i_record *i,*j;
{
  return ((int) (i->data - j->data));
}
int i_compare_dsc(i,j)
i_record *i,*j;
{
  return ((int) (j->data - i->data));
}

void sorti_(iarray, size, dir, index)
long *size;
long *dir;
long *iarray;
long *index;
{
  i_record *array;
  long i;

  array = (i_record *) scralloc((int) (*size * sizeof(i_record)) );
  for (i=0; i<*size;i++)
    {
      array[i].data = iarray[i];
      array[i].index = i+1;
    }
  if (*dir>0)
    qsort(array, *size, sizeof(i_record), i_compare_asc);
  else
    qsort(array, *size, sizeof(i_record), i_compare_dsc);
  for (i=0; i<*size; i++)
    index[i] = array[i].index;
  scrfree(array, *size * sizeof(i_record));
}
/* INTEGER*2 arrays */
typedef struct
{
  short data;
  long index;
} j_record;

int j_compare_asc(i,j)
j_record *i,*j;
{
  return ((int) (i->data - j->data));
}
int j_compare_dsc(i,j)
j_record *i,*j;
{
  return ((int) (j->data - i->data));
}

void sortj_(jarray, size, dir, index)
long *size;
long *dir;
short *jarray;
long *index;
{
  j_record *array;
  long i;

  array = (j_record *) scralloc((int) (*size * sizeof(j_record)) );
  for (i=0; i<*size;i++)
    {
      array[i].data = jarray[i];
      array[i].index = i+1;
    }
  if (*dir>0)
    qsort(array, *size, sizeof(j_record), j_compare_asc);
  else
    qsort(array, *size, sizeof(j_record), j_compare_dsc);
  for (i=0; i<*size; i++)
    index[i] = array[i].index;
  scrfree(array, *size * sizeof(j_record));
}

