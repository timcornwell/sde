/*
	National Radio Astronomy Observatory, Socorro, NM 87801
	Software Development Environment (SDE)
	@(#)dai.c	1.8 5/26/94
*/

/*
		****************************
		* Database Internals (DAI) *
		****************************

The database is structured as a directory tree, each sub-directory being
implemented as a binary tree. Several different type of node are allowed:

Directory Nodes	- These contain a one element array of pointers that point
		to another sub-driectory binary trees.
Header Nodes	- These contain simple header data the values of which are
		always copied to and from application variables.
Array Nodes	- These contain application accessible arrays which
		application code accesses using pointers.
Null Nodes	- These are skeleton nodes that have been deleted but
		not removed from the binary tree.

Both name strings and arrays can be of arbitrary length. Consequently a
node contains pointers to these items. Strings are different from normal C
strings as they must interface to FORTRAN character variables. All strings
are stored and passed as a char array plus an int length. No terminating
zero char is used. 

The nodes themselves, the name strings and the arrays are all dynamically
allocated from memory as either fixed or relocatable. Fixed items can
never be moved, while relocatable ones can be moved to reduce memory
fragmentation. Only array nodes are relocatable in this implementation,
although this is not yet fully implemented.


A few routines in this file are system dependent. dai_ALLOC and
dai_DEALLOC allocate both fixed and relocatable memory. 

Audit trail:
	Changed the ever-troublesome daigetar_ routine in the 'S'
	handling part.
				T.J. Cornwell	Sept 13 1989
	Fixed the bug whereby a deleted node could not be recreated with
	the same name: changed daialloc to recognize null nodes.
				T.J. Cornwell	Sept 24 1989
	Fixed (?) alignment problem on Suns by always allocating an array
	which has an extra two elements. The address is then incremented
	by 1 in daigetar_.
				T.J. Cornwell	Oct 4 1989
	dailist_ now checks for existence of directory. Also fixed long
	standing bug whereby if dai_search_cache failed then the next
	search would miss any trailing characters e.g. Comps==Comps1
				T.J. Cornwell	Feb 22 1990

*/

/* For machines with different ways to talk to fortran */
#if COMP_CRAY
#define daiinit_ DAIINIT
#define daiexit_ DAIEXIT
#define dailistf_ DAILISTF
#define daicreat_ DAICREAT
#define daiexist_ DAIEXIST
#define daiput_ DAIPUT
#define daiget_ DAIGET
#define daialloc_ DAIALLOC
#define daigetar_ DAIGETAR
#define daidelet_ DAIDELET
#define dairenam_ DAIRENAM
#define dailist_ DAILIST
#define daiddump_ DAIDDUMP
#define daiwrite_ DAIWRITE
#define dairead_ DAIREAD
#define daipwrit_ DAIPWRIT
#define daipread_ DAIPREAD
#endif
#if COMP_IBM || COMP_HP
#define daiinit_ daiinit
#define daiexit_ daiexit
#define dailistf_ dailistf
#define daicreat_ daicreat
#define daiexist_ daiexist
#define daiput_ daiput
#define daiget_ daiget
#define daialloc_ daialloc
#define daigetar_ daigetar
#define daidelet_ daidelet
#define dairenam_ dairenam
#define dailist_ dailist
#define daiddump_ daiddump
#define daiwrite_ daiwrite
#define dairead_ dairead
#define daipwrit_ daipwrit
#define daipread_ daipread
#endif

/* Include files for i/o */
#include <stdio.h>
/* Include files for PVM */
#if PVM
#include "pvm3.h"
#endif

/*
All nodes have the same structure as follows: 
*/

struct dat_node {
  char *name;			/* pointer to node name string */
  int name_len;			/* length of name string */
  char type;			/* code	0 = null node
					1 = directory node
					i,r,d,x,c = header node
					I,R,D,X,C = array node */
  struct dat_node *lesser;	/* pointer to lexically lesser nodes */
  struct dat_node *greater;	/* pointer to lexically greater nodes */
  int size;			/* number of items */
  char *values;			/* pointer to values array */
};

/* Define complex data type */
typedef struct {
   float re;
   float im;
} complex;

#define STRINGLEN 64
typedef struct {
   char chars[STRINGLEN];
} string;

/* Various char codes */
#define	DELIMITER	'/'		/* Directory name delimiter */
#define BLANK		' '		/* ASCII blank */
#define TYPE_NULL	'0'		/* Node type codes */
#define TYPE_DIR	'1'
#define TYPE_HEADER_MIN	'a'
#define TYPE_HEADER_MAX	'z'
#define TYPE_ARRAY_MIN	'A'
#define TYPE_ARRAY_MAX	'Z'
#define TYPE_NAME	'C'		/* Code for name string allocation */
static char SOD = 'S';		/* Codes for disk format */
static char EOD	= 'E';		/* Codes for disk format */
static char SOH	= '<';		/* Codes for disk format */
static char EOH	= '>';		/* Codes for disk format */

/* Root node of whole database */
char master_name[] = "MASTER_ROOT";
static struct dat_node master_root = {master_name,11,TYPE_NULL};

/* Cache of latest name translation */
#define MAX_CACHE	256		/* Length of cache string */
#define	MAX_DEPTH	16		/* Maximum depth of database */
static char cache_start[MAX_CACHE];	/* Cached name */
static char *cache_end = cache_start-1;	/* Last valid character in cache */
static int cache_depth = 0;		/* Depth to which cache valid */
static struct dat_node *cache_id[MAX_DEPTH] = {&master_root};	/* Cached
					translation */

/* The supplied name is saved together with pointers to the start and
end characters of the current directory level */
static char *name_start;		/* Start of saved name */
static char *name_end;			/* End of saved name */
static char *part_start;		/* Start of saved level */
static char *part_end;			/* End of saved level */
static int depth;			/* Current directory level */
static int empty_dir_flag;		/* Empty directory flag */


/* Memory allocation routines */
extern char *scralloc();
extern void scrfree();

/* Directory list routine definitions */
extern void dailistf_();		/* fortran callback routine */
#define	CODE_INIT	1
#define	CODE_END	2
#define	CODE_DIR	3
#define CODE_DATA	4
#define CODE_NULL	5


/*
The internal database routines pass an error status around. This allows
the database to be used by the error system itself without problems of
recursion. The fortran interface to these routines makes standard error
system calls if any errors occur in the database internal routines.

The following error values are used:
*/

#define	DAT_OK		0	/* no error */
#define	DAT_FATAL	1	/* serious internal error */
#define DAT_CORRUPT	2	/* database corrupt i.e. name empty */
#define	DAT_BADID	3	/* bad identifier */
#define	DAT_BADNAME	4	/* bad node or link name */
#define	DAT_BADCODE	5	/* bad node type code */
#define	DAT_BADSIZE	6	/* bad array size */
#define DAT_BADCONT	7	/* bad context */
#define	DAT_NOMEM	8	/* insufficient memory */
#define	DAT_NONODE	9	/* node not found */
#define	DAT_NOLINK	10	/* named linkage not found */
#define	DAT_NODEL	11	/* linkage prevents complete deletion */
#define	DAT_NOARR	12	/* no application array found */
#define DAT_NOCONT	13	/* no free context slots */
#define	DAT_MISMATCH	14	/* node type mismatch */
#define	DAT_TRUNCATE	15	/* returned array truncated */
#define	DAT_DUPARR	16	/* duplicate application array */
#define	DAT_CACHEFULL	17	/* the cache of translated names is full */
#define	DAT_NOTFOUND	18	/* Was not found */
#define	DAT_EXISTS	19	/* Exists already */
#define DAT_NULLNODE	20	/* Node exists but is null */
#define DAT_INPUT	21	/* Input error */
#define DAT_OUTPUT	22	/* Output error */


/*
		***********************
		* DAI Static Routines *
		***********************

These routines are totally internal to the database system and are only
called from other database routines. They are always called by C routines
so call by value arguments are available.
*/

/* Routine to return memory size in chars given a type code and number of
elements */

static int dai_mem_size(type,size,err)
char type;				/* input type code */
int size;				/* input number of items */
int *err;				/* in/out error status */
{
  if(*err) return(0);
  switch(type) {
  case TYPE_NULL:			/* special for node allocation */
    return(sizeof(struct dat_node));
  case TYPE_DIR:			/* directory */
    return(sizeof(struct dat_node *));
  case 'C':				/* char */
  case 'c':
    return(size*sizeof(char));
  case 'S':				/* string */
  case 's':
    return(size*sizeof(string));
  case 'I':				/* integer */
  case 'i':
  case 'L':
  case 'l':
    return(size*sizeof(int));
  case 'R':				/* real */
  case 'r':
    return(size*sizeof(float));
  case 'D':				/* double */
  case 'd':
    return(size*sizeof(double));
  case 'X':				/* complex */
  case 'x':
    return(size*sizeof(complex));
  default:				/* error */
    *err = DAT_FATAL;
    return(0);
  }
}


/* Routine to allocate memory and return a char pointer to it */
/* This is a temporary version that does not distinguish fixed and
relocatable allocation */

static char *dai_alloc(type,size,err)
char type;				/* input type code */
int size;				/* input number of items */
int *err;				/* in/out error status */
{
  char *addr;				/* memory address */

  size = dai_mem_size(type,size,err);
  if(*err || size<=0) return(0);
  if(type>=TYPE_ARRAY_MIN && type<=TYPE_ARRAY_MAX)
    addr = scralloc(size);		/* relocatable */
  else
    addr = scralloc(size);		/* fixed */
  if(addr)
    return(addr);
  *err = DAT_NOMEM;
  return(0);
}


/* Routine to deallocate memory */

static void dai_dealloc(addr,type,size,err)
char *addr;				/* input memory address */
char type;				/* input type code */
int size;				/* input number of items */
int *err;				/* in/out error status */
{
  int isize = dai_mem_size(type,size,err);
  if(*err==0 && isize>0)
    if(type>=TYPE_ARRAY_MIN && type<=TYPE_ARRAY_MAX)
      scrfree(addr,isize);			/* relocatable */
    else
      scrfree(addr,isize);			/* fixed */
}


/* Routine to compare two names. It returns -1 if name1<name2,
0 if they are the same and +1 if name1>name2 */

static int dai_name_cmp(name1,len1,name2,len2)
char *name1;				/* input name string */
int len1;				/* input string length */
char *name2;				/* input name string */
int len2;				/* input string length */
{
  int i = -1;				/* char count */
  int len;				/* comparison length */

/* test each character */
  len = (len1<len2) ? len1 : len2;
  while(++i<len) {
    if(name1[i]<name2[i])
      return(-1);
    else if(name1[i]>name2[i])
      return(1);
  }

/* any remaining characters */
  if(len1<len2)
    return(-1);
  else if(len1>len2)
    return(1);
  return(0);
}


/* Routine to find a node by name and return a pointer to it.
The search starts at the supplied node. If the search fails an error
is set and the pointer is to the node immediately above where the
node should be */

static struct dat_node *dai_find_node(id,name,len,err)
struct dat_node *id;			/* input pointer to starting node */
char *name;				/* input node name */
int len;				/* input name length */
int *err;				/* in/out error status */
{
  int equal;				/* node found flag */

  if(*err)
    return(id);

  if(id->name == 0) {			/* Check for corruption */
     *err = DAT_CORRUPT;
     return(id);
  }

  equal = dai_name_cmp(name,len,id->name,id->name_len);
  if (equal==0) {
     return(id);
  }

/*  if(id->type==TYPE_DIR) {	
     array = (struct dat_node **) id->values;
     id = array[0];
     if(array[0])
       id = array[0];
     else {
       *err = DAT_NOTFOUND;
       empty_dir_flag = 1;
       return(id);
     }
  } */

  if(id->name == 0) {			/* Check for corruption */
     *err = DAT_CORRUPT;
     return(id);
  }

  equal = dai_name_cmp(name,len,id->name,id->name_len);

  if(equal<0) {				/* try lesser node */
    if(id->lesser && id->lesser->name)
      return(dai_find_node(id->lesser,name,len,err));
    else				/* not found */
      *err = DAT_NOTFOUND;
  }

  else if(equal>0) {			/* try greater node */
    if(id->greater && id->greater->name)
      return(dai_find_node(id->greater,name,len,err));
    else				/* not found */
      *err = DAT_NOTFOUND;
  }

  return(id);				/* return found node or parent */
}


/* Routine to move down to the next directory level by parsing the
saved name string for the next delimiter character. The number of
characters remaining in the name is returned, or -1 if it is not
possible to move down a level */

static int dai_move_down(err)
int *err;				/* In/out error status */
{
  if(*err || part_end>=name_end) return(-1);

  depth++;
  part_start = ++part_end+1;
  while(++part_end<name_end)
    if(*part_end==DELIMITER) {
      --part_end;
      return(name_end-part_end);
    }
  return(0);
}


/* Routine to move back up to the next directory level. This is
achieved by backward parsing of the stored name for a delimiter
character. A cached identifier is returned */

static struct dat_node *dai_move_up(err)
int *err;				/* In/out error status */
{
  if(*err || depth<=0) return(0);

  part_end = --part_start-1;
  while(--part_start>=name_start)
    if(*part_start==DELIMITER) {
      ++part_start;
      break;
    }
  return(cache_id[--depth]);
}


/* Routine to update the cache values to be valid for one additional
directory level */

static void dai_update_cache(id,err)
struct dat_node *id;			/* Input identifier */
int *err;				/* In/out error status */
{
  char *from = part_start-1;		/* string copy pointer */
  char *to = cache_end;			/* string copy pointer */
  if(*err) return;

  if(depth-cache_depth!=1) {		/* sanity check */
    *err = DAT_FATAL;
    return;
  }

  if(depth>=MAX_DEPTH) {		/* too deep */
    *err = DAT_CACHEFULL;
    return;
  }

  while(++from<=part_end)		/* copy name to cache */
    *++to = *from;
  *++to = DELIMITER;			/* add terminating delimiter */
  cache_end = to;			/* update cache */
  cache_id[++cache_depth] = id;
}


/* Routine to save a supplied name and check in the cache for as
much of the name as possible. If part of the name remains a
directory search is implemented */

static struct dat_node *dai_search_cache(name,len,err)
char *name;				/* Input node name */
int len;				/* Input name length */
int *err;				/* In/out error status */
{
  struct dat_node *id = cache_id[0];	/* start at root identifier */
  char *cache = cache_start-1;		/* cache pointer */
  struct dat_node **array;		/* directory pointer array */
  if(*err) return(0);

  if(len>=MAX_CACHE) {			/* name + delimiter too long */
    *err = DAT_CACHEFULL;
    return(0);
  }

  name_start = name;			/* start of name less delimiter */
  if(*name_start==DELIMITER) ++name_start;
  name_end = name+len;			/* end of name less blanks */
  while(*--name_end==BLANK)
    ;
  if(*name_end==DELIMITER) --name_end;	/* and less delimiter */
  part_end = name_start-2;
  name = name_start-1;
  depth = 0;

/* Search cache for name */
  while(*++name==*++cache && name<=name_end && cache<=cache_end) {
    if(*name==DELIMITER) {
      part_start = part_end+2;
      part_end = name-1;
      id = cache_id[++depth];
    }
  }
  if(name>name_end && cache>cache_end) {/* complete match in cache */
    part_start = part_end+2;
    part_end = name_end;
    return(cache_id[++depth]);
  }
  else {				/* cache now only partly valid */
    cache_depth = depth;
    cache_end = cache_start+(part_end-name_start)+1;
  }

/* Search directory structure for remaining part of name */
  empty_dir_flag = 0;
  while(dai_move_down(err)>=0) {
    if(id->type==TYPE_DIR) {
      array = (struct dat_node **) id->values;
      if(array[0])
        id = array[0];
      else {
        *err = DAT_NOTFOUND;
        empty_dir_flag = 1;
      }
    }
    else if(id->type==TYPE_NULL && depth > 1) {
      *err = DAT_NULLNODE;
      break;
    }
    id = dai_find_node(id,part_start,part_end-part_start+1,err);
    dai_update_cache(id,err);
  }
  return(id);
}


/* Routine to add a node to a binary tree, the node is created and linked
into the tree. The node is left as a null node with no array */

static struct dat_node *dai_add_node(parent,name,len,err)
struct dat_node *parent;		/* Input immediate parent */
char *name;				/* Name of new node */
int len;				/* Length of name */
int *err;				/* In/out error status */
{
  struct dat_node *id;			/* node identifier */
  struct dat_node **array;		/* directory pointer array */
  char *from = name-1;			/* string copy pointers */
  char *to;
  int i = len;				/* name length */
  if(*err) return(0);

/* Allocate memory for node and name string */
  id = (struct dat_node *) dai_alloc(TYPE_NULL,0,err);
  to = dai_alloc(TYPE_NAME,i,err);
  if(*err || id==NULL) return(0);

/* Fill in node */
  id->type = TYPE_NULL;
  id->size = 0;
  id->values = 0;
  id->lesser = 0;
  id->greater = 0;
  id->name = to;
  id->name_len = i;
  while(++from<=part_end)
    *to++ = *from;

/* First node of a binary tree */
  if(empty_dir_flag) {
    array = (struct dat_node **) parent->values;
    array[0] = id;
  }

/* Link into binary tree */
  else {
    i = dai_name_cmp(id->name,id->name_len,parent->name,parent->name_len);

    if(i<0) {
      if(parent->lesser==0 || parent->lesser->name ==0) {
        parent->lesser = id;
      }
      else
        *err = DAT_FATAL;
    }

    else if(i>0) {
      if(parent->greater==0 || parent->greater->name==0) {
        parent->greater = id;
      }
      else
        *err = DAT_FATAL;
    }

    else
      *err = DAT_FATAL;

  }
  return(id);
}


/* Routine to update a values array on a node */

static char *dai_update_vals(id,type,size,err)
struct dat_node *id;			/* Input identifier */
char type;				/* Input required array type */
int size;				/* Input required array size */
int *err;				/* In/out error status */
{
  char *array;				/* array pointer */
  if(*err) return(0);
  if(id->type!=TYPE_NULL && id->type!=type) {
    *err = DAT_MISMATCH;
    return(0);
  }
  if(size==id->size)
    return(id->values);

  if(id->size) dai_dealloc(id->values,id->type,id->size,err);
  if(size>0) array = dai_alloc(type,size,err);
  if(*err) return(0);
  id->type = type;
  id->values = array;
  id->size = size;
  return(array);
}


/* Routine to copy a values array. This cannot copy application arrays as
 
it does not understand their typing */

static void dai_copy_vals (code,size,in,out,err)
char code;				/* input data type code */
int size;				/* input array size */
char *in;				/* input array */
char *out;				/* output array */
int *err;				/* in/out error status */
{
  int i = 0;				/* loop counter */
  int *i_in,*i_out;			/* int pointers */
  float *r_in,*r_out;			/* float pointers */
  double *d_in,*d_out;			/* double pointers */
  complex *x_in,*x_out;			/* complex pointers */

  if(*err)
    return;
  switch(code) {
  case 'C':				/* char */
  case 'c':
    while(++i<=size)
      *out++ = *in++;
    return;
  case 'I':				/* integer */
  case 'i':
  case 'L':
  case 'l':
    i_in = (int *) in;
    i_out = (int *) out;
    while(++i<=size)
      *i_out++ = *i_in++;
    return;
  case 'R':				/* real */
  case 'r':
    r_in = (float *) in;
    r_out = (float *) out;
    while(++i<=size)
      *r_out++ = *r_in++;
    return;
  case 'D':				/* double */
  case 'd':
    d_in = (double *) in;
    d_out = (double *) out;
    while(++i<=size) {
      *d_out++ = *d_in++;
    };
  return;
  case 'X':
  case 'x':
    x_in = (complex *) in;
    x_out = (complex *) out;
    while(++i<=size)
      *x_out++ = *x_in++;
    return;
  default:				/* error */
    *err = DAT_FATAL;
    return;
  }
}


/* Routine to delete a node and all nodes below it */

static void dai_del_all(id,err)
struct dat_node *id;			/* Input identifier */
int *err;				/* In/out error status */
{
  struct dat_node **array;		/* directory pointer array */
  if(*err || id==0) return;
  dai_del_all(id->lesser,err);		/* lesser nodes */
  id->lesser=0;
  if(id->type==TYPE_DIR) {		/* next directory down */
    array = (struct dat_node **) id->values;
    dai_del_all(array[0],err);
    array[0] = 0;
  }
  if(id->size) {
    dai_dealloc(id->values,id->type,id->size,err);
    id->values = 0;
    id->type = TYPE_NULL;
    id->size = 0;
  }
  dai_dealloc(id->name,TYPE_NAME,id->name_len,err);
  dai_dealloc((char *) id,TYPE_NULL,0,err);
  dai_del_all(id->greater,err);		/* greater nodes */
  id->greater=0;
}


/* Routine implement a directory list */

static void dai_list_sub(id,depth_count,routine,err)
struct dat_node *id;			/* Input identifier */
int depth_count;			/* Input depth */
void (*routine)();			/* Input fortran routine */
int *err;				/* In/out error status */
{
  int code;				/* call code */
  int one = 1;
  struct dat_node **array;		/* directory pointer array */

  if(*err || id==0) return;
  dai_list_sub(id->lesser,depth_count,routine,err);
  if(id->type==TYPE_DIR) {
    code = CODE_DIR;
      dailistf_(&code,&depth_count,id->name,&id->name_len,&id->type,&one, 
        &id->size, routine, 0L);
    if(code==CODE_DIR) {
      ++depth_count;
      array = (struct dat_node **) id->values;
      dai_list_sub(array[0],depth_count,routine,err);
      --depth_count;
    }
  }
  else if(id->type!=TYPE_NULL) {
    code = CODE_DATA;
      dailistf_(&code,&depth_count,id->name,&id->name_len,&id->type,&one, 
        &id->size, routine, 0L);
  }
  else {
    code = CODE_NULL;
      dailistf_(&code,&depth_count,id->name,&id->name_len,&id->type,&one, 
        &id->size, routine,0L);
  }
  dai_list_sub(id->greater,depth_count,routine,err);
}


/* Routine implement a directory dump using c i/o */

static void dai_dump_sub(id,depth_count,err)
struct dat_node *id;			/* Input identifier */
int depth_count;			/* Input depth */
int *err;				/* In/out error status */
{
  struct dat_node **array;		/* directory pointer array */
  char int_name[128];			/* Internal name string */
  char int_type[2];			/* Internal type string */
  int i;

  if(*err || id==0) return;
  dai_dump_sub(id->lesser,depth_count,err);

  int_type[0] = id->type;
  int_type[1] = '\0';
  for(i=0;i<id->name_len&&i<128;i++)				/* copy filename */
    int_name[i] = id->name[i];
  int_name[id->name_len]='\0';

  for (i=0;i<depth_count;i++) printf("   ");
  if(id->type==TYPE_DIR) {
    printf("Directory: %s\n",int_name);
    if((array = (struct dat_node **) id->values)) {
      ++depth_count;
      dai_dump_sub(array[0],depth_count,err);
      --depth_count;
    }
    else {
       printf("Bad data node\n");
    }
  }
  else if(id->type!=TYPE_NULL) {
    printf("Data: %s, type:%s\n",int_name,int_type);
  }
  else {
    printf("Null node: %s, type:%s\n",int_name,int_type);
  }
  dai_dump_sub(id->greater,depth_count,err);
}


/* Routine to save a directory structure */

static void dai_write_sub(id,outfile,err)
struct dat_node *id;			/* Input identifier */
FILE *outfile;				/* outfile indentifer */
int *err;				/* In/out error status */
{
  struct dat_node **array;		/* directory pointer array */
  if(*err || id==0) return;

  dai_write_sub(id->lesser,outfile,err);
  if (fwrite(&SOH,sizeof(SOH),1,outfile)!=1) {
     *err = DAT_OUTPUT;
     return;
   }
  if (fwrite (&id->name_len, sizeof(id->name_len), 1, outfile)!=1) {
     *err = DAT_OUTPUT;
     return;
   }
  if (fwrite (id->name, sizeof(*id->name), id->name_len, 
     outfile)!=id->name_len) {
     *err = DAT_OUTPUT;
     return;
  }
  if (fwrite (&id->type, sizeof(id->type), 1, outfile)!=1) {
     *err = DAT_OUTPUT;
     return;
  }
  if (fwrite (&id->size, sizeof(id->size), 1, outfile)!=1) {
     *err = DAT_OUTPUT;
     return;
  }
  if(id->type==TYPE_DIR) {
     array = (struct dat_node **) id->values;
     if (fwrite(&SOD,sizeof(SOD),1,outfile)!=1) {
        *err = DAT_OUTPUT;
        return;
     }
     dai_write_sub(array[0],outfile,err);
     if (fwrite(&EOD,sizeof(EOD),1,outfile)!=1) {
       *err = DAT_OUTPUT;
       return;
     }
  }
  else {
     if (fwrite (id->values, dai_mem_size(id->type,1,err), id->size, 
       outfile)!=id->size) {
       *err = DAT_OUTPUT;
       return;
     }
     if (fwrite(&EOH,sizeof(EOH),1,outfile)!=1) {
       *err = DAT_OUTPUT;
       return;
     }
  }
  dai_write_sub(id->greater,outfile,err);
}


/* Routine to get a directory structure */

static void dai_read_sub(initial_id,infile,err)
struct dat_node *initial_id;		/* Input identifier */
FILE *infile;				/* infile indentifer */
int *err;				/* In/out error status */
{
  struct dat_node **array;		/* directory pointer array */
  char code = EOH;
  struct dat_node *id = initial_id;

  if(*err||id==0) return;

  if (fread (&code,sizeof(code),1,infile) == 0 || code != SOD) {
    *err = DAT_INPUT;
    return;
  }
  if (fread (&code,sizeof(code),1,infile) == 0 || code != SOH) {
    *err = DAT_INPUT;
    return;
  }
  while (fread (&id->name_len, sizeof(id->name_len), 1, infile)) {
    id->greater=0;
    id->lesser=0;
    id->name = dai_alloc(TYPE_NAME, id->name_len, err);
    fread (id->name, sizeof(*id->name), id->name_len, infile);
    fread (&id->type, sizeof(id->type), 1, infile);
    fread (&id->size, sizeof(id->size), 1, infile);
    id->values = dai_alloc(id->type, id->size, err);
    if(id->type==TYPE_DIR) {
      array = (struct dat_node **) id->values;
      array[0] = (struct dat_node *) dai_alloc(TYPE_NULL,0,err);
      dai_read_sub(array[0],infile,err);
    }
    else {
      fread (id->values, dai_mem_size(id->type,1,err), id->size, infile);
      if (fread(&code,sizeof(code),1,infile) == 0 || code!=EOH) {
        *err = DAT_INPUT;
        return;
      }	
    }

    if (fread(&code,sizeof(code),1,infile) == 0 || code==EOD) {
      return;
    }	
    id->greater = (struct dat_node *) dai_alloc(TYPE_NULL,0,err);
    id->lesser = 0;
    id = id->greater;
  }

}


#if PVM
/* Routine to save a directory structure to PVM */

static void dai_pvm_write_sub(id,err)
struct dat_node *id;			/* Input identifier */
int *err;				/* In/out error status */
{
  struct dat_node **array;		/* directory pointer array */
  if(*err || id==0) return;

  dai_pvm_write_sub(id->lesser,err);
  if(id->type!=TYPE_NULL) {
     if (pvm_pkbyte(&SOH,sizeof(SOH),1)) {
        *err = DAT_OUTPUT;
        return;
     }
/*   printf (", SOH"); */
     if (pvm_pkbyte (&id->name_len, sizeof(id->name_len), 1)) {
        *err = DAT_OUTPUT;
        return;
     }
     if (pvm_pkbyte (id->name, id->name_len, 1)) {
        *err = DAT_OUTPUT;
        return;
     }
     if (pvm_pkbyte (&id->type, sizeof(id->type), 1)) {
        *err = DAT_OUTPUT;
        return;
     }
     if (pvm_pkbyte (&id->size, sizeof(id->size), 1)) {
        *err = DAT_OUTPUT;
        return;
     }
/*   printf(", name, type, size"); */
     if(id->type==TYPE_DIR) {
        if (pvm_pkbyte(&SOD,sizeof(SOD),1)) {
           *err = DAT_OUTPUT;
           return;
        }
/*      printf(", directory\nWrote SOD"); */
        array = (struct dat_node **) id->values;
        dai_pvm_write_sub(array[0],err);
        if (pvm_pkbyte(&EOD,sizeof(EOD),1)) {
          *err = DAT_OUTPUT;
          return;
        }
/*      printf(", EOD\n"); */
     }
     else {
        if (pvm_pkbyte (id->values,
			dai_mem_size(id->type,id->size,err), 1)) {
          *err = DAT_OUTPUT;
          return;
        }
/*      printf(", values"); */
        if (pvm_pkbyte(&EOH,sizeof(EOH),1)) {
          *err = DAT_OUTPUT;
          return;
        }
/*      printf(", EOH"); */
     }
  }
  dai_pvm_write_sub(id->greater,err);
}


/* Routine to get a directory structure */

static void dai_pvm_read_sub(initial_id,err)
struct dat_node *initial_id;		/* Input identifier */
int *err;				/* In/out error status */
{
  struct dat_node **array;		/* directory pointer array */
  char code = EOH;
  struct dat_node *id = initial_id;

  if(*err||id==0) return;

  if (pvm_upkbyte (&code,sizeof(code),1)|| code != SOD) {
    *err = DAT_INPUT;
    return;
  }
/*printf("Read SOD"); */
  if (pvm_upkbyte (&code,sizeof(code),1)|| code != SOH) {
    *err = DAT_INPUT;
    return;
  }
/*printf(", SOH"); */
  while (pvm_upkbyte (&id->name_len, sizeof(id->name_len), 1)==0) {
    id->greater=0;
    id->lesser=0;
    id->name = dai_alloc(TYPE_NAME, id->name_len, err);
    pvm_upkbyte (id->name, id->name_len, 1);
    pvm_upkbyte (&id->type, sizeof(id->type), 1);
    pvm_upkbyte (&id->size, sizeof(id->size), 1);
/*  printf(", name, type, size"); */
    id->values = dai_alloc(id->type, id->size, err);
    if(id->type==TYPE_DIR) {
/*    printf(", directory\n"); */
      array = (struct dat_node **) id->values;
      array[0] = (struct dat_node *) dai_alloc(TYPE_NULL,0,err);
      dai_pvm_read_sub(array[0],err);
    }
    else {
      pvm_upkbyte (id->values, dai_mem_size(id->type,id->size,err), 1);
/*    printf(", values"); */
      if (pvm_upkbyte(&code,sizeof(code),1)|| code!=EOH) {
        *err = DAT_INPUT;
        return;
      }	
/*    printf(", EOH"); */
    }

    if (pvm_upkbyte(&code,sizeof(code),1)|| code==EOD) {
/*    printf(", EOD\n"); */
      return;
    }	
    id->greater = (struct dat_node *) dai_alloc(TYPE_NULL,0,err);
    id->lesser = 0;
    id = id->greater;
  }

}
#endif


/*			************************
			* fortran Entry Points *
			************************
*/

/* Routine to initialize system */
void daiinit_()
{
  scrinit();
}
/* Routine to clean-up system */
void daiexit_()
{
  screxit();
}

/* Routine to create a new directory structure */
void daicreat_(name,ilen,err)
char *name;				/* Input structure name */
int *ilen;				/* Input name length */
int *err;				/* In/out error status */
{
  int len = *ilen;
  struct dat_node *id;			/* node identifier */
  struct dat_node **array;		/* directory values array */
  int loop = 0;				/* loop flag */
  int create = 0;			/* create node flag */

  if(*err) return;
  id = dai_search_cache(name,len,err);
  if(*err==DAT_OK) {
    if(id->type==TYPE_DIR)		/* already exists */
      return;
    else {				/* wrong type of node exists */
      if(id->type!=TYPE_NULL) {
         *err = DAT_MISMATCH;
         return;
       }
       else {
          array = (struct dat_node **) dai_update_vals(id,TYPE_DIR,1,err);
          if(*err==DAT_OK) {
            array[0] = 0;
            empty_dir_flag = 1;
            return;
          }
	}
    }
  }
  else if(*err==DAT_NOTFOUND) {		/* create new directory node */
    *err = DAT_OK;
    create = 1;
  }
  else if(*err==DAT_NULLNODE)		/* modify null node */
    *err = DAT_OK;
  else					/* error */
    return;

  while(loop>=0) {			/* loop over all lower levels */
    if(create) id = dai_add_node(id,part_start,part_end-part_start+1,err);
    create = 1;				/* force creation of lower levels */
    array = (struct dat_node **) dai_update_vals(id,TYPE_DIR,1,err);
    if(*err==DAT_OK) {
      array[0] = 0;
      empty_dir_flag = 1;
      dai_update_cache(id,err);
    }
    loop = dai_move_down(err);
  }
}


/* Routine to test if a directory entry exists */
void daiexist_(name,ilen,exists,err)
char *name;				/* Input structure name */
int *ilen;				/* Input name length */
int *exists;				/* 1 if exists else 0 */
int *err;				/* In/out error status */
{
  int len = *ilen;
  struct dat_node *id;			/* node identifier */

  if(*err) return;
  id = dai_search_cache(name,len,err);
  if(*err==DAT_OK) {
    if(id->type==TYPE_NULL) {
      *exists = 0;
      return;
    }
    else {
      *exists = 1;
      return;
    }
  }
  else if(*err==DAT_NOTFOUND) {	
    *err = DAT_OK;
    *exists = 0;
  }
  else if(*err==DAT_NULLNODE) {
    *err = DAT_OK;
    *exists = 0;
  }
  else	{				/* error */
    *exists = 0;
    return;
  }
}


/* Routine to put a header item */

void daiput_(name,ilen,type,itlen,values,ivlen,n,err)
char *name;				/* Input structure name */
char *type;				/* Input data type */
char *values;				/* Input values */
int *n;					/* Input number of values */
int *err;				/* In/out error status */
int *ilen;				/* Input name length */
int *itlen;				/* Input type length */
int *ivlen;				/* Input values length */
{
  int len = *ilen;
  int tlen = *itlen;
  int vlen = *ivlen;
  struct dat_node *id;			/* node identifier */
  char *array;				/* values array */
  if(*err) return;
  id = dai_search_cache(name,len,err);
  if(*err==DAT_OK)
    array = dai_update_vals(id,*type,*n,err);
  else if(*err==DAT_NOTFOUND) {
    *err = DAT_OK;
    if(dai_move_down(err)<0) {		/* must be last part of name only */
      id = dai_add_node(id,part_start,part_end-part_start+1,err);;
      array = dai_update_vals(id,*type,*n,err);
    }
  }
  else if(*err==DAT_NULLNODE) {
    *err = DAT_OK;
    if(dai_move_down(err)<0)		/* must be last part of name only */
      array = dai_update_vals(id,*type,*n,err);
  }
  dai_copy_vals(*type,*n,values,array,err);
}


/* Routine to get a header item */

void daiget_(name,ilen,type,itlen,values,ivlen,dim,n,err)
char *name;				/* Input structure name */
char *type;				/* Input data type */
char *values;				/* Output values */
int *dim;				/* Input values dimension size */
int *n;					/* Output number of values */
int *err;				/* In/out error status */
int *ilen;				/* Input name length */
int *itlen;				/* Input type length */
int *ivlen;				/* Input values length */
{
  int len = *ilen;
  int tlen = *itlen;
  int vlen = *ivlen;
  struct dat_node *id;			/* node identifier */
  char *last;				/* pointer to last part of name */
  int len_last;				/* length of last part of name */
  char *array;				/* values array */
  struct dat_node **darray;		/* directory values array */
  *n = 0;
  if(*err) return;
  id = dai_search_cache(name,len,err);
  if(*err==DAT_NOTFOUND && part_end==name_end) {
    last = part_start;
    len_last = part_end-part_start+1;
    while(*err==DAT_NOTFOUND) {
      *err = DAT_OK;
      id = dai_move_up(err);
      if(id) {
        if(id->type==TYPE_DIR) {
          darray = (struct dat_node **) id->values;
          if(darray[0])
            id = darray[0];
          else {
            *err = DAT_NOTFOUND;
            empty_dir_flag = 1;
          }
	}
        id = dai_find_node(id,last,len_last,err);
      }
      else {
        *err = DAT_NOTFOUND;
        break;
      }
    }
  }
  if(*err==DAT_OK) {
    array = id->values;
    *n = id->size;
    if(*n<=*dim)
      dai_copy_vals(*type,*n,array,values,err);
    else {
      dai_copy_vals(*type,*dim,array,values,err);
      *err = DAT_TRUNCATE;
    }
  }
  else if(*err==DAT_NULLNODE)
    *err = DAT_NOTFOUND;
}


/* Routine to allocate a user visible array */

void daialloc_(name,ilen,type,itlen,size,err)
char *name;				/* Input structure name */
char *type;				/* Input data type */
int *size;				/* Input array size */
int *err;				/* In/out error status */
int *ilen;				/* Input name length */
int *itlen;				/* Input type length */
{
  int len = *ilen;
  int tlen = *itlen;
  int lsize = *size + 2;
  struct dat_node *id;			/* node identifier */
  if(*err) return;
  id = dai_search_cache(name,len,err);
  if(*err==DAT_NOTFOUND) {		/* Could not find node */
    *err = DAT_OK;			/* => can create it */
    if(dai_move_down(err)<=0) {		/* must be last part of name only */
      id = dai_add_node(id,part_start,part_end-part_start+1,err);
      dai_update_vals(id,*type,lsize,err);
					/* not worth updating cache */
    }
    else
      *err = DAT_NOTFOUND;
  }
  else if(*err==DAT_OK) {		/* Found node */
         if(id->type==TYPE_NULL) {	/* But it could be a null node */
            *err = DAT_OK;
            dai_update_vals(id,*type,lsize,err);
	  }
         else {				/* Was not null */
            *err = DAT_EXISTS;
	  }
       }
  return;
}


/* Routine to return a user visible array with addresses referenced
with respect to some address raddress which is usually the relevant
common MEMI (or MEMC for strings) */

void daigetar_(name,ilen,type,itlen,raddress,irlen,caddress,iclen,
   address,size,err)
char *name;				/* Input structure name */
char *type;				/* Output data type */
char *raddress;				/* Reference array address */
char *caddress;				/* Reference array address */
int *address;				/* Output array index */
int *size;				/* Output array size */
int *err;				/* In/out error status */
int *ilen;				/* Input name length */
int *itlen;				/* Output type length */
int *irlen;				/* Length of raddress */
int *iclen;				/* Length of caddress */
{
  int len = *ilen;
  struct dat_node *id;			/* node identifier */
  char *last;				/* last part of name */
  int len_last;				/* length last part of name */
  *address = 1;
  *size = 0;
  if(*err) return;
  id = dai_search_cache(name,len,err);
  if(*err==DAT_NOTFOUND && part_end==name_end) {
    last = part_start;
    len_last = part_end-part_start+1;
    while(*err==DAT_NOTFOUND) {
      *err = DAT_OK;
      id = dai_move_up(err);
      if(id)
        id = dai_find_node(id,last,len_last,err);
      else {
        *err = DAT_NOTFOUND;
        break;
      }
    }
  }
/*  

This next part is potentially buggy: we are trying to align the
c and fortran data structures. This will all probably work on most
sensible machines but the string definition may prove a problem. The
main symptom of problems would be an illegal data access in a routine
which puts stuff in the string common MEMC. There are two +1's. The first
is needed to avoid alignment problems, the second is needed to obey the
fortran convention whereby arrays start with index 1.

The following code for debugging alignment problems.  Uncomment it to get
C's idea of the array addresses.  Use the debugger on the fortran
side to deterine the same information, and compare them.
(That is, use print &mem?[datadd('name')] or print &mem?[1] and
compute by hand.)
  {char temp[80];
   strncpy (temp, name, (size_t) *ilen);
   temp[*ilen] = '\0';
   fprintf (stderr, "DAIGETAR name = %s, address = %p\n", temp, id->values);}
*/
 
  if(*err==DAT_OK) {
    *type=id->type;
    *itlen = 1;
    if (*type=='R') {
       *address = ((float *) id->values - (float *) raddress) + 1;
    } else if (*type=='D') {
       *address = ((double *) id->values - (double *) raddress) + 1;
    } else if (*type=='I') {
       *address = ((int *) id->values - (int *) raddress) + 1;
    } else if (*type=='L') {
       *address = ((int *) id->values - (int *) raddress) + 1;
    } else if (*type=='X') {
       *address = ((complex *) id->values - (complex *) raddress) + 1;
    } else if (*type=='S') {
       *address = ((string *) id->values - (string *) caddress) + 1;
    } else {
       *err = DAT_BADCONT;
    }
    *address = *address + 1;		/* Obey fortran convention */
    *size = id->size - 2;		/* Adjust size */

  /* The SGI IRIX memory alignment is very strange */
#if COMP_SGI
  *address = *address - 1;
#endif

  }
  else {
    *err = DAT_MISMATCH;
  }
}


/* Routine to delete a structure */

void daidelet_(name,ilen,err)
char *name;				/* Input structure name */
int *ilen;				/* Input name length */
int *err;				/* In/out error status */
{
  int len = *ilen;
  struct dat_node *id;			/* node identifier */
  struct dat_node **array;
  if(*err) return;
  id = dai_search_cache(name,len,err);
  if(*err==DAT_OK) {

/* We can delete totally any stuff hanging off this node as a directory */

    if(id->type==TYPE_DIR) {
      array = (struct dat_node **) id->values;
      if (array[0]) 
         dai_del_all(array[0],err);
    }

/* We must preserve this node in the binary tree but as a null node 
possessing no values */

    if(id->type!=TYPE_NULL) {
      dai_dealloc(id->values,id->type,id->size,err);
      id->values = 0;
      id->size = 0;
      id->type = TYPE_NULL;
    }
    cache_end = cache_start-1;		/* invalidate cache */
    cache_depth = 0;
  }
}


/* Routine to rename a structure */
void dairenam_(old,iold_len,new,inew_len,err)
char *old;				/* Input structure name */
int *iold_len;				/* Input name length */
char *new;				/* Input structure name */
int *inew_len;				/* Input name length */
int *err;				/* In/out error status */
{
  int old_len = *iold_len;
  int new_len = *inew_len;
  struct dat_node *new_id;		/* new node identifier */
  struct dat_node *old_id;		/* old node identifier */
  if(*err) return;
  old_id = dai_search_cache(old,old_len,err);
  if(*err) return;
  new_id = dai_search_cache(new,new_len,err);
  if(*err==DAT_OK) {
    if (new_id->type==TYPE_NULL) {
      *err = DAT_OK;
      new_id->type = TYPE_DIR;
    }
    else {
      *err = DAT_EXISTS;
      return;
    }
  }
  else if(*err==DAT_NOTFOUND) {
     *err = DAT_OK;
     daicreat_(new,inew_len,err);
     new_id = dai_search_cache(new,new_len,err);
     if(*err) return;
  }

  if(*err==DAT_OK) {
    new_id->values = old_id->values;
    new_id->size = old_id->size;
    old_id->values = 0;
    old_id->size = 0;
    old_id->type = TYPE_NULL;
  }
  else
    dai_update_vals(new_id,TYPE_NULL,0,err);
}


/* Routine to list a structure */
void dailist_(name,ilen,routine,err)
char *name;				/* Input structure name */
int *ilen;				/* Input name length */
void (*routine)();			/* Input Routine */
int *err;				/* In/out error status */
{
  int len = *ilen;
  struct dat_node *id;			/* node identifier */
  int code;				/* call code */
  int depth_count = 0;			/* directory depth */
  static char c = 0;			/* dummy arguments */
  static int i = 0;
  int one = 1;
  struct dat_node **array;		/* directory pointer array */

  if(*err) return;
  id = dai_search_cache(name,len,err);
  if(*err==DAT_NULLNODE)
    *err = DAT_OK;
  else if(*err)
    return;
  array = (struct dat_node **) id->values;
  if(array[0]) {
     code = CODE_INIT;
     dailistf_(&code,&depth_count,name,&len,&c,&one,&i,routine,0L);
     dai_list_sub(array[0],depth_count,routine,err);
     code = CODE_END;
     dailistf_(&code,&depth_count,name,&len,&c,&one,&i,routine,0L);
   }
  else {
     *err = DAT_NOTFOUND;
     return;
   }
}

/* Routine to dump a structure */
void daiddump_(name,ilen,err)
char *name;				/* Input structure name */
int *ilen;				/* Input name length */
int *err;				/* In/out error status */
{
  int len = *ilen;
  struct dat_node *id;			/* node identifier */
  int depth_count = 0;			/* directory depth */
  struct dat_node **array;		/* directory pointer array */

  if(*err) return;
  id = dai_search_cache(name,len,err);
  if(*err==DAT_NULLNODE)
    *err = DAT_OK;
  else if(*err)
    return;
  array = (struct dat_node **) id->values;
  if(array[0]) {
     printf("\nDirectory dump\n"); 
     dai_dump_sub(array[0],depth_count,err);
  }
  else {
     *err = DAT_NOTFOUND;
  };
}


/* Routine to save a structure to a disk file */
void daiwrite_(name,ilen,filename,iflen,err)
char *name;				/* Input structure name */
char *filename;				/* Filename */
int *err;				/* In/out error status */
int *ilen;				/* Input name length */
int *iflen;				/* length of filename */
{
  int len = *ilen;
  int flen = *iflen;
  struct dat_node *id;			/* node identifier */
  FILE *outfile;			/* identifier for output file */
  char *int_filename;			/* internal filename */
  char *iomode = "w";
  int i = 0;
  struct dat_node **array;

  if(*err) return;
  
#if COMP_MWAY
   _pmode = 0x8000;			/* Binary io only */
#endif

  id = dai_search_cache(name,len,err);	/* find identifier */
  if(*err) return;
  if(id->type!=TYPE_DIR) {
    *err = DAT_MISMATCH;
    return;
  }
  else {
    array = (struct dat_node **) id->values;
    if (array[0]) {
      int_filename = dai_alloc(TYPE_NAME,flen+1,err);	/* allocate space */
      if(*err) return;
      while (++i<=flen)			/* copy filename */
        int_filename[i-1] = filename[i-1];
      int_filename[flen]='\0';

      outfile = fopen (int_filename, iomode);	/* open file for write */

      if (outfile) {
        if (fwrite(&SOD,sizeof(SOD),1,outfile)!=1) {
           *err = DAT_OUTPUT;
        }
        dai_write_sub(array[0],outfile,err);	/* start saving */
        if (fwrite(&EOD,sizeof(EOD),1,outfile)!=1) {
           *err = DAT_OUTPUT;
        }
        fclose(outfile);
      }
      else {
        *err = DAT_OUTPUT;
      }
      dai_dealloc(int_filename,TYPE_NAME,flen,err);
    }
    else {
      *err = DAT_NOTFOUND;
    }
  }
}

/* Routine to get a structure from a disk file */
void dairead_(name,ilen,filename,iflen,err)
char *name;				/* Input structure name */
char *filename;				/* Filename */
int *err;				/* In/out error status */
int *ilen;				/* Input name length */
int *iflen;				/* length of filename */
{
  int len = *ilen;
  int flen = *iflen;
  struct dat_node *id;			/* node identifier */
  FILE *infile;				/* identifier for input file */
  char *int_filename;			/* internal filename */
  char *iomode = "r";
  int i = 0;
  struct dat_node **array;

  if(*err) return;
  
#if COMP_MWAY
   _pmode = 0x8000;			/* Binary io only */
#endif

  id = dai_search_cache(name,len,err);	/* find identifier */
  if(*err) return;
  if(id->type!=TYPE_DIR) {
    *err = DAT_MISMATCH;
    return;
  }
  else {
    array = (struct dat_node **) id->values;
    if (array[0]) {
      *err = DAT_EXISTS;
      return;
    }
    else {
      int_filename = dai_alloc(TYPE_NAME,flen+1,err);	/* allocate space */
      if(*err) return;
      while (++i<=flen)			/* copy filename */
        int_filename[i-1] = filename[i-1];
      int_filename[flen]='\0';

      infile = fopen (int_filename, iomode);	/* open file for write */    
      if (infile) {
        array[0] = (struct dat_node *) dai_alloc(TYPE_NULL,0,err);
        dai_read_sub(array[0],infile,err);
        fclose(infile);
      }
      else {
        printf("Cannot open file %s for reading\n",int_filename);
        *err = DAT_NOTFOUND;
      }
    }
  }
  dai_dealloc(int_filename,TYPE_NAME,flen,err);
  cache_end = cache_start-1;			/* Invalidate cache */
  cache_depth = 0;
}


#if PVM
/* Routine to save a structure to PVM */
void daipwrit_(name,ilen,err)
char *name;				/* Input structure name */
int *err;				/* In/out error status */
int *ilen;				/* Input name length */
{
  int len=*ilen;
  struct dat_node *id;			/* node identifier */
  int i = 0;
  struct dat_node **array;

  if(*err) return;
  
  id = dai_search_cache(name,len,err);	/* find identifier */
  if(*err) return;
  if(id->type!=TYPE_DIR) {
    *err = DAT_MISMATCH;
    return;
  }
  else {
    array = (struct dat_node **) id->values;
    if (array[0]) {
      if (pvm_pkbyte(&SOD,sizeof(SOD),1)) {
         *err = DAT_OUTPUT;
         return;
      };
/*    printf("Wrote SOD"); */
      dai_pvm_write_sub(array[0],err);	/* start saving */
      if (pvm_pkbyte(&EOD,sizeof(EOD),1)) {
         *err = DAT_OUTPUT;
         return;
      }
/*    printf(", EOD\n"); */
    }
    else {
      *err = DAT_NOTFOUND;
    }
  }
}

/* Routine to get a structure from PVM */
void daipread_(name,ilen,err)
char *name;				/* Input structure name */
int *err;				/* In/out error status */
int *ilen;				/* Input name length */
{
  int len=*ilen;
  struct dat_node *id;			/* node identifier */
  int i = 0;
  struct dat_node **array;

  if(*err) return;
  
  id = dai_search_cache(name,len,err);	/* find identifier */
  if(*err) return;
  if(id->type!=TYPE_DIR) {
    *err = DAT_MISMATCH;
    return;
  }
  else {
    array = (struct dat_node **) id->values;
    if (array[0]) {
      *err = DAT_EXISTS;
      return;
    }
    else {
      if(*err) return;
      array[0] = (struct dat_node *) dai_alloc(TYPE_NULL,0,err);
      dai_pvm_read_sub(array[0],err);
    }
  }
  cache_end = cache_start-1;			/* Invalidate cache */
  cache_depth = 0;
}

#endif
