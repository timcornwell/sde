/*
	National Radio Astronomy Observatory, Socorro, NM 87801
	Software Development Environment (SDE)
 */
/*
@(#)fti.c	1.6    24 Feb 1995
Routines for packing and unpacking numbers

Audit Trail:
       Removed byte copy function calls
				T.J. Cornwell		Sept 12 1989
       Further simplification
				T.J. Cornwell		Sept 13 1989
       Added double precision
				T.J. Cornwell		Sept 21 1989
       Added IEEE support
				T.J. Cornwell		Jan 4 1990
       Cleaned up code: removed Compaq support
				T.J. Cornwell		Feb 15 1990
       Added code to convert between IEEE and Cray floating
                                R.T. Duquet             Feb 21 1990
       Added bscale, bzero conversion for IEEE formats (except CRAY)
                                R.G. Marson             Oct 24 1990
       Added bscale, bzero output conversion for IEEE formats 
                                T.J. Cornwell           March 18 1991
       Added support for the DEC Alpha machines (byte swapping).
                                J.D. Ellithorpe         Oct 21 1994
*/
#if COMP_CRAY
#define fticr_ FTICR
#define ftidr_ FTIDR
#endif
#if COMP_IBM || COMP_HP
#define fticr_ fticr
#define ftidr_ ftidr
#endif

#if OS_SYSV
#define BCOPY(a,b,n) memcpy(b,a,n)
#else
#define BCOPY(a,b,n) bcopy(a,b,n)
#endif

#if COMP_ALPHA
/*
Byte swapping routines for the DEC Alpha architecture.
*/
void swap2(a,b)
char *a, *b;
{
  char tmp;
  tmp = *a;
  *a  = *b;
  *b  = tmp;
}

void swap4(a,b,c,d)
char *a, *b, *c, *d;
{
  swap2(a,d);
  swap2(b,c);
}
#endif

/*
Convert incoming bytes to real data. Allows stepping in input data.
*/

void fticr_ (nbytes, bscale, bzero, n, buff, offset, step, data)
int *nbytes;
double *bscale;
double *bzero;
int *n;
char *buff;
int *offset;
int *step;
float *data;
{
   int i, one;
   int ltmp;

#if COMP_ALPHA
   short stmp;
   float ftmp;
   char *tmp;
#endif

   switch(*nbytes) {
   case -4:				/* IEEE */
#if COMP_CRAY
     one = 1; 
     if(*step!=1) {
        for (i=0;i<*n;i++)
           IESCTC((float *)(buff +4*(*offset+i*(*step))), &one, 
                  (float *)(data+i), &one);
      }
     else {
           IESCTC((float *)(buff +4*(*offset)), &one, 
                  (float *)data, n, &one);
	 }
#else
# if COMP_ALPHA
     if ((*bscale == (double) 1.0) && (*bzero == (double) 0.0)) {
       for(i=0;i<*n;i++) {
	 BCOPY(buff+4*(*offset+i*(*step)), (char *)(data+i), 4);
	 tmp = (char*)(data+i);
	 swap4(tmp, tmp+1, tmp+2, tmp+3);
       } 
     }
# else
     if ((*bscale == (double) 1.0) && (*bzero == (double) 0.0))
       if(*step!=1)
	 for (i=0;i<*n;i++)
	   BCOPY(buff +4*(*offset+i*(*step)), (char *)(data+i), 4);
       else 				/* Step is unity */
	 BCOPY(buff +4*(*offset), (char *)data, 4*(*n));
     else
       if(*step!=1)
	 for (i=0;i<*n;i++)
	   {
	     BCOPY(buff +4*(*offset+i*(*step)), (char *)(data+i), 4);
	     *(data + i) = *(data + i) * (*bscale) + (*bzero);
	   }
       else 				/* Step is unity */
	 {
	   BCOPY(buff +4*(*offset), (char *)data, 4*(*n));
	   for (i=0; i<*n;i++)
	     *(data + i) = *(data + i) * (*bscale) + (*bzero);
	 }
# endif
#endif
     break;
   case 2:				/* 16 bit integer */
     for (i=0;i<*n;i++) {
#if COMP_ALPHA
        BCOPY( buff +2*(*offset+i*(*step)), (char *)&stmp, 2);
	tmp = (char*)&stmp;
	swap2(tmp, tmp+1);
	*(data+i) = *bscale * stmp + *bzero;
#else
        BCOPY( buff +2*(*offset+i*(*step)), (char *)&ltmp, 2);
        ltmp >>= 8*(sizeof(int) - 2);	/* Shift right */
# if COMP_IBM || COMP_HP
        if(ltmp & 0x8000) ltmp |= 0xffff0000;
# else
        if(ltmp & 0x8000) ltmp |= 0xffffffffffff0000;
# endif
					/* Mask top bits */
        *(data+i) = *bscale * ltmp + *bzero;
#endif
     }
   break;
   case 4:				/* 32 bit integer */
     for (i=0;i<*n;i++) {
       BCOPY( buff +4 * (*offset+i*(*step)), (char *)&ltmp, 4);
#if COMP_ALPHA
       tmp = (char*)&ltmp;
       swap4(tmp, tmp+1, tmp+2, tmp+3);
#else
       ltmp >>= 8 * (sizeof(int) - 4);	/* Shift right */
# if COMP_IBM || COMP_HP
# else
       if(ltmp & 0x80000000) ltmp |= 0xffffffff00000000;
# endif
#endif
					/* Mask top bits */
       *(data+i) = *bscale * ltmp+*bzero;
     }
   break;
   }
}

void ftidr_ (nbytes, bscale, bzero, n, buff, offset, step, data)

/*
Convert real data to outgoing bytes: can skip in output data
*/

int *nbytes;
double *bscale;
double *bzero;
int *n;
char *buff;
int *offset;
int *step;
float *data;
{
   int i,j;
   int ltmp;
   int ier, one;
   float ftmp;
#if COMP_ALPHA
   char *tmp;
   short stmp;
#endif

 
   if(*nbytes==-4) {			/* IEEE  is simple */
#if COMP_CRAY
     one = 1;
     if ((*bscale == (double) 1.0) && (*bzero == (double) 0.0))
       if(*step!=1) {
         for (i=0;i<*n;i++)		/* Step is not unity */
           IESCTI((float *)(data+i), (float *)(buff +4*(*offset+i*(*step))), 
              &one, &one, &ier);
       }
       else {				/* Step is unity */
           IESCTI((float *)data, (float *)(buff +4*(*offset)), 
              &one, n, &ier);
       }
     else
     for (i=0;i<*n;i++) {	
       ftmp = (float)(*bscale * *(data+i)+*bzero);
       IESCTI((float *)&ftmp, (float *)(buff +4*(*offset)), 
          &one, n, &ier);
     }
#else
# if COMP_ALPHA
     for(i=0;i<*n;i++) {
       ftmp = (float)(*bscale * *(data+i) + *bzero);
       tmp = (char *)&ftmp;
       swap4(tmp, tmp+1, tmp+2, tmp+3);
       BCOPY((char *)&ftmp, buff +4*(*offset+i*(*step)), 4);
     }
# else
     if ((*bscale == (double) 1.0) && (*bzero == (double) 0.0))
       if(*step!=1) {
         for (i=0;i<*n;i++)		/* Step is not unity */
           BCOPY((char *)(data+i), buff +4*(*offset+i*(*step)), 4);
       }
       else {				/* Step is unity */
         BCOPY((char *)data, buff +4*(*offset), 4*(*n));
       }
     else
       for (i=0;i<*n;i++) {	
         ftmp = (float)(*bscale * *(data+i)+*bzero);
         BCOPY((char *)&ftmp, buff +4*(*offset+i*(*step)), 4);
       }
# endif
#endif
   }
   else {				/* nbyte integer */
#if COMP_ALPHA
     if(*nbytes == 2) {
       for(i=0;i<*n;i++) {
	 stmp = (short)(*bscale * *(data+i) + *bzero);
	 swap2(&stmp, &stmp+1);
	 BCOPY((char*)&stmp, buff + 2*((*offset)+i*(*step)), 2);
       }
     } else {
       for(i=0;i<*n;i++) {
	 ltmp = (int)(*bscale * *(data+i)+*bzero);
	 tmp = (char*)&ltmp;
	 swap4(tmp, tmp+1, tmp+2, tmp+3);
	 BCOPY((char*)&ltmp, buff + 4*((*offset)+i*(*step)), 4);
       }
     }
#else
     for (i=0;i<*n;i++) {
       ltmp = (int)(*bscale * *(data+i)+*bzero);
       ltmp <<= 8*(sizeof(int) - sizeof(char) * (*nbytes)); /* Shift left */
       BCOPY((char *)&ltmp, buff+(*nbytes)*((*offset)+i*(*step)),
          (*nbytes));
     }
#endif
   }
}

