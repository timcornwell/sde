#if defined(__STDC__) || defined(sgi)
#include <float.h>
#define NEGLIM  -FLT_EPSILON
#define POSLIM   FLT_EPSILON 
#else
#define NEGLIM  -1.0e-38
#define POSLIM   1.0e-38 
#endif


notzero(num)
double num;
{
	if ((num > NEGLIM) && (num < POSLIM))
		return 0;
	else
		return 1;
}
