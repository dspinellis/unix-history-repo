/*
#	MATH(3.icon)
#
#	Miscellaneous math functions
#
#	Ralph E. Griswold
#
#	Last modified 8/19/84
#
*/

#include "../h/rt.h"
#include <errno.h>

int errno;
/*
 * exp(x), x in radians
 */
Xexp(nargs, arg1, arg0)
int nargs;
struct descrip arg1, arg0;
   {
   int t;
   double y;
   union numeric r;
   double exp();
   
   if ((t = cvreal(&arg1, &r)) == NULL) runerr(102, &arg1);
   y = exp(r.real);
   if (errno == ERANGE) runerr(252, NULL);
   mkreal(y,&arg0);
   }
Procblock(exp,1)

/*
 * log(x), x in radians
 */
Xlog(nargs, arg1, arg0)
int nargs;
struct descrip arg1, arg0;
   {
   int t;
   double y;
   union numeric r;
   double log();
   
   if ((t = cvreal(&arg1, &r)) == NULL) runerr(102, &arg1);
   y = log(r.real);
   if (errno == EDOM) runerr(251, NULL);
   mkreal(y,&arg0);
   }
Procblock(log,1)

/*
 * log10(x), x in radians
 */
Xlog10(nargs, arg1, arg0)
int nargs;
struct descrip arg1, arg0;
   {
   int t;
   double y;
   union numeric r;
   double log10();
   
   if ((t = cvreal(&arg1, &r)) == NULL) runerr(102, &arg1);
   y = log10(r.real);
   if (errno == EDOM) runerr(251, NULL);
   mkreal(y,&arg0);
   }
Procblock(log10,1)

/*
 * sqrt(x), x in radians
 */
Xsqrt(nargs, arg1, arg0)
int nargs;
struct descrip arg1, arg0;
   {
   int t;
   double y;
   union numeric r;
   double sqrt();
   
   if ((t = cvreal(&arg1, &r)) == NULL) runerr(102, &arg1);
   y = sqrt(r.real);
   if (errno == EDOM) runerr(251, NULL);
   mkreal(y,&arg0);
   }
Procblock(sqrt,1)
