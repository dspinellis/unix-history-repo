/*
#	TRIG(3.icon)
#
#	Trigonometric functions
#
#	Ralph E. Griswold and Stephen B. Wampler
#
#	Last modified 8/19/84
#
*/

#include "../h/rt.h"
#include <errno.h>

int errno;

/*
 * sin(x), x in radians
 */
Xsin(nargs, arg1, arg0)
int nargs;
struct descrip arg1, arg0;
   {
   int t;
   union numeric r;
   double sin();
   
   if ((t = cvreal(&arg1, &r)) == NULL) runerr(102, &arg1);
   mkreal(sin(r.real),&arg0);
   }
Procblock(sin,1)

/*
 * cos(x), x in radians
 */
Xcos(nargs, arg1, arg0)
int nargs;
struct descrip arg1, arg0;
   {
   int t;
   union numeric r;
   double cos();
   
   if ((t = cvreal(&arg1, &r)) == NULL) runerr(102, &arg1);
   mkreal(cos(r.real),&arg0);
   }
Procblock(cos,1)

/*
 * tan(x), x in radians
 */
Xtan(nargs, arg1, arg0)
int nargs;
struct descrip arg1, arg0;
   {
   int t;
   double y;
   union numeric r;
   double tan();
   
   if ((t = cvreal(&arg1, &r)) == NULL) runerr(102, &arg1);
   y = tan(r.real);
   if (errno == ERANGE) runerr(252, NULL);
   mkreal(y,&arg0);
   }
Procblock(tan,1)

/*
 * acos(x), x in radians
 */
Xacos(nargs, arg1, arg0)
int nargs;
struct descrip arg1, arg0;
   {
   int t;
   double y;
   union numeric r;
   double acos();
   
   if ((t = cvreal(&arg1, &r)) == NULL) runerr(102, &arg1);
   y = acos(r.real);
   if (errno == EDOM) runerr(251, NULL);
   mkreal(y,&arg0);
   }
Procblock(acos,1)

/*
 * asin(x), x in radians
 */
Xasin(nargs, arg1, arg0)
int nargs;
struct descrip arg1, arg0;
   {
   int t;
   double y;
   union numeric r;
   double asin();
   
   if ((t = cvreal(&arg1, &r)) == NULL) runerr(102, &arg1);
   y = asin(r.real);
   if (errno == EDOM) runerr(251, NULL);
   mkreal(y,&arg0);
   }
Procblock(asin,1)

/*
 * atan(x), x in radians
 */
Xatan(nargs, arg1, arg0)
int nargs;
struct descrip arg1, arg0;
   {
   int t;
   union numeric r;
   double atan();
   
   if ((t = cvreal(&arg1, &r)) == NULL) runerr(102, &arg1);
   mkreal(atan(r.real),&arg0);
   }
struct b_iproc Batan = {
   T_PROC,
   sizeof(struct b_proc),
   EntryPoint(Xatan),
   1,
   -1,
   0,
   0,
   {4, "atan"}
   };

/*
 * atan2(x,y), x, y in radians
 */
Xatan2(nargs, arg2, arg1, arg0)
int nargs;
struct descrip arg2, arg1, arg0;
   {
   int t;
   union numeric r1, r2;
   double atan2();

   if ((t = cvreal(&arg2, &r2)) == NULL) runerr(102, &arg2);
   if ((t = cvreal(&arg1, &r1)) == NULL) runerr(102, &arg1);
   mkreal(atan2(r1.real,r2.real),&arg0);
   }
Procblock(atan2,2)

#define PI 3.14159

/*
 * dtor(x), x in degrees
 */
Xdtor(nargs, arg1, arg0)
int nargs;
struct descrip arg1, arg0;
   {
   union numeric r;

   if (cvreal(&arg1, &r) == NULL) runerr(102, &arg1);
   mkreal(r.real * PI / 180, &arg0);
   }
Procblock(dtor,1)

/*
 * rtod(x), x in radians
 */
Xrtod(nargs, arg1, arg0)
int nargs;
struct descrip arg1, arg0;
   {
   union numeric r;

   if (cvreal(&arg1, &r) == NULL) runerr(102, &arg1);
   mkreal(r.real * 180 / PI, &arg0);
   }
Procblock(rtod,1)
