#include "../h/ctype.h"
#include "../h/rt.h"
#include <math.h>
#define LOGHUGE 39

/*
 * cvnum - convert to numeric.
 */

cvnum(d, result)
register struct descrip *d;
union numeric *result;
   {
   char sbuf[MAXSTRING];
   extern char *cvstr();

   deref(d);

   if (QUAL(*d)) {
      qtos(d, sbuf);
      return (ston(sbuf, result));
      }

   switch (TYPE(*d)) {
      case T_INTEGER:
         result->i = (long)INTVAL(*d);
         return (T_LONGINT);
#ifndef BIT32
      case T_LONGINT:
         result->i = BLKLOC(*d)->intval;
         return (T_LONGINT);
#endif

      case T_REAL:
         result->r = BLKLOC(*d)->realval;
         return (T_REAL);
      default:
         if (cvstr(d, sbuf) == NULL)
            return (NULL);
         return (ston(STRLOC(*d), result));
      }
   }

#define BIG 72057594037927936.  /* numbers larger than 2^56 lose precision */

static ston(s, result)
register char *s;
union numeric *result;
   {
   register int c;
   int realflag = 0;	/* indicates a real number */
   char msign = '+';	/* sign of mantissa */
   char esign = '+';	/* sign of exponent */
   double mantissa = 0;	/* scaled mantissa with no fractional part */
   int scale = 0;	/* number of decimal places to shift mantissa */
   int digits = 0;	/* total number of digits seen */
   int sdigits = 0;	/* number of significant digits seen */
   int exponent = 0;	/* exponent part of real number */
   double fiveto;	/* holds 5^scale */
   double power;	/* holds successive squares of 5 to compute fiveto */
   extern int errno;

   c = *s++;

   /* skip leading white space */

   while (isspace(c))
      c = *s++;

   /* check for sign */

   if (c == '+' || c == '-') {
      msign = c;
      c = *s++;
      }

   /* get integer part of mantissa */

   while (isdigit(c)) {
      digits++;
      if (mantissa < BIG) {
	 mantissa = mantissa * 10 + (c - '0');
   	 if (mantissa > 0.0)
	    sdigits++;
	 }
      else
	 scale++;
      c = *s++;
      }

   /* check for based integer */

   if (c == 'r' || c == 'R')
      return (radix(msign, (int)mantissa, s, result));

   /* get fractional part of mantissa */

   if (c == '.') {
      realflag++;
      c = *s++;
      while (isdigit(c)) {
	 digits++;
	 if (mantissa < BIG) {
	    mantissa = mantissa * 10 + (c - '0');
	    scale--;
   	    if (mantissa > 0.0)
	       sdigits++;
	    }
	 c = *s++;
	 }
      }

   /* check that at least one digit has been seen so far */

   if (digits == 0)
      return (NULL);

   /* get exponent part */

   if (c == 'e' || c == 'E') {
      realflag++;
      c = *s++;
      if (c == '+' || c == '-') {
	 esign = c;
	 c = *s++;
	 }
      if (!isdigit(c))
	 return (NULL);
      while (isdigit(c)) {
	 exponent = exponent * 10 + (c - '0');
	 c = *s++;
	 }
      scale += (esign == '+')? exponent : -exponent;
      }

   /* skip trailing white space */

   while (isspace(c))
      c = *s++;

   /* check that entire string has been consumed */

   if (c != '\0')
      return (NULL);

   /* test for integer */

   if (!realflag && mantissa >= MINLONG && mantissa <= MAXLONG) {
      result->i = (msign == '+')? mantissa : -mantissa;
      return (T_LONGINT);
      }

   /* rough tests for overflow and underflow */

   if (sdigits + scale > LOGHUGE)
      return (NULL);

   if (sdigits + scale < -LOGHUGE) {
      result->r = 0.0;
      return (T_REAL);
      }

   /* put the number together */
   /* first multiply mantissa by 5^scale */
   /* then use ldexp() to multiply by 2^scale */

#ifdef PDP11
   ldfps(0200);
#endif
   exponent = (scale > 0)? scale : -scale;
   fiveto = 1.0;
   power = 5.0;
   for (;;) {
      if (exponent & 01)
	 fiveto *= power;
      exponent >>= 1;
      if (exponent == 0)
	 break;
      power *= power;
      }
   if (scale > 0)
      mantissa *= fiveto;
   else
      mantissa /= fiveto;

   errno = 0;
   mantissa = ldexp(mantissa, scale);
#ifdef PDP11
   ldfps(03200);
#endif
   if (errno > 0 && mantissa > 0)	/* ldexp caused overflow */
      return (NULL);

   result->r = (msign == '+')? mantissa : -mantissa;
   return (T_REAL);
   }

static radix(sign, r, s, result)
char sign;
register int r;
register char *s;
union numeric *result;
   {
   register int c;
   long num;

   if (r < 2 || r > 36)
      return (NULL);

   c = *s++;
   num = 0L;
   while (isalnum(c)) {
      c = tonum(c);
      if (c >= r)
	 return (NULL);
      num = num * r + c;
      c = *s++;
      }

   while (isspace(c))
      c = *s++;

   if (c != '\0')
      return (NULL);

   result->i = (sign == '+')? num : -num;
   return (T_LONGINT);
   }
