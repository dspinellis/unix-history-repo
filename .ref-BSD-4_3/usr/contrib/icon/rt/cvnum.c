#include "../h/ctype.h"
#include "../h/rt.h"
#include <math.h>

/*
 * cvnum - convert the value represented by d into a numeric quantity and
 *  place the value into *result.  T_LONGINT is returned for integer and
 *  long integer results; T_REAL for real results, and NULL is returned
 *  if d can't be converted to a numeric quantity.
 */

cvnum(d, result)
register struct descrip *d;
union numeric *result;
   {
   char sbuf[MAXSTRING];
   extern char *cvstr();

   DeRef(*d)

   if (QUAL(*d)) {
      /*
       * d is a string.  Convert it into an integer by first converting
       *  it into a C-style string and then converting that string into
       *  an integer with ston.
       */
      qtos(d, sbuf);
      return (ston(sbuf, result));
      }

   switch (TYPE(*d)) {
      case T_INTEGER:
         /*
          * d is already an integer.  Cast the value into a long.
          */
         result->integer = (long)INTVAL(*d);
         return (T_LONGINT);
#ifdef LONGS
      case T_LONGINT:
         /*
          * d is a long integer.  Assign it to *i and return.
          */
         result->integer = BLKLOC(*d)->intval;
         return (T_LONGINT);
#endif LONGS

      case T_REAL:
         /*
          * d is a real number, return it.
          */
         result->real = BLKLOC(*d)->realblk.realval;
         return (T_REAL);
      default:
         /*
          * d is not already numeric, try to convert it to a string and
          *  then try to convert the string to an integer.
          */
         if (cvstr(d, sbuf) == NULL)
            return (NULL);
         return (ston(STRLOC(*d), result));
      }
   }

#define BIG 72057594037927936.  /* numbers larger than 2^56 lose precision */

/*
 * ston - convert a string to a numeric quantity if possible.
 */
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

   /*
    * Skip leading white space.
    */
   while (isspace(c))
      c = *s++;

   /*
    * Check for sign.
    */
   if (c == '+' || c == '-') {
      msign = c;
      c = *s++;
      }

   /*
    * Get integer part of mantissa.
    */
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

   /*
    * Check for based integer.
    */
   if (c == 'r' || c == 'R')
      return (radix(msign, (int)mantissa, s, result));

   /*
    * Get fractional part of mantissa.
    */
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

   /*
    * Check that at least one digit has been seen so far.
    */
   if (digits == 0)
      return (NULL);

   /*
    * Get exponent part.
    */
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

   /*
    * Skip trailing white space.
    */
   while (isspace(c))
      c = *s++;

   /*
    * Check that entire string has been consumed.
    */
   if (c != '\0')
      return (NULL);

   /*
    * Test for integer.
    */
   if (!realflag && mantissa >= MINLONG && mantissa <= MAXLONG) {
      result->integer = (msign == '+')? mantissa : -mantissa;
      return (T_LONGINT);
      }

   /*
    * Rough tests for overflow and underflow.
    */
   if (sdigits + scale > LGHUGE)
      return (NULL);

   if (sdigits + scale < -LGHUGE) {
      result->real = 0.0;
      return (T_REAL);
      }

   /*
    * Put the number together by multiplying the mantissa by 5^scale and
    *  then using ldexp() to multiply by 2^scale.
    */

#ifdef PDP11
   /*
    * Load floating point status register on PDP-11.
    */
   ldfps(0200);
#endif PDP11
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
   /*
    * Load floating point status register on PDP-11
    */
   ldfps(03200);
#endif PDP11
   if (errno > 0 && mantissa > 0)
      /*
       * ldexp caused overflow.
       */
      return (NULL);

   result->real = (msign == '+')? mantissa : -mantissa;
   return (T_REAL);
   }

/*
 * radix - convert string s in radix r into an integer in *result.  sign
 *  will be either '+' or '-'.
 */
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

   result->integer = (sign == '+')? num : -num;
   return (T_LONGINT);
   }
