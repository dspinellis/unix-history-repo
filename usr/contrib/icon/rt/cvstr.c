#include "../h/rt.h"

/*
 * cvstr(d,s) - convert d (in place) into a string, using s as buffer
 *  if necessary.  cvstr returns 0 if the conversion fails, 1 if d
 *  wasn't a string but was converted into one, and 2 if d was already
 *  a string.  When a string conversion takes place, sbuf gets the
 *  resulting string.
 */

cvstr(d, sbuf)
register struct descrip *d;
char *sbuf;
   {
   
   /*
    * Dereference d if necessary.
    */
      DeRef(*d)

   if (QUAL(*d)) {
      /*
       * d is already a string.
       */
      if (NULLDESC(*d))
         return (NULL);
      return (2);
      }

   switch (TYPE(*d)) {
      /*
       * For types that can be converted into strings, call the appropriate
       *  conversion routine and return their result.  Note that the
       *  conversion routines write over d.
       */
      case T_INTEGER:
         return (itos((long)INTVAL(*d), d, sbuf));

#ifdef LONGS
      case T_LONGINT:
         return (itos(BLKLOC(*d)->longint.intval, d, sbuf));
#endif LONGS
      case T_REAL:
         return (rtos(BLKLOC(*d)->realblk.realval, d, sbuf));

      case T_CSET:
         return (cstos(BLKLOC(*d)->cset.bits, d, sbuf));

      default:
         /*
          * d can't be converted to a string.
          */
         return (NULL);
      }
   }

/*
 * itos - convert the integer num into a string using s as a buffer and
 *  making q a descriptor for the resulting string.
 */
static itos(num, q, s)
long num;
struct descrip *q;
char *s;
   {
   register char *p;
   long ival;

   p = s + MAXSTRING - 1;
   ival = num;

   *p = '\0';
   if (num >= 0L)
      do {
         *--p = ival % 10L + '0';
         ival /= 10L;
         } while (ival != 0L);
   else {
      do {
         *--p = '0' - (ival % 10L);
         ival /= 10L;
         } while (ival != 0L);
      *--p = '-';
      }
   
   STRLEN(*q) = s + MAXSTRING - 1 - p;
   STRLOC(*q) = p;
   return (1);
   }

/*
 * rtos - convert the real number n into a string using s as a buffer and
 *  making q a descriptor for the resulting string.
 */
rtos(n, q, s)
double n;
struct descrip *q;
char *s;
   {
   /*
    * gcvt does all the work.
    */
   gcvt(n, 8, s);
   STRLEN(*q) = strlen(s);
   STRLOC(*q) = s;
   return (1);
   }

/*
 * cstos - convert the cset bit array pointed at by cs into a string using
 *  s as a buffer and making q a descriptor for the resulting string.
 */
static cstos(cs, q, s)
int *cs;
struct descrip *q;
char *s;
   {
   register char *p;
   register int i;

   p = s;
   for (i = 0; i < CSETSIZE*INTSIZE; i++) {
      if (tstb(i, cs))
         *p++ = (char)i;
      }
   *p = '\0';

   STRLEN(*q) = p - s;
   STRLOC(*q) = s;
   return (1);
   }
