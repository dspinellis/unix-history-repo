#include "../h/rt.h"

/*
 * cvstr(d,q,s) - convert d to string in q,
 * using s as buffer if necessary.
 */

cvstr(d, sbuf)
register struct descrip *d;
char *sbuf;
   {
   if (!QUAL(*d) && VAR(*d))
      deref(d);

   if (QUAL(*d)) {                    /* if already is string */
      if (NULLDESC(*d))
         return (NULL);
      return (2);
      }

   switch (TYPE(*d)) {
      case T_INTEGER:
	 return (itos((long)INTVAL(*d), d, sbuf));

#ifndef BIT32
      case T_LONGINT:
	 return (itos(BLKLOC(*d)->longint.intval, d, sbuf));

#endif
      case T_REAL:
	 return (rtos(BLKLOC(*d)->real.realval, d, sbuf));

      case T_CSET:
	 return (cstos(BLKLOC(*d)->cset.bits, d, sbuf));

      default:
	 return (NULL);
      }
   }

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

static rtos(n, q, s)
double n;
struct descrip *q;
char *s;
   {
   gcvt(n, 8, s);
   STRLEN(*q) = strlen(s);
   STRLOC(*q) = s;
   return (1);
   }

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
