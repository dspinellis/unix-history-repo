/*
#	ISCOPE(3.icon)
#
#	Inspect Icon internals
#
#	Ralph E. Griswold and William H. Mitchell
#
#	Last modified 8/19/84
#
*/

#include "../h/rt.h"
#ifdef VAX

/*
 * Word2(x,y) - return second word of descriptor as integer
 */

XWord2(nargs, arg2, arg1, arg0)
int nargs;
struct descrip arg2, arg1, arg0;
   {
   long i;

   defint(&arg2, &i, 0);
   if (i == 0)
      DeRef(arg1)
   mkint(arg1.value.integr, &arg0);
   }

Procblock(Word2,2)

/*
 * Word1(x,y) - return first word of descriptor as integer.
 */

XWord1(nargs, arg2, arg1, arg0)
int nargs;
struct descrip arg2, arg1, arg0;
   {
   long i;

   defint(&arg2, &i, 0);
   if (i == 0)
      DeRef(arg1)
   mkint(arg1.type, &arg0);
   }

Procblock(Word1,2)

/*
 * Descr(x,y) - consstruct descriptor from integers i1 and i2.
 */

XDescr(nargs, arg2, arg1, arg0)
int nargs;
struct descrip arg2, arg1, arg0;
   {
   int t1, t2;
   long i1, i2;
   DeRef(arg1)
   DeRef(arg2)
   defint(&arg1, &i1, 0);
   defint(&arg2, &i2, 0);
   arg0.type = i1;
   arg0.value.integr = i2;

   }

Procblock(Descr,2)

/*
 * Indir(x) - return integer to where x points.
 */

XIndir(nargs, arg1, arg0)
int nargs;
struct descrip arg1, arg0;
   {
   int *i;
   int j;

   DeRef(arg1)
   i = (int *) arg1.value.integr;
   j = *i;
   mkint(j, &arg0);
   }

Procblock(Indir,1)

XPfp(nargs, arg0)
int nargs;
struct descrip arg0;
{
        register int r11, r10;

        asm("	movl	12(fp),r11");
        mkint(r11, &arg0);
}
Procblock(Pfp,0)

XEfp(nargs, arg0)
int nargs;
struct descrip arg0;
{
        register int r11, r10;

        asm("   movl	-4(ap),r11");
        mkint(r11, &arg0);
}
Procblock(Efp,0)

XGfp(nargs, arg0)
int nargs;
struct descrip arg0;
{
        register int r11, r10;

        asm("	movl	-8(ap),r11");
        mkint(r11, &arg0);
}
Procblock(Gfp,0)

/*
 * Symbol(x) - get address of Icon symbol.
 */

XSymbol(nargs, arg1, arg0)
int nargs;
struct descrip arg1, arg0;
   {
   extern globals, eglobals, gnames;
   char sbuf[MAXSTRING];
   DeRef(arg1)
   if (cvstr(&arg1, sbuf) == NULL)
      runerr(103, &arg1);
   qtos(&arg1, sbuf);
   ((arg0).type) = D_INTEGER;
   if (strcmp(sbuf, "globals") == 0)
      INTVAL(arg0) = (int) &globals;
   else if (strcmp(sbuf, "eglobals") == 0)
      INTVAL(arg0) = (int) &eglobals;
   else if (strcmp(sbuf, "gnames") == 0)
      INTVAL(arg0) = (int) &gnames;
   else if (strcmp(sbuf, "strings") == 0)
      INTVAL(arg0) = (int) strings;
   else if (strcmp(sbuf, "sfree") == 0)
      INTVAL(arg0) = (int) sfree;
   else if (strcmp(sbuf, "hpbase") == 0)
      INTVAL(arg0) = (int) hpbase;
   else if (strcmp(sbuf, "hpfree") == 0)
      INTVAL(arg0) = (int) hpfree;
   else if (strcmp(sbuf, "stacks") == 0)
      INTVAL(arg0) = (int) stacks;
   else if (strcmp(sbuf, "esfree") == 0)
      INTVAL(arg0) = (int) esfree;
   else fail();
   }

Procblock(Symbol,1)
#endif VAX
