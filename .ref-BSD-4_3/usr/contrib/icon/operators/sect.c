#include "../h/rt.h"

/*
 * x[i:j] - form a substring or list section of x.
 */

sect(nargs, arg1v, arg3, arg2, arg1, arg0)
int nargs;
struct descrip arg1v, arg3, arg2, arg1, arg0;
   {
   register int i, j;
   int t, typ1;
   char sbuf[MAXSTRING];
   long l1, l2;
   extern char *alcstr();

   SetBound;
   hneed(sizeof(struct b_tvsubs));              /* check heap */

   if (cvint(&arg2, &l1) == NULL)
      runerr(101, &arg2);
   if (cvint(&arg3, &l2) == NULL)
      runerr(101, &arg3);

   arg1v = arg1;
   DeRef(arg1)

   if (!QUAL(arg1) && TYPE(arg1) == T_LIST) {
      i = cvpos(l1, BLKLOC(arg1)->list.cursize);
      j = cvpos(l2, BLKLOC(arg1)->list.cursize);
      if (i > j) {
         t = i;
         i = j;
         j = t;
         }
      cplist(&arg1, &arg0, i, j);
      ClearBound;
      return;
      }

   if ((typ1 = cvstr(&arg1, sbuf)) == NULL)
      runerr(110, &arg1);

   i = cvpos(l1, STRLEN(arg1));
   j = cvpos(l2, STRLEN(arg1));
   if (i > j) {				/* convert section to substring */
      t = i;
      i = j;
      j = t - j;
      }
   else
      j = j - i;

   if (typ1 == 1) {			/* if string was created, */
      sneed(j);				/*   just return a string */
      STRLEN(arg0) = j;
      STRLOC(arg0) = alcstr(STRLOC(arg1)+i-1, j);
      }
   else					/* else make a substring tv */
      mksubs(&arg1v, &arg1, i, j, &arg0);
   ClearBound;
   }

Opblockx(sect,4,":",3)
