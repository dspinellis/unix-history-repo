#include "../h/rt.h"

/*
 * map(s1,s2,s3) - map s1, using s2 and s3.
 */

Xmap(nargs, arg3, arg2, arg1, arg0)
int nargs;
struct descrip arg3, arg2, arg1, arg0;
   {
   register int i;
   register char *s1, *s2, *s3;
   char sbuf1[MAXSTRING], sbuf2[MAXSTRING], sbuf3[MAXSTRING];
   static char maptab[MAXSTRING];
   extern char *alcstr();

   if (cvstr(&arg1, sbuf1) == NULL)
      runerr(103, &arg1);
   defany(&arg2, &ucase);
   defany(&arg3, &lcase);

   if (maps2.type != arg2.type || maps3.type != arg3.type ||
       BLKLOC(maps2) != BLKLOC(arg2) || BLKLOC(maps3) != BLKLOC(arg3)) {
      maps2 = arg2;
      maps3 = arg3;
      if (cvstr(&arg2, sbuf2) == NULL)
         runerr(103, &arg2);
      if (cvstr(&arg3, sbuf3) == NULL)
         runerr(103, &arg3);
      if (STRLEN(arg2) != STRLEN(arg3))
         runerr(208, NULL);
      s2 = STRLOC(arg2);
      s3 = STRLOC(arg3);
      for (i = MAXSTRING - 1; i >= 0; i--)
         maptab[i] = i;
      for (i = 0; i < STRLEN(arg2); i++)
         maptab[s2[i]&0377] = s3[i];
      }

   if (STRLEN(arg1) == 0) {
      arg0.type = D_NULL;
      INTVAL(arg0) = 1;
      return;
      }

   i = STRLEN(arg1);
   sneed(i);
   s1 = STRLOC(arg1);

   STRLEN(arg0) = i;
   STRLOC(arg0) = s2 = alcstr(NULL, i);
   while (i-- > 0)
      *s2++ = maptab[(*s1++)&0377];
   }

struct b_iproc Bmap = {
   T_PROC,
   sizeof(struct b_proc),
   EntryPoint(Xmap),
   3,
   -1,
   0,
   0,
   {3, "map"}
   };
