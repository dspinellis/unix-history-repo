#include "../h/rt.h"

/*
 * x || y - concatenate strings x and y.
 */

cat(nargs, arg2, arg1, arg0)
int nargs;
struct descrip arg2, arg1, arg0;
   {
   DclSave
   char sbuf1[MAXSTRING], sbuf2[MAXSTRING];
   extern char *alcstr();

   SetBound;
   if (cvstr(&arg1, sbuf1) == NULL)
      runerr(103, &arg1);
   if (cvstr(&arg2, sbuf2) == NULL)
      runerr(103, &arg2);

   sneed(STRLEN(arg1)+STRLEN(arg2));
   if (STRLOC(arg1) + STRLEN(arg1) == sfree)
      STRLOC(arg0) = STRLOC(arg1);
   else
      STRLOC(arg0) = alcstr(STRLOC(arg1),STRLEN(arg1));
   alcstr(STRLOC(arg2),STRLEN(arg2));

   STRLEN(arg0) = STRLEN(arg1) + STRLEN(arg2);
   ClearBound;
   }
struct b_iproc Bcat = {
   T_PROC,
   sizeof(struct b_proc),
   EntryPoint(cat),
   2,
   -1,
   0,
   0,
   {2, "||"}
   };
