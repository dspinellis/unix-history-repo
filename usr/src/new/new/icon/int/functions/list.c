#include "../h/rt.h"

/*
 * list(n,x) - create a list of size n, with initial value x.
 */

Xlist(nargs, arg2, arg1, arg0)
int nargs;
struct descrip arg2, arg1, arg0;
   {
   register int i, size;
   register struct b_listb *bp;
   register struct b_list *hp;
   int nelem;
   extern struct b_list *alclist();
   extern struct b_listb *alclstb();

   defshort(&arg1, 0);
   deref(&arg2);

   nelem = size = arg1.value.integer;
   if (size < 0)
      runerr(205, &arg1);
   if (nelem < LISTBLKSIZE)
      nelem = LISTBLKSIZE;

   hneed(sizeof(struct b_list) + sizeof(struct b_listb) +
         nelem * sizeof(struct descrip));

   hp = alclist(size);
   bp = alclstb(nelem, 0, size);
   hp->listhead.type = hp->listtail.type = D_LISTB;
   BLKLOC(hp->listhead) = BLKLOC(hp->listtail) = bp;
   for (i = 0; i < size; i++)
      bp->lelem[i] = arg2;
   arg0.type = D_LIST;
   BLKLOC(arg0) = hp;
   }

struct b_iproc Blist = {
   T_PROC,
   sizeof(struct b_proc),
   EntryPoint(Xlist),
   2,
   -1,
   0,
   0,
   {4, "list"}
   };
