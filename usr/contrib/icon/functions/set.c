#include "../h/rt.h"
#ifdef SETS

/*
 * set(list) - create a set with members in list.
 *  The members are linked into hash chains which are
 *  arranged in increasing order by hash number.
 */
Xset(nargs,arg1,arg0)
int nargs;
struct descrip arg1, arg0;
   {
   register int hn;
   register struct descrip *pd;
   register struct b_set *ps;
   union block *pb;
   struct b_selem *ne;
   struct descrip *pe;
   int res, i, j;
   extern struct descrip *memb();
   extern struct b_set *alcset();
   extern struct b_selem *alcselem();

   DeRef(arg1)
   if (QUAL(arg1) || TYPE(arg1) != T_LIST)
      runerr(108,&arg1);

   hneed(sizeof(struct b_set) + (BLKLOC(arg1)->list.cursize *
      sizeof(struct b_selem)));

   pb = BLKLOC(arg1);
   arg0.type = D_SET;
   ps = alcset();
   BLKLOC(arg0) = (union block *) ps;
   /*
    * Chain through each list block and for
    *  each element contained in the block
    *  insert the element into the set if not there.
    */
   for (arg1 = pb->list.listhead; arg1.type == D_LELEM;
      arg1 = BLKLOC(arg1)->lelem.listnext) {
         pb = BLKLOC(arg1);
         for (i = 0; i < pb->lelem.nused; i++) {
            j = pb->lelem.first + i;
            if (j >= pb->lelem.nelem)
               j -= pb->lelem.nelem;
            pd = &pb->lelem.lslots[j];
            pe = memb(ps, pd, hn = hash(pd), &res);
            if (res == 0) {
               ne = alcselem(pd,hn);
                addmem(ps,ne,pe);
                }
            }
      }
   }

Procblock(set,1)
#else SETS
char junk;			/* prevent null object file  */
#endif SETS
