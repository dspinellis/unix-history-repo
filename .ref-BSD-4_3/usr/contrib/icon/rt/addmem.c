#include "../h/rt.h"
#ifdef SETS
/*
 * addmem - add a new set element block in the correct spot in
 *  the bucket chain.
 */

addmem(ps,pe,pl)
struct descrip *pl;
struct b_set *ps;
struct b_selem *pe;
   {
   ps->setsize++;
   if (!NULLDESC(*pl) ) {
      BLKLOC(pe->sblink) = BLKLOC(*pl);
      pe->sblink.type = D_SELEM;
      }
   BLKLOC(*pl) = (union block *) pe;
   pl->type = D_SELEM;
   }
#else SETS
char junk;        /* prevent null object file */
#endif SETS
