#include "../h/rt.h"
#ifdef SETS

/*
 * locate - returns 1 if obj is in the hash chain which
 *  starts at ep in some set, returns 0 if not there
 *  (used only in diff.c and inter.c).
 */

locate(ep, obj)
struct b_selem *ep, *obj;
   {
   while (ep != NULL) {
      if (ep->hnum > obj->hnum)
          return 0;
      else if ((ep->hnum == obj->hnum) &&
                 (equiv(&ep->setmem, &obj->setmem)))
          return 1;
      ep = (struct b_selem *) BLKLOC(ep->sblink);
      }
   return 0;
   }
#else SETS
char junk;	/* prevent null object file */
#endif SETS
