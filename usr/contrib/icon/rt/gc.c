#include "../h/rt.h"
#include "../h/gc.h"
#ifdef VAX
#define MAIN
#endif VAX
#ifdef PORT
#define MAIN
#endif PORT
#ifdef MAIN
/*
 * hneed - insure that at least bytes of space are left in the heap.
 *  The amount of space needed is transmitted to the collector via
 *  the global variable heapneed.
 */

hneed(bytes)
unsigned bytes;
   {
   heapneed = bytes;
   if (bytes > maxheap - hpfree)
      gcollect(0);
   }

/*
 * sneed - insure that at least chars of space are left in the string
 *  space.  The amount of space needed is transmitted to the collector
 *  via the global variable strneed.
 */

sneed(chars)
unsigned chars;
   {
   strneed = chars;
   if (chars > estrings - sfree)
      gcollect(0);
   }

/*
 * esneed - insure that there is a free co-expression stack.  esfree
 *  points to the linked list of free stacks.
 */

esneed()
   {
   if (esfree == NULL)
      gcollect(1);
   }

/*
 * escollect - collect the expression stack space.  This is done after
 *  the marking phase of garbage collection and the stacks that are
 *  reachable have pointers to data blocks, rather than T_ESTACK,
 *  in their type field.
 */

escollect()
   {
   register int *ep;
   register struct b_estack *sp;

   /*
    * Reset the type for &main.
    */
   BLKLOC(k_main)->estack.type = T_ESTACK;

   /*
    * Reset the free list pointer.
    */
   esfree = NULL;

   /*
    * The co-expression stacks start at stacks and lie contiguously.
    *  ep is pointed at the low word of each stack and sp is pointed
    *  at the b_estack block contained in the space for the stack.
    *  (Note that the last word of the b_estack block is the last word
    *  of the space for the co-expression stack.
    */
   for (ep = stacks; ep < estacks; ep += stksize) {
      sp = (struct b_estack *) (ep + (stksize - sizeof(struct b_estack)/WORDSIZE));
      if (blktype(sp) == T_ESTACK) {
         /*
          * This co-expression was not marked, so it can be collected.
          *  The stacks are linked through the first word of the stack
          *  space with esfree pointing to the last-collected stack.
          */
         *ep = (int) esfree;
         esfree = ep;
         }
      else
         /*
          * The co-expression was marked, so just reset the type field.
          */
         blktype(sp) = T_ESTACK;
      }
   }

/*
 * collect - do a garbage collection.  esneed indicates if a co-expression
 *  stack is needed.
 */

collect(esneed)
int esneed;
   {
   register int extra;
   register char *newend;
   register struct descrip *dp;
   char *sptr;
   extern char *brk();

   /*
    * Reset the string qualifier free list pointer.
    */
   sqfree = sqlist;

   /*
    * Mark the stacks for &main and the current co-expression.
    */
   mark(&k_main);
   mark(&current);
   /*
    * Mark &subject and the cached s2 and s3 strings for map().
    */
   mark(&k_subject);
   mark(&maps2);
   mark(&maps3);
   /*
    * Mark the tended descriptors and the global and static variables.
    */
   for (dp = tended; dp < etended; dp++)
      mark(dp);
   for (dp = globals; dp < eglobals; dp++)
      mark(dp);
   for (dp = statics; dp < estatics; dp++)
      mark(dp);

   /*
    * Collect available co-expression stacks.
    */
   escollect();
   if (esneed && esfree == NULL) {
      /*
       * A co-expression stack is needed, but none are available.  The
       *  new stack at the end of the stack space and is made available
       *  by pointing esfree at it.  *estacks is zeroed to terminate the
       *  (now one element) co-expression free list.
       */
      esfree = estacks;
      *estacks = 0;
      /*
       * Move back the end of the expression space by the size of a
       *  stack and indicate stksize words of memory are needed.
       */
      estacks += stksize;
      extra = stksize*WORDSIZE;
      newend = (char *) sqlist + extra;
      /*
       * This next calculation determines if there is space for the new
       *  stack, but it's not clear what all's going on here.
       */
      if (newend < (char *)sqlist || newend > (char *)0x7fffffff ||
          (newend > (char *)esqlist && ((int) brk(newend) == -1)))
         runerr(305, NULL);
      }
   else
      /*
       * Another co-expression stack is not needed.
       */
      extra = 0;

   /*
    * Collect the string space, indicating that it must be moved back
    *  extra bytes.
    */
   scollect(extra);
   /*
    * sptr is post-gc value for strings.  Move back pointers for estrings
    *  and sqlist according to value of extra.
    */
   sptr = strings + extra;
   estrings += extra;
   sqlist =  sqlist + extra;
   if (sqlist > esqlist)
      esqlist = sqlist;

   /*
    * Calculate a value for extra space.  The value is (the larger of
    *  (twice the string space needed) or (the number of words currently
    *  in the string space)) plus the unallocated string space.
    */
   extra = (MAX(2*strneed, (estrings-(char *)estacks)/4) -
            (estrings - extra - sfree) + (GRANSIZE-1)) & ~(GRANSIZE-1);

   while (extra > 0) {
      /*
       * Try to get extra more bytes of storage.  If it can't be gotten,
       *  decrease the value by GRANSIZE and try again.  If it's gotten,
       *  move back estrings and sqlist.
       */
      newend = (char *)sqlist + extra;
      if (newend >= (char *)sqlist &&
          (newend <= (char *)esqlist || ((int) brk(newend) != -1))) {
         estrings += extra;
         sqlist = (struct descrip **) newend;
         break;
         }
      extra -= GRANSIZE;
      }

   /*
    * Adjust the pointers in the heap.  Note that hpbase is the old base
    *  of the heap and estrings will be the post-gc base of the heap.
    */
   adjust(hpbase,estrings);
   /*
    * Compact the heap.
    */
   compact(hpbase);
   /*
    * Calculate a value for extra space.  The value is (the larger of
    *  (twice the heap space needed) or (the number of words currently
    *  in the heap space)) plus the unallocated heap space.
    */
   extra = (MAX(2*heapneed, (maxheap-hpbase)/4) +
            hpfree - maxheap + (GRANSIZE-1)) & ~(GRANSIZE-1);
   while (extra > 0) {
      /*
       * Try to get extra more bytes of storage.  If it can't be gotten,
       *  decrease the value by GRANSIZE and try again.  If it's gotten,
       *  move back sqlist.
       */
      newend = (char *)sqlist + extra;
      if (newend >= (char *)sqlist &&
          (newend <= (char *)esqlist || ((int) brk(newend) != -1))) {
         sqlist = (struct descrip **) newend;
         break;
         }
      extra -= GRANSIZE;
      }
   if (sqlist > esqlist)
      esqlist = sqlist;

   if (estrings != hpbase) {
      /*
       * estrings is not equal to hpbase and this indicates that the
       *  co-expression and/or string space was expanded and thus
       *  the heap must be moved.  There is an assumption here that the
       *  heap always moves up in memory, i.e., the co-expression and
       *  string spaces never shrink.  With this assumption in hand,
       *  the heap must be moved before the string space lest the string
       *  space overwrite heap data.  The assumption is valid, but beware
       *  if shrinking regions are ever implemented.
       */
      mvc((unsigned)(hpfree - hpbase), hpbase, estrings);
      hpfree += estrings - hpbase;
      hpbase = estrings;
      }
   if (sptr != strings) {
      /*
       * sptr is not equal to strings and this indicates that the
       *  co-expression space was expanded and thus the string space
       *  must be moved up in memory.
       */
      mvc((unsigned)(sfree - strings), strings, sptr);
      sfree += sptr - strings;
      strings = sptr;
      }
      
   /*
    * Expand the heap.
    */
   maxheap = (char *)sqlist;
   return;
   }
/*
 * mark - mark each accessible block in the heap and build back-list of
 *  descriptors pointing to that block. (Phase I of garbage collection.)
 */

mark(cdesc)
struct descrip *cdesc;
   {
   register struct descrip *ndesc;
   register char *endblock, *block;
   static int type;
   static int fdesc;

   if (QUAL(*cdesc))
      /*
       * The descriptor is for a string, so pass the buck to marksq.
       */
      marksq(cdesc);
   else if (isptr(cdesc)) {
      /*
       * The descriptor is a pointer to a block or a variable.  Point
       *  block at the block referenced by the descriptor.
       */
      block = (char *) BLKLOC(*cdesc);
      if (VAR(*cdesc) && !TVAR(*cdesc))
         /*
          * The descriptor is a variable; point block at the start of the
          *  block containing the descriptor that cdesc points to.  For
          *  example, descriptors of this sort are created by subscripting
          *  lists.
          */
         block = (char *) ((int *) block - OFFSET(*cdesc));

      if (block >= hpbase && block < hpfree) {
         /*
          * The block is the heap (blocks outside the heap are ignored);
          *  get the type of the block.
          */
         type = blktype(block);
         if (type <= MAXTYPE)
            /*
             * type is a valid type, indicating that this block hasn't
             *  been marked.  Point endblock at the byte past the end
             *  of the block.
             */
            endblock = block + getsize(block);
         /*
          * Add cdesc to the back-chain for the block and point the
          *  block (via the type field) at cdesc.
          */
         BLKLOC(*cdesc) = (union block *) type;
         blktype(block) = (int) cdesc;
         if ((type <=  MAXTYPE) && ((fdesc = firstd[(int)type]) > 0))
            /*
             * The block has not been marked, and it does contain descriptors.
             *  Mark each descriptor.
             */
            for (ndesc = (struct descrip *) (block + fdesc);
               (char *) ndesc < endblock; ndesc++)
                mark(ndesc);
         }
      if (!VAR(*cdesc) && TYPE(*cdesc) == T_ESTACK &&
         blktype(block) <= MAXTYPE) {
         /*
          * cdesc points to a co-expression block that hasn't been marked.
          *  Point the block at cdesc.  Sweep the co-expression's stack
          *  and mark the blocks for the activating co-expression and
          *  the co-expression's refresh block.
          */
         blktype(block) = (int) cdesc;
         sweep(((struct b_estack *)block)->boundary);
         mark(&((struct b_estack *)block)->activator);
         mark(&((struct b_estack *)block)->freshblk);
         }
      }
   }

/*
 * adjust - adjust pointers into heap, beginning with block oblk and
 *  basing the "new" heap at nblk.  (Phase II of garbage collection.)
 */

adjust(oblk,nblk)
char *oblk, *nblk;
   {
   register struct descrip *nxtptr, *tptr;

   /*
    * Loop through to end of allocated heap space moving oblk to each
    *  block in turn, using the size of a block to find the next block.
    */
   while (oblk < hpfree) {
      if ((int) (nxtptr = (struct descrip *) blktype(oblk)) > MAXTYPE) {
         /*
          * The type field of oblk is a back-pointer.  Work along the chain
          *  of back pointers, changing each block location from oblk
          *  to nblk.
          */
         while ((unsigned)nxtptr > MAXTYPE) {
            tptr = nxtptr;
            nxtptr = (struct descrip *) BLKLOC(*nxtptr);
            if (VAR(*tptr) && !TVAR(*tptr))
               BLKLOC(*tptr) = (union block *) ((int *) nblk + OFFSET(*tptr));
            else
               BLKLOC(*tptr) = (union block *) nblk;
            }
         blktype(oblk) = (unsigned)nxtptr | MARK;
         nblk += getsize(oblk);
         }
      oblk += getsize(oblk);
      }
   }

/*
 * compact - compact good blocks in heap. (Phase III of garbage collection.)
 */

compact(oblk)
char *oblk;
   {
   register char *nblk;
   register int size;

   /*
    * Start at oblk, which happens to be hpbase.
    */
   nblk = oblk;
   /*
    * Loop through to end of allocated heap space moving oblk to each
    *  block in turn, using the size of a block to find the next block.
    *  If a block has been marked, it is copied to the location pointed
    *  at by nblk and nblk is pointed past the end of the block, which
    *  is the location to place the next good block at.  Good blocks
    *  are un-marked.
    */
   while (oblk < hpfree) {
      size = getsize(oblk);
      if (blktype(oblk) & MARK) {
         blktype(oblk) &= ~MARK;
         if (oblk != nblk)
            mvc((unsigned)size,oblk,nblk);
         nblk += size;
         }
      oblk += size;
      }
   /*
    * nblk is the location of the next free block, so now that compaction
    *  is complete, point hpfree at that location.
    */
   hpfree = nblk;
   }

/*
 * marksq - mark a string qualifier.  Strings outside the string space
 *  are ignored.
 */

marksq(d)
struct descrip *d;
   {
   extern char *brk();

   if (STRLOC(*d) >= strings && STRLOC(*d) < estrings) {
      /*
       * The string is in the string space, add it to the string qualifier
       *  list.  But before adding it, expand the string qualifier list
       *  if necessary.
       */
      if (sqfree >= esqlist) {
         esqlist += SQLINC;
         if ((int) brk(esqlist) == -1)
            runerr(303, NULL);
         }
      *sqfree++ = d;
      }
   }

/*
 * scollect - collect the string space.  sqlist is a list of pointers to
 *  descriptors for all the reachable strings in the string space.  For
 *  ease of description, it is referred to as if it were composed of
 *  descriptors rather than pointers to them.
 */

scollect(extra)
int extra;
   {
   register char *s, *d;
   register struct descrip **p;
   char *e;
   extern int sqcmp();

   if (sqfree <= sqlist) {
      /*
       * There are no accessible strings, thus there are none to collect
       *  and the whole string space is free.
       */
      sfree = strings;
      return;
      }
   /*
    * Sort the sqlist in ascending order of string locations.
    */
   qsort(sqlist, sqfree-sqlist, sizeof(struct descrip *), sqcmp);
   /*
    * The string qualifiers are now ordered by starting location.
    *  The algorithm used is described in detail in one of the references
    *  cited in the "tour", but briefly...
    *
    * The string region can be thought of as being made up of clumps,
    *  where a clump is a contiguous area of strings that are referenced.
    *  For example, imagine sqlist looks like:
    *
    *   [2,400]
    *   [3,400]
    *   [10,400]
    *   [12,415]
    *   [4,420]
    *   [3,430]
    *   [1,430]
    *
    * There are three clumps:  The first starts at location 400 and extends
    *  to 409.  The second starts at 415 and extends to 426.  The third
    *  starts at 430 and extends to 432.  Note that there are gaps, i.e.
    *  garbage, at 410-414 and 427-429.
    *
    * After collection, sqlist will look like:
    *
    *        [2,400]
    *        [3,400]
    *        [10,400]
    *        [12,410]
    *        [4,415]
    *        [3,422]
    *        [1,422]
    *
    * Note how the gaps have been closed by moving the strings downward
    *  in memory.
    *
    * The method used is to look at each qualifier in sqlist in turn
    *  and determine which ones lie in clumps and the extent of each
    *  clump.  The qualifiers referencing strings in each clump are
    *  relocated and then the clump is moved down (compacted).
    *
    * d points to the next free location to compact into.  s is the
    *  start of the current clump and e is the end.
    */
   d = strings;
   s = e = STRLOC(**sqlist);
   /*
    * Loop through qualifiers for accessible strings.
    */
   for (p = sqlist; p < sqfree; p++) {
      if (STRLOC(**p) > e) {
         /*
          * p is a qualifier for a string in the next clump; the last
          *  clump is moved and s and e are set for the next clump.
          */
         while (s < e)
            *d++ = *s++;
         s = e = STRLOC(**p);
         }
      if (STRLOC(**p)+STRLEN(**p) > e)
         /*
          * p is a qualifier for a string in this clump, extend the clump.
          */
         e = STRLOC(**p) + STRLEN(**p);
      /*
       * Relocate the string qualifier.
       */
      STRLOC(**p) += d - s + extra;
      }
   /*
    * Move the last clump.
    */
   while (s < e)
      *d++ = *s++;
   sfree = d;
   }

/*
 * sqcmp - compare the location fields of two string qualifiers for qsort.
 */

sqcmp(q1,q2)
struct descrip **q1, **q2;
   {
   return (STRLOC(**q1) - STRLOC(**q2));
   }

/*
 * mvc - move n bytes from src to dst.
 */

mvc(n, s, d)
unsigned n;
register char *s, *d;
   {
   register int words;
   register int *srcw, *dstw;
   int bytes;

   words = n / sizeof(int);
   bytes = n % sizeof(int);

   srcw = (int *)s;
   dstw = (int *)d;

   if (d < s) {
      /*
       * The move is from higher memory to lower memory.  (It so happens
       *  that leftover bytes are not moved.)
       */
      while (--words >= 0)
         *(dstw)++ = *(srcw)++;
      while (--bytes >= 0)
         *d++ = *s++;
      }
   else if (d > s) {
      /*
       * The move is from lower memory to higher memory.
       */
      s += n;
      d += n;
      while (--bytes >= 0)
         *--d = *--s;
      srcw = (int *)s;
      dstw = (int *)d;
      while (--words >= 0)
         *--dstw = *--srcw;
      }
   }

#endif MAIN
#ifdef PDP11
/*
 * hneed(bytes) - insure at least 'bytes' space left in heap.
 */

hneed(bytes)
unsigned bytes;
   {
   heapneed = bytes;
   if (bytes > maxheap - hpfree)
      gcollect(0);
   }

/*
 * sneed(chars) - insure at least 'chars' bytes left in string space.
 */

sneed(chars)
unsigned chars;
   {
   strneed = chars;
   if (chars > estrings - sfree)
      gcollect(0);
   }

/*
 * esneed() - insure stack space free list is not empty.
 */

esneed()
   {
   if (esfree == NULL)
      gcollect(1);
   }

/*
 * escollect() - collect the expression stack space after marking.
 */

escollect()
   {
   register int *ep;
   register struct b_estack *sp;
   register struct descrip *nxtptr, *tptr;

   BLKLOC(k_main)->estack.type = T_ESTACK;   /* must reset */

   esfree = NULL;
   for (ep = stacks; ep < estacks; ep += stksize) {
      sp = ep + (stksize - sizeof(struct b_estack)/2);
      if (blktype(sp) == T_ESTACK) {      /* add to free list */
         *ep = esfree;
         esfree = ep;
         }
      else                               /* adjust type field */
         blktype(sp) = T_ESTACK;
      }
   }

/*
 * collect - call the heap garbage collector.
 */

collect(esneed)
int esneed;
   {
   register int extra;
   register char *newend;
   register struct descrip *dp;
   char *sptr;
   extern char *brk();

   sqfree = sqlist;                /* initialize string qualifier list */

   mark(&k_main);               /* mark main stack */
   mark(&current);              /* mark current stack */
   mark(&k_subject);            /* mark tended descriptors */
   mark(&maps2);
   mark(&maps3);
   for (dp = tended; dp < etended; dp++)
      mark(dp);
   for (dp = globals; dp < eglobals; dp++)
      mark(dp);
   for (dp = statics; dp < estatics; dp++)
      mark(dp);

   escollect();                        /* collect available expression stacks */
   if (esneed && esfree == NULL) {
      esfree = estacks;                /* need to make room for another stack */
      *estacks = 0;
      estacks += stksize;
      extra = stksize*sizeof(int);        /* string and heap ptrs are chars */
      newend = sqlist + extra;
      if (newend < (char *)sqlist || newend > (char *)0177700 ||
          (newend > (char *)esqlist && brk(newend) == -1))
         runerr(305, NULL);
      }
   else
      extra = 0;

   scollect(extra);                /* collect string space */
   sptr = strings + extra;      /* remember new location of string space */
   estrings += extra;
   (char *)sqlist += extra;
   if (sqlist > esqlist)
      esqlist = sqlist;

   extra = (MAX(2*strneed, (estrings-estacks)/4) -
            (estrings - extra - sfree) + 63) & ~077;
   while (extra > 0) {                /* need breathing room? */
      newend = (char *)sqlist + extra;
      if (newend >= (char *)sqlist && newend <= (char *)0177700 &&
          (newend <= (char *)esqlist ||        brk(newend) != -1)) {
         estrings += extra;
         sqlist = newend;
         break;
         }
      extra -= 64;
      }
   adjust(hpbase,estrings);        /* adjust pointers into heap */
   compact(hpbase);                /* compact heap */
   extra = (MAX(2*heapneed, (maxheap-hpbase)/4) +
            hpfree - maxheap + 63) & ~077;
   while (extra > 0) {                /* need breathing room? */
      newend = (char *)sqlist + extra;
      if (newend >= (char *)sqlist && newend <= (char *)0177700 &&
          (newend <= (char *)esqlist ||        brk(newend) != -1)) {
         sqlist = newend;
         break;
         }
      extra -= 64;
      }
   if (sqlist > esqlist)
      esqlist = sqlist;
   if (estrings != hpbase) {                /* move heap */
      mvc((unsigned)(hpfree - hpbase), hpbase, estrings);
      hpfree += estrings - hpbase;
      hpbase = estrings;
      }
   if (sptr != strings) {                /* move string space */
      mvc((unsigned)(sfree - strings), strings, sptr);
      sfree += sptr - strings;
      strings = sptr;
      }
   maxheap = (char *)sqlist;                /* expand heap */

   return;
   }

/*
 * mark - mark each accessible block in the heap and build back-list of
 *  descriptors pointing to that block. (Phase I of garbage collection)
 */

mark(cdesc)
struct descrip *cdesc;                /* current descriptor */
   {
   register struct descrip *ndesc;
   register char *endblock, *block;
   static char *type;
   static int fdesc;

   if (QUAL(*cdesc))                 /* if descriptor is a string qualifier, */
      marksq(cdesc);                /*   mark it for scollect */
   else if (isptr(cdesc)) {        /* ok, descriptor is a pointer */
      block = BLKLOC(*cdesc);        /* get pointer to top of block */
      if (VAR(*cdesc) && !TVAR(*cdesc))  /* if variable, need offset */
         block = (int *)block - OFFSET(*cdesc);

      if (block >= hpbase && block < hpfree) {        /* insure it points to heap */
         type = blktype(block);         /* save type and end of block */
         if (type <= MAXTYPE)
            endblock = block + getsize(block);
         BLKLOC(*cdesc) = type;         /* add descriptor to back chain */
         blktype(block) = cdesc;
                                        /* sweep descriptors in block */
         if ((type <=  MAXTYPE) && ((fdesc = firstd[(int)type]) > 0))
            for (ndesc = block + fdesc; ndesc < endblock; ndesc++)
                mark(ndesc);
         }
      if (!VAR(*cdesc) && TYPE(*cdesc) == T_ESTACK &&
         (char *)blktype(block) <= MAXTYPE) {
         blktype(block) = cdesc;             /* note block as visited */
         sweep(((struct b_estack *)block)->boundary);
         mark(&((struct b_estack *)block)->activator);
         mark(&((struct b_estack *)block)->freshblk);
         }
      }
   }

/*
 * adjust - adjust pointers into heap, beginning with heapblock 'oblk'.
 *   (Phase II of garbage collection)
 */

adjust(oblk,nblk)
char *oblk, *nblk;
   {
   register struct descrip *nxtptr, *tptr;

   while (oblk < hpfree) {              /* linear sweep through heap */
      if ((nxtptr = blktype(oblk)) > MAXTYPE) {
         while ((unsigned)nxtptr > MAXTYPE) {
            tptr = nxtptr;
            nxtptr = BLKLOC(*nxtptr);
            if (VAR(*tptr) && !TVAR(*tptr))
               BLKLOC(*tptr) = (int *)nblk + OFFSET(*tptr);
            else
               BLKLOC(*tptr) = nblk;
            }
         blktype(oblk) = (unsigned)nxtptr | MARK;
         nblk += getsize(oblk);
         }
      oblk += getsize(oblk);
      }
   }

/*
 * compact - compact good blocks in heap, beginning with block 'oblk'.
 *   (Phase III of garbage collection)
 */

compact(oblk)
char *oblk;
   {
   register char *nblk;
   register int size;

   nblk = oblk;                  /* linear sweep through heap */
   while (oblk < hpfree) {
      size = getsize(oblk);
      if (blktype(oblk) & MARK) {    /* move good block */
         blktype(oblk) &= ~MARK;     /* turn off mark */
         if (oblk != nblk)
            mvc((unsigned)size,oblk,nblk);
         nblk += size;
         }
      oblk += size;
      }
   hpfree = nblk;                /* reset free space pointer */
   }

/*
 * marksq - mark a string qualifier.  If it points into the
 * string space, put a pointer to it in the string qualifier
 * list.
 */

marksq(d)
struct descrip *d;
   {
   extern char *brk();

   if (STRLOC(*d) >= strings && STRLOC(*d) < estrings) {
      if (sqfree >= esqlist) {
         esqlist += SQLINC;
         if ((int)brk(esqlist) == -1)
            runerr(303, NULL);
         }
      *sqfree++ = d;
      }
   }

/*
 * scollect - collect the string space.
 * A list of string qualifiers points to all valid strings.
 */

scollect(extra)
int extra;
   {
   register char *s, *d;
   register struct descrip **p;
   char *e;
   extern int sqcmp();

   if (sqfree <= sqlist) {
      sfree = strings;
      return;
      }
   qsort(sqlist, sqfree-sqlist, sizeof(struct descrip *), sqcmp);
   d = strings;
   s = e = STRLOC(**sqlist);
   for (p = sqlist; p < sqfree; p++) {
      if (STRLOC(**p) > e) {                /* outside last clump */
         while (s < e)                        /* move the clump */
            *d++ = *s++;
         s = e = STRLOC(**p);                /* start a new clump */
         }
      if (STRLOC(**p)+STRLEN(**p) > e)        /* extend the clump */
         e = STRLOC(**p) + STRLEN(**p);
      STRLOC(**p) += d - s + extra;        /* relocate the string qualifier */
      }
   while (s < e)                        /* move the last clump */
      *d++ = *s++;
   sfree = d;
   }

/*
 * sqcmp - compare the location fields of two string qualifiers for qsort.
 */

sqcmp(q1,q2)
struct descrip **q1, **q2;
   {
   return (STRLOC(**q1) - STRLOC(**q2));
   }

/*
 * mvc - move n bytes from src to dst.
 * src and dst must be at word boundaries.
 */

mvc(n, s, d)
unsigned n;
register char *s, *d;
   {
   register int words;
   int bytes;

   words = n / sizeof(int);
   bytes = n % sizeof(int);

   if (d < s) {                  /* move back */
      while (--words >= 0)
         *((int *)d)++ = *((int *)s)++;
      while (--bytes >= 0)
         *d++ = *s++;
      }
   else if (d > s) {             /* move forward */
      s += n;
      d += n;
      while (--bytes >= 0)
         *--d = *--s;
      while (--words >= 0)
         *--(int *)d = *--(int *)s;
      }
   }
#endif PDP11

