#include "../h/gc.h"
#ifdef VAX
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
   for (ep = stacks; ep < estacks; ep += STACKSIZE) {
      sp = ep + (STACKSIZE - sizeof(struct b_estack)/ADDRSIZE);
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

   sqfree = sqlist;		/* initialize string qualifier list */

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

   escollect();			/* collect available expression stacks */
   if (esneed && esfree == NULL) {
      esfree = estacks;		/* need to make room for another stack */
      *estacks = 0;
      estacks += STACKSIZE;
      extra = STACKSIZE*sizeof(int *);	/* string and heap ptrs are chars */
      					/*+ note that the above is a
					   calc. of number of words.
					   Actually, it was sizeof(int), so
					   I don't guess that using int* will
					   help much. -- whm */
      newend = sqlist + extra;
      if (newend < (char *)sqlist || newend > (char *)0x7fffffff ||
	  (newend > (char *)esqlist && brk(newend) != 0))
	 runerr(305, NULL);
      }
   else
      extra = 0;

   scollect(extra);		/* collect string space */
   sptr = strings + extra;      /* remember new location of string space */
   estrings += extra;
   sqlist += extra;
   if (sqlist > esqlist)
      esqlist = sqlist;

   extra = (MAX(2*strneed, (estrings-(char *)estacks)/4) -
	    (estrings - extra - sfree) + 63) & ~077;
   while (extra > 0) {	        /* need breathing room? */
      newend = (char *)sqlist + extra;
      if (newend >= (char *)sqlist &&
	  (newend <= (char *)esqlist ||	brk(newend) == 0)) {
         estrings += extra;
         sqlist = newend;
	 break;
         }
      extra -= 64;
      }
   adjust(hpbase,estrings);	/* adjust pointers into heap */
   compact(hpbase);		/* compact heap */
   extra = (MAX(2*heapneed, (maxheap-hpbase)/4) +
	    hpfree - maxheap + 63) & ~077;
   while (extra > 0) {	        /* need breathing room? */
      newend = (char *)sqlist + extra;
      if (newend >= (char *)sqlist &&
	  (newend <= (char *)esqlist ||	brk(newend) == 0)) {
         sqlist = newend;
	 break;
         }
      extra -= 64;
      }
   if (sqlist > esqlist)
      esqlist = sqlist;
   if (estrings != hpbase) {	        /* move heap */
      mvc((unsigned)(hpfree - hpbase), hpbase, estrings);
      hpfree += estrings - hpbase;
      hpbase = estrings;
      }
   if (sptr != strings) {	        /* move string space */
      mvc((unsigned)(sfree - strings), strings, sptr);
      sfree += sptr - strings;
      strings = sptr;
      }
   maxheap = (char *)sqlist;		/* expand heap */

   return;
   }

/*
 * yields ap of a frame addressed by fp
 */

int *vax_apof(fp)
	register int *fp;
{	register int mask = fp[1]>>15 & 0x1ffe;
	fp += 5;
	while (mask >>= 1)
		fp += mask & 1;
	return fp;
}

/*
 * sweep - sweep the stack, marking all descriptors there.
 *
 * This needs some documentation due to its cleverness--whm
 */

sweep(b)
int *b;
   {
   register int *sp, *r5, *r4;
   int *r3, nargs;

   register int *ap;

   sp = b;
   r5 = sp;
   ap = vax_apof(r5);
   sp = r5 - 2;
   nargs = 0;
   while ((r5 != 0 || nargs)) {
      if (sp == r5 - FRAMELIMIT) {  /* at a procedure frame */
	 r4 = ap[-1];
	 r3 = ap[-2];
	 sp = ap + 2;
	 ap = r5[2];
	 r5 = r5[3];
	 nargs = sp[-1];
	 }
      else if (sp == r3 - 3) {	/* at a generator frame */
	 r5 = r3[0];
	 sp = r5 - FRAMELIMIT;
	 ap = vax_apof(r5);
	 }
      else if (sp == r4 - 2) {	/* at an expression frame */
	 r3 = r4[-1];
	 r4 = r4[0];
	 sp += 3;
	 }
      else {			/* must be a descriptor! */
  	 mark(sp);
	 sp += 2;
	 if (nargs)
	    nargs--;
	 }
      }
   }

/*
 * mark - mark each accessible block in the heap and build back-list of
 *  descriptors pointing to that block. (Phase I of garbage collection)
 */

mark(cdesc)
struct descrip *cdesc;		/* current descriptor */
   {
   register struct descrip *ndesc;
   register char *endblock, *block;
   static char *type;
   static int fdesc;

   if (QUAL(*cdesc)) 		/* if descriptor is a string qualifier, */
      marksq(cdesc);		/*   mark it for scollect */
   else if (isptr(cdesc)) {	/* ok, descriptor is a pointer */
      block = BLKLOC(*cdesc);	/* get pointer to top of block */
      if (VAR(*cdesc) && !TVAR(*cdesc))  /* if variable, need offset */
	 block = (int *)block - OFFSET(*cdesc);

      if (block >= hpbase && block < hpfree) {	/* insure it points to heap */
	 type = blktype(block); 	/* save type and end of block */
	 if (type <= MAXTYPE)
	    endblock = block + getsize(block);
	 BLKLOC(*cdesc) = type; 	/* add descriptor to back chain */
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

   nblk = oblk; 		 /* linear sweep through heap */
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
   hpfree = nblk;		/* reset free space pointer */
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
   int slen;
   char s[100];

   strncpy(s,STRLOC(*d),20);
   if (STRLOC(*d) >= strings && STRLOC(*d) < estrings) {
      if (sqfree >= esqlist) {
	 esqlist += SQLINC;
	 if (brk(esqlist))
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
      if (STRLOC(**p) > e) {		/* outside last clump */
	 while (s < e)			/* move the clump */
	    *d++ = *s++;
	 s = e = STRLOC(**p);		/* start a new clump */
	 }
      if (STRLOC(**p)+STRLEN(**p) > e)	/* extend the clump */
	 e = STRLOC(**p) + STRLEN(**p);
      STRLOC(**p) += d - s + extra;	/* relocate the string qualifier */
      }
   while (s < e)		        /* move the last clump */
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
   register int *srcw, *dstw;	/* word aligned pointers */
   int bytes;

   words = n / sizeof(int);
   bytes = n % sizeof(int);

   srcw = (int *)s;
   dstw = (int *)d;

   if (d < s) {  		/* move back */
      while (--words >= 0)
	 *(dstw)++ = *(srcw)++;
      while (--bytes >= 0)
	 *d++ = *s++;
      }
   else if (d > s) {     	/* move forward */
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
#endif VAX
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
   for (ep = stacks; ep < estacks; ep += STACKSIZE) {
      sp = ep + (STACKSIZE - sizeof(struct b_estack)/2);
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

   sqfree = sqlist;		/* initialize string qualifier list */

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

   escollect();			/* collect available expression stacks */
   if (esneed && esfree == NULL) {
      esfree = estacks;		/* need to make room for another stack */
      *estacks = 0;
      estacks += STACKSIZE;
      extra = STACKSIZE*sizeof(int);	/* string and heap ptrs are chars */
      newend = sqlist + extra;
      if (newend < (char *)sqlist || newend > (char *)0177700 ||
	  (newend > (char *)esqlist && brk(newend) != 0))
	 runerr(305, NULL);
      }
   else
      extra = 0;

   scollect(extra);		/* collect string space */
   sptr = strings + extra;      /* remember new location of string space */
   estrings += extra;
   (char *)sqlist += extra;
   if (sqlist > esqlist)
      esqlist = sqlist;

   extra = (MAX(2*strneed, (estrings-estacks)/4) -
	    (estrings - extra - sfree) + 63) & ~077;
   while (extra > 0) {	        /* need breathing room? */
      newend = (char *)sqlist + extra;
      if (newend >= (char *)sqlist && newend <= (char *)0177700 &&
	  (newend <= (char *)esqlist ||	brk(newend) == 0)) {
         estrings += extra;
         sqlist = newend;
	 break;
         }
      extra -= 64;
      }
   adjust(hpbase,estrings);	/* adjust pointers into heap */
   compact(hpbase);		/* compact heap */
   extra = (MAX(2*heapneed, (maxheap-hpbase)/4) +
	    hpfree - maxheap + 63) & ~077;
   while (extra > 0) {	        /* need breathing room? */
      newend = (char *)sqlist + extra;
      if (newend >= (char *)sqlist && newend <= (char *)0177700 &&
	  (newend <= (char *)esqlist ||	brk(newend) == 0)) {
         sqlist = newend;
	 break;
         }
      extra -= 64;
      }
   if (sqlist > esqlist)
      esqlist = sqlist;
   if (estrings != hpbase) {	        /* move heap */
      mvc((unsigned)(hpfree - hpbase), hpbase, estrings);
      hpfree += estrings - hpbase;
      hpbase = estrings;
      }
   if (sptr != strings) {	        /* move string space */
      mvc((unsigned)(sfree - strings), strings, sptr);
      sfree += sptr - strings;
      strings = sptr;
      }
   maxheap = (char *)sqlist;		/* expand heap */

   return;
   }

/*
 * sweep - sweep the stack, marking all descriptors there.
 */

sweep(b)
int *b;
   {
   register int *sp, *r5, *r4;
   int *r3, nargs;

   r5 = b;			/* start at last Icon/C boundary */
   sp = r5 - 5;
   nargs = 0;
   while ((r5 != 0 || nargs) && sp < (int *)0177776) {
      if (sp == r5 - 5) {  /* at a procedure frame */
	 r3 = r5[-2];
	 r4 = r5[-1];
	 r5 = r5[0];
	 sp += 8;
	 nargs = sp[-1];
	 }
      else if (sp == r3 - 3) {	/* at a generator frame */
	 r5 = r3[0];		/*   go to next boundary */
	 sp = r5 - 5;
	 }
      else if (sp == r4 - 2) {	/* at an expression frame */
	 r3 = r4[-1];
	 r4 = r4[0];
	 sp += 3;
	 }
      else {			/* must be a descriptor! */
  	 mark(sp);
	 sp += 2;
	 if (nargs)
	    nargs--;
	 }
      }
   }

/*
 * mark - mark each accessible block in the heap and build back-list of
 *  descriptors pointing to that block. (Phase I of garbage collection)
 */

mark(cdesc)
struct descrip *cdesc;		/* current descriptor */
   {
   register struct descrip *ndesc;
   register char *endblock, *block;
   static char *type;
   static int fdesc;

   if (QUAL(*cdesc)) 		/* if descriptor is a string qualifier, */
      marksq(cdesc);		/*   mark it for scollect */
   else if (isptr(cdesc)) {	/* ok, descriptor is a pointer */
      block = BLKLOC(*cdesc);	/* get pointer to top of block */
      if (VAR(*cdesc) && !TVAR(*cdesc))  /* if variable, need offset */
	 block = (int *)block - OFFSET(*cdesc);

      if (block >= hpbase && block < hpfree) {	/* insure it points to heap */
	 type = blktype(block); 	/* save type and end of block */
	 if (type <= MAXTYPE)
	    endblock = block + getsize(block);
	 BLKLOC(*cdesc) = type; 	/* add descriptor to back chain */
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

   nblk = oblk; 		 /* linear sweep through heap */
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
   hpfree = nblk;		/* reset free space pointer */
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
	 if (brk(esqlist))
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
      if (STRLOC(**p) > e) {		/* outside last clump */
	 while (s < e)			/* move the clump */
	    *d++ = *s++;
	 s = e = STRLOC(**p);		/* start a new clump */
	 }
      if (STRLOC(**p)+STRLEN(**p) > e)	/* extend the clump */
	 e = STRLOC(**p) + STRLEN(**p);
      STRLOC(**p) += d - s + extra;	/* relocate the string qualifier */
      }
   while (s < e)		        /* move the last clump */
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

   if (d < s) {  		/* move back */
      while (--words >= 0)
	 *((int *)d)++ = *((int *)s)++;
      while (--bytes >= 0)
	 *d++ = *s++;
      }
   else if (d > s) {     	/* move forward */
      s += n;
      d += n;
      while (--bytes >= 0)
	 *--d = *--s;
      while (--words >= 0)
	 *--(int *)d = *--(int *)s;
      }
   }
#endif PDP11
