#include "../h/rt.h"
#include "../h/gc.h"
#include "../h/record.h"

/*
 * allocate - returns pointer to nbytes of free storage in heap.
 */

union block *allocate(nbytes)
int nbytes;
   {
   register unsigned fspace, *sloc;

   /*
    * See if there is enough room in the heap.
    */
   fspace = maxheap - hpfree;
   if (fspace < nbytes)
      runerr(301, NULL);

   /*
    * Decrement the free space in the heap by the number of bytes allocated
    *  and return the address of the first byte of the allocated block.
    */
   sloc = (unsigned *) hpfree;
   heapneed -= nbytes;
   hpfree = hpfree + nbytes;
   return (union block *) (sloc);
   }

#ifdef LONGS
/*
 * alclint - allocate a long integer block in the heap.
 */

struct b_int *alclint(val)
long val;
   {
   register struct b_int *blk;
   extern union block *allocate();

   blk = allocate(sizeof(struct b_int));
   blk->type = T_LONGINT;
   blk->intval = val;
   return (blk);
   }
#endif LONGS
/*
 * alcreal - allocate a real value in the heap.
 */

struct b_real *alcreal(val)
double val;
   {
   register struct b_real *blk;
   extern union block *allocate();

   blk = (struct b_real *) allocate(sizeof(struct b_real));
   blk->type = T_REAL;
   blk->realval = val;
   return (blk);
   }

/*
 * alccset - allocate a cset in the heap.
 */

struct b_cset *alccset()
   {
   register struct b_cset *blk;
   register i;
   extern union block *allocate();

   blk = (struct b_cset *) allocate(sizeof(struct b_cset));
   blk->type = T_CSET;
   /*
    * Zero out the bit array.
    */
   for (i = 0; i < CSETSIZE; i++)
     blk->bits[i] = 0;
   return (blk);
   }


/*
 * alcfile - allocate a file block in the heap.
 */

struct b_file *alcfile(fd, status, name)
FILE *fd;
int status;
struct descrip *name;
   {
   register struct b_file *blk;
   extern union block *allocate();

   blk = (struct b_file *) allocate(sizeof(struct b_file));
   blk->type = T_FILE;
   blk->fd = fd;
   blk->status = status;
   blk->fname = *name;
   return (blk);
   }

/*
 * alcrecd - allocate record with nfield fields in the heap.
 */

struct b_record *alcrecd(nfields, recptr)
int nfields;
struct b_proc *recptr;
   {
   register struct b_record *blk;
   register i, size;
   extern union block *allocate();

   size = vsizeof(struct b_record) + nfields*sizeof(struct descrip);
   blk = (struct b_record *) allocate(size);
   blk->type = T_RECORD;
   blk->size = size;
   blk->recptr = recptr;
   /*
    * Assign &null to each field in the record.
    */
   for (i = 0; i < nfields; i++)
       blk->fields[i] = nulldesc;
   return (blk);
   }

/*
 * alclist - allocate a list header block in the heap.
 */

struct b_list *alclist(size)
int size;
   {
   register struct b_list *blk;
   extern union block *allocate();

   blk = (struct b_list *) allocate(sizeof(struct b_list));
   blk->type = T_LIST;
   blk->cursize = size;
   blk->listhead = nulldesc;
   return (blk);
   }

/*
 * alclstb - allocate a list element block in the heap.
 */

struct b_lelem *alclstb(nelem, first, nused)
int nelem, first, nused;
   {
   register struct b_lelem *blk;
   register int i, size;
   extern union block *allocate();

#ifdef MAXLISTSIZE
   if (nelem >= MAXLISTSIZE)
      runerr(205, NULL);
#endif MAXLISTSIZE
   size = vsizeof(struct b_lelem)+nelem*sizeof(struct descrip);
   blk = (struct b_lelem *) allocate(size);
   blk->type = T_LELEM;
   blk->size = size;
   blk->nelem = nelem;
   blk->first = first;
   blk->nused = nused;
   blk->listprev = nulldesc;
   blk->listnext = nulldesc;
   /*
    * Set all elements to &null.
    */
   for (i = 0; i < nelem; i++)
      blk->lslots[i] = nulldesc;
   return (blk);
   }

/*
 * alctable - allocate a table header block in the heap.
 */

struct b_table *alctable(def)
struct descrip *def;
   {
   register int i;
   register struct b_table *blk;
   extern union block *allocate();

   blk = (struct b_table *) allocate(sizeof(struct b_table));
   blk->type = T_TABLE;
   blk->cursize = 0;
   blk->defvalue = *def;
   /*
    * Zero out the buckets.
    */
   for (i = 0; i < NBUCKETS; i++)
      blk->buckets[i] = nulldesc;
   return (blk);
   }
/*
 *  alctelem - allocate a table element block in the heap.
 */

struct b_telem *alctelem()
   {
   register struct b_telem *blk;
   extern union block *allocate();

   blk = (struct b_telem *) allocate(sizeof(struct b_telem));
   blk->type = T_TELEM;
   blk->hashnum = 0;
   blk->blink = nulldesc;
   blk->tref = nulldesc;
   blk->tval = nulldesc;
   return (blk);
   }
#ifdef SETS

/*
 * alcset - allocate a set header heap block.
 */

struct b_set *alcset()
   {
     register int i;
     register struct b_set *blk;
     extern union block *allocate();

     blk = (struct b_set *) allocate(sizeof(struct b_set));
     blk->type = T_SET;
     blk->setsize = 0;
     /*
      *  Zero out the buckets.
      */
     for (i = 0; i < NBUCKETS; i++)
        blk->sbucks[i] = nulldesc;
     return (blk);
     }

/* 
 *   alcselem - allocate a set element heap block.
 */

struct b_selem *alcselem(mbr,hn)
int hn;
struct descrip *mbr;

   { register struct b_selem *blk;
     extern union block *allocate();

     blk = (struct b_selem *) allocate(sizeof(struct b_selem));
     blk->type = T_SELEM;
     blk->sblink = nulldesc;
     blk->setmem = *mbr;
     blk->hnum = hn;
     return (blk);
     }
#endif SETS

/*
 * alcsubs - allocate a substring trapped variable in heap.
 */

struct b_tvsubs *alcsubs(len, pos, var)
int len, pos;
struct descrip *var;
   {
   register struct b_tvsubs *blk;
   extern union block *allocate();

   blk = (struct b_tvsubs *) allocate(sizeof(struct b_tvsubs));
   blk->type = T_TVSUBS;
   blk->sslen = len;
   blk->sspos = pos;
   blk->ssvar = *var;
   return (blk);
   }

/*
 * alctvtbl - allocate a table element trapped variable block in the heap.
 */

struct b_tvtbl *alctvtbl(tbl, ref, hnum)
register struct descrip *tbl, *ref;
int hnum;
   {
   register struct b_tvtbl *blk;
   extern union block *allocate();

   blk = (struct b_tvtbl *) allocate(sizeof(struct b_tvtbl));
   blk->type = T_TVTBL;
   blk->hashnum = hnum;
   blk->tvtable = *tbl;
   blk->tvtref = *ref;
   blk->tvtval = nulldesc;
   return (blk);
   }

/*
 * alcstr - allocate a string in the string space.
 */

char *alcstr(s, slen)
register char *s;
register int slen;
   {
   register char *d;
   char *ofree;

   /*
    * See if there is enough room in the string space.
    */
   if (sfree + slen > estrings)
      runerr(302, NULL);
   strneed -= slen;
   /*
    * Copy the string into the string space, saving a pointer to its
    *  beginning.  Note that s may be null, in which case the space is
    *  still to be allocated, but nothing is to be copied into it.
    */
   ofree = d = sfree;
   if (s != NULL) {
      while (slen-- > 0)
         *d++ = *s++;
      }
   else
      d += slen;
   sfree = d;
   return (ofree);
   }

/*
 * alcestk - allocate a co-expression stack block.
 */

struct b_estack *alcestk()
   {
   struct b_estack *ep;

   if (esfree == NULL)
      syserr("no expression stack space");
   /*
    * Locate the start of the block.
    */
   ep = (struct b_estack *) (esfree + 
      (stksize-sizeof(struct b_estack)/WORDSIZE));
   /*
    * Move co-expression stack free-list pointer to next stack.
    */
   esfree = (int *) *esfree;
   ep->type = T_ESTACK;
   return (ep);
   }

/*
 * alceblk - allocate a co-expression heap block.
 */

struct b_eblock *alceblk(entry, na, nl)
int *entry, na, nl;
   {
   int size;
   struct b_eblock *blk;
   extern union block *allocate();

   size = vsizeof(struct b_eblock)+(na+nl+1)*sizeof(struct descrip);
   blk = (struct b_eblock *) allocate(size);
   blk->type = T_EBLOCK;
   blk->size = size;
   blk->ep = entry;
   blk->numargs = na;
   blk->numlocals = nl;
   return (blk);
   }
