#include "../h/gc.h"
#include "../h/record.h"

/*
 * allocate - returns pointer to nwords of free storage in heap.
 */

union block *allocate(nbytes)
int nbytes;
   {
   register unsigned fspace, *sloc;

   fspace = maxheap - hpfree;

   if (fspace < nbytes)                 /* need more room, couldn't get it */
      runerr(301, NULL);

   sloc = hpfree;
   heapneed -= nbytes;
   hpfree = hpfree + nbytes;
   return (sloc);
   }

#ifndef BIT32
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
#endif BIT32
/*
 * alcreal - allocate a real value in the heap.
 */

struct b_real *alcreal(val)
double val;
   {
   register struct b_real *blk;
   extern union block *allocate();

   blk = allocate(sizeof(struct b_real));
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

   blk = allocate(sizeof(struct b_cset));
   blk->type = T_CSET;
   for (i = 0; i < CSETSIZE; i++)      /* guarantee null cset */
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

   blk = allocate(sizeof(struct b_file));
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
   {
   register struct b_record *blk;
   register i, size;
   extern union block *allocate();

   size = sizeof(struct b_record) + nfields*sizeof(struct descrip);
   blk = allocate(size);
   blk->type = T_RECORD;
   blk->size = size;
   blk->recptr = recptr;
   for (i = 0; i < nfields; i++)        /* guarantee null fields */
       blk->fields[i] = nulldesc;
   return (blk);
   }

/*
 * alclist - allocate a list header in the heap.
 */

struct b_list *alclist(size)
int size;
   {
   register struct b_list *blk;
   extern union block *allocate();

   blk = allocate(sizeof(struct b_list));
   blk->type = T_LIST;
   blk->cursize = size;
   blk->listhead = nulldesc;
   return (blk);
   }

/*
 * alclstb - allocate a list element block in the heap.
 */

struct b_listb *alclstb(nelem, first, nused)
int nelem, first, nused;
   {
   register struct b_listb *blk;
   register int i, size;
   extern union block *allocate();

#ifdef MAXLISTSIZE
   if (nelem >= MAXLISTSIZE)
      runerr(205, NULL);
#endif MAXLISTSIZE
   size = sizeof(struct b_listb)+nelem*sizeof(struct descrip);
   blk = allocate(size);
   blk->type = T_LISTB;
   blk->size = size;
   blk->nelem = nelem;
   blk->first = first;
   blk->nused = nused;
   blk->listprev = nulldesc;
   blk->listnext = nulldesc;
   for (i = 0; i < nelem; i++)
      blk->lelem[i] = nulldesc;
   return (blk);
   }

/*
 * alctable - allocate a table header in the heap.
 */

struct b_table *alctable(def)
struct descrip *def;
   {
   register int i;
   register struct b_table *blk;
   extern union block *allocate();

   blk = allocate(sizeof(struct b_table));
   blk->type = T_TABLE;
   blk->cursize = 0;
   blk->defvalue = *def;
   for (i = 0; i < NBUCKETS; i++)
      blk->buckets[i] = nulldesc;
   return (blk);
   }

/*
 * alctelem - allocate a table element block in the heap.
 */

struct b_telem *alctelem(link, ref, val)
struct descrip *link, *ref, *val;
   {
   register struct b_telem *blk;
   extern union block *allocate();

   blk = allocate(sizeof(struct b_telem));
   blk->type = T_TELEM;
   blk->blink = *link;
   blk->tref = *ref;
   blk->tval = *val;
   return (blk);
   }

/*
 * alcsubs - allocate a substring trapped variable in heap.
 */

struct b_tvsubs *alcsubs(len, pos, var)
int len, pos;
struct descrip *var;
   {
   register struct b_tvsubs *blk;
   extern union block *allocate();

   blk = allocate(sizeof(struct b_tvsubs));
   blk->type = T_TVSUBS;
   blk->sslen = len;
   blk->sspos = pos;
   blk->ssvar = *var;
   return (blk);
   }

/*
 * alctvtbl - allocate a table element trapped variable in heap.
 */

struct b_tvtbl *alctvtbl(tbl, ref)
register struct descrip *tbl, *ref;
   {
   register struct b_tvtbl *blk;
   extern union block *allocate();

   blk = allocate(sizeof(struct b_tvtbl));
   blk->type = T_TVTBL;
   blk->tvtable = *tbl;
   blk->tvtref = *ref;
   return (blk);
   }

/*
 * alcstr - allocate a string in the string space
 */

char *alcstr(s, slen)
register char *s;
register int slen;
   {
   register char *d;
   char *ofree;

   if (sfree + slen > estrings)
      runerr(302, NULL);
   strneed -= slen;
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
 * alcestk - allocate a co-expression stack block
 */

struct b_estack *alcestk()
   {
   struct b_estack *ep;

   if (esfree == NULL)
      syserr("no expression stack space");
   ep = esfree + 
   	(STACKSIZE-sizeof(struct b_estack)/ADDRSIZE); /* start of block */
   esfree = *esfree;          /* adjust free list pointer */
   ep->estack.type = T_ESTACK;
   return (ep);
   }

/*
 * alceblk - allocate a co-expression heap block
 */

struct b_eblock *alceblk(entry, na, nl)
int *entry, na, nl;
   {
   int size;
   union block *blk;
   extern union block *allocate();

   size = sizeof(struct b_eblock)+(na+nl+1)*sizeof(struct descrip);
   blk = allocate(size);
   blk->eblock.type = T_EBLOCK;
   blk->eblock.size = size;
   blk->eblock.ep = entry;
   blk->eblock.numargs = na;
   blk->eblock.numlocals = nl;
   return (blk);
   }
