/*
 * Memory initialization and allocation for the translator.
 */

#include "itran.h"
#include "sym.h"
#include "tree.h"

struct lentry **lhash;		/* hash area for local table */
struct gentry **ghash;		/* hash area for global table */
struct centry **chash;		/* hash area for constant table */
struct ientry **ihash;		/* hash area for identifier table */

nodeptr       tree;		/* parse tree space */
nodeptr       tend;		/* end of parse tree space */
struct lentry *ltable;		/* local table */
struct gentry *gtable;		/* global table */
struct centry *ctable;		/* constant table */
struct ientry *itable;		/* identifier table */

char *strings;			/* string space */
char *send;			/* end of string space */

nodeptr        tfree;		/* free pointer for parse tree space */
struct lentry *lfree;		/* free pointer for local table */
struct gentry *gfree;		/* free pointer for global table */
struct centry *ctfree;		/* free pointer to constant table */
struct ientry *ifree;		/* free pointer for identifier table */
char	      *sfree;		/* free pointer for string space */

int tsize  = TSIZE;		/* initial size of parse tree space */
int lsize  = LSIZE;		/* initial size of local table */
int gsize  = GSIZE;		/* initial size of global table */
int csize  = CSIZE;		/* initial size of constant table */
int isize  = ISIZE;		/* initial size of identifier table */
int ssize  = SSIZE;		/* initial size of string space */
int lhsize = LHSIZE;		/* initial size of local hash table */
int ghsize = GHSIZE;		/* initial size of global hash table */
int chsize = CHSIZE;		/* initial size of constant hash table */
int ihsize = IHSIZE;		/* initial size of identifier hash table */
int lmask;			/* mask for local table hash */
int gmask;			/* mask for global table hash */
int cmask;			/* mask for constant table hash */
int imask;			/* mask for identifier table hash */

char *memfree = NULL;

/*
 * meminit does per-file initialization of various data structures used
 *  by the translator.
 */
meminit()
   {
   register *p;

   if (memfree == NULL)
      memalloc();		/* allocate data regions for first file */
   /*
    * Reset the free pointer for each region.
    */
   lfree = ltable;
   gfree = gtable;
   ctfree = ctable;
   ifree = itable;
   sfree = strings;
   tfree = tree;
   /*
    * Zero out the hash tables.
    */
   for (p = (int *)lhash; p < (int *)&lhash[lhsize]; p++)
      *p = NULL;
   for (p = (int *)ghash; p < (int *)&ghash[ghsize]; p++)
      *p = NULL;
   for (p = (int *)chash; p < (int *)&chash[chsize]; p++)
      *p = NULL;
   for (p = (int *)ihash; p < (int *)&ihash[ihsize]; p++)
      *p = NULL;

   /*
    * Vestigial structures - these flags are only incremented after
    *  a call to syserr.  Idea was apparently to count number of
    *  entries in an overflowing table, but wasn't completely
    *  implemented.
    */
   alclflg = 0;
   alcgflg = 0;
   alccflg = 0;
   }

/*
 * allocate gets n*size bytes of storage and returns a pointer to it.
 */

char *allocate(n, size)
int n, size;
   {
   register int need;
   register char *mfree;
   extern char *brk();

   need = n * size;
   mfree = memfree;
   if ((int)brk(memfree += need) == -1)
      return (NULL);
   return (mfree);
   }

/*
 * memalloc computes sizes of data regions needed by the translator
 * obtains space for them, and initializes pointers to them
 */

memalloc()
{
register int i;
char *allocate();
extern char *sbrk();


   memfree = sbrk(0);

   /*
    * Round sizes of hash tables for locals, globals, constants, and
    *  identifiers to next larger power of two.  The corresponding
    *  mask values are set to one less than the hash table size so that
    *  an integer value can be &'d with the mask to produce a hash value.
    *  (See [lgc]hasher in sym.h.)
    */
   for (i = 1; i < lhsize; i <<= 1) ;
   lhsize = i;
   lmask = i - 1;
   for (i = 1; i < ghsize; i <<= 1) ;
   ghsize = i;
   gmask = i - 1;
   for (i = 1; i < chsize; i <<= 1) ;
   chsize = i;
   cmask = i - 1;
   for (i = 1; i < ihsize; i <<= 1) ;
   ihsize = i;
   imask = i - 1;

   /*
    * Allocate the various data structures.
    */
   lhash = (struct lentry **)	allocate(lhsize, sizeof(struct lentry *));
   ghash = (struct gentry **)	allocate(ghsize, sizeof(struct gentry *));
   chash = (struct centry **)	allocate(chsize, sizeof(struct centry *));
   ihash = (struct ientry **)	allocate(ihsize, sizeof(struct ientry *));
   ltable = (struct lentry *)	allocate(lsize, sizeof(struct lentry));
   gtable = (struct gentry *)	allocate(gsize, sizeof(struct gentry));
   ctable = (struct centry *)	allocate(csize, sizeof(struct centry));
   itable = (struct ientry *)	allocate(isize, sizeof(struct ientry));
   tree = (nodeptr)		allocate(tsize, sizeof(int));
   strings =			allocate(ssize, sizeof(char));
   tend = (nodeptr)((int *)tree + tsize);
   send = strings + ssize;
   /*
    * Check to see if there was enough memory.  This assumes that the
    *  allocation for strings fails if any of the other allocations
    *  failed.  Apparent bug - That assumption is not necessarily valid.
    */
   if (strings == NULL) {
      fprintf(stderr, "Can't get enough memory\n");
      exit(1);
      }

}
