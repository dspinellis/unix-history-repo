#include "utran.h"
#include "sym.h"
#include "tree.h"

struct lentry **lhash;          /* hash area for local table */
struct gentry **ghash;          /* hash area for global table */
struct centry **chash;          /* hash area for constant table */
struct ientry **ihash;          /* hash area for identifier table */

nodeptr       tree;             /* parse tree space */
nodeptr       tend;             /* end of parse tree space */
struct lentry *ltable;          /* local table */
struct gentry *gtable;          /* global table */
struct centry *ctable;          /* constant table */
struct ientry *itable;          /* identifier table */

char *strings;                  /* string space */
char *send;                     /* end of string space */

nodeptr        tfree;           /* free pointer for parse tree space */
struct lentry *lfree;           /* free pointer for local table */
struct gentry *gfree;           /* free pointer for global table */
struct centry *ctfree;          /* free pointer to constant table */
struct ientry *ifree;           /* free pointer for identifier table */
char          *sfree;           /* free pointer for string space */

int tsize  = TSIZE;             /* initial size of parse tree space */
int lsize  = LSIZE;             /* initial size of local table */
int gsize  = GSIZE;             /* initial size of global table */
int csize  = CSIZE;             /* initial size of constant table */
int isize  = ISIZE;             /* initial size of identifier table */
int ssize  = SSIZE;             /* initial size of string space */
int lhsize = LHSIZE;            /* initial size of local hash table */
int ghsize = GHSIZE;            /* initial size of global hash table */
int chsize = CHSIZE;            /* initial size of constant hash table */
int ihsize = IHSIZE;            /* initial size of identifier hash table */
int lmask;                      /* mask for local table hash */
int gmask;                      /* mask for global table hash */
int cmask;                      /* mask for constant table hash */
int imask;                      /* mask for identifier table hash */

extern end;                     /* first unused location */
char *memfree;

meminit()
   {
   register int i;
   register *p;
   extern char *allocate();

   memfree = &end;
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
   lhash   = (struct lentry **) allocate(lhsize, sizeof(struct lentry *));
   ghash   = (struct gentry **) allocate(ghsize, sizeof(struct gentry *));
   chash   = (struct centry **) allocate(chsize, sizeof(struct centry *));
   ihash   = (struct ientry **) allocate(ihsize, sizeof(struct ientry *));
   ltable  = (struct lentry *)  allocate(lsize,  sizeof(struct lentry));
   gtable  = (struct gentry *)  allocate(gsize,  sizeof(struct gentry));
   ctable  = (struct centry *)  allocate(csize,  sizeof(struct centry));
   itable  = (struct ientry *)  allocate(isize,  sizeof(struct ientry));
   tree    = (nodeptr)          allocate(tsize,  sizeof(int));
   strings = (char *)           allocate(ssize,  sizeof(char));
   tend = (int *)tree + tsize;
   send = strings + ssize;
   if (strings == NULL) {
      fprintf(stderr, "Can't get enough memory\n");
      exit(1);
      }
   lfree = ltable;
   gfree = gtable;
   ctfree = ctable;
   ifree = itable;
   sfree = strings;
   tfree = tree;
   for (p = lhash; p < &lhash[lhsize]; p++)
      *p = NULL;
   for (p = ghash; p < &ghash[ghsize]; p++)
      *p = NULL;
   for (p = chash; p < &chash[chsize]; p++)
      *p = NULL;
   for (p = ihash; p < &ihash[ihsize]; p++)
      *p = NULL;
   alclflg = 0;
   alcgflg = 0;
   alccflg = 0;
   }

/*
 * allocate - get more memory from system
 */

char *allocate(n, size)
int n, size;
   {
   register int need;
   register char *mfree;

   need = n * size;
   mfree = memfree;
   if (brk(memfree += need) == -1)
      return (NULL);
   return (mfree);
   }
