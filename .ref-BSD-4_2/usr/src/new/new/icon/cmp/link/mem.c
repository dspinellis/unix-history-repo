#include "ulink.h"

/*
 * Memory initialization
 */

struct gentry **ghash;		/* hash area for global table */
struct ientry **ihash;		/* hash area for identifier table */
struct fentry **fhash;		/* hash area for field table */

struct lentry *ltable;		/* local table */
struct gentry *gtable;		/* global table */
struct centry *ctable;		/* constant table */
struct ientry *itable;		/* identifier table */
struct fentry *ftable;		/* field table headers */
struct rentry *rtable;		/* field table record lists */

char *strings;			/* string space */

struct gentry *gfree;		/* free pointer for global table */
struct ientry *ifree;		/* free pointer for identifier table */
struct fentry *ffree;		/* free pointer for field table headers */
struct rentry *rfree;		/* free pointer for field table record lists */
char	      *sfree;		/* free pointer for string space */

int lsize  = LSIZE;		/* initial size of local table */
int gsize  = GSIZE;		/* initial size of global table */
int csize  = CSIZE;		/* initial size of constant table */
int isize  = ISIZE;		/* initial size of identifier table */
int fsize  = FSIZE;		/* initial size of field table headers */
int rsize  = RSIZE;		/* initial size of field table record lists */
int ssize  = SSIZE;		/* initial size of string space */
int ghsize = GHSIZE;		/* initial size of global hash table */
int ihsize = IHSIZE;		/* initial size of identifier hash table */
int fhsize = FHSIZE;		/* initial size of field hash table */
int gmask;			/* mask for global table hash */
int imask;			/* mask for identifier table hash */
int fmask;			/* mask for field table hash */

extern char end;		/* first unused location */
char *memfree;

char *filelist[64];             /* list of input file names */

meminit(argc,argv)
int argc;
char **argv;
   {
   int aval;
   register int i;
   register char **fp;
   register union {
      struct gentry **gp;
      struct ientry **ip;
      struct fentry **fp;
      } p;
   extern char *allocate();
   extern char *instalid();
   extern struct gentry *putglob();

   memfree = &end;
   fp = filelist;
   while (--argc) {
      if (**++argv == '-') {
         switch ((*argv)[1]) {
	    case 'm':
            case 'u':
	       continue;
	    case 't':
	       trace = -1;
	       continue;
            case 'S':
               if ((*argv)[3] == 'h') { /* change hash table size */
                  aval = atoi(&(*argv)[4]);
                  if (aval <= 0)
                     goto badarg;
                  switch ((*argv)[2]) {
                     case 'i': ihsize = aval; continue;
                     case 'g': ghsize = aval; continue;
                     case 'c':                continue;
                     case 'f': fhsize = aval; continue;
                     case 'l':                continue;
                     }
                  }
               else {                   /* change symbol table size */
                  aval = atoi(&(*argv)[3]);
                  if (aval <= 0)
                     goto badarg;
                  switch ((*argv)[2]) {
                     case 'c': csize = aval; continue;
                     case 'i': isize = aval; continue;
                     case 'g': gsize = aval; continue;
                     case 'l': lsize = aval; continue;
                     case 's': ssize = aval; continue;
                     case 't':               continue;
                     case 'f': fsize = aval; continue;
                     case 'r': rsize = aval; continue;
                     }
                  }
	    default:
            badarg:
               printf("bad argument: %s\n", *argv);
               continue;
	    }
         }
      else
         *fp++ = *argv;
      }
   *fp++ = 0;
   for (i = 1; i < ghsize; i <<= 1) ;
   ghsize = i;
   gmask = i - 1;
   for (i = 1; i < ihsize; i <<= 1) ;
   ihsize = i;
   imask = i - 1;
   for (i = 1; i < fhsize; i <<= 1) ;
   fhsize = i;
   fmask = i - 1;
   ghash   = (struct gentry **) allocate(ghsize, sizeof(struct gentry *));
   ihash   = (struct ientry **) allocate(ihsize, sizeof(struct ientry *));
   fhash   = (struct fentry **) allocate(fhsize, sizeof(struct fentry *));
   ltable  = (struct lentry *)  allocate(lsize,  sizeof(struct lentry));
   gtable  = (struct gentry *)  allocate(gsize,  sizeof(struct gentry));
   ctable  = (struct centry *)  allocate(csize,  sizeof(struct centry));
   itable  = (struct ientry *)  allocate(isize,  sizeof(struct ientry ));
   ftable  = (struct fentry *)  allocate(fsize,  sizeof(struct fentry));
   rtable  = (struct rentry *)  allocate(rsize,  sizeof(struct rentry));
   strings = (char *)           allocate(ssize,  sizeof(char *));
   if (strings == NULL) {
      syserr("can't get enough memory");
      }
   gfree = gtable;
   ifree = itable;
   ffree = ftable;
   rfree = rtable;
   sfree = strings;
   for (p.gp = ghash; p.gp < &ghash[ghsize]; p.gp++)
      *p.gp = NULL;
   for (p.ip = ihash; p.ip < &ihash[ihsize]; p.ip++)
      *p.ip = NULL;
   for (p.fp = fhash; p.fp < &fhash[fhsize]; p.fp++)
      *p.fp = NULL;
   putglob(instalid("main"), F_GLOBAL, 0);
   }

/*
 * allocate - get more memory from system
 */

char *allocate(n, size)
int n, size;
   {
   register int need;
   register char *mfree;
   extern char *brk();

   need = n * size;
   mfree = memfree;
   if (brk(memfree += need) == (char *) -1)
      return (NULL);
   return (mfree);
   }
