/*
 * Memory initialization and allocation; also parses arguments.
 */

#include "ilink.h"
#include <sys/types.h>
#include <sys/stat.h>

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
int  *labels;			/* label table */
char *code;			/* generated code space */

struct gentry *gfree;		/* free pointer for global table */
struct ientry *ifree;		/* free pointer for identifier table */
struct fentry *ffree;		/* free pointer for field table headers */
struct rentry *rfree;		/* free pointer for field table record lists */
char	      *sfree;		/* free pointer for string space */
char	      *codep;		/* free pointer for code space */

int lsize = LSIZE;		/* size of local table */
int gsize = GSIZE;		/* size of global table */
int csize = CSIZE;		/* size of constant table */
int isize = ISIZE;		/* size of identifier table */
int fsize = FSIZE;		/* size of field table headers */
int rsize = RSIZE;		/* size of field table record lists */
int ssize = SSIZE;		/* size of string space */
int ghsize = GHSIZE;		/* size of global hash table */
int ihsize = IHSIZE;		/* size of identifier hash table */
int fhsize = FHSIZE;		/* size of field hash table */
int maxlabels = MAXLABELS;	/* maximum number of labels per procedure */
int maxcode = MAXCODE;		/* maximum amount of code per procedure */
int gmask;			/* mask for global table hash */
int imask;			/* mask for identifier table hash */
int fmask;			/* mask for field table hash */


extern char end;		/* first unused location */
char *memfree;
char *ipath;

/*
 * meminit - scan the command line arguments and initialize data structures.
 */
meminit(argc,argv)
int argc;
char **argv;
   {
   int aval;
   register int i;
   register union {
      struct gentry **gp;
      struct ientry **ip;
      struct fentry **fp;
      } p;
   extern char *allocate();
   extern char *instalid();
   extern char *getenv();
   extern char *sbrk();
   extern struct gentry *putglob();

   memfree = sbrk(0);		/* Start allocating at the end of uninitialized data. */
   lfiles = NULL;		/* Zero queue of files to link. */
   if ((ipath = getenv("IPATH")) == NULL)
      ipath = ".";		/* Just look in current directory if no IPATH. */
   /*
    * Process the command line arguments.
    */
   while (--argc) {
      if (**++argv == '-') {
         switch ((*argv)[1]) {
            case 'm':		/* -m and -u are for the translator. */
            case 'u':
               continue;
            case 't':		/* Set &trace to -1 when Icon starts up. */
               trace = -1;
               continue;
            case 'D':		/* Produce a .ux file, which is a readable
				    version of the icode file produced. */
               Dflag++;
               continue;
            case 'o':		/* Output file is next argument. */
                strcpy(outname,*++argv);
                argc--;
                continue;
            case 'S':		/* Change some table size. */
               if ((*argv)[3] == 'h') {	/* Change hash table size. */
                  aval = atoi(&(*argv)[4]);
                  if (aval <= 0)
                     goto badarg;
                  switch ((*argv)[2]) {
                     case 'i': ihsize = aval; continue;
                     case 'g': ghsize = aval; continue;
                     case 'c': continue;
                     case 'f': fhsize = aval; continue;
                     case 'l': continue;
                     }
                  }
               else {		/* Change symbol table size. */
                  aval = atoi(&(*argv)[3]);
                  if (aval <= 0)
                     goto badarg;
                  switch ((*argv)[2]) {
                     case 'c': csize = aval; continue;
                     case 'i': isize = aval; continue;
                     case 'g': gsize = aval; continue;
                     case 'l': lsize = aval; continue;
                     case 's': ssize = aval; continue;
                     case 't': continue;
                     case 'f': fsize = aval; continue;
                     case 'r': rsize = aval; continue;
                     case 'L': maxlabels = aval; continue;
                     case 'C': maxcode = aval; continue;
                     }
                  }
	    case 'i': {
		iconx = *++argv;
		argc--;
		continue;
		}
            default:
            badarg:
               printf("bad argument: %s\n", *argv);
               continue;
            }
         }
      else {		/* If not an argument, assume it's an input file. */
	 if (access(*argv) != 0) {
	     fprintf(stderr, "ilink: cannot open %s\n", *argv);
	     exit(1);
	     }
         addlfile(*argv);
	 }
      }

   /*
    * Round sizes of hash tables for locals, globals, constants, and
    *  identifiers to next larger power of two.  The corresponding
    *  mask values are set to one less than the hash table size so that
    *  an integer value can be &'d with the mask to produce a hash value.
    *  (See [lgc]hasher in sym.h.)
    */
   for (i = 1; i < ghsize; i <<= 1) ;
   ghsize = i;
   gmask = i - 1;
   for (i = 1; i < ihsize; i <<= 1) ;
   ihsize = i;
   imask = i - 1;
   for (i = 1; i < fhsize; i <<= 1) ;
   fhsize = i;
   fmask = i - 1;
   /*
    * Allocate the various data structures that are made on a per-file
    *  basis.
    */
   ghash   = (struct gentry **)	allocate(ghsize, sizeof(struct gentry *));
   ihash   = (struct ientry **)	allocate(ihsize, sizeof(struct ientry *));
   fhash   = (struct fentry **)	allocate(fhsize, sizeof(struct fentry *));
   ltable  = (struct lentry *)	allocate(lsize, sizeof(struct lentry));
   gtable  = (struct gentry *)	allocate(gsize, sizeof(struct gentry));
   ctable  = (struct centry *)	allocate(csize, sizeof(struct centry));
   itable  = (struct ientry *)	allocate(isize, sizeof(struct ientry ));
   ftable  = (struct fentry *)	allocate(fsize, sizeof(struct fentry));
   rtable  = (struct rentry *)	allocate(rsize, sizeof(struct rentry));
   strings = (char *)		allocate(ssize, sizeof(char *));
   labels  = (int  *)		allocate(maxlabels, sizeof(int  *));
   code    = (char *)		allocate(maxcode, sizeof(char *));
   /*
    * Check to see if there was enough memory.  This assumes that the
    *  allocation for strings fails if any of the other allocations
    *  failed.  Apparent bug - that assumption is not necessarily valid.
    */

   if (code == NULL)
      syserr("can't get enough memory");
   /*
    * Reset the free pointer for each region.
    */
   gfree = gtable;
   ifree = itable;
   ffree = ftable;
   rfree = rtable;
   sfree = strings;
   codep = code;
   /*
    * Zero out the hash tables.
    */
   for (p.gp = ghash; p.gp < &ghash[ghsize]; p.gp++)
      *p.gp = NULL;
   for (p.ip = ihash; p.ip < &ihash[ihsize]; p.ip++)
      *p.ip = NULL;
   for (p.fp = fhash; p.fp < &fhash[fhsize]; p.fp++)
      *p.fp = NULL;
   /*
    * Install "main" as a global variable in order to insure that it
    *  is the first global variable.  iconx/start.s depends on main
    *  being global number 0.
    */
   putglob(instalid("main"), F_GLOBAL, 0);
   }

/*
 * allocate - get more memory from system.
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

/*
 * alclfile - allocate an lfile structure for the named file, fill
 *  in the name and return a pointer to it.
 */
struct lfile *alclfile(name)
char *name;
   {
   struct lfile *p;
   char *np;
   int l;
   
   p = (struct lfile *)allocate(1,sizeof(struct lfile));
   if (!p)
      syserr("not enough memory for file list");
   p->lf_link = NULL;
   l = strlen(name);
   np = allocate(1,(l+1+sizeof(int *)) & ~(sizeof(int *)-1));
   if (!np)
      syserr("not enough memory for file list");
   strncpy(np,name,l);
   p->lf_name = np;
   return p;
   }

/*
 * dumplfiles - print the list of files to link.  Used for debugging only.
 */

dumplfiles()
   {
   struct lfile *p,*lfls;

   printf("lfiles:\n");
   lfls = lfiles;
   while (p = getlfile(&lfls))
       printf("'%s'\n",p->lf_name);
   }
/*
 * addlfile - create an lfile structure for the named file and add it to the
 *  end of the list of files (lfiles) to generate link instructions for.
 */
char *pptr;   
addlfile(name)
char *name;
   {
   struct lfile *nlf, *p;
   char file[256], ok;
   
   if (index(name,'/') == 0) {
      pptr = ipath;
      ok = 0;
      while (trypath(name,file)) {
         if (canread(file)) {
            ok++;
            break;
            }
         }
      if (!ok) {
         fprintf(stderr, "Can't resolve reference to file '%s'\n",name);
         exit(1);
         }
      }
   else
      strcpy(file,name);
   nlf = alclfile(file);
   if (lfiles == NULL) {
      lfiles = nlf;
      }
   else {
      p = lfiles;
      while (p->lf_link != NULL) {
         if (strcmp(p->lf_name,file) == 0)
            return;
         p = p->lf_link;
         }
      if (strcmp(p->lf_name,file) == 0)
         return;
      p->lf_link = nlf;
      }
   }

/*
 * getlfile - return a pointer (p) to the lfile structure pointed at by lptr
 *  and move lptr to the lfile structure that p points at.  That is, getlfile
 *  returns a pointer to the current (wrt. lptr) lfile and advances lptr.
 */
struct lfile *
getlfile(lptr)
struct lfile **lptr;
   {
   struct lfile *p;
   
   if (*lptr == NULL)
      return NULL;
   else {
      p = *lptr;
      *lptr = p->lf_link;
      return p;
      }
   }

/*
 * canread - see if file can be read and be sure that it's just an
 *  ordinary file.
 */
canread(file)
char *file;
   {
   struct stat statb;
   if (access(file,4) == 0) {
      stat(file,&statb);
      if (statb.st_mode & S_IFREG)
         return 1;
      }
   return 0;
   }

/*
 * trypath - form a file name in file by concatenating name onto the
 *  next path element.
 */
trypath(name,file)
char *name, *file;
   {
   char *n, c;
   
   while (*pptr == ':')
      pptr++;
   if (!*pptr)
      return 0;
   do {
      c = (*file++ = *pptr++);
      } while (c != ':' && c);
   pptr--;
   file--;
   
   *file++ = '/';
   while (*file++ = *name++);
   *file = 0;
   }
