/*
 * Initialization and error routines.
 */

#include "../h/rt.h"
#include "../h/gc.h"
#include <signal.h>
#include <sys/types.h>
#include <sys/times.h>
#define MAXHDRLN	100		/* max len of #! line */
#define MAXHDR		1024L		/* size of autoloading header--!! must
					    agree with that in link/ilink.c */
char *file = "";			/* source program file name */
int line = 0;				/* source program line number */
char *code;				/* interpreter code buffer */
int *records;				/* ptr to record procedure blocks */
int *ftab;				/* ptr to record/field table */
struct descrip *globals, *eglobals;	/* ptr to global variables */
struct descrip *gnames, *egnames;	/* ptr to global variable names */
struct descrip *statics, *estatics;	/* ptr to static variables */
char *ident;				/* ptr to identifier table */
int *monbuf;				/* monitor buffer for profiling */
int monres = 0;				/* resolution of monitor buffer */
int monsize = 0;			/* size of monitor buffer */

int numbufs = NUMBUF;			/* number of i/o buffers */
char (*bufs)[BUFSIZ];			/* pointer to buffers */
FILE **bufused;				/* pointer to buffer use markers */

int nstacks = MAXSTACKS;		/* initial number of coexpr stacks */
int stksize = STACKSIZE;		/* coexpression stack size */
int dodump;				/* if non-zero, core dump on error */
int noerrbuf;				/* if non-zero, DON'T buffer stderr */
int *stacks;				/* start of stack space */
int *estacks;				/* end of stack space */
int *esfree;				/* stack space free list pointer */

int ssize = MAXSTRSPACE;		/* initial string space size (bytes) */
char *strings;				/* start of string space */
char *estrings;				/* end of string space */
char *sfree;				/* string space free pointer */

int hpsize = MAXHEAPSIZE;		/* initial heap size (bytes) */
char *hpbase;				/* start of heap */
char *maxheap;				/* end of heap storage */
char *hpfree;				/* heap free space pointer */
unsigned heapneed;			/* stated need for heap space */
unsigned strneed;			/* stated need for string space */

struct descrip **sqlist;		/* string qualifier list */
struct descrip **sqfree;		/* s. q. list free pointer */
struct descrip **esqlist;		/* end of s. q. list */

struct descrip current;			/* current expression stack pointer */

/*
 * &ascii cset, first 128 bits on, second 128 bits off.
 */
struct b_cset  k_ascii = {
   T_CSET,
   cset_display(~0, ~0, ~0, ~0, ~0, ~0, ~0, ~0,
                 0,  0,  0,  0,  0,  0,  0,  0)
   };

/*
 * &cset cset, all 256 bits on.
 */
struct b_cset  k_cset = {
   T_CSET,
   cset_display(~0, ~0, ~0, ~0, ~0, ~0, ~0, ~0,
                ~0, ~0, ~0, ~0, ~0, ~0, ~0, ~0)
   };

/*
 * File block for &errout.
 */
struct b_file  k_errout = {
   T_FILE,
   stderr,
   FS_WRITE,
   7,
   /*"&errout", */
   };

/*
 * File block for &input.
 */
struct b_file  k_input = {
   T_FILE,
   stdin,
   FS_READ,
   6,
   /*"&input",*/
   };

/*
 * cset for &lcase, bits corresponding to lowercase letters are on.
 */
struct b_cset  k_lcase = {
   T_CSET,
   cset_display( 0,  0,  0,  0,  0,  0, ~01, 03777,
                 0,  0,  0,  0,  0,  0,  0,  0)
   };

int            k_level = 0;		/* &level */
struct descrip k_main;			/* &main */
int            k_pos = 1;		/* &pos */

/*
 * File block for &output.
 */
struct b_file  k_output = {
   T_FILE,
   stdout,
   FS_WRITE,
   7,
   /*"&output",*/
   };

long           k_random = 0L;		/* &random */
struct descrip k_subject = {		/* &subject */
   0,
   /*1,*/
   };
int k_trace = 0;
/*
 * cset for &ucase, bits corresponding to uppercase characters are on.
 */
struct b_cset  k_ucase = {
   T_CSET,
    cset_display(0,  0,  0,  0, ~01, 03777, 0, 0,
                 0,  0,  0,  0,  0,  0,  0,  0)
   };

/*
 * maps2 and maps3 are used by the map function as caches.
 */
struct descrip maps2 = {
   D_NULL,
   /*0,*/
   };
struct descrip maps3 = {
   D_NULL,
   /*0,*/
   };

long starttime;				/* starttime of job in milliseconds */

struct descrip nulldesc = {D_NULL, /*0*/};
struct descrip zerodesc = {D_INTEGER, /*0*/};
struct descrip onedesc  = {D_INTEGER, /*1*/};
struct descrip nullstr  = {0, /*""*/};
struct descrip blank    = {1, /*" "*/};
struct descrip letr     = {1, /*"r"*/};
struct descrip input    = {D_FILE, /*&k_input*/};
struct descrip errout   = {D_FILE, /*&k_errout*/};
struct descrip lcase    = {26, /*lowercase*/};
struct descrip ucase    = {26, /*uppercase*/};

static struct b_estack mainhead;	/* expression stack head for main */

/*
 * init - initialize memory and prepare for Icon execution.
 */

#ifdef VAX
init(name)
#endif VAX
#ifdef PORT
init(name)
#endif PORT
#ifdef PDP11
init(nargs, name)
int nargs;
#endif PDP11
char *name;
   {
   register int i;
   int cbread;
   int f;
   FILE *ufile;
   char uheader[MAXHDRLN];
   int directex;
   /*
    * Interpretable file header
    */
   struct header {
      int size;			/* size of icode file */
      int trace;		/* initial value of &trace */
      int records;		/* records */
      int ftab;			/* record field table */
      int globals;		/* global array */
      int gnames;		/* global name array */
      int statics;		/* static array */
      int ident;		/* strings for identifiers, etc. */
      } hdr;
   struct tms tp;
   extern char *brk(), end;
   extern char Pstart, Pstop;
   extern fpetrap(), segvtrap();

   /*
    * Catch floating point traps and memory faults.
    */
   signal(SIGFPE, fpetrap);
   signal(SIGSEGV, segvtrap);

   /*
    * Initializations that can't be performed statically.
    */
   STRLOC(k_errout.fname)	= "&errout";
   STRLOC(k_input.fname)	= "&input";
   STRLOC(k_output.fname)	= "&output";
   STRLOC(k_subject)		= (char *) 1;
   STRLOC(maps2)		= 0;
   STRLOC(maps3)		= 0;
   STRLOC(nulldesc)		= 0;
   INTVAL(zerodesc)		= 0;
   INTVAL(onedesc)		= 1;
   STRLOC(nullstr)		= "";
   STRLOC(blank)		= " ";
   STRLOC(letr)			= "r";
   BLKLOC(input)		= (union block *) &k_input;
   BLKLOC(errout)		= (union block *) &k_errout;
   STRLOC(lcase)		= "abcdefghijklmnopqrstuvwxyz";
   STRLOC(ucase)		= "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
   
   /*
    * Initialize &main.
    */
   mainhead.type = T_ESTACK;
   mainhead.activator.type = D_NULL;
   STRLOC(mainhead.activator) = NULL;
   mainhead.sbase = (int *)(STKBASE);
   mainhead.sp = NULL;
   mainhead.boundary = NULL;
   mainhead.nresults = 0;
   mainhead.freshblk.type = D_NULL;
   STRLOC(mainhead.freshblk)  = 0;

   /*
    * Open the interpretable file and read the header.
    */
   i = strlen(name);
   f = open(name, 0);
   if (f < 0)
      error("can't open interpreter file");
   /*
    * We check to see if the header starts with #! and if so, we assume
    *  that it is being directly executed and seek past the header.
    */
   ufile = fdopen(f,"r");
   fgets(uheader,MAXHDRLN,ufile);
   if (strncmp(uheader,"#!",2) != 0) {
      fseek(ufile,MAXHDR,0);
      fgets(uheader,MAXHDRLN,ufile);
      if (strncmp(uheader,"#!",2) == 0)
         lseek(f,MAXHDR+(long)strlen(uheader),0);
      else
        error("invalid format for interpretable file");
        }
   else
      lseek(f,(long)strlen(uheader),0);

   if (read(f, &hdr, sizeof hdr) != sizeof hdr)
      error("can't read interpreter file header");
    
   /*
    * Establish pointers to data regions.
    */
   code = (char *) sbrk(0);
   k_trace = hdr.trace;
   records = (int *) (code + hdr.records);
   ftab = (int *) (code + hdr.ftab);
   globals = (struct descrip *) (code + hdr.globals);
   gnames = eglobals = (struct descrip *) (code + hdr.gnames);
   statics = egnames = (struct descrip *) (code + hdr.statics);
   estatics = (struct descrip *) (code + hdr.ident);
   ident = (char *) estatics;

   /*
    * Examine the environment and make appropriate settings.
    */
   envlook();
 
   /*
    * Set up stuff for monitoring.
    */
   if (monres > 0)
      monsize = (&Pstop - &Pstart + monres - 1) / monres;
   monbuf = (int *)((int)(code + hdr.size + 1) & ~01);

   /*
    * Set up allocated memory.  The regions are:
    *	Monitoring buffer
    *	Co-expression stacks
    *	String space
    *	Heap
    *	String qualifier list
    */
    bufs = (char **) (monbuf + monsize);
   bufused = (FILE **) (bufs + numbufs);
   stacks = (int *)(((int)(bufused + numbufs) + 63)  & ~077);
   estacks = stacks + nstacks * stksize;
   sfree = strings = (char *)((int)(estacks + 63) & ~077);
   hpfree = hpbase = estrings = (char *)((int)(strings + ssize + 63) & ~077);
   sqlist = sqfree = esqlist =
    (struct descrip **)(maxheap = (char *)((int)(hpbase + hpsize + 63) & ~077));

   /*
    * Try to move the break back to the end of memory to allocate (the
    *  end of the string qualifier list) and die if the space isn't
    *  available.
    */
   if (brk(esqlist))
      error("insufficient memory");
   
   /*
    * Read the interpretable code and data into memory.
    */
   if ((cbread = read(f, code, hdr.size)) != hdr.size) {
      fprintf(stderr,"Tried to read %d bytes of code, and got %d\n",
        hdr.size,cbread);
      error("can't read interpreter code");
      }
   close(f);

   /*
    * Resolve references from icode to runtime system.
    */
   resolve();

   /*
    * Establish linked list of free co-expression stacks.  esfree
    *  is the base.
    */
   esfree = NULL;
   for (i = nstacks-1; i >= 0; i--) {
      *(stacks + (i * stksize)) = (int) esfree;
      esfree = stacks + (i * stksize);
      *(esfree+(stksize-sizeof(struct b_estack)/WORDSIZE)) = T_ESTACK;
      }

   /*
    * Mark all buffers as available.
    */
   for (i = 0; i < numbufs; i++)
      bufused[i] = NULL;

   /*
    * Buffer stdin if a buffer is available.
    */
   if (numbufs >= 1) {
      setbuf(stdin, bufs[0]);
      bufused[0] = stdin;
      }
   else
      setbuf(stdin, NULL);

   /*
    * Buffer stdout if a buffer is available.
    */
   if (numbufs >= 2) {
      setbuf(stdout, bufs[1]);
      bufused[1] = stdout;
      }
   else
      setbuf(stdout, NULL);
   
   /*
    * Buffer stderr if a buffer is available.
    */
   if (numbufs >= 3 && !noerrbuf) {
      setbuf(stderr, bufs[2]);
      bufused[2] = stderr;
      }
   else
      setbuf(stderr, NULL);

   /*
    * Point &main at the stack for the main procedure and set current,
    *  the pointer to the current co-expression to &main.
    */
   k_main.type = D_ESTACK;
   BLKLOC(k_main) = (union block *) &mainhead;
   current = k_main;

#ifdef AZ_NEVER
   /*
    * Turn on monitoring if so directed.
    */
   if (monres > 0)
      monitor(&Pstart, &Pstop, monbuf, monsize, 0);
#endif AZ_NEVER

   /*
    * Get startup time.
    */
   times(&tp);
   starttime = tp.tms_utime;
   }

/*
 * Check for environment variables that Icon uses and set system
 *  values as is appropriate.
 */
envlook()
   {
   register char *p;
   extern char *getenv();

   if ((p = getenv("TRACE")) != NULL && *p != '\0')
      k_trace = atoi(p);
   if ((p = getenv("NBUFS")) != NULL && *p != '\0')
      numbufs = atoi(p);
   if ((p = getenv("NSTACKS")) != NULL && *p != '\0')
      nstacks = atoi(p);
   if ((p = getenv("STKSIZE")) != NULL && *p != '\0')
      stksize = atoi(p);
   if ((p = getenv("STRSIZE")) != NULL && *p != '\0')
      ssize = atoi(p);
   if ((p = getenv("HEAPSIZE")) != NULL && *p != '\0')
      hpsize = atoi(p);
#ifdef AZ_NEVER
   if ((p = getenv("PROFILE")) != NULL && *p != '\0')
      monres = atoi(p);
#endif AZ_NEVER
   if ((p = getenv("ICONCORE")) != NULL) {
      signal(SIGFPE, SIG_DFL);
      signal(SIGSEGV, SIG_DFL);
      dodump++;
      }
   if ((p = getenv("NOERRBUF")) != NULL)
      noerrbuf++;
   }

/*
 * Produce run-time error 204 on floating point traps.
 */
fpetrap()
   {
   runerr(204, NULL);
   }

/*
 * Produce run-time error 304 on segmentation faults.
 */
segvtrap()
   {
   runerr(304, NULL);
   }

/*
 * error - print error message s, used only in startup code.
 */
error(s)
char *s;
   {
   if (line > 0)
      fprintf(stderr, "error at line %d in %s\n%s\n", line, file, s);
   else
      fprintf(stderr, "error in startup code\n%s\n", s);
   fflush(stderr);
   if (dodump)
      abort();
   c_exit(2);
   }

/*
 * syserr - print s as a system error.
 */
syserr(s)
char *s;
   {
   if (line > 0)
      fprintf(stderr, "System error at line %d in %s\n%s\n", line, file, s);
   else
      fprintf(stderr, "System error in startup code\n%s\n", s);
   fflush(stderr);
   if (dodump)
      abort();
   c_exit(2);
   }

/*
 * errtab maps run-time error numbers into messages.
 */
struct errtab {
   int errno;
   char *errmsg;
   } errtab[] = {
#include "../h/err.h"
   0,   0
   };

/*
 * runerr - print message corresponding to error n and if v is non-null,
 *  print it as the offending value.
 */
runerr(n, v)
register int n;
struct descrip *v;
   {
   register struct errtab *p;

   if (line > 0)
      fprintf(stderr, "Run-time error %d at line %d in %s\n", n, line, file);
   else
      fprintf(stderr, "Run-time error %d in startup code\n", n);
   for (p = errtab; p->errno > 0; p++)
      if (p->errno == n) {
         fprintf(stderr, "%s\n", p->errmsg);
         break;
         }
   if (v != NULL) {
      fprintf(stderr, "offending value: ");
      outimage(stderr, v, 0);
      putc('\n', stderr);
      }
   fflush(stderr);
   if (dodump)
      abort();
   c_exit(2);
   }

/*
 * External declarations for blocks of built-in procedures.
 */
extern struct b_proc
#define PDEF(p) B/**/p,
#include "../h/pdef.h"
	interp; /* Hack to avoid ,; at end */
#undef PDEF

/*
 * Array of addresses of blocks for built-in procedures.  It is important
 *  that this table and the one in link/builtin.c agree; the linker
 *  supplies iconx with indices into this array.
 */
struct b_proc *functab[] = {
#define PDEF(p) &B/**/p,
#include "../h/pdef.h"
#undef PDEF
   0
   };

/*
 * resolve - perform various fixups on the data read from the interpretable
 *  file.
 */
resolve()
   {
   register int i;
   register struct b_proc *pp;
   register struct descrip *dp;
   extern mkrec();

   /*
    * Scan the global variable list for procedures and fill in appropriate
    *  addresses.
    */
   for (dp = globals; dp < eglobals; dp++) {
      if (TYPE(*dp) != T_PROC)
         continue;
      /*
       * The second word of the descriptor for procedure variables tells
       *  where the procedure is.  Negative values are used for built-in
       *  procedures and positive values are used for Icon procedures.
       */
      i = INTVAL(*dp);
      if (i < 0) {
         /*
          * *dp names a built-in function, negate i and use it as an index
          *  into functab to get the location of the procedure block.
          */
         BLKLOC(*dp) = (union block *) functab[-i-1];
         }
      else {
         /*
          * *dp names an Icon procedure or a record.  i is an offset to
          *  location of the procedure block in the code section.  Point
          *  pp at the block and replace BLKLOC(*dp).
          */
         pp = (struct b_proc *) (code + i);
         BLKLOC(*dp) = (union block *) pp;
         /*
          * Relocate the address of the name of the procedure.
          */
         STRLOC(pp->pname) += (int)ident;
         if (pp->ndynam == -2)
            /*
             * This procedure is a record constructor.  Make its entry point
             *  be the entry point of mkrec().
             */
            pp->entryp = EntryPoint(mkrec);
         else {
            /*
             * This is an Icon procedure.  Relocate the entry point and
             *  the names of the parameters, locals, and static variables.
             */
            pp->entryp = code + (int)pp->entryp;
            for (i = 0; i < pp->nparam+pp->ndynam+pp->nstatic; i++)
               STRLOC(pp->lnames[i]) += (int)ident;
            }
         }
      }
   /*
    * Relocate the names of the global variables.
    */
   for (dp = gnames; dp < egnames; dp++)
      STRLOC(*dp) += (int)ident;
   }
