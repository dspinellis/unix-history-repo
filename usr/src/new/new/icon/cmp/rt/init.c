#include "../h/gc.h"
#include <signal.h>
#include <sys/types.h>
#include <sys/times.h>
#define MAXHDRLN	100		/* max len of #! line 		    */
#define MAXHDR		1024L		/* size of autoloading header--!! must
					   agree with that in link/ulink.c */
char *file = "";			/* source program file name         */
int line = 0;				/* source program line number       */
#ifdef INT
char *code;				/* interpreter code buffer	    */
int *records;				/* ptr to record procedure blocks   */
int *ftab;				/* ptr to record/field table	    */
struct descrip *globals, *eglobals;	/* ptr to global variables	    */
struct descrip *gnames, *egnames;	/* ptr to global variable names	    */
struct descrip *statics, *estatics;	/* ptr to static variables	    */
char *ident;				/* ptr to identifier table	    */
#endif INT
int *monbuf;				/* monitor buffer for profiling     */
int monres = 0;				/* resolution of monitor buffer     */
int monsize = 0;			/* size of monitor buffer           */

int numbufs = NUMBUF;			/* number of i/o buffers            */
char (*bufs)[BUFSIZ];			/* pointer to buffers               */
FILE **bufused;				/* pointer to buffer use markers    */

int nstacks = MAXSTACKS;		/* initial number of coexpr stacks  */
int stksize = STACKSIZE;		/* coexpression stack size          */
int dodump;				/* if non-zero, core dump on error */
int noerrbuf;				/* if non-zero, DON'T buffer stderr */
int *stacks;                            /* start of stack space             */
int *estacks;                           /* end of stack space               */
int *esfree;                            /* stack space free list pointer    */

int ssize = MAXSTRSPACE;                /* initial string space size (bytes)*/
char *strings;                          /* start of string space            */
char *estrings;                         /* end of string space              */
char *sfree;                            /* string space free pointer        */

int hpsize = MAXHEAPSIZE;               /* initial heap size (bytes)        */
char *hpbase;                           /* start of heap                    */
char *maxheap;                          /* end of heap storage              */
char *hpfree;                           /* heap free space pointer          */
unsigned heapneed;			/* stated need for heap space       */
unsigned strneed;			/* stated need for string space     */

struct descrip **sqlist;		/* string qualifier list            */
struct descrip **sqfree;		/* s. q. list free pointer          */
struct descrip **esqlist;		/* end of s. q. list                */

struct descrip current;                 /* current expression stack pointer */

struct b_cset  k_ascii = {              /* &ascii         */
   T_CSET,                              /*   type         */
   cset_display(~0, ~0, ~0, ~0, ~0, ~0, ~0, ~0,      /*   128 bits on  */
                 0,  0,  0,  0,  0,  0,  0,  0)      /*   128 bits off */
   };

struct b_cset  k_cset = {               /* &cset          */
   T_CSET,                              /*   type         */
   cset_display(~0, ~0, ~0, ~0, ~0, ~0, ~0, ~0,      /*   256 bits on  */
                ~0, ~0, ~0, ~0, ~0, ~0, ~0, ~0)
   };

struct b_file  k_errout = {             /* &errout               */
   T_FILE,                              /*   type                */
   stderr,                              /*   stream              */
   FS_WRITE,                            /*   status              */
   7,                                   /*   length of file name */
   /*"&errout",*/                       /*   file name, fix in init */
   };

struct b_file  k_input = {              /* &input                */
   T_FILE,                              /*   type                */
   stdin,                               /*   stream              */
   FS_READ,                             /*   status              */
   6,                                   /*   length of file name */
   /*"&input",*/                        /*   file name, fix in init */
   };

struct b_cset  k_lcase = {              /* &lcase         */
   T_CSET,                              /*   type         */
   cset_display( 0,  0,  0,  0,  0,  0,~01, 03777,   /*   lower case   */
                 0,  0,  0,  0,  0,  0,  0,  0)      /*   letters only */
   };

int            k_level = 0;             /* &level */

struct descrip k_main;                  /* &main  */

int            k_pos = 1;               /* &pos   */

struct b_file  k_output = {             /* &output               */
   T_FILE,                              /*   type                */
   stdout,                              /*   stream              */
   FS_WRITE,                            /*   status              */
   7,                                   /*   length of file name */
   /*"&output",*/                       /*   file name, fix in init */
   };

long           k_random = 0L;           /* &random */

struct descrip k_subject = {		/* &subject          */
   0,                                   /*   string length   */
   /*1,*/                               /*   string location */
   };
#ifdef INT
int k_trace = 0;
#endif INT
struct b_cset  k_ucase = {              /* &ucase         */
   T_CSET,                              /*   type         */
    cset_display(0,  0,  0,  0,~01, 03777, 0, 0,     /*   upper case   */
                 0,  0,  0,  0,  0,  0,  0,  0)      /*   letters only */
   };

struct descrip maps2 = {                /* save space for 2nd arg to map() */
   D_NULL,                              /*   string length                 */
   /*0,*/                               /*   string location               */
   };

struct descrip maps3 = {                /* save space for 3rd arg to map() */
   D_NULL,                              /*   string length                 */
   /*0,*/                               /*   string location               */
   };

long starttime;                         /* starttime of job in milliseconds */

struct descrip nulldesc = {D_NULL, /*0*/};    /* universal &null */
struct descrip zerodesc = {D_INTEGER, /*0*/}; /* universal 0     */
struct descrip onedesc  = {D_INTEGER, /*1*/}; /* universal 1     */
struct descrip nullstr  = {0, /*""*/};        /* universal null string */
struct descrip blank    = {1, /*" "*/};       /* universal blank */
struct descrip letr     = {1, /*"r"*/};       /* universal letter 'r' */
struct descrip input    = {D_FILE, /*&k_input*/};  /* universal input file */
struct descrip errout   = {D_FILE, /*&k_errout*/}; /* universal error file */
struct descrip lcase    = {26, /*lowercase*/};/* universal lower case */
struct descrip ucase    = {26, /*uppercase*/};/* universal upper case */

static struct b_estack mainhead;        /* expression stack head for main */

/*
 * init - initialize memory and prepare for Icon execution
 */

#ifdef INT
init(nargs,name)
int nargs;
char *name;
#endif INT
#ifdef CMP
init()
#endif CMP
   {
   register int i;
#ifdef INT
   int cbread;
   int f;
   FILE *ufile;
   char uheader[MAXHDRLN];
   int directex;
   struct header {
      int size;
      int trace;
      int records;
      int ftab;
      int globals;
      int gnames;
      int statics;
      int ident;
      } hdr;
#endif INT
   struct tms tp;
   extern char *brk(), end;
   extern char Pstart, Pstop;
   extern fpetrap(), segvtrap();

   signal(SIGFPE, fpetrap);		/* catch floating point traps */
   signal(SIGSEGV, segvtrap);		/* catch memory faults */

   /* initialization fix-ups */
      STRLOC(k_errout.fname) = "&errout";
      STRLOC(k_input.fname)  = "&input";
      STRLOC(k_output.fname) = "&output";
      STRLOC(k_subject)      = 1;
      STRLOC(maps2)          = 0;
      STRLOC(maps3)          = 0;
      STRLOC(nulldesc)       = 0;
      INTVAL(zerodesc)       = 0;
      INTVAL(onedesc)        = 1;
      STRLOC(nullstr)        = "";
      STRLOC(blank)          = " ";
      STRLOC(letr)           = "r";
      BLKLOC(input)    	     = &k_input;
      BLKLOC(errout)         = &k_errout;
      STRLOC(lcase)          = "abcdefghijklmnopqrstuvwxyz";
      STRLOC(ucase)	     = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";

      mainhead.type = T_ESTACK;
      mainhead.activator.type = D_NULL;
      STRLOC(mainhead.activator) = NULL;
#ifdef VAX
      mainhead.sbase = (int *)(0x7fffffff);
#endif VAX
#ifdef PDP11
      mainhead.sbase = (int *)(0177776);
#endif PDP11
      mainhead.sp = NULL;
      mainhead.boundary = NULL;
      mainhead.nresults = 0;
      mainhead.freshblk.type = D_NULL;
      STRLOC(mainhead.freshblk)  = 0;

#ifdef INT
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
   if (strncmp(uheader,"#! ",3) != 0) {
   	fseek(ufile,MAXHDR,0);
	fgets(uheader,MAXHDRLN,ufile);
	if (strncmp(uheader,"#! ",3) == 0)
		lseek(f,MAXHDR+(long)strlen(uheader),0);
	else
		error("invalid format for interpretable file");
	}
   else
        lseek(f,(long)strlen(uheader),0);

   if (read(f, &hdr, sizeof hdr) != sizeof hdr)
      error("can't read interpreter file header");
    
   code = &end;
   k_trace = hdr.trace;
   records =            code + hdr.records;
   ftab    =            code + hdr.ftab;
   globals =            code + hdr.globals;
   gnames  = eglobals = code + hdr.gnames;
   statics = egnames  = code + hdr.statics;
   ident   = estatics = code + hdr.ident;
#endif INT

   envlook();

   if (monres > 0)
      monsize = (&Pstop - &Pstart + monres - 1) / monres;

#ifdef INT
   monbuf = (int *)((int)(code + hdr.size + 1) & ~01); /* ??-whm */
#endif INT
#ifdef CMP
   monbuf = &end;
#endif CMP
   bufs = monbuf + monsize;
   bufused = bufs + numbufs;
   stacks = (char *)(((int)(bufused + numbufs) + 63)  & ~077);
   estacks = stacks + nstacks * stksize;
   sfree = strings = (char *)((int)(estacks + 63) & ~077);
   hpfree = hpbase = estrings = (char *)((int)(strings + ssize + 63) & ~077);
   sqlist = sqfree = esqlist =
    (struct descrip **)(maxheap = (char *)((int)(hpbase + hpsize + 63) & ~077));

   if (brk(esqlist))
#ifdef INT
      error("insufficient memory");
#endif INT
#ifdef CMP
      syserr("insufficient memory");
#endif CMP
   
#ifdef INT
   if ((cbread = read(f, code, hdr.size)) != hdr.size) {
      fprintf(stderr,"Tried to read %d bytes of code, and got %d\n",hdr.size,cbread);
      error("can't read interpreter code");
      }
   close(f);
   resolve();	/* resolve references from ucode to runtime system */
#endif INT

   esfree = NULL;
   for (i = nstacks-1; i >= 0; i--) {   /* set stack space free list */
      *(stacks + (i * stksize)) = esfree;
      esfree = stacks + (i * stksize);
      *(esfree+(stksize-sizeof(struct b_estack)/ADDRSIZE)) = T_ESTACK;
      }

   for (i = 0; i < numbufs; i++)
      bufused[i] = NULL;

   if (numbufs >= 1) {
      setbuf(stdin, bufs[0]);
      bufused[0] = stdin;
      }
   else
      setbuf(stdin, NULL);
/*
 * We're going to try buffering stdout even if it's a terminal,
 *  whm--Fri Feb 25 02:00:08 1983
 *
 * if (!isatty(fileno(stdout)) && numbufs >= 2) {
 */
   if (numbufs >= 2) {
      setbuf(stdout, bufs[1]);
      bufused[1] = stdout;
      }
   else
      setbuf(stdout, NULL);
   
   /*
    * Going to try buffering stderr for a while,
    *  whm--Tue Mar  1 02:01:48 1983
    *   setbuf(stderr, NULL);
    */
   if (numbufs >= 3 && !noerrbuf) {
      setbuf(stderr, bufs[2]);
      bufused[2] = stderr;
      }
   else
      setbuf(stderr, NULL);

   k_main.type = D_ESTACK;      /* point to expression stack header for main */
   BLKLOC(k_main) = &mainhead;

   current = k_main;    /* start in expression stack for main procedure */

   if (monres > 0)
      monitor(&Pstart, &Pstop, monbuf, monsize, 0);

   times(&tp);          /* get startup time */
   starttime = tp.tms_utime;
   }

envlook()
   {
   register char *p;

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
   if ((p = getenv("PROFILE")) != NULL && *p != '\0')
      monres = atoi(p);
   if ((p = getenv("ICONCORE")) != NULL) {
      signal(SIGFPE, SIG_DFL);		/* catch floating point traps */
      signal(SIGSEGV, SIG_DFL);		/* catch memory faults */
      dodump++;
      }
   if ((p = getenv("NOERRBUF")) != NULL)
      noerrbuf++;
   }

fpetrap()
   {
   runerr(204, NULL);
   }

segvtrap()
   {
   runerr(304, NULL);
   }
#ifdef INT
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
#endif INT

syserr(s)
char *s;
   {
#ifdef INT
   if (line > 0)
      fprintf(stderr, "System error at line %d in %s\n%s\n", line, file, s);
   else
      fprintf(stderr, "System error in startup code\n%s\n", s);
#endif INT
#ifdef CMP
   fprintf(stderr, "System error at line %d in %s\n%s\n", line, file, s);
#endif CMP
   fflush(stderr);
   if (dodump)
      abort();
   c_exit(2);
   }

struct errtab {
   int errno;
   char *errmsg;
   } errtab[] = {
#include "../h/err.h"
   0,   0
   };

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
#ifdef INT
extern struct b_proc
   Babs,           Bany,           Bbal,           Bcenter,
   Bclose,         Bcopy,          Bcset,          Bdisplay,
   Bexit,          Bfind,          Bget,           Bimage,
   Binteger,       Bleft,          Blist,
   Bmany,          Bmap,           Bmatch,         Bmove,
   Bnumeric,       Bopen,          Bpop,           Bpos,
   Bpull,          Bpush,          Bput,           Bread,
   Breads,         Breal,          Brepl,          Breverse,
   Bright,         
#ifdef EXT
   Bseq,
#endif EXT
   Bsort,          Bstop,          Bstring,
   Bsystem,        Btab,           Btable,         Btrim,
   Btype,          Bupto,          Bwrite,         Bwrites;

struct b_proc *functab[] = {
   &Babs,          &Bany,          &Bbal,          &Bcenter,
   &Bclose,        &Bcopy,         &Bcset,         &Bdisplay,
   &Bexit,         &Bfind,         &Bget,          &Bimage,
   &Binteger,      &Bleft,         &Blist,
   &Bmany,         &Bmap,          &Bmatch,        &Bmove,
   &Bnumeric,      &Bopen,         &Bpop,          &Bpos,
   &Bpull,         &Bpush,         &Bput,          &Bread,
   &Breads,        &Breal,         &Brepl,         &Breverse,
   &Bright,
#ifdef EXT
   &Bseq,
#endif EXT
   &Bsort,         &Bstop,         &Bstring,
   &Bsystem,       &Btab,          &Btable,        &Btrim,
   &Btype,         &Bupto,         &Bwrite,        &Bwrites
   };

resolve()
   {
   register int i;
   register struct b_proc *pp;
   register struct descrip *dp;
   extern mkrec();

   /* adjust global variables to point to procedure and function blocks */

   for (dp = globals; dp < eglobals; dp++) {
      if (TYPE(*dp) != T_PROC)
	 continue;
      i = INTVAL(*dp);
      if (i < 0) {
	 /* function */
	 /* set pointer to proper procedure block */

	 BLKLOC(*dp) = functab[-i-1];
	 }
      else {
	 /* procedure or record */
	 /* relocate pointer to procedure block */
	 /* if record, set entry point to mkrec() */
	 /* if procedure, relocate entry point */
	 /* relocate pointers to identifier table */

	 pp = code + i;
	 BLKLOC(*dp) = pp;
	 STRLOC(pp->pname) += (int)ident;
	 if (pp->ndynam == -2)
	    pp->entryp = EntryPoint(mkrec);/* jump past entry mask of mkrec */
	 else {
	    pp->entryp = code + (int)pp->entryp;
	    for (i = 0; i < pp->nparam+pp->ndynam+pp->nstatic; i++)
	       STRLOC(pp->lnames[i]) += (int)ident; /* ??-whm */
	    }
	 }
      }
   for (dp = gnames; dp < egnames; dp++)
      STRLOC(*dp) += (int)ident; /* ??-whm */
   }
#endif INT
