/*
 * random stuff for atariST
 */

#include "EXTERN.h"
#include "perl.h"

/* call back stuff, atari specific stuff below */
/* Be sure to refetch the stack pointer after calling these routines. */

int
callback(subname, sp, gimme, hasargs, numargs)
char *subname;
int sp;			/* stack pointer after args are pushed */
int gimme;		/* called in array or scalar context */
int hasargs;		/* whether to create a @_ array for routine */
int numargs;		/* how many args are pushed on the stack */
{
    static ARG myarg[3];	/* fake syntax tree node */
    int arglast[3];
    
    arglast[2] = sp;
    sp -= numargs;
    arglast[1] = sp--;
    arglast[0] = sp;

    if (!myarg[0].arg_ptr.arg_str)
	myarg[0].arg_ptr.arg_str = str_make("",0);

    myarg[1].arg_type = A_WORD;
    myarg[1].arg_ptr.arg_stab = stabent(subname, FALSE);

    myarg[2].arg_type = hasargs ? A_EXPR : A_NULL;

    return do_subr(myarg, gimme, arglast);
}

int
callv(subname, sp, gimme, argv)
char *subname;
register int sp;	/* current stack pointer */
int gimme;		/* called in array or scalar context */
register char **argv;	/* null terminated arg list, NULL for no arglist */
{
    register int items = 0;
    int hasargs = (argv != 0);

    astore(stack, ++sp, Nullstr);	/* reserve spot for 1st return arg */
    if (hasargs) {
	while (*argv) {
	    astore(stack, ++sp, str_2mortal(str_make(*argv,0)));
	    items++;
	    argv++;
	}
    }
    return callback(subname, sp, gimme, hasargs, items);
}

#include <process.h>
#include <stdio.h>

long _stksize = 64*1024L;
unsigned long __DEFAULT_BUFSIZ__ = 4 * 1024L;

/*
 * The following code is based on the do_exec and do_aexec functions
 * in file doio.c
 */
int
do_aspawn(really,arglast)
STR *really;
int *arglast;
{
    register STR **st = stack->ary_array;
    register int sp = arglast[1];
    register int items = arglast[2] - sp;
    register char **a;
    char **argv;
    char *tmps;
    int status;

    if (items) {
	New(1101,argv, items+1, char*);
	a = argv;
	for (st += ++sp; items > 0; items--,st++) {
	    if (*st)
		*a++ = str_get(*st);
	    else
		*a++ = "";
	}
	*a = Nullch;
	if (really && *(tmps = str_get(really)))
	    status = spawnvp(-P_WAIT,tmps,argv); /* -P_WAIT is a hack, see spawnvp.c in the lib */
	else
	    status = spawnvp(-P_WAIT,argv[0],argv);
	Safefree(argv);
    }
    return status;
}


int
do_spawn(cmd)
char *cmd;
{
    return system(cmd);
}

#if 0 /* patchlevel 79 onwards we can */
/*
 * we unfortunately cannot use the super efficient fread/write from the lib
 */
size_t fread(void *data, size_t size, size_t count, FILE *fp)
{
    size_t i, j;
    unsigned char *buf = (unsigned char *)data;
    int c;

    for(i = 0; i < count; i++)
    {
	for(j = 0; j < size; j++)
	{
	    if((c = getc(fp)) == EOF)
	       return 0;
	    *buf++ = c;
        }
    }
    return i;
}

size_t fwrite(const void *data, size_t size, size_t count, FILE *fp)
{
    size_t i, j;
    const unsigned char *buf = (const unsigned char *)data;

    for(i = 0; i < count; i++)
    {
	for(j = 0; j < size; j++)
	{
	    if(fputc(*buf++, fp) == EOF)
	       return 0;
        }
    }
    return i;
}
#endif

#ifdef HAS_SYSCALL
#define __NO_INLINE__
#include <osbind.h> /* must include this for proper protos */

/* these must match osbind.pl */
#define TRAP_1_W		1
#define TRAP_1_WW		2
#define TRAP_1_WL		3
#define TRAP_1_WLW		4
#define TRAP_1_WWW		5
#define TRAP_1_WLL		6
#define TRAP_1_WWLL		7
#define TRAP_1_WLWW		8
#define TRAP_1_WWLLL		9
#define TRAP_13_W		10
#define TRAP_13_WW		11
#define TRAP_13_WL		12
#define TRAP_13_WWW		13
#define TRAP_13_WWL		14
#define TRAP_13_WWLWWW		15
#define TRAP_14_W		16
#define TRAP_14_WW		17
#define TRAP_14_WL		18
#define TRAP_14_WWW		19
#define TRAP_14_WWL		20
#define TRAP_14_WWLL		21
#define TRAP_14_WLLW		22
#define TRAP_14_WLLL		23
#define TRAP_14_WWWL		24
#define TRAP_14_WWWWL		25
#define TRAP_14_WLLWW		26
#define TRAP_14_WWWWWWW		27
#define TRAP_14_WLLWWWWW	28
#define TRAP_14_WLLWWWWLW	29
#define TRAP_14_WLLWWWWWLW	30

int syscall(trap, fn, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12 )
unsigned long trap, fn, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12;
{
  /* for now */
  switch(trap)
  {
    case TRAP_1_W:
      return trap_1_w(fn);
      
    case TRAP_1_WW:
      return trap_1_ww(fn, a1);
      
    case TRAP_1_WL:
      return trap_1_wl(fn, a1);
      
    case TRAP_1_WLW:
      return trap_1_wlw(fn, a1, a2);
      
    case TRAP_1_WWW:
      return trap_1_www(fn, a1, a2);
      
    case TRAP_1_WLL:
      return trap_1_wll(fn, a1, a2);
      
    case TRAP_1_WWLL:
      return trap_1_wwll(fn, a1, a2, a3);
      
    case TRAP_1_WLWW:
      return trap_1_wlww(fn, a1, a2, a3);
      
    case TRAP_1_WWLLL:
      return trap_1_wwlll(fn, a1, a2, a3, a4);
      
    case TRAP_13_W:
      return trap_13_w(fn);
      
    case TRAP_13_WW:
      return trap_13_ww(fn, a1);
      
    case TRAP_13_WL:
      return trap_13_wl(fn, a1);
      
    case TRAP_13_WWW:
      return trap_13_www(fn, a1, a2);
      
    case TRAP_13_WWL:
      return trap_13_wwl(fn, a1, a2);
      
    case TRAP_13_WWLWWW:
      return trap_13_wwlwww(fn, a1, a2, a3, a4, a5);
      
    case TRAP_14_W:
      return trap_14_w(fn);
      
    case TRAP_14_WW:
      return trap_14_ww(fn, a1);
      
    case TRAP_14_WL:
      return trap_14_wl(fn, a1);
      
    case TRAP_14_WWW:
      return trap_14_www(fn, a1, a2);
      
    case TRAP_14_WWL:
      return trap_14_wwl(fn, a1, a2);
      
    case TRAP_14_WWLL:
      return trap_14_wwll(fn, a1, a2, a3);
      
    case TRAP_14_WLLW:
      return trap_14_wllw(fn, a1, a2, a3);
      
    case TRAP_14_WLLL:
      return trap_14_wlll(fn, a1, a2, a3);
      
    case TRAP_14_WWWL:
      return trap_14_wwwl(fn, a1, a2, a3);
      
    case TRAP_14_WWWWL:
      return trap_14_wwwwl(fn, a1, a2, a3, a4);
      
    case TRAP_14_WLLWW:
      return trap_14_wllww(fn, a1, a2, a3, a4);
      
    case TRAP_14_WWWWWWW:
      return trap_14_wwwwwww(fn, a1, a2, a3, a4, a5, a6);
      
    case TRAP_14_WLLWWWWW:
      return trap_14_wllwwwww(fn, a1, a2, a3, a4, a5, a6, a7);
      
    case TRAP_14_WLLWWWWLW:
      return trap_14_wllwwwwlw(fn, a1, a2, a3, a4, a5, a6, a7, a8);
      
    case TRAP_14_WLLWWWWWLW:
      return trap_14_wllwwwwwlw(fn, a1, a2, a3, a4, a5, a6, a7, a8, a9);
  }      
}
#endif

