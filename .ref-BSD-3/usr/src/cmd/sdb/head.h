
#include <stdio.h>
#include <setjmp.h>
#include "sgtty.h"
#include <sys/vm.h>
#define	PAGSIZ	(CLSIZE*NBPG)
#include "bio.h"
#include "defs.h"

/* input line decoding */
char	proc[30];	/* procedure name */
int	integ;		/* count or number in input */
char 	cmd;		/* command letter */
char	re[128];	/* regular expression */
char	args[128];	/* arguments */
char	*argsp;		/* pointer to args */
char	var[60];	/* variable name */
int	scallf;		/* set to 1 iff procedure call */
int	reflag;		/* set to 1 iff re */
int	redir;		/* set to 1 iff forward search */
int	colonflag;	/* set to 1 iff colon typed */

/* source file i/o */
char	curfile[30];	/* name of file being edited */
int	fline;		/* line number in file */
int	maxfline;	/* maximum line number in file, 0 if unknown */
struct brbuf  fiobuf;	/* file descriptor */
char	fbuf[256];	/* current line from file */
char 	filework[128];	/* place to put filename */
char 	*fp;		/* pointer to it */
int	nolines;	/* set to 1 iff no lines in file */

/* returned by slookup */
char	sl_name[8];
char	sl_class;
short	sl_type;
int	sl_size, sl_addr;
int	subflag;

/* procedure call information */
int scallx;			/* procedure call in progress */
ADDR fps, aps, pcs;		/* old stack frame */
BKPTR bkpts;			/* old breakpoint */
int flagss;			/*  and its flags */
char dschar;			/* '/' if value should be displayed */

/* symbol table info */
long	ststart;		/* offset of symbol table in a.out */
struct brbuf sbuf;		/* buffer for symbol table */
long	extstart;		/* offset of first external in a.out */

/* address info */
ADDR	dot;			/* current address */
ADDR	callpc, frame, argp;	/* current stack frame */

/* other */
char	odesc[10];		/* descriptor of last displayed variable */
ADDR	oaddr;			/* address of last displayed variable */
char	otype;			/* type of last displayed variable */
char	oclass;			/* class of last displayed variable */
char	oincr;			/* size of last displayed variable */
struct sgttyb sdbttym, userttym;
				/* tty modes for sdb and user */
char	oldargs[128];
char prname[50];		/* print name used by outvar */
jmp_buf	env;			/* environment for setjmp, longjmp */
#define	WINDOW	10		/* window size for display commands */
#define	COMMANDS	"\004\"+-=!/BCDRSTabcdegklmpqrstwzVXY"
				/* each sdb command must appear here */
int	debug;			/* toggled by Y command */
time_t	symtime;		/* modification time of symfil */

#define	STABMASK	0376

#define NUMARGS 16		/* number of args allowed in sub call */
#define SUBSTSP 512		/* length of space for sub args and strings */
#define WORDSIZE 4		/* wordsize in bytes on this machine */

#define BIGNUM	0x7fffffff
#define MAXADDR 1L<<30

struct filet {
	char	sfilename[31];	/* source file name */
	char	lineflag;	/* set iff this is a '#line' file */
	ADDR	faddr;		/* address in core */
	long	stf_offset;	/* offset in a.out */
} *files, *badfile;

struct proct {
	char	pname[8];	/* procedure name */
	ADDR	paddr;		/* address in core */
	long	st_offset;	/* offset in a.out */
	struct filet  *sfptr;	/* source file name pointer */
	int	lineno;		/* line number in source file */
	char	entrypt;	/* 1 iff a F77 entry */
} *procs, *badproc;


#define PROCINCR 20
#define FILEINCR 10

#define varchar(x)	((x>='A' && x<='Z') || (x>='a' &&  x<='z') || x == '_' || x == '.' || x == '[' || x == ']' || x == '-' || x == '>' || x == '*' || x == '?')
#define number(x)	(x >= '0' && x <= '9')

char *readline(), readchar(), rdc();
char *cpname();
char *cpstr(), *cpall();
char *sbrk();
char *typetodesc();
int octdigit(), decdigit(); hexdigit();
int octconv(), decconv(); hexconv();
long readint(), rint();
long adrtostoffset();
long getval(), argvalue();
long slookup(), globallookup();
ADDR varaddr(), dispvar();
ADDR extaddr(), formaddr(), stackreg();
struct proct *curproc();
struct proct *findproc();
struct proct *adrtoproc();
struct proct *initframe(), *nextframe();
struct filet *findfile(), *adrtofilep();
