static char Sccsid[] = "xed.c @(#)xed.c	1.1	10/1/82 Berkeley ";
#

/*
 *	V7.15 81/09/16 14:22	Fixed bug causing bus errors occasionally
 *	V7.14 81/09/10 21:50	-B dynamically allocated line buffer
 *	V7.13 81/07/25 22:51	added -O and "long" pointers
 *
 * Editor
 *
 *	Major additions beginning	77/04
 *	Conversion to version 7		79/12
 *	Conversion to VAX		80/11
 *
 *	Purdue University, Engineering Computer Network
 *
 *	Tgi -- Room 337A
 *	Electrical Engineering Dept
 *	Purdue University
 *	West Lafayette, Indiana
 *	47907
 *	317/49-41592
 */

#define XED		/* enable powerful stuff */
#define V7		/* enable environment stuff for protocol */

/*
 *	At the entry to a function (PDP-11) the following sequence
 *	of instructions is executed:
 *		func:	jsr	r5,csv
 *		csv:	mov	r5,r0
 *		csv+2:	mov	sp,r5
 *	If a signal occurs between the first and second, or between the
 *	second and third instructions, r5 will NOT contain a valid
 *	stack address.  Hence, when longjmp() attempts to validate
 *	the environment pointer (r5) links, an instruction will be
 *	fetched instead of a valid pointer, and will generally cause
 *	one of "memory fault", "bus error", or "illegal instruction"
 *	resulting in the loss of the editing session.
 *
 *	Due to this wonderful feature of version 7 ingenuity, I have
 *	reverted to using setexit/reset, since they are simpler and
 *	do not assume that I do not know what I am doing.
 *
 *	80/11/10 V7.01 Tgi
 */

#ifndef pdp11
#include <setjmp.h>
jmp_buf	_env_rst;
#define setexit() setjmp(_env_rst)
#define reset()	longjmp(_env_rst,1)
#endif

#include <sys/types.h>
#include <sys/stat.h>

/*
 * Machine-dependent definitions
 */

#ifdef pdp11
# define BPB	8	/* bits per byte */
# define BPW	(BPB*sizeof(int))/* bits per word */
# define BPWC	4	/* log2(BPW) */
# define BPWM	017	/* mask of BPWC bits */
  typedef short	block;
  typedef short	charac;
  typedef short	filedes;
  typedef short	flag;
  typedef int	(*func)();
  typedef short	linep;
#endif

#ifdef vax
# define BPB	8	/* bits per byte */
# define BPW	(BPB*sizeof(int))/* bits per word */
# define BPWC	5	/* log2(BPW) */
# define BPWM	037	/* mask of BPWC bits */
  typedef long	block;
  typedef int	charac;
  typedef short	filedes;
  typedef char	flag;
  typedef int	(*func)();
  typedef long	linep;	/* NEW */
#endif

/*
 * conditional compilation
 */

#ifdef XED
# define AGAIN		/* enable "o" "again" command */
# define ALLOC	1024	/* line buffer size */
# define APLMAP		/* enable Apl character mapping */
# define DEBUG		/* enable "du" command */
# define DUMB	0	/* enable command to disable spcl chars */
# define EOL		/* enable special eol stuff */
# define EXTMARK	/* extended "k" capability */
# define G_VFY		/* enable verifying on "g" command */
# define HELP	"/etc/xed.doc"
# ifdef pdp11		/* only needed on 16-bit machines */
#  define HUGE		/* enable "huge" file stuff */
# endif
# define PAGE		/* enable proper line counting on ":" */
# define PARENS		/* enable "b" suffix to count parentheses */
# define PIPE		/* enable | command to pipe to process */
# define PROMPT	">"
# define STRLEN		/* enable string-length counting code */
# define TABS	((LBSIZE+sizeof(int)-1)/sizeof(int)) /* words for tab stops */
# define TTL	"XED\tV7.15"
/* #define TTL_NL	9	/* location of newline in TTL */
# define UNDO		/* enable "u"ndo command */
# define USE		/* enable "@" command */
# define XDEL		/* enable undelete stuff */
# define YINT		/* enable special interrupt processing */
#endif

/*
 * stuff for EED, instead of XED
 */

#ifndef XED
# define HELP	"/etc/eed.doc"
# define PROMPT	"*"
# define TTL	"EED"
#else
# define EEDHELP	"/etc/eed.doc"
# define EEDPROMPT "*"
# define EEDTTL	"EED"
#endif

/*
 * stuff normally enabled
 */

#define CLEAR	"\33:\33H\33J\32\14"	/* HP-2640A, Lear ADM-3A */
#define CMDS	"edsav"	/* all commands written if exists */
#ifndef DUMB
# define DUMB	1	/* enable command to disable spcl chars */
#endif

/*
 * special stuff, occasionally enabled
 */

#define LOG	"/a/tgi/etc/xed.log"	/* feature use logging */

/*
 * data #defines
 */

#define BAK	4	/* file.bak - backup() */
#define BLKSIZE	512	/* disk block size (bytes) */
#define BS1	0100000	/* stty() */
#define CBACK	14	/* back-reference: \(blah\)more\1 */
#define CBRA	1	/* \( */
#define CCHR	2	/* literal character */
#define CCL	6	/* character class [x...y] */
#define CCOUNT	(80-1)	/* terminal width */
#define CDOL	10	/* ...$ */
#define CDOT	4	/*  .  */
#define CEOF	11	/* end of pattern */
#define CKET	12	/* \) */
#define EOF	-1	/* end of file */
#define ESIZE	128	/* regular expression size */
#define FILE	0	/* no extension - backup() */
#define FNSIZE	64	/* max size of pathname to file */
#define GBSIZE	256	/* max global command length */
#define HUP	3	/* file.hup - backup() */
#define INT	2	/* file.int - backup() */
#define LBSIZE	512	/* max line length */
#define LMASK	077	/* mask for "locked" file */
#define LMODE	(MODE&~LMASK)	/* mode for "locked" file */
#define MODCNT	35	/* default mod count before auto-write */
#define MODE	0666	/* mode for normal files */
#define NBRA	9	/* number of \( \) pairs */
#define NCCL	8	/* not in character class: [^x...y] */
#define PAGSIZ	22	/* page size for ":" command */
#define READ	0	/* getblock: read function */
#define SIGBUS	10	/* Bus error */
#define SIGEMT	7	/* EMT trap */
#define SIGFPE	8	/* Floating Point Exception */
#define SIGHUP	1	/* Hangup signal */
#define SIGILL	4	/* Illegal instruction */
#define SIGINT	2	/* Interrupt signal */
#define SIGIOT	6	/* IOT trap */
#define SIGPIP	13	/* Broken pipe for ! stuff */
#define SIGQIT	3	/* Quit signal */
#define SIGSEGV	11	/* Memory fault */
#define SIGSYS	12	/* Bad system call */
#define SIGTRC	5	/* Trace/BPT for mail stuff */
#define SIGTRM	15	/* Termination */
#define STAR	1	/*  *  */
#define TABFILL	'\t'	/* fill character for tab expansion */
#define TMP	1	/* file.edt - backup() */
#define TRM	5	/* file.trm - backup() */
#define TTSIZE	(512+4)	/* terminal output buffer size */
#define WRITE	1	/* getblock: write function */
#define ever	(;;)

#define error	errfunc
#define ctrl(x)	((x)&037)

#ifdef AGAIN
char	agbuf[GBSIZE],	/* save area for "again" command */
	*agp	= 0;	/* "again" command pointer */
flag	agf	= 0;	/* "again" flag  (executing the command) */
#endif

#ifdef ALLOC
int	lbsize	= LBSIZE;/* line buffer size */
#endif

#ifdef APLMAP
flag	aplmap	= 0;	/* Apl character mapping */
#include "aplmap.h"	/* apl ADM-3A char set mapping tables */
#endif

#ifdef CKPT
char	*cfname	= "/tmp/ce00000";/* filename for checkpoint */
int	recovry	= 0,	/* non-zero to recover checkpointed session */
	tfnum;		/* index into tfname for "00000" string */
#endif

#ifdef CLEAR
char	*clears = CLEAR;/* screen-clear sequence */
flag	zflg	= 0;	/* if "stty bs1" not set */
			/* bs1 displays ctrl-z as ^Z on tty */
#endif

#ifdef CMDS
filedes	cmd	= 0;	/* file des for command-save file */
char	cmdfil[] = CMDS;/* command-save file */
#endif

#ifdef DEBUG
flag	tflg	= 0;	/* tracing flag */
#endif

#ifdef DUMB
flag	dumbf	= DUMB;	/* 1 = disable special chars in patterns */
#endif

#ifdef EOL
charac	eol	= 0;	/* "end-of-line" char for multiple commands */
			/*   per line */
flag	prompt3	= 1;	/* disable prompts for "eol" stuff */
#endif

#ifdef G_VFY
flag	gaskf	= 0;	/* verify mode on global command */
#endif

#ifdef HELP
filedes	doc	= 0;	/* "help" file descriptor */
char	*help	= HELP;	/* "help" file name */
#endif

#ifdef LOG
char	logfile[]= LOG;	/* logging use of features */
filedes	lfile	= 0;	/* logging file descriptor */
short	logamp;		/* since s/x/&/ may be done many times */
struct logstat {
	short	l_uid;	/* user id of caller */
	char	l_xed,		/* 1 if xed, 0 if eed, 2 if apled */
		l_inter;	/* 1 if interactive */
/* command features */
	short	lc_shell,	/*  !  */
		lc_pipe,	/*  |  */
		lc_piplus,	/*  |+  */
		lc_piminus,	/*  |-  */
		lc_dpipe,	/*  ||  */
		lc_pfrom,	/*  |<  */
		lc_pto,		/*  |>  */
		lc_at,		/*  @  */
		lc_colon,	/*  :  */
		lc_star,	/*  *  */
		lc_clnminus,	/*  :-  */
		lc_comment,	/*  : stuff  or  * stuff  */
		lc_append,	/*  a  */
		lc_abort,	/*  abort  */
		lc_aspace,	/*  a line  */
		lc_aslash,	/*  a/string/  */
		lc_browse,	/*  bN  */
		lc_change,	/*  c  */
		lc_cslash,	/*  c/s1/s2/  */
		lc_copy,	/*  coNN  */
		lc_delete,	/*  d  */
		lc_depth,	/*  d=NN  */
		lc_directory,	/*  d path  */
		lc_edit,	/*  e file  */
		lc_eol,		/*  e=C  */
		lc_errmsg,	/*  eNN  */
		lc_exp,		/*  exp  */
		lc_eplus,	/*  e+  */
		lc_eminus,	/*  e-  */
		lc_fshow,	/*  f  */
		lc_fset,	/*  f file  */
		lc_fillset,	/*  f=C  */
		lc_global,	/*  g/str/cmd  */
		lc_gvfy,	/*  g/str/vcmd  */
		lc_header,	/*  h  */
		lc_help,	/*  help  */
		lc_insert,	/*  i  */
		lc_islash,	/*  i/string/  */
		lc_join,	/*  j  */
		lc_jglue,	/*  j/glue/  */
		lc_klist,	/*  k  */
		lc_kset,	/*  kC  */
		lc_list,	/*  l  */
		lc_move,	/*  mNN  */
		lc_moove,	/*  moNN  */
		lc_magic,	/*  m  */
		lc_numbers,	/*  n  */
		lc_numinus,	/*  n-  */
		lc_numplus,	/*  n+  */
		lc_o,		/*  o ^Q  */
		lc_print,	/*  p  */
		lc_pprint,	/*  pp  */
		lc_quit,	/*  q  */
		lc_qimm,	/*  qi  */
		lc_quote,	/*  q=C  */
		lc_read,	/*  r  */
		lc_substitute,	/*  s/s1/s2/  */
		lc_stop,	/*  s  */
		lc_savecount,	/*  saNN  */
		lc_tablist,	/*  t  */
		lc_tabset,	/*  t,NN  */
		lc_tabchar,	/*  t=C  */
		lc_transfer,	/*  tNN  */
		lc_undo,	/*  u  */
		lc_vglobal,	/*  v/str/cmd  */
		lc_write,	/*  w  */
		lc_wonto,	/*  w>  */
		lc_wimm,	/*  wi  */
		lc_width,	/*  w=NN  */
		lc_xundelete,	/*  x  */
		lc_yintr,	/*  y  */
		lc_yminus,	/*  y-  */
		lc_yplus,	/*  y+  */
/* address features */
		la_dot,		/*  .  */
		la_dotdot,	/*  ..  */
		la_dol,		/*  $  */
		la_num,		/*  NN  */
		la_plus,	/*  +  */
		la_minus,	/*  -  */
		la_caret,	/*  ^  */
		la_quote,	/*  'a  */
		la_letter,	/*  A  */
		la_slash,	/*  /str/  */
		la_query,	/*  ?str?  */
		la_equal,	/*  =  */
/* pattern features */
		lp_caret,	/*  ^  */
		lp_dol,		/*  $  */
		lp_dot,		/*  .  */
		lp_star,	/*  *  */
		lp_ccl,		/*  [ ]  */
		lp_nccl,	/*  [^ ]  */
		lp_paren,	/*  \( \)  */
		lp_digit,	/*  \1 \2 \3 ...  */
/* substitution features */
		lp_amp,		/*  &  */
/* miscellaneous features */
		lm_quote,	/*  ...q  */
		lm_bracket,	/*  ...b  */
		lm_overwrite;	/*  -O,wi  */
/* resources */
	long	lt_start,	/* starting time */
		lt_end,		/* elapsed time */
		lt_usercpu,	/* user cpu time */
		lt_syscpu,	/* system cpu time */
		lt_kidscpu,	/* total ! kids time */
		lt_rlines,	/* total lines read */
		lt_wlines;	/* total lines written */
} logstats;
#endif

#ifdef PAGE
int	ccount	= CCOUNT;/* terminal width */
#endif

#ifdef PARENS
int	parenc[3] = {0, 0, 0};/* parentheses counts */
flag	parenf	= 0;	/* count parentheses and brackets */
#endif

#ifdef PIPE
filedes	pfile	= 0;	/* "pipe" file descriptor */
char	*pfname	= "/tmp/ep00000";/* for "double-piping" */
flag	piperr	= 0,	/* pipe error flag - shell() */
	pno	= -1,	/* piping line numbering flag (default n-) */
	strict	= 1;	/* strict exit status checking for | */
#endif

#ifdef STRLEN
charac	quotec	= '\0',	/* quote character other than " or ' */
	quotec2	= '\0';	/* closing quote */
int	quotef	= 0;	/* length of strings within " or ' chars */
#endif

#ifdef TABS
charac	tabfill	= TABFILL,/* fill character */
	tabc	= 0;	/* tab character - if 0 no tab processing */
int	maxtab	= -1,	/* last column number with tab stop */
	tabs[TABS];	/* each bit on = tab stop */
#endif

#ifdef UNDO
linep	undo_oldp,	/* original line pointer */
	undo_newp;	/* replacement line */
#endif

#ifdef USE
filedes	alt	= 0;	/* alternate command input file */
char	altfile[FNSIZE];
flag	eflg2	= 0;	/* another kludge */
#endif

#ifdef XDEL
linep	deleted = 0;	/* pointer to deleted line pointers */
int	ndeleted = 0;	/* number of lines */
#endif

#ifdef YINT
flag	yflg	= 0;	/* page upon interrupt */
linep	*yplus	= 0;	/* page from this line - if zero, from dot */
#endif

/*
 * globals
 */

block	iblock	= -1,	/* block number of input buffer */
	oblock	= -1;	/* output buffer block number */

char	*braelist[NBRA],	/* bracket \( \) end list */
	*braslist[NBRA],	/* bracket \( \) start list */
	dotbak[] = ".bak",
	dotedt[] = ".edt",	/* "file saved" file */
	dothup[] = ".hup",
	dotint[] = ".int",
	dottrm[] = ".trm",
	*dots[] = { dothup, dottrm, dotedt, dotint, 0 },
	expbuf[ESIZE + 4],	/* expression buffer */
	file[FNSIZE],		/* filename buffer */
#ifndef ALLOC
	genbuf[LBSIZE],		/* generated line buffer */
#else
	*genbuf,		/* generated line buffer pointer */
#endif
	ibuff[BLKSIZE],		/* input tmpfile buffer */
	line[TTSIZE + 4],	/* terminal output buffer */
#ifndef ALLOC
	linebuf[LBSIZE],	/* line buffer for getline()/putline() */
#else
	*linebuf,		/* line buffer for getline()/putline() */
#endif
	no[]	= "no ",
	null[]	= "",		/* "" */
	obuff[BLKSIZE],		/* output tmpfile buffer */
	off[]	= "off",
	on[]	= "on",
	prcntu[] = "%u\n",	/* %u */
	quote_s[] = "s",	/* "s" */
	rhsbuf[LBSIZE / 2],	/* right-hand-side expression buffer */
	savedfile[FNSIZE],	/* saved filename */
	tempfile[FNSIZE],	/* scratch area for filename */

	*editor,	/* argv[0] */
	*e_prompt = PROMPT,/* editor command prompt */
	*fmtlno	= "%7u=",/* format for line-number output */
	*globp,		/* global command pointer */
	*linp	= line,	/* line pointer */
	*linebp,
	*loc1,		/* start pointer of & string */
	*loc2,		/* end pointer of & string */
	*locs,
	*nextip,
	*overfile,	/* filename mode was changed on */
	*tfname	= "/tmp/e00000",/* "buffer" name */
	*ver	= TTL;	/* ID message */

charac	lastc	= 0,	/* peekc set to lastc on interrupt */
	peekc	= 0;	/* one character pushback */

int	brcount	= 1,	/* number of lines to output on "newline" */
	col	= 0,	/* column counter for calculating line wraps */
	line_num,	/* integer for line number on output */
	modcount = MODCNT,/* number of mods before auto-write */
	overmode,	/* mode of overridden file */
	mods	= 0,	/* number of mods */
	nbra	= 0,	/* count of currently defined \( \) pairs */
	ninbuf,		/* bytes in tmpfile input buffer */
	nleft,		/* bytes remaining in tmpfile output buffer */
	num_reads = 0,	/* indicator to aid text_modified-- */
			/* first read isn't really a modify */
	pcount	= PAGSIZ-1,/* number of lines to display on ":" command */
	s_cnt	= 0,	/* counter for "s/str1/str2/nn" */
	s_tmp	= 0,	/* scratch var for same */
	savf,		/* counter for auto-write stuff */
	text_modified = 0;/* flag--on if text was modified */

filedes	fout	= 1,	/* putchar() writes on this fildes */
	io	= 0,	/* file descriptor for "r", "w", "e" */
	tfile	= -1;	/* file des for "buffer" */

flag	aflg	= 0,	/* "apl mode" flag */
	appflg	= 0,	/* append flag (if "w>file") */
	badf	= 0,	/* bad read on temp file */
	bflg	= 0,	/* "back-up" flag -- Generate back-up file */
	bflg2	= 0,	/* Secondary "back-up" flag */
	circfl,		/* reg expr started with ^ */
	curt	= 0,	/* short error messages -- ie: '?' */
	deltflg	= 0,	/* don't delete .edt file upon exit */
	eflg	= 0,	/* echo input flag */
	eof	= 0,	/* eof was last char typed */
	fflg	= 0,	/* "create" flag */
	globf2	= 0,	/* kludge for -f */
#ifdef HUGE
	hugef	= 0,	/* -h is process huge file */
	hugef2	= 0,	/* getblock() conversion to huge */
#endif
	hupflag	= 0,	/* hangup signal has been caught */
	ichanged,	/* ibuf has been changed */
	iflg	= 0,	/* file.int and exit on interrupt */
	immflg	= 0,	/* immediate flag -- q and e */
	io_w	= 0,	/* writing in progress */
	listf	= 0,	/* list control chars explicitly */
	noshell	= 0,	/* true if no ! command allowed */
	over	= 0,	/* override permissions on write if possible */
	pflag,		/* print line after doing command */
	pipef	= 0,	/* for talking to pipes */
	prompt1	= 1,	/* flag--enable or disable line-num prompts */
	prompt2	= 1,	/* flag--enable or disable ALL prompting */
	reading	= 0,	/* waiting on tty read */
	seekf	= 0,	/* no seek to EOF on error on fd 0 */
	termflg	= 0;	/* if termination signal (15) occurred */

linep	*addr1,		/* lower line bound */
	*addr2,		/* upper line bound */
	*dol,		/* last line in file */
	*dot,		/* "current" line */
	*dotdot,	/* last different "dot" */
	*endcore,	/* current end of memory */
	*fendcore,	/* start of dynamic area */
	*lastdot,	/* last "dot" */
	names['z' - 'a' + 1],	/* "k" command markers */
#ifdef EXTMARK
	names2['z' - 'a' + 1],	/* "k" command markers */
#endif
	*old_a1,	/* previous address bounds */
	*old_a2,
	tline,		/* pointer to next available pos in tmpfile */
	*zero;		/* anchor line for all other lines */

/*
 * magic constants used in many places
 */

#ifdef pdp11
# define _1a	 ~0377
# define _2a	  0400
# define _3a	  0377
# define _4a	  0774
# define _5a	   255
# define _6a	077776
# define _1b	 ~0177
# define _2b	  0200
# define _3b	  0777
# define _4b	  0774
# define _5b	   511
# define _6b	077777
#endif

#ifdef vax
# define _1a	    ~0377
# define _2a	     0400
# define _3a	077777777
# define _4a	     0774
# define _5a	    65535
# define _6a	077777776
#endif

#ifdef HUGE
int	_1[]	= { _1a, _1b },	/* tl &= _1; getline() */
	_2[]	= { _2a, _2b },	/* tl += _2; getline()... */
	_3[]	= { _3a, _3b },	/* bno = ... & _3; getblock() */
	_4[]	= { _4a, _4b },	/* off = ... & _4; getblock() */
	_5[]	= { _5a, _5b },	/* if (bno >= _5)... getblock() */
	_6[]	= { _6a, _6b };	/* tline += ... & _6; */
#else
# define _1	_1a
# define _2	_2a
# define _3	_3a
# define _4	_4a
# define _5	_5a
# define _6	_6a
#endif

/*
 * error messages
 *
 *	(there are more than these)
 */

char	*errtext[] = {
	/*  0 */	"syntax is k[a-z]",
	/*  1 */	"illegal command format",
	/*  2 */	"no command",
	/*  3 */	"no tab character",
	/*  4 */	"can't change filename",
	/*  5 */	"file name syntax",
	/*  6 */	"recursive \"@\" command",
	/*  7 */	"null file name illegal",
	/*  8 */	"unrecognized command",
	/*  9 */	"no tabs set",
	/* 10 */	"global command not allowed with huge file",
	/* 11 */	"file name too long",
	/* 12 */	"expanded line too long",
	/* 13 */	"no such line",
	/* 14 */	"can't fork",
	/* 15 */	"can't write to process",
	/* 16 */	"no lines",
	/* 17 */	"backup(FILE) error (?)",
	/* 18 */	"string not found",
	/* 19 */	"  '  must be followed by [a-z]",
	/* 20 */	"address syntax error",
	/* 21 */	"lower address bound > upper one",
	/* 22 */	"address illegal here",
	/* 23 */	"non-existent line number",
	/* 24 */	"bottom of file reached",
	/* 25 */	"command syntax error",
	/* 26 */	"\"advance\" error (?)",
	/* 27 */	"null string illegal",
	/* 28 */	"destination not found",
	/* 29 */	"INTERRUPT!",
	/* 30 */	"line too long",
	/* 31 */	"missing destination address",
	/* 32 */	"I/O error--file not saved!",
	/* 33 */	"file overflows available memory",
	/* 34 */	"file too large (TMPERR)",
	/* 35 */	"I/O error on temp file (TMPERR)",
	/* 36 */	"open error on temp file (TMPERR)",
	/* 37 */	"recursive global command",
	/* 38 */	"global command list too long",
	/* 39 */	"substitute pattern not found",
	/* 40 */	"missing substring",
	/* 41 */	"string2 too long",
	/* 42 */	"substring too long",
	/* 43 */	"substituted string too long",
	/* 44 */	"too many  \\(",
	/* 45 */	"unbalanced  \\(  \\)",
	/* 46 */	"\\n  illegal",
	/* 47 */	"unimplemented feature",
	/* 48 */	"[nothing written]",
	/* 49 */	"pattern too complicated",
	/* 50 */	"can't create temp file (TMPERR)",
	/* 51 */	"bad directory",
	/* 52 */	"no ! allowed",
	/* 53 */	"can't read ",
	/* 54 */	"can't create ",
	/* 55 */	"%u line%s\n",
	/* 56 */	"[file saved]",
	/* 57 */	"\nHangup!\n",
	/* 58 */	"\nTerminated...\n",
	/* 59 */	"EOF illegal here",
	/* 60 */	"can't join to line 0",
	/* 61 */	"! not allowed with global command",
	/* 62 */	"no filename specified",
	/* 63 */	"not enough  \\( \\)  pairs",
	/* 64 */	"can't create pipe file (PIPERR)",
	/* 65 */	"open error on pipe file (PIPERR)",
	/* 66 */	"can't checkpoint",
	/* 67 */	"can't recover",
};
#define NERR	(sizeof errtext / sizeof errtext[0])

/*
 * ! error strings
 */

char *status[] = {
	/*  0 */	0,
	/*  1 */	"hangup",
	/*  2 */	"interrupt",
	/*  3 */	"quit",
	/*  4 */	"illegal instruction",
	/*  5 */	"bpt",
	/*  6 */	"iot",
	/*  7 */	"emt",
	/*  8 */	"fpp",
	/*  9 */	"killed",
	/* 10 */	"bus error",
	/* 11 */	"memory fault",
	/* 12 */	"bad sys call",
	/* 13 */	"broken pipe",
	/* 14 */	"alarm",
	/* 15 */	"terminated",
#ifdef pdp11
	/* 16 */	"time limit",
#else
	/* 16 */	0,
	/* 17 */	"stopped",
	/* 18 */	"terminal stop",
	/* 19 */	"continue",
	/* 20 */	"child status changed",
	/* 21 */	"terminal input",
	/* 22 */	"terminal output",
	/* 23 */	"terminal input ready",
	/* 24 */	"cpu timelimit exceeded",
	/* 25 */	"filesize limit exceeded",
#endif
};
#define NSTR	(sizeof status / sizeof status[0])

#define putsn(x) (puts2((x)),putchar('\n'))
#define min(x,y) ((x)<(y)?(x):(y))
#define max(x,y) ((x)>(y)?(x):(y))

/*
 * function declarations
 */

linep	*findmark();
char	*getblock();
long	lseek();
func	signal();
#ifndef CKPT
int	badsig(),
#else
int	checkpoint(),
#endif
	hangup(),
	mail(),
	onintr(),
	term();

/*
 * signals
 */

struct sigtab {
	int	s_sig,
		s_func;
} sigtab1[] = {
#ifdef CKPT
	SIGILL,		checkpoint,
	SIGIOT,		checkpoint,
	SIGEMT,		checkpoint,
	SIGFPE,		checkpoint,
	SIGBUS,		checkpoint,
	SIGSEGV,	checkpoint,
	SIGSYS,		checkpoint,
#else
	SIGILL,		badsig,
	SIGIOT,		badsig,
	SIGEMT,		badsig,
	SIGFPE,		badsig,
	SIGBUS,		badsig,
	SIGSEGV,	badsig,
	SIGSYS,		badsig,
#endif
	0,		0,
}, sigtab2[] = {
	SIGTRC,		mail,
	SIGHUP,		hangup,
	SIGTRM,		term,
	SIGINT,		onintr,
	0,		0,
};

main(argc, argv)
char **argv;
{
#ifdef CKPT
	extern checkpoint();
#else
	extern badsig();
#endif
	extern onintr(), hangup(), mail(), term();
#ifdef DEBUG
#ifdef EXPDMP
	extern expdmp();
#endif
#endif
	register n;
	register char *p1, *p2, *ep;
	func savint;

	signal(SIGQIT, 1);
	savint = signal(SIGINT, 1);
	ep = *argv;
#ifdef XED
	p1 = ep;
	p2 = p1;
	while (*p1)
		if (*p1++ == '/' && *p1 && *p1 != '/')
			p2 = p1;
	p1 = p2;
	*argv = p2;
	n = 0;
	while (*p1)
		if (*p1++ == 'x') {	/* xed .vs. eed */
			++n;
			break;
		}
	if (n == 0) {
		e_prompt = EEDPROMPT;
		ver = EEDTTL;
		help = EEDHELP;
		dumbf = 1;
	}
#ifdef LOG
	logstats.l_xed = n;
#endif
#endif
	prompt2 = istty(0);
#ifdef V7
	if (getenv("_OVERWRITE_"))
		++over;
#endif
	while (--argc)
		if (**++argv == '-') {
			while (*++*argv) {
				switch (**argv) {
				case '!':  /* no ! allowed */
					noshell = 1;
					break;
#ifdef APLMAP
				case 'A':  /* apl char mapping */
					aplmap = 1;
					errtext[29] = "G interrupt G";
					p1 = ver;
					while (*p1) {
						if ('A' <= *p1 && *p1 <= 'Z')
							*p1 |= 040;
						++p1;
					}
#endif
				case 'a':  /* apl mode */
					aflg = 1;
					fmtlno = "[ %u ]\t";
#ifdef DUMB
					dumbf = 1;
#endif
#ifdef XED
#ifdef TTL_NL
					ver[TTL_NL] = 0;
#endif
#endif
					break;
				case 'b':  /* file.bak on entry */
					bflg = 1;
					bflg2 = 1;
					break;
#ifdef PAGE
				case 'c':  /* crt depth in lines */
					++*argv;
					n = argnum(argv);
					if (n >= 0)
						pcount = n;
					break;
#endif
				case 'd':  /* don't delete .edt file */
					deltflg = 1;
					break;
				case 'e':  /* echo input commands */
					eflg = 1;
					break;
				case 'f':  /* create mode */
					fflg = 1;
					break;
#ifdef HUGE
				case 'h':  /* edit "huge" file */
					hugef = 1;
					break;
#endif
				case 'i':  /* file.int on interrupt */
					iflg = 1;
					break;
				case 'k':  /* kill verbose messages */
					curt = 1;
					break;
#ifdef EOL
				case 'l':  /* set eol char to "x" */
					if (*++*argv)
						eol = **argv;
					else
						--*argv;
					break;
#endif
				case 'm':  /* mod cnt for autosave */
					++*argv;
					n = argnum(argv);
					if (n >= 0)
						modcount = n;
					break;
				case 'n':  /* no line num */
					prompt1 = 0;
					break;
				case 'o':  /* no seek to EOF on error */
					seekf = 1;
					break;
				case 'p':  /* force prompts for pipe */
					pipef = 1;
					prompt2 = 1;
					break;
				case 'q':  /* don't inhibit quits */
					signal(SIGQIT, 0);
					break;
#ifdef DUMB
				case 'r':  /* spcl char meaning */
					dumbf ^= 01;
					break;
#endif
				case 's':  /* silent mode */
					prompt2 = 0;
					break;
#ifdef TABS
				case 't':  /* tab char */
					if (*++*argv)
						tabc = **argv;
					else
						--*argv;
					break;
#endif
#ifdef TABS
				case 'v':  /* tab fill char */
					if (*++*argv)
						tabfill = **argv;
					else
						--*argv;
					break;
#endif
#ifdef PAGE
				case 'w':  /* crt width */
					++*argv;
					n = argnum(argv);
					if (--n >= 2)
						ccount = n;
					break;
#endif
#ifdef YINT
				case 'y':  /* page on interrupt */
					yflg = 1;
					break;
#endif
#ifdef USE
				case '@':  /* set "@" filename */
					p2 = altfile;
					p1 = ++*argv;
					while (*p1 && p2 < &altfile[FNSIZE - 2])
						*p2++ = *p1++;
					if (*p1) {
						p2 = altfile;
						putsn(errtext[11]);
					}
					*p2 = '\0';
					*argv = &p1[-1];
					break;
#endif
#ifdef ALLOC
				case 'B':  /* line buffer size */
					++*argv;
					n = argnum(argv);
					if (n >= LBSIZE)
						lbsize = n;
					break;
#endif
#ifdef DEBUG
				case 'D':  /* trace mode -- debug */
					tflg = 1;
					break;
#ifdef EXPDMP
				case 'Q':  /* show pattern on quit */
					signal(SIGQIT, expdmp);
					break;
#endif
#endif
#ifdef XED
				case 'I':  /* suppress ID message */
					ver = 0;
					break;
				case 'P':  /* prompt */
					p1 = *argv;
					e_prompt = ++p1;
					while (*p1++);
					*argv = &p1[-2];
					break;
				case 'L':  /* line number prompt */
					p1 = *argv;
					fmtlno = ++p1;
					while (*p1++);
					*argv = &p1[-2];
					break;
				case 'C':  /* screen-clear */
					p1 = *argv;
					clears = ++p1;
					while (*p1++);
					*argv = &p1[-2];
					break;
#ifdef CKPT
				case 'R':  /* recover */
					++*argv;
					if ((recovry = argnum(argv)) == 0)
						recovry = 1;
					break;
#endif
				case 'O':  /* over-ride write perm */
					over ^= 01;
					break;
				case 'T':  /* temp filename */
					p1 = *argv;
					tfname = ++p1;
#ifdef PIPE
					while (*p1)
						if (*p1++ == ':') {
							p1[-1] = '\0';
							pfname = p1;
#ifdef CKPT
							break;
#endif
						}
#endif
#ifdef CKPT
					while (*p1)
						if (*p1++ == ':') {
							p1[-1] = '\0';
							cfname = p1;
						}
#endif
					*argv = &p1[-1];
					break;
#endif
				default:  /* tabs stops/illegals */
					if (!**argv || **argv == '-'
#ifdef TABS
						    || **argv == ','
#endif
								    )
						break;
#ifdef TABS
					if (**argv < '0' ||
					    **argv > '9') {
#endif
						printf("bad flag: -%c\n",
						    **argv);
						exit(1);
#ifdef TABS
					}
					n = argnum(argv);
					settab(n);
#endif
				}
			}
		} else {
			p1 = *argv;
			p2 = savedfile;
			while (*p2++ = *p1++)
				if (p2 >= &savedfile[FNSIZE - 2]) {
					putsn(errtext[11]);
					exit(1);
				}
			globf2 = 1;
			if (fflg)
				globp = "a\n";
			else
				globp = "r\n";
		}


#ifdef YINT
	if (iflg)
		yflg = 0;
#endif
#ifdef ALLOC
	linebuf = sbrk(lbsize);
	genbuf = sbrk(lbsize);
#endif
	fendcore = sbrk(0);
#ifdef CKPT
	if ((n = recovry) == 0)
#endif
		n = getpid();
#ifdef PIPE
	tmpname(pfname, n);
#endif
#ifdef CKPT
	tmpname(cfname, n);
#endif
	tmpname(tfname, n);	/* MUST be last call to tmpname */
#ifdef LOG
	logstats.l_uid = getuid();
	time(&logstats.lt_start);
	if (prompt2)
		++logstats.l_inter;
	if (aflg)
		logstats.l_xed = 2;	/* magic num for apled */
#endif
#ifdef CKPT
	if (recovry)
		recover();
#endif
	editor = ep;
	if (prompt2) {
#ifdef CMDS
		if ((cmd = open(cmdfil, 1)) > 0)
			lseek(cmd, 0L, 2);
		else
			cmd = 0;
#endif
		if (ver)
			putsn(ver);	/* XED V0.00 ... */
		flush_buf();
	} else
		modcount = 0;
#ifdef CMDS
	if (cmd && *savedfile) {
		write(cmd, "e,", 2);
		p1 = savedfile;
		while (*p1++);
		write(cmd, savedfile, --p1 - savedfile);
		write(cmd, "\n", 1);
	}
#endif
	signals(sigtab1);
	setexit();
	if (((int)savint & 01) == 0)
		signals(sigtab2);
#ifdef YINT
	else
		yflg = 0;
#endif
#ifdef CKPT
	if (!recovry)
#endif
		init();
	setexit();
cant:	do {
		commands(0);
	} while (are_you_sure());
	if (immflg == 0) {
		if (fflg)
			if (backup(FILE))
				text_modified = 0;
			else
				goto cant;
		if (text_modified == 0)
			backup(-TMP);
		else if (modcount && eof)
			if (backup(TMP) && prompt2)
				if (!curt)
					putsn(errtext[56]);
	}
	delexit(0);
}

abort() {
	register char *p;
	register charac c;

	setnoaddr();
	peekc = 0;
	p = "ort\n";
	while (*p)
		if ((c = getchar()) != *p++) {
			peekc = c;
			errmsg(25);
		}
#ifdef LOG
	++logstats.lc_abort;
#endif
	delexit(1);
}

linep *
address() {
	register minus;
	register charac c;
	register linep *a1, *start;
	register n, relerr;

	minus = 0;
	a1 = 0;
	for ever {
		c = getchar();
		if ('0' <= c && c <= '9') {
			peekc = c;
			n = getnum();
			if (a1 == 0) {
				a1 = zero;
				n += aflg;
#ifdef LOG
				if (!globp)
					++logstats.la_num;
#endif
			}
			if (minus < 0)
				n = -n;
			a1 += n;
			minus = 0;
			continue;
		}
		relerr = 0;
		if (a1 || minus)
			relerr++;
		switch (c) {
		case ' ':
		case '\t':
			continue;

		case '+':
			minus += brcount;
			if (a1 == 0)
				a1 = dot;
#ifdef LOG
			if (!globp)
				++logstats.la_plus;
#endif
			continue;

		case '-':
		case '^':	/* for upwards compatibility */
			minus -= brcount;
			if (a1 == 0)
				a1 = dot;
#ifdef LOG
			if (!globp)
				if (c == '^')
					++logstats.la_caret;
				else
					++logstats.la_minus;
#endif
			continue;

	     /* search:	*/
		case '?':
			minus++;
		case '/':
			compile(c);
			if (a1 == 0)
				a1 = dot;
			if (a1 < zero)
				a1 = zero;
			if (a1 > dol)
				a1 = dol;
			start = a1;
#ifdef LOG
			if (!globp)
				if (minus)
					++logstats.la_query;
				else
					++logstats.la_slash;
#endif
			for ever {
				if (minus == 0) {
					if (++a1 > dol)
						a1 = zero;
				} else {
					if (--a1 < zero)
						a1 = dol;
				}
				if (execute(0, a1)) {
					minus = 0;
					relerr = 0;
					break;
				}
				if (a1 == start)
					errmsg(18);
			}
			break;

		case '$':
			a1 = dol;
#ifdef LOG
			if (!globp)
				++logstats.la_dol;
#endif
			break;

		case '.':
			if ((peekc = getchar()) == '.') {
				peekc = 0;
				a1 = dotdot;
#ifdef LOG
				if (!globp)
					++logstats.la_dotdot;
#endif
			} else {
				a1 = dot;
#ifdef LOG
				if (!globp)
					++logstats.la_dot;
#endif
			}
			break;

		case '\'':
			if (((c = getchar()) | 040) < 'a' ||
			    (c | 040) > 'z') {
				peekc = c;
				errmsg(19);
			}
			c |= 040;
#ifdef LOG
			if (!globp)
				++logstats.la_quote;
#endif
		casemark:
#ifdef EXTMARK
			n = 3;
			if ((peekc = getchar()) == '^') {
				n = 1;
				peekc = 0;
			} else if (peekc == '$') {
				n = 2;
				if (names2[c - 'a'] == 0)
					n = 1;
				peekc = 0;
			}
			if (n & 01)
				if ((a1=findmark(names[c-'a'],0))==0) {
					a1 = dol;
					n = 0;
				}
			if (n > 1 && names2[c - 'a']) {
				if (n == 3) {
					if (addr1 || addr2)
						errmsg(20);
					addr1 = a1;
				}
				a1 = findmark(names2[c - 'a'], dol);
				if (n == 2)
					break;
				addr2 = a1;
				return(0);
			}
#else
			a1 = findmark(names[c - 'a'], dol);
#endif
			break;

		case '=':
#ifdef LOG
			if (!globp)
				++logstats.la_equal;
#endif
			if ((peekc = getchar()) == '^')
				a1 = old_a1;
			else if (peekc == '$')
				a1 = old_a2;
			else {
				if (addr1 || addr2 || a1)
					errmsg(20);
				addr1 = old_a1;
				addr2 = old_a2;
				return(0);
			}
			peekc = 0;
			break;

		default:
			if ('A' <= c && c <= 'Z') {
				c |= 040;
#ifdef LOG
				if (!globp)
					++logstats.la_letter;
#endif
				goto casemark;
			}
			peekc = c;
			if (a1 == 0)
				if (c == ',' || c == ';')
					if (dot + 1 > dol)
						return(dol);
					else
						return(dot + 1);
				else
					return(0);
			a1 += minus;
			if (a1 < zero)
				a1 = zero + (zero != dol);
			else if (a1 > dol)
				a1 = dol;
			return(a1);
		}
		if (relerr)
			errmsg(20);
	}
}

advance(alp, aep) {
	register char *lp, *ep, *curlp;
	register s_sav, i;

	lp = alp;
	ep = aep;
	for ever switch (*ep++) {

	case CCHR:
		if (*ep++ == *lp++)
			continue;
		return(0);

	case CDOT:
		if (*lp++)
			continue;
		return(0);

	case CDOL:
		if (*lp == 0)
			continue;
		return(0);

	case CEOF:
		loc2 = lp;
		if (--s_tmp > 0)
			return(0);
		return(1);

	case CCL:
	case NCCL:
		if (cclass(ep, *lp++, ep[-1] == CCL)) {
			ep += *ep;
			continue;
		}
		return(0);

	case CBRA:
		braslist[*ep++] = lp;
		continue;

	case CKET:
		braelist[*ep++] = lp;
		continue;

	case CBACK:
		if (braelist[i = *ep++] == 0)
			errmsg(63);
		if (backref(i, lp)) {
			lp += braelist[i] - braslist[i];
			continue;
		}
		return(0);

	case CBACK | STAR:
		if (braelist[i = *ep++] == 0)
			errmsg(63);
		curlp = lp;
		while (backref(i, lp))
			lp += braelist[i] - braslist[i];
		while (lp >= curlp) {
			if (advance(lp, ep))
				return(1);
			lp -= braelist[i] - braslist[i];
		}
		continue;

	case CDOT | STAR:
		curlp = lp;
		while (*lp++);
		goto star;

	case CCHR | STAR:
		curlp = lp;
		while (*lp++ == *ep);
		ep++;
		goto star;

	case CCL | STAR:
	case NCCL | STAR:
		curlp = lp;
		while (cclass(ep, *lp++, ep[-1] == (CCL | STAR)));
		ep += *ep;
		/* goto star; */

	star:
		s_sav = s_tmp;
		do {
			if (--lp == locs)
				break;
			if (advance(lp, ep))
				return(1);
			s_tmp = s_sav;
		} while (lp > curlp);
		--s_tmp;
		return(0);

	default:
		errmsg(-26);
	}
}

linep *
append(f, a, single, bkpf)
func f;
linep *a;
{
	register linep *a1, *a2, *rdot;
	register nline, tl;

	nline = 0;
	dot = a;
	while ((*f)(single) == 0) {
		if (dol >= endcore) {
			if (sbrk(1024) == -1)
				errmsg(33);
			endcore = (int)endcore + 1024;
		}
		tl = putline();
		nline++;
		a1 = ++dol;
		a2 = a1 + 1;
		rdot = ++dot;
		while (a1 > rdot)
			*--a2 = *--a1;
		*rdot = tl;
		if (bkpf) {
			mods++;
			backchk();
		}
		if (single)
			break;
	}
	return(nline);
}

are_you_sure() {
#ifdef USE
	if (alt)
		return(1);
#endif
	if (!text_modified || fflg || immflg || zero == dol)
		return(0);
	return(yes_no(curt? "?forget" :
	    "did you forget to save your text", 1, 1, 0));
}

argnum(ap)
char **ap;
{
	register n;
	register char *p;

	p = *ap;
	n = 0;
	while ('0' <= *p && *p <= '9')
		n = n * 10 + *p++ - '0';
	*ap = --p;
	return(n);
}

backchk() {
	if (modcount == 0)
		return;
	if (mods >= modcount) {
		mods = 0;
#ifdef EOL
		if (backup(TMP) && prompt2 && prompt3)
#endif
#ifndef EOL
		if (backup(TMP) && prompt2)
#endif
			if (!curt)
				putsn(errtext[56]);
	}
}

backref(an, alp)
char *alp;
{
	register n;
	register char *bp, *lp;

	n = an;
	bp = braslist[n];
	lp = alp;
	while (*bp++ == *lp++)
		if (bp >= braelist[n])
			return(1);
	return(0);
}

backup(af) {
	register char *p1, *p2, *t2;
	register linep *a1, *a2;
	register func savint, savhup;

	if (io) {
		putsn("******** backup: I/O in progress");
		return(0);
	}
	flush_buf();
	p1 = savedfile;
	t2 = p2 = file;
	while (*p2 = *p1++)
		if (*p2++ == '/')
			t2 = p2;
	if (p2 > t2 + 10)
		p2 = t2 + 10;
	if (af != FILE && p2 >= &file[FNSIZE - 6])
		errmsg(11);
	switch (af < 0? -af : af) {
		case FILE:	p1 = 0;		break;
		case TMP:	p1 = dotedt;	break;
		case INT:	p1 = dotint;	break;
		case HUP:	p1 = dothup;	break;
		case BAK:	p1 = dotbak;	break;
		case TRM:	p1 = dottrm;	break;
		default:	errmsg(-17);
	}
	if (p1)
		while (*p2++ = *p1++);
	if (af < 0) {
		if (deltflg)
			return(1);
		return(!unlink(file));
	}
	if (dol == zero && !fflg) {
		if (af == FILE)
			if (curt)
				putsn("?48");
			else
				putsn(errtext[48]);
		return(1);
	}
	if (dol == zero)
		return(1);
	if (!iflg) {
		savhup = signal(SIGHUP, 1);
		savint = signal(SIGINT, 1);
	}
	if (over && af == FILE)
		override();
	io = create(file, (af == FILE? MODE : LMODE));
	if (overfile) {
		chmod(overfile, overmode);
		overfile = 0;
	}
	if (io < 0) {
		io = 0;
		if (af != TMP) {
			if (curt)
				putsn("?54");
			else {
				puts2(errtext[54]);
				putsn(file);
			}
		}
		if (!iflg) {
			signal(SIGHUP, savhup);
			signal(SIGINT, savint);
		}
		return(0);
	}
	io_w++;
	a1 = addr1;
	a2 = addr2;
	addr1 = zero + 1;
	addr2 = dol;
	putfile();
	close(io);
	io = 0;
	io_w = 0;
	addr1 = a1;
	addr2 = a2;
	if (!iflg) {
		signal(SIGHUP, savhup);
		signal(SIGINT, savint);
	}
	return(1);
}

#ifndef CKPT
badsig(an) {
	register n;

	if (n = backup(TMP))
		putsn(errtext[56]);
	puts("  Fatal Signal: ");
	if (0 < an && an < NSTR && status[an])
		putsn(status[an]);
	else
		printf("%d\n", an);
	flush_buf();
	if (n)
		exit(1);
	error();
}
#endif

blkio(b, buf, iofcn)
func iofcn;
{
	lseek(tfile, 512L * b, 0);
	if ((*iofcn)(tfile, buf, BLKSIZE) != BLKSIZE) {
		badf++;
		errmsg(-35);
	}
}

cclass(aset, ac, af)
char *aset;
charac ac;
{
	register char *set;
	register n;
	register charac c, m;

	set = aset;
	if ((c = ac) == 0)
		return(0);
	n = *set++;
	while (--n)
		if (set[1] == '-' && n > 2) {
			c = min(set[0], set[2]);
			m = max(set[0], set[2]);
			do {
				if (c == ac)
					return(af);
			} while (++c != m);
			set += 2;
			c = ac;
		} else
			if (*set++ == c)
				return(af);
	return(!af);
}

#ifdef CKPT
checkpoint(asig) {
	extern etext;
	register filedes f;
	register char *p;
	register func savint, savqit;
	int n;

	savint = signal(SIGINT, 1);
	savqit = signal(SIGQIT, 1);
	p = &etext;	/* won't work for -n, -i, or -z */
	n = (char *)dol - p;
	n += sizeof (int);
	if ((f = create(cfname, LMODE)) < 0 ||
	    write(f, &n, sizeof n) != sizeof n) {
		n = 66;
		goto cerror;
	}
#ifdef pdp11	/* 16 bit byte count only */
	if (n < 0) {
		if (write(f, etext, 32256) != 32256) {
			n = 66;
			goto cerror;
		}
		n -= 32256;	/* 63 blocks, since 64 is < 0 */
		p += 32256;
	}
#endif
	if (write(f, p, n) != n)
		n = 66;
	else
		n = 0;
    cerror:
	close(f);
	if (n) {
		signal(SIGINT, savint);
		signal(SIGQIT, savqit);
		recovry = 0;
		errmsg(n);
	}
	if (asig) {
		puts2(status[asig]);
		if (!curt)
			puts(": Fatal error");
	}
	if (curt)
		puts2("?CKPT ");
	else {
		puts2("Editing session checkpointed to \"");
		puts2(cfname);
		puts("\".\nTo recover your work, type:");
		if (editor && *editor)
			puts2(editor);
		else
			puts2("edit");
		puts2(" -R");
	}
	puts(&tfname[tfnum]);
	exit(1);
}
#endif

chk() {
	register charac c;
	register char *p2, *p1, *t2, **p;
	register charac oldc;
	long t;
	struct stat s;

	if (*savedfile == '\0')
		return(0);
	t2 = p2 = file;
	p1 = savedfile;
	if (stat(p1, &s) >= 0)
		t = s.st_mtime;
	else
		t = 0L;
	while (*p2 = *p1++)
		if (*p2++ == '/')
			t2 = p2;
	if (p2 > t2 + 10)
		p2 = t2 + 10;
	if (p2 >= &file[FNSIZE - 6])
		return(0);
	t2 = p2;
	p = dots;
	while (p1 = *p++) {
		p2 = t2;
		while (*p2++ = *p1++);
		if (stat(file, &s) >= 0 && s.st_mtime >= t) {
			if (curt) {
				puts2("?\"");
				puts2(file);
				puts2("\"  ");
			} else {
				putsn("When you were last editing this file");
				putsn("you did not exit the editor normally,");
				puts2("leaving the file:  \"");
				puts2(file);
				if (p1 == dotedt) {
					putsn("\".\nIt contains your file up to the last \"[file saved]\"");
					putsn("message.  This file will be deleted if you do");
					putsn("not read it into the editor now.  If you read");
					putsn("it, then decide not to use it, exit the editor");
					putsn("with \"qi\".");
				} else
					putsn("\".");
			}
			return(yes_no(curt? "read" : "Do you wish to read it",
			    1, -1, 0));
		}
	}
	return(0);
}

_cleanup() {
	flush_buf();
}

#ifdef CLEAR
clear() {
	register l, i;

	l = listf;
	listf = 0;
	puts2(clears);	/* clear sequence */
	flush_buf();
	i = 5;
	while (--i >= 0)
		putchar(0);	/* ADM-3A's need padding at 19.2 */
	putchar('\n');
	listf = l;
}
#endif

commands(baseflg) {
	extern getfile(), gettty();
	register linep *a1;
	register charac c;
	register char *p;
	register r, num;

    for ever {
#ifdef AGAIN
	if (agp) {
		*agp++ = '\n';
		*agp = '\0';
	}
	agf = 0;
	agp = 0;
#endif
	immflg = 0;
#ifdef LOG
	if (logamp) {
		++logstats.lp_amp;
		logamp = 0;
	}
#endif
	if (!globp && (hupflag || termflg)) {
		if (hupflag) {
			backup(HUP);
			puts2(errtext[57]);
		} else {
			backup(TRM);
			puts2(errtext[58]);
		}
		delexit(1);
	}
	if (pflag) {
		pflag = 0;
		addr1 = addr2 = dot;
		goto print;
	}
	if (!globp) {
#ifdef USE
	    if (!alt) {
#endif
		if (modcount) {
			if (text_modified > savf)
				mods++;
			savf = text_modified;
			backchk();
		}
#ifdef EOL
#ifdef USE
		if (prompt2 && prompt3 && !eflg2)
#endif
#ifndef USE
		if (prompt2 && prompt3)
#endif
#endif
#ifndef EOL
#ifdef USE
		if (prompt2 && !eflg2)
#endif
#ifndef USE
		if (prompt2)
#endif
#endif
		{
			puts2(e_prompt);
			flush_buf();
		}
#ifdef USE
	    }
#endif
	}
	if (!globp) {
		if (dotdot > dol)
			dotdot = dol;
		if (dotdot < zero)
			dotdot = zero;
		if (dot != lastdot) {
			dotdot = lastdot;
			lastdot = dot;
		}
	}
	addr1 = 0;
	addr2 = 0;
	s_tmp = 0;
	s_cnt = 0;
	r = 1;
	do {
		addr1 = addr2;
		if ((a1 = address()) == 0) {
			c = getchar();
			break;
		}
		addr2 = a1;
		if ((c = getchar()) == ';') {
			c = ',';
			dot = a1;
		}
	} while (c == ',' && --r >= 0);
	if (addr1 == 0)
		addr1 = addr2;
	if (!globp && !baseflg) {
		if (addr1) {
			old_a1 = addr1;
			old_a2 = addr2;
		}
	}
	line_num = (addr1? addr1 : dot) - zero;
#ifdef AGAIN
	if (c == 'o' || c == ctrl('Q')) { /* again command "o" */
		if (c != ctrl('Q') && (peekc = getchar()) != '\n')
			errmsg(1);
		if (c == ctrl('Q'))
			putchar(lastc = '\n');
		if (*agbuf == 0)
			errmsg(2);
		agf++;
		agp = agbuf;
		c = *agp++;
		peekc = 0;
#ifdef LOG
		++logstats.lc_o;
#endif
	} else if (baseflg == 0 && globp == 0)
		if (c != '\n') {
			agp = agbuf;
			*agp++ = c;  /* first char not yet saved */
		}
#endif

	switch (c) {
	case 'a':
		if ((peekc = getchar()) == 'b')
			abort();
		setdot();
#ifdef XED
		if (peekc != ' ' && peekc != '\n') {
			c = peekc;
			peekc = 0;
			if (tack(c, 1))
				text_modified++;
#ifdef LOG
			++logstats.lc_aslash;
#endif
			continue;
		}
#endif
		line_num++;
		num = addr2;
#ifdef LOG
		++logstats.lc_append;
#endif
	caseadd:
		if ((c = getchar()) == ' ') {
			r = 1;
#ifdef LOG
			++logstats.lc_aspace;
#endif
		} else {
#ifndef XED
			if (c != '\n') {
				peekc = c;
				newline();
			}
#endif
			r = 0;
		}
		if (append(gettty, num, r, 1))
			text_modified++;
		continue;

	case 'b':
		setnoaddr();
		white_space();
		if ((brcount = setnum(1)) <= 0)
			brcount = 1;
#ifdef LOG
		++logstats.lc_browse;
#endif
		continue;

	case 'c':
		if ((peekc = getchar()) == 'o') {  /* co == t */
			peekc = 0;
#ifdef LOG
			++logstats.lc_copy;
#endif
			goto casecopy;
		}
		if (peekc != '\n')
			goto casesub;
		newline();
		setdot();
		nonzero();
		delete();
		text_modified++;
#ifdef LOG
		++logstats.lc_change;
#endif
		append(gettty, addr1 - 1, 0, 0);
		continue;

	case 'd':
#ifdef DEBUG
/*		*du	Dump command (testing only)	*/
		if ((peekc = getchar()) == 'u') {
			peekc = 0;
			dump();
			continue;
		}
#endif
		if ((peekc = getchar()) == ' ' || peekc == ',') {
			peekc = 0;
			white_space();
			p = linebuf;
			if ((c = getchar()) == '\n')
				errmsg(51);
			do {
				*p++ = c;
#ifndef ALLOC
				if (p >= &linebuf[LBSIZE - 2])
#else
				if (p >= &linebuf[lbsize - 2])
#endif
					errmsg(11);
			} while ((c = getchar()) >= 0 && c != '\n');
			*p = 0;
#ifdef LOG
			++logstats.lc_directory;
#endif
			if (chdir(linebuf) < 0)
				errmsg(51);
			continue;
		}
#ifdef PAGE
		if (peekc == '=') {	/* d=<depth> */
			peekc = 0;
			setnoaddr();
			pcount = setnum(PAGSIZ);
#ifdef LOG
			++logstats.lc_depth;
#endif
			continue;
		}
#endif
		newline();
		setdot();
		nonzero();
		delete();
		text_modified++;
#ifdef LOG
		++logstats.lc_delete;
#endif
		continue;

	case 'e':
	    eagain:
		if ((peekc = getchar()) != '\n') {
#ifdef EOL
			if (peekc == '=') {  /* e=c - set eol to 'c' */
				peekc = 0;
				if (immflg)
					errmsg(8);
				setnoaddr();
				eol = setchar(0);
#ifdef LOG
				++logstats.lc_eol;
#endif
				continue;
			}
#endif
#ifdef TABS
			if (peekc == 'x') {
				peekc = 0;
				if (immflg)
					errmsg(8);
				if ((c = getchar()) != 'p')
					errmsg(8);
				newline();
				if (!tabc)
					errmsg(3);
				if (maxtab < 0)
					errmsg(9);
				if (exp())
					text_modified++;
#ifdef LOG
				++logstats.lc_exp;
#endif
				continue;
			}
#endif
			if ('0' <= peekc && peekc <= '9') {
				c = getnum();
				newline();
				if (0 <= c && c < NERR)
					printf("%3d: %s\n",
					    c, errtext[c]);
#ifdef LOG
				++logstats.lc_errmsg;
#endif
				continue;
			}
			if (peekc == '+' || peekc == '-') {
				c = peekc;
				peekc = 0;
				newline();
				curt = c == '-';
#ifdef LOG
				if (curt)
					++logstats.lc_eminus;
				else
					++logstats.lc_eplus;
#endif
				continue;
			}
			if (peekc == 'i') {
				peekc = 0;
				if (immflg)
					errmsg(8);
				immflg++;
				goto eagain;
			}
			setnoaddr();
			if (fflg)
				errmsg(4);
			if (peekc != ' ' && peekc != ',')
		illfnm:		errmsg(5);
			scopy(savedfile, tempfile);
			if (zero == dol || text_modified == 0)
				backup(-TMP);
			savedfile[0] = 0;
			filename();
			if (text_modified && are_you_sure()) {
				scopy(tempfile, savedfile);
				error();
			}
			peekc = '\n';
		} else {
			setnoaddr();
			if (text_modified) {
				r = peekc;
				peekc = 0;
				if (are_you_sure()) {
					peekc = r;
					error();
				}
				peekc = r;
			}
			if (zero == dol || text_modified == 0)
				backup(-TMP);
		}
		if (init())
			continue;
#ifdef LOG
		++logstats.lc_edit;
#endif
		goto caseread;

	case 'f':
		setnoaddr();
#ifdef TABS
		if ((peekc = getchar()) == '=') {  /* f=c fill char */
			peekc = 0;
			tabfill = setchar(TABFILL);
#ifdef LOG
			++logstats.lc_fillset;
#endif
			continue;
		}
#endif
		if ((c = getchar()) != '\n') {
			if (fflg)
				errmsg(4);
			peekc = c;
			filename();
			scopy(file, savedfile);
#ifdef LOG
			++logstats.lc_fset;
#endif
			if (prompt2 == 0)
				continue;
		}
#ifdef LOG
		  else
			++logstats.lc_fshow;
#endif
		putsn(savedfile);
		continue;

	case 'g':
		global(1);
		continue;

	case 'h':
#ifdef HELP
		if ((peekc = getchar()) == 'e') {  /* he[lp] */
			peekc = 0;
			setnoaddr();
			skip_rest();
#ifdef LOG
			++logstats.lc_help;
#endif
			if ((doc = open(help, 0)) > 0) {
				while ((c = read(doc, linebuf,
#ifndef ALLOC
				    LBSIZE)) > 0)
#else
				    lbsize)) > 0)
#endif
					write(1, linebuf, c);
				close(doc);
			}
			doc = 0;
			continue;
		}
#endif
		if (zero != dol) {
			setdot();
			nonzero();
		}
#ifdef LOG
		++logstats.lc_header;
#endif
		header();
		continue;

	case 'i':
		setdot();
#ifdef XED
		if ((peekc = getchar()) != ' ' && peekc != '\n') {
			c = peekc;
			peekc = 0;
			if (tack(c, 0))
				text_modified++;
#ifdef LOG
			++logstats.lc_islash;
#endif
			continue;
		}
#endif
		nonzero();
		num = addr2 - 1;
#ifdef LOG
		++logstats.lc_insert;
#endif
		goto caseadd;

#ifdef XED
	case 'j':
		setdot();
		nonzero();
		join();
		text_modified++;
		continue;
#endif

	case 'k':
		if ((c = getchar()) == '\n') {
			setnoaddr();
#ifdef LOG
			++logstats.lc_klist;
#endif
			printmarks();
			continue;
		}
		if ('A' <= c && c <= 'Z')
			c |= 040;
		if (c < 'a' || c > 'z')
			errmsg(0);
		newline();
		setdot();
		nonzero();
#ifdef HUGE
		r = hugef ^ 01;
#else
#define r	1
#endif
#ifdef EXTMARK
		if (addr1 != addr2) {
			names[c - 'a'] = *addr1 | r;
			names2[c - 'a'] = *addr2 | r;
		} else {
			names[c - 'a'] = *addr2 | r;
			names2[c - 'a'] = 0;
		}
#else
		names[c - 'a'] = *addr2 | r;
#endif
#ifdef LOG
		++logstats.lc_kset;
#endif
		continue;
#ifndef HUGE
#undef  r
#endif

	case 'm':
#ifdef DUMB
		if ((peekc = getchar()) == '\n') {
			setnoaddr();
			peekc = 0;
			dumbf ^= 01;
#ifdef LOG
			++logstats.lc_magic;
#endif
			if (curt)
				putsn(dumbf? off : on);
			else {
				puts2("\"$&[^.*\\(\\)\" have ");
				if (dumbf)
					puts2(no);
				putsn("special meaning");
			}
			continue;
		}
#endif
		if ((peekc = getchar()) == 'o') {  /* mo == m */
			peekc = 0;
#ifdef LOG
			++logstats.lc_moove;
#endif
		}
#ifdef LOG
		  else
			++logstats.lc_move;
#endif
		move(0);
		text_modified++;
		continue;

	case 'n':
		setnoaddr();
#ifdef PIPE
		if ((peekc = getchar()) == '+') {
			peekc = 0;
			newline();
			pno = 1;
#ifdef LOG
			++logstats.lc_numplus;
#endif
			continue;
		}
		if (peekc == '-') {
			peekc = 0;
			newline();
			pno = -1;
#ifdef LOG
			++logstats.lc_numinus;
#endif
			continue;
		}
#endif
		newline();
		prompt1 ^= 01;
#ifdef LOG
		++logstats.lc_numbers;
#endif
		if (curt)
			putsn(prompt1? on : off);
		else {
			if (prompt1 == 0)
				puts2(no);
			putsn("line numbers");
		}
		continue;

	case '\n':
		if (globp || addr2 > dol)
			continue;
		if (addr2 == 0) {
			addr1 = addr2 = dot + 1;
			if (addr2 <= dol)
				line_num++;
			if (brcount != 1) {
				addr2 = dot + brcount;
				if (addr1 > dol)
					continue;
				if (addr2 > dol)
					addr2 = dol;
				if (addr2 < zero)
					addr2 = zero;
#ifdef CLEAR
				if (zflg && addr2 - addr1 > pcount / 2)
					clear();
#endif
			}
		}
		if (addr2 <= dol) {
			pflag = 0;
			goto print;
		}
		continue;

	case 'l':
		listf++;
#ifdef LOG
		++logstats.lc_list;
#endif
	case 'p':
		if ((peekc = getchar()) == 'a') {
			peekc = 0;
	case ':':
	case '*':
			num = 0;
			if (c == ':' || c == '*') {
				if ((peekc = getchar()) == ' ' ||
				    peekc == '\t') {
					skip_rest();	/* comments */
#ifdef LOG
					++logstats.lc_comment;
#endif
					continue;
				}
				r = peekc;
				peekc = 0;
#ifdef LOG
				if (r == '-' || r == '+')
					++logstats.lc_clnminus;
				else if (c == '*')
					++logstats.lc_star;
				else
					++logstats.lc_colon;
#endif
				while (r == '-' || r == '^' || r == '+') {
					if (r == '+')
						++num;
					else
						--num;
					r = getchar();
				}
				peekc = r;
			}
#ifdef LOG
			  else
				if (c != 'p')
					++logstats.lc_print;
#endif
			newline();
			pflag = 0;
			if (addr1 == addr2) {
				if (num == -1 && c == ':') {
					if (addr2 == 0)
						addr2 = dot;
					addr1 = addr2 - pcount;
					if (addr1 <= zero)
						addr1 = zero + 1;
#ifdef CLEAR
					if (zflg)
						clear();
#endif
					goto print;
				}
				num *= pcount;
				if (c == '*')
					num -= (pcount + 1) / 2;
				if (addr1 == 0)
					a1 = dot + (num? num : 1);
				else
					a1 = addr1 + num;
				if (a1 <= zero)
					a1 = zero + 1;
				addr1 = addr2 = a1;
			}
			nonzero();
			if (addr1 == addr2 || addr2 > dol ||
			    addr2 <= zero)
				addr2 = dol;
			setdot();
#ifdef CLEAR
			if (zflg && addr2 - addr1 > pcount / 2)
				clear();
#endif
#ifndef PAGE
			if (addr2 > addr1 + pcount)
				addr2 = addr1 + pcount;
			goto print;
#else
			page();
			listf = 0;
#ifdef PARENS
			parenf = 0;
#endif
#ifdef STRLEN
			quotef = 0;
#endif
			continue;
#endif
		} else if (peekc == 'p' || peekc == 'l') {
			peekc = 0;
			addr1 = zero + 1;
			addr2 = dol;
#ifdef LOG
			++logstats.lc_pprint;
#endif
		}
		newline();
		pflag = 0;
    print:
		setdot();
		nonzero();
		a1 = addr1;
#ifdef PARENS
		parenc[0] = 0;
		parenc[1] = 0;
		parenc[2] = 0;
#endif
		do {
			col = 0;
			printlno(line_num = a1 - zero);
			puts(getline(*a1++));
		} while (a1 <= addr2);
		dot = addr2;
		listf = 0;
#ifdef PARENS
		parenf = 0;
#endif
#ifdef STRLEN
		quotef = 0;
#endif
		continue;

	case 'q':
		if ((peekc = getchar()) == 'i') {  /* qi - immediate */
			peekc = 0;
			newline();
			immflg++;
#ifdef LOG
			++logstats.lc_qimm;
#endif
			return;
		}
#ifdef STRLEN
		if (peekc == '=') {	/* q=cc  set quote chars */
			peekc = 0;
			if ((c = getchar()) == '\\')
				c = getchar();
			quotec = quotec2 = c;
#ifdef LOG
			++logstats.lc_quote;
#endif
			if (c != '\n') {
				if ((c = getchar()) == '\\')
					c = getchar();
				if (c == '\n')
					continue;
				quotec2 = c;
			} else {
				quotec = 0;
				quotec2 = 0;
				continue;
			}
			if (c != '\n')
				newline();
			continue;
		}
#endif
		setnoaddr();
		if ((peekc = getchar()) != '\n')
			errmsg(25);
		peekc = 0;
#ifdef LOG
		++logstats.lc_quit;
#endif
		return;

	case 'r':
#ifdef LOG
		++logstats.lc_read;
#endif
	caseread:
		filename();
		if ((io = open(file, 0)) < 0) {
			io = 0;
			lastc = '\n';
			if (curt)
				errmsg(53);
			puts2(errtext[53]);
			putsn(file);
			error();
		}
		setall();
		ninbuf = 0;
		r = append(getfile, addr2, 0, 0);
		if (prompt2)
			printf((curt? prcntu : errtext[55]),
			    r, (r == 1? null : quote_s));
		close(io);
		io = 0;
#ifdef LOG
		logstats.lt_rlines += r;
#endif
		if (num_reads++ || fflg) {
			if (r)
				text_modified++;
		} /* else
			text_modified = 0; */
		if ((c == 'e' && bflg == 1) || bflg2 == 1) {
			bflg2 = 0;
			backup(BAK);
		}
		continue;

	case 's':
		if (!globp && (peekc = getchar()) == '\n') {  /* w;q */
			setnoaddr();
#ifdef LOG
			++logstats.lc_stop;
#endif
			if (text_modified && !fflg)
				if (backup(FILE) == 0)
					error();  /* errmsg(10) */
			peekc = text_modified = 0;
#ifdef LOG
			logstats.lt_wlines += dol - zero;
#endif
			return;
		}
		if (peekc == 'a') {  /* sa - count before auto-save */
			setnoaddr();
			peekc = 0;
			modcount = setnum(MODCNT);
			mods = 0;
#ifdef LOG
			++logstats.lc_savecount;
#endif
			continue;
		}
	casesub:
		setdot();
		nonzero();
		substitute(globp);
		text_modified++;
#ifdef LOG
		if (c == 's')
			++logstats.lc_substitute;
		else
			++logstats.lc_cslash;
#endif
		continue;

	case 't':
#ifdef TABS
		if ((peekc = getchar()) == '=') {  /* t=c tab char */
			setnoaddr();
			peekc = 0;
			tabc = setchar(0);
#ifdef LOG
			++logstats.lc_tabchar;
#endif
			continue;
		} else if (peekc == ',') {	/* t,nn  set tabs */
			setnoaddr();
#ifdef LOG
			++logstats.lc_tabset;
#endif
			while ((c = getchar()) == ',') {
				while ((c = getchar()) == ',');
				if ((c < '0' || c > '9') && c != '-')
					break;
				peekc = c;
				settab(getsnum());
			}
			peekc = c;
			newline();
			continue;
		} else if (peekc == '\n') {	/* list tabs */
			setnoaddr();
			peekc = 0;
#ifdef LOG
			++logstats.lc_tablist;
#endif
			listabs();
			continue;
		}
#endif
#ifdef LOG
		++logstats.lc_transfer;
#endif
	casecopy:
		move(1);
		text_modified++;
		continue;

#ifdef UNDO
	case 'u':
		setnoaddr();
		newline();
		undo();
#ifdef LOG
		++logstats.lc_undo;
#endif
		continue;
#endif

	case 'v':
		global(0);
		continue;

	case 'w':
#ifdef PAGE
		if ((peekc = getchar()) == '=') {  /* w=<width> */
			peekc = 0;
			ccount = setnum(CCOUNT);
#ifdef LOG
			++logstats.lc_width;
#endif
			continue;
		}
#endif
		if ((peekc = getchar()) == 'i') {	/* wi over-ride */
			++immflg;
			peekc = 0;
#ifdef LOG
			++logstats.lc_wimm;
#endif
		}
#ifdef LOG
		  else
			++logstats.lc_write;
#endif
		filename();
#ifdef LOG
		if (appflg) {
			--logstats.lc_write;
			++logstats.lc_wonto;
		}
#endif
		if (globp) {
			setdot();
			appflg++;
		} else
			setall();
		if (dol == zero) {
			if (curt)
				putsn("?48");
			else
				putsn(errtext[48]);
			if (!fflg || appflg)
				continue;
		}
		if (over || immflg)
			override();
		if (appflg)
			io = open(file, 1);
		if (!appflg || io < 0)
			io = creat(file, MODE);
		if (overfile) {
			chmod(overfile, overmode);
			overfile = 0;
		}
		if (io < 0) {
			io = 0;
			if (curt)
				errmsg(54);
			puts2(errtext[54]);
			putsn(file);
			error();
		}
		io_w++;
		if (appflg)
			lseek(io, 0L, 2);	/* append onto file */
		putfile();
		close(io);
		io = 0;
		io_w = 0;
		if (addr1 == zero + 1 && addr2 == dol)
			text_modified = 0;
#ifdef LOG
		logstats.lt_wlines += dol - zero;
#endif
		continue;

#ifdef USE
	case '@':
		setnoaddr();
		if (alt)
			errmsg(6);
		if ((peekc = getchar()) == 'p')
			peekc = 0;
		else
			eflg2++;
		if ((peekc = getchar()) == '\n' && altfile[0]) {
			peekc = 0;
			goto altname;
		}
		if ((c = getchar()) != ' ' && c != '\t' && c != ',')
			errmsg(5);
		white_space();
		if ((c = getchar()) == '\n')
			errmsg(7);
		p = altfile;
		*p++ = c;
		while ((c = getchar()) != '\n') {
			if (c < 0)
				errmsg(59);
			*p++ = c;
		}
		*p = '\0';
	altname:
#ifdef LOG
		++logstats.lc_at;
#endif
		if ((alt = open(altfile, 0)) < 0) {
			alt = 0;	/* this MUST be 0 */
			lastc = '\n';
			if (curt)
				errmsg(53);
			puts2(errtext[53]);
			putsn(altfile);
			error();
		}
		continue;
#endif

#ifdef DEBUG
	case '#':	/* toggle debug flag */
		if (addr1 != addr2 || addr1 != zero)
			goto illcmd;
		newline();
		tflg ^= 01;
		continue;

#ifdef XDEL
	case '`':
		setnoaddr();
		newline();
		if (ndeleted == 0)
			errmsg(16);
		printf("deleted = %o  ndeleted = %d\n", deleted, ndeleted);
		{ register n, *bp, nl;
		    int tl;

		    tl = deleted;
		    bp = getblock(tl, READ);
#ifdef HUGE
		    tl &= _1[hugef];
#else
		    tl &= _1;
#endif
		    nl = nleft / sizeof(linep);
		    for (n = 0; n < ndeleted; n++) {
		    	printf("%7d: %6o\n", n + 1, *bp++);
		    	if (--nl <= 0) {
#ifdef HUGE
			    bp = getblock(tl += _2[hugef], READ);
#else
			    bp = getblock(tl += _2, READ);
#endif
			    nl = nleft / sizeof(linep);
		    	}
		    }
		}
		continue;
#endif
#endif

#ifdef XDEL
	case 'x':
		newline();
		r = undelete();
#ifdef LOG
		++logstats.lc_xundelete;
#endif
		if (prompt2)
			printf((curt? prcntu : errtext[55]),
			    r, (r == 1? null : quote_s));
		text_modified++;
		continue;
#endif

#ifdef YINT
	case 'y':
		if ((c = getchar()) == '+') {
			setdot();
			nonzero();
			newline();
			if (zero == dol)
				yplus = 0;
			else
				yplus = *addr2 | 01;
			yflg = 1;
#ifdef LOG
			++logstats.lc_yplus;
#endif
		} else if (c == '-') {
			setnoaddr();
			newline();
			yflg = 0;
#ifdef LOG
			++logstats.lc_yminus;
#endif
		} else {
			setnoaddr();
			peekc = c;
			newline();
			yflg = 1;
			yplus = 0;
#ifdef LOG
			++logstats.lc_yintr;
#endif
		}
		continue;
#endif

#ifdef PIPE
	case '|':
#ifndef XED
		if (globp)		/* turkeys will be turkeys */
			errmsg(61);
#endif
		if (noshell)
			errmsg(52);
		if ((peekc = getchar()) == '+') {
			peekc = 0;
			newline();
			setnoaddr();
			strict++;
#ifdef LOG
			++logstats.lc_piplus;
#endif
			continue;
		} else if (peekc == '-') {
			peekc = 0;
			newline();
			setnoaddr();
			strict = 0;
#ifdef LOG
			++logstats.lc_piminus;
#endif
			continue;
		}
		setdot();
		shell();
		continue;
#endif

	case '!':
#ifndef XED
		if (globp)
			errmsg(61);
#endif
		if (noshell)
			errmsg(52);
		setnoaddr();
		shell();
		continue;

#ifdef CKPT
	case 'z':
		newline();
		setnoaddr();
		checkpoint(0);	/* doesn't return */
#endif

	case EOF:
#ifdef USE
		if (!baseflg && alt) {
			close(alt);
			alt = 0;
			eflg2 = 0;
			continue;
		}
#endif
		return;

	}
    illcmd:
		errmsg(8);
    }	/* nothing lasts "for ever" */
}

compile(aeof)
charac aeof;
{
	register charac eof, c;
	register char *ep, *lastep, *bracketp;
	register cclcnt;
	char bracket[NBRA];

	ep = expbuf;
	lastep = 0;
	eof = aeof;
	bracketp = bracket;
	if ((c = getchar()) == eof || c == '\n') {
		if (*ep == 0)
			errmsg(27);
		if (c == '\n')
			peekc = c;
		return;
	}
	circfl = 0;
	nbra = 0;
	if (
#ifdef DUMB
	    !dumbf &&
#endif
			c == '^') {
		c = getchar();
		circfl++;
#ifdef LOG
		++logstats.lp_caret;
#endif
	}
/*	if (c == '*') {		/* if first = *, then quote it */
/*		*ep++ = CCHR;	*/
/*		*ep++ = c;	*/
/*		c = 0;	*/
/*	}	*/
	peekc = c;
	for ever {
		if (ep >= &expbuf[ESIZE - 2 - (bracketp - bracket) * 2])
			break;
		c = getchar();
		if (c == eof || c == '\n') {
			while (bracketp > bracket) {
				*ep++ = CKET;
				*ep++ = *--bracketp;
			}
			*ep++ = CEOF;
			if (c == '\n') {
				if (prompt2)
					pflag++;
				peekc = c;
			}
			return;
		}
		if (c != '*')
			lastep = ep;
#ifdef DUMB
		if (dumbf && c >= 0)
			goto defchar;
#endif
		switch (c) {
		case '\\':
			if ((c = getchar()) == '(') {
				if (nbra >= NBRA) {
					c = 44;
					goto cerror;
				}
				*bracketp++ = nbra;
				*ep++ = CBRA;
				*ep++ = nbra++;
#ifdef LOG
				++logstats.lp_paren;
#endif
				continue;
			}
			if (c == ')') {
				if (bracketp <= bracket) {
					c = 45;
					goto cerror;
				}
				*ep++ = CKET;
				*ep++ = *--bracketp;
				continue;
			}
			if ('1' <= c && c < '1' + nbra) {
				*ep++ = CBACK;
				*ep++ = c - '1';
#ifdef LOG
				++logstats.lp_digit;
#endif
				continue;
			}
			if (c == '\n') {
				c = 46;
				goto cerror;
			}
		defchar:
		default:
			*ep++ = CCHR;
			*ep++ = c;
			continue;

		case '.':
			*ep++ = CDOT;
#ifdef LOG
			++logstats.lp_dot;
#endif
			continue;

		case '*':
			if (lastep == 0 ||
			    *lastep == CBRA || *lastep == CKET)
				goto defchar;
			*lastep |= STAR;
#ifdef LOG
			++logstats.lp_star;
#endif
			continue;

		case '$':
			if ((peekc = getchar()) != eof && peekc != '\n')
				goto defchar;
			*ep++ = CDOL;
#ifdef LOG
			++logstats.lp_dol;
#endif
			continue;

		case '[':
			*ep++ = CCL;
			*ep++ = 0;
			cclcnt = 1;
			if ((c = getchar()) == '^') {
				c = getchar();
				ep[-2] = NCCL;
#ifdef LOG
				++logstats.lp_nccl;
#endif
			}
#ifdef LOG
			  else
				++logstats.lp_ccl;
#endif
			do {
				if (c == '\\')
					c = getchar();
				if (c == '\n') {
					peekc = c;
					break;
				}
				if (c < 0) {
					c = 59;
					goto cerror;
				}
				*ep++ = c;
				cclcnt++;
				if (ep >= &expbuf[ESIZE - 2]) {
					c = 49;
					goto cerror;
				}
			} while ((c = getchar()) != ']');
			lastep[1] = cclcnt;
			continue;

		case EOF:
			c = 59;
			goto cerror;
		}
	}
	c = 42;
   cerror:
	expbuf[0] = '\0';
	nbra = 0;
	errmsg(c);
}

compsub() {
	register charac seof, c;
	register char *p;

	if ((seof = getchar()) == '\n')
		errmsg(40);
	compile(seof);
	p = rhsbuf;
	for ever {
		c = getchar();
		if (c < 0)
			errmsg(59);
		if (c == '\\')
			c = getchar() | 0200;
		if (c == '\n') {
			*p = 0;
			peekc = 0;
			if (prompt2)
				pflag++;
			return(0);
		}
		if (c == seof)
			break;
		*p++ = c;
		if (p >= &rhsbuf[LBSIZE / 2 - 2])
			errmsg(41);
	}
	*p = '\0';
	p = 0;
	if ((peekc = getchar()) == 'g') {	/* if in 'glob' */
		peekc = 0;
		p = 1;
	} else if ('0' <= peekc && peekc <= '9')
		if ((s_cnt = getnum()) < 0)
			s_cnt = 0;
	newline();
	return(p);
}

create(as, am)
char *as;
{
	register savmask;
	register func savint, savqit;
	register ret;

	if (am == LMODE) {
		savint = signal(SIGINT, 1);
		savqit = signal(SIGQIT, 1);
		savmask = umask(LMASK);
	}
	ret = creat(as, am);
	if (am == LMODE) {
		umask(savmask);
		signal(SIGINT, savint);
		signal(SIGQIT, savqit);
	}
	return(ret);
}

delete() {
	register linep *a1, *a2, *a3;

	a1 = addr1;
	a2 = addr2 + 1;
#ifdef XDEL
	if (!globp)
		saveline();
#endif
	a3 = dol;
	dol -= a2 - a1;
	do {
		*a1++ = *a2++;
	} while (a2 <= a3);
	a1 = addr1;
	if (a1 > dol)
		a1 = dol;
	dot = a1;
}

delexit(aretcode) {
#ifdef LOG
#include <sys/times.h>
	struct tms t;
#endif

	unlink(tfname);
#ifdef LOG
	if ((io = open(LOG, 1)) >= 0) {
		time(&logstats.lt_end);
		times(&t);
		logstats.lt_usercpu = t.tms_utime;
		logstats.lt_syscpu = t.tms_stime;
		logstats.lt_kidscpu = t.tms_cutime + t.tms_cstime;
		lseek(io, 0L, 2);
		write(io, &logstats, sizeof logstats);
	}
#endif
	exit(aretcode);
}

dosub() {
	register char *lp, *sp, *rp;
	register charac c;

	lp = linebuf;
	sp = genbuf;
	rp = rhsbuf;
	while (lp < loc1)
		*sp++ = *lp++;
	while (c = *rp++) {
#ifdef DUMB
		if (!dumbf)
#endif
			if (c == '&') {
#ifdef LOG
				++logamp;
#endif
				sp = place(sp, loc1, loc2);
				continue;
			} else if (c < 0 && (c &= 0177) >= '1' &&
			    c < nbra + '1') {
				c -= '1';
				sp = place(sp,braslist[c],braelist[c]);
				continue;
			}
		*sp++ = c & 0177;
#ifndef ALLOC
		if (sp >= &genbuf[LBSIZE - 2])
#else
		if (sp >= &genbuf[lbsize - 2])
#endif
			errmsg(42);
	}
	lp = loc2;
	loc2 = sp + (linebuf - genbuf);
	while (*sp++ = *lp++)
#ifndef ALLOC
		if (sp >= &genbuf[LBSIZE - 2])
#else
		if (sp >= &genbuf[lbsize - 2])
#endif
			errmsg(43);
	lp = linebuf;
	sp = genbuf;
	while (*lp++ = *sp++);
}

#ifdef DEBUG
dump() {
	register linep *i;
	register b, o;

	setdot();
	nonzero();
	newline();
	line_num = addr1 - zero;
	for (i = addr1; i <= addr2; i++) {
#ifdef HUGE
		b = (*i >> (8 - hugef)) & _3[hugef];
		o = (*i << (1 + hugef)) & _4[hugef];
#else
		b = (*i >> 8) & _3;
		o = (*i << 1) & _4;
#endif
		printlno(line_num++);
#ifdef pdp11
		printf("%6o:%7o%6d/%d\n", i, *i, b, o);
#else
		printf("%6o:%7o%6d/%d\n", i, *i&(unsigned)(unsigned short)-1, b, o);
#endif
	}
	dot = addr2;
}
#endif

echo(ch)
charac ch;
{
#ifdef AGAIN
	static charac lastchar;
#endif
#ifdef USE
	if (eflg || alt && !eflg2)
#endif
#ifndef USE
	if (eflg)
#endif
		write(2, &ch, 1);
#ifdef AGAIN
	if (!agf && agp) {	/* save current command for "again" */
		if (agp >= &agbuf[GBSIZE - 2])
			agp = agbuf[0] = 0;
		else
			*agp++ = ch;
		if (ch == '\n' && lastchar != '\\')
			agp = *agp = 0;
	}
	lastchar = ch;
#endif
	return(ch);
}

errfunc() {
	if (io) {
		close(io);
		io = 0;
		io_w = 0;
	}
	if (overfile) {
		chmod(overfile, overmode);
		overfile = 0;
	}
#ifdef HELP
	if (doc) {
		close(doc);
		doc = 0;
	}
#endif
#ifdef PIPE
	if (pfile) {
		close(pfile);
		unlink(pfname);
		pfile = 0;
	}
#endif
/*	puts("?");	*/
	if (!seekf)
		lseek(0, 0L, 2);
	pflag = 0;
	listf = 0;
#ifdef PARENS
	parenf = 0;
#endif
#ifdef STRLEN
	quotef = 0;
#endif
	if (globp) {
		lastc = '\n';
		globp = 0;
	}
	peekc = lastc;
	skip_rest();
	eflg2 = 0;
	reset();
}

errmsg(n) {
	extern errno;

	listf = 0;
#ifdef PARENS
	parenf = 0;
#endif
#ifdef STRLEN
	quotef = 0;
#endif
	col = 0;
	if (n < 0) {
		puts2("\7******** ");
		printf("%d ", errno);
		n = -n;
	}
	if (curt)
		printf("?%d\n", n);
	else
		if (0 <= n && n < NERR)
			putsn(errtext[n]);
		else
			printf("bad error number: %d\n", n);
	error();
}

execute(gf, addr)
linep *addr;
{
	register char *p1, *p2;
	register charac c;

	if (gf) {
		if (circfl)
			return(0);
		p1 = linebuf;
		p2 = genbuf;
		while (*p1++ = *p2++);
		locs = p1 = loc2;
	} else {
		if (addr == zero)
			return(0);
		p1 = getline(*addr);
		locs = 0;
	}
	p2 = expbuf;
	c = NBRA;
	while (--c >= 0) {
		braslist[c] = 0;
		braelist[c] = 0;
	}
	if (circfl) {
		loc1 = p1;
		return(advance(p1, p2));
	}
	/* fast check for first character */
	if (*p2 == CCHR) {
		c = p2[1];
		do {
			if (*p1 != c)
				continue;
			if (advance(p1, p2)) {
				loc1 = p1;
				return(1);
			}
		} while (*p1++);
		return(0);
	}
	/* regular algorithm */
	do {
		if (advance(p1, p2)) {
			loc1 = p1;
			return(1);
		}
	} while (*p1++);
	return(0);
}

#ifdef TABS
exp() {
	register n;
	register linep p, *a1;

	setdot();
	nonzero();
	n = 0;
	for (a1 = addr1; a1 <= addr2; a1++) {
		getline(*a1);
		if (expand()) {
			n++;
			p = *a1;
			*a1 = putline();
			savemark(p, *a1);
		}
	}
	dot = addr2;
	return(n);
}

expand() {
	register char *p2;
	register charac c;
	register n;
	register char *p1;
	register cnt, i;
	register flag tabflg;

	p1 = linebuf;
	p2 = genbuf;
	while (*p2++ = *p1++);
	p2 = linebuf;
	p1 = genbuf;
	cnt = col = n = 0;
	while (c = *p1++) {
		if (c == tabc && col < maxtab) {
			n = col;
			c = tabfill;
			if (c == '\t') {
				n = col;
				tabflg = 0;
				while (n < maxtab) {
					if ((++n & 07) == 0) {
						*p2++ = c;
						tabflg++;
						if (p2 >=
#ifndef ALLOC
						    &linebuf[LBSIZE - 2]
#else
						    &linebuf[lbsize - 2]
#endif
						    || n >= TABS * BPW)
							goto experr;
					}
					if (tabs[n >> BPWC] >> (n & BPWM) & 01)
						if (*p1 == tabc &&
						    n < maxtab) {
							p1++;
							cnt++;
						} else
							break;
				}
				i = n;
				if (tabflg)
					n &= 07;
				else
					n -= col;
				col = i;
				while (n--) {
					*p2++ = ' ';
#ifndef ALLOC
					if (p2 >= &linebuf[LBSIZE - 2])
#else
					if (p2 >= &linebuf[lbsize - 2])
#endif
						goto experr;
				}
				cnt++;
				continue;
			}
			do {
				*p2++ = c;
#ifndef ALLOC
				if (p2 >= &linebuf[LBSIZE - 2]
#else
				if (p2 >= &linebuf[lbsize - 2]
#endif
				    || n >= TABS * BPW)
					goto experr;
				n++;
			} while (!(tabs[n >> BPWC] >> (n & BPWM) & 01));
			col = n;
			cnt++;
			continue;
		} else if ('\0' < c && c < ' ') {
			switch (c) {
			case '\b':
				col -= 2;
				break;
			case '\t':
				col += 7 - (col & 07);
				break;
			case '\r':
				col = -1;
				break;
			default:
				col--;
			}
		}
		if (c) {
			*p2++ = c;
			col++;
#ifndef ALLOC
			if (p2 >= &linebuf[LBSIZE - 2] ||
#else
			if (p2 >= &linebuf[lbsize - 2] ||
#endif
			    col >= TABS * BPW)
	experr:			errmsg(12);
		}
	}
	*p2++ = 0;
	return(cnt);
}
#endif

#ifdef DEBUG
#ifdef EXPDMP
expdmp() {
	register n;
	register char *ep, **p;
	register flag star;

	signal(SIGQIT, expdmp);
	ep = expbuf;
	putchar('"');
	if (circfl)
		putchar('^');
	while (*ep) {
		n = 0;
		star = 0;
		switch (*ep++) {
		case CCHR | STAR:
			star++;
		case CCHR:
			putchar(*ep++);
			break;
		case CDOT | STAR:
			star++;
		case CDOT:
			putchar('.');
			break;
		case CDOL:
			putchar('$');
			break;
		case CEOF:
			putchar('"');
			goto done;
		case NCCL | STAR:
			star++;
		case NCCL:
			n++;
			goto ccl;
		case CCL | STAR:
			star++;
		case CCL: ccl:
			putchar('[');
			if (n)
				putchar('^');
			n = *ep++;
			while (--n)
				putchar(*ep++);
			putchar(']');
			break;
		case CBRA:
			putchar('\\');
			putchar('(');
			ep++;
			break;
		case CKET:
			putchar('\\');
			putchar(')');
			ep++;
			break;
		case CBACK | STAR:
			star++;
		case CBACK:
			putchar('\\');
			putchar(*ep++ + '1');
			break;
		default:
			putchar('?');
			break;
		}
		if (star)
			putchar('*');
	}
done:	putchar('\n');
	if (reading)
		reset();
}
#endif
#endif

filename() {
	register char *p1, *p2;
	register charac c;

	appflg = 0;
	c = getchar();
	if (c < 0)
		errmsg(59);
	if (c == '\n') {
    noname:
		if (savedfile[0] == 0)
			errmsg(62);
		scopy(savedfile, file);
		return;
	}
	if (c != ' ' && c != '\t' && c != ',' && c != '>')
		errmsg(5);
	if (c != ',')
		peekc = c;
	white_space();
	if ((c = getchar()) == '>') {
		while ((c = getchar()) == '>');
		appflg++;
		if (c == '\n')
			goto noname;
		peekc = c;
		white_space();
		c = getchar();
	}
	if (c == '\n')
		errmsg(7);
	p1 = file;
	do {
		*p1++ = c;
		if (p1 >= &file[FNSIZE - 2])
			errmsg(11);
	} while ((c = getchar()) != '\n');
	*p1++ = 0;
	if (savedfile[0] == 0)
		scopy(file, savedfile);
}

linep *
findmark(alp, adflt)
linep alp, *adflt;
{
	register linep *a, v;
#ifdef HUGE
	register i;

	i = hugef ^ 01;
#endif
	v = alp;
	for (a = zero + 1; a <= dol; a++)
		if (v == (*a |
#ifdef HUGE
				i
#else
				1
#endif
				))
			return(a);
	return(adflt);
}

flush_buf() {
	register n;

	if (n = linp - line) {
		linp = line;
		write(fout, line, n);
	}
}

#ifdef G_VFY
gask(a1)
linep *a1;
{
	register charac c;

	col = 0;
	printlno(line_num = a1 - zero);
	puts(linebuf);
	puts2("Ok? ");
	flush_buf();
	if ((c = getchar()) < 0) {
		putchar('\n');
		return(-1);
	}
	if (c == '\n')
		return(1);
	skip_rest();
	return(c != 'n');
}
#endif

char *
getblock(atl, iof) {
	extern read(), write();
	register bno, off;
	register linep *a;
#ifdef HUGE
	register flag c;
	register func savint;

	bno = (atl >> (8 - hugef)) & _3[hugef];
	off = (atl << (1 + hugef)) & _4[hugef];
#else
	bno = (atl >> 8) & _3;
	off = (atl << 1) & _4;
#endif
	if (bno >=
#ifdef HUGE
		   _5[hugef]
#else
		   _5
#endif
			    ) {
#ifdef HUGE
		if (!hugef && !globp) {	/* let's try converting to huge */
			if (!curt) {
				putsn("File too large for normal editing.");
				puts2("Conversion to \"huge\" mode disallows ");
				puts2("use of the global command.");
			}
			c = yes_no(curt? "?huge" : "Do you wish to convert",
			    1, 0, 0);
			if (c) {
				savint = signal(SIGINT, 1);
				for (a = zero + 1; a <= dol; a++)
					*a = ((unsigned)*a) >> 1;
				hugef++;
				hugef2++;	/* conversion occurred */
				atl = (atl >> 1) & _6[hugef];
#ifdef XDEL
				ndeleted = 0;	/* useless pointers */
				deleted = 0;
#endif
				signal(SIGINT, savint);
				goto huger;
			}
		}
#endif
		errmsg(34);
	}
#ifdef HUGE
    huger:
#endif
	nleft = BLKSIZE - off;
	if (bno == iblock && !badf) {
		ichanged |= iof;
		return(ibuff + off);
	}
	if (bno == oblock)
		return(obuff + off);
	if (iof == READ) {
		if (ichanged) {
			blkio(iblock, ibuff, write);
			ichanged = 0;
		}
		iblock = bno;
		blkio(bno, ibuff, read);
		return(ibuff + off);
	}
	if (oblock >= 0)
		blkio(oblock, obuff, write);
	oblock = bno;
	return(obuff + off);
}

charac
getchar() {
	flush_buf();
	eof = 0;
	if (lastc = peekc) {
		peekc = 0;
		return(lastc);
	}
	if (globp) {
		if (lastc = *globp++) {
			if (globf2 && *globp == 0)
				globp = globf2 = 0;
			return(lastc);
		}
		globp = 0;
		return(EOF);
	}
#ifdef AGAIN
	if (agf
#ifdef USE
		&& !alt
#endif
			) {	/* "again" stuff */
		if (lastc = *agp++)
			return(lastc);
		agf = agp = 0;
	}
#endif
	reading++;
#ifdef USE
	if (read(alt, &lastc, 1) <= 0) {
#endif
#ifndef USE
	if (read(0, &lastc, 1) <= 0) {
#endif
		reading = 0;
		eof++;
		return(lastc = EOF);
	}
	reading = 0;
	lastc &= 0177;
#ifdef APLMAP
	if (aplmap && lastc > ' ')
		lastc = map_ascii[lastc - (' ' + 1)];
#endif
#ifdef EOL
	if (lastc == '\n')
		prompt3 = 1;
	else if (eol && lastc == eol) {
		lastc = '\n';
		prompt3 = 0;
	}
#endif
#ifdef CMDS
	if (cmd)
		write(cmd, &lastc, 1);
#endif
	eof = 0;
	return(echo(lastc));
}

getcopy() {
	if (addr1 > addr2)
		return(EOF);
	getline(*addr1++);
	return(0);
}

getfile() {
	register charac c;
	register char *lp, *fp;

	lp = linebuf;
	fp = nextip;
	do {
		if (--ninbuf < 0) {
#ifndef ALLOC
			if ((ninbuf = read(io, genbuf, LBSIZE) - 1) < 0)
#else
			if ((ninbuf = read(io, genbuf, lbsize) - 1) < 0)
#endif
				if (lp == linebuf)
					return(EOF);
				else {
					lp++;
					break;
				}
			fp = genbuf;
		}
#ifndef ALLOC
		if (lp >= &linebuf[LBSIZE - 2] && (*fp & 0177) != '\n') {
#else
		if (lp >= &linebuf[lbsize - 2] && (*fp & 0177) != '\n') {
#endif
			printf((curt? "?long%u\n" : "line %u in file too long\n"),
			    dot - zero + 1);
			flush_buf();
			lp++;
			ninbuf++;
			break;
		}
		if ((*lp++ = c = *fp++ & 0177) == 0) {
			lp--;
			continue;
		}
	} while (c != '\n');
	*--lp = 0;
	nextip = fp;
	return(0);
}

getline(tl) {
	register char *bp, *lp;
	register nl;

#ifdef DEBUG
	if (tflg)
		printf("getline:\t%o\n", tl);
#endif
	lp = linebuf;
	bp = getblock(tl, READ);
#ifdef HUGE
	if (hugef2) {
		hugef2 = 0;
		tl = (tl >> 1) & _6[hugef];
	}
	tl &= _1[hugef];
#else
	tl &= _1;
#endif
	nl = nleft;
	while (*lp++ = *bp++)
		if (--nl <= 0) {
#ifdef HUGE
			bp = getblock(tl += _2[hugef], READ);
#else
			bp = getblock(tl += _2, READ);
#endif
			nl = nleft;
		}
	return(linebuf);
}

getnum() {
	register n;
	register charac c;

	n = 0;
	while ('0' <= (c = getchar()) && c <= '9')
		n = n * 10 + c - '0';
	peekc = c;
	return(n);
}

getsnum() {
	register sign;

	if (sign = (peekc = getchar()) == '-')
		peekc = 0;
	return(sign? -getnum() : getnum());
}

getsub() {
	register char *p1, *p2;

	p1 = linebuf;
	if ((p2 = linebp) == 0)
		return(EOF);
	while (*p1++ = *p2++);
	linebp = 0;
	return(0);
}

gettty(single) {
	register charac c;
	register char *p, *gf;
#ifdef TABS
	int tabf;

	tabf = 0;
#endif
	p = linebuf;
	gf = globp;
#ifdef EOL
	if (prompt2 && prompt3 && !single) {
#endif
#ifndef EOL
	if (prompt2 && !single) {
#endif
		printlno(line_num);
		flush_buf();
	}
	line_num++;
	while ((c = getchar()) != '\n') {
		if (c < 0) {
			if (gf)
				peekc = c;
			else if (prompt2 && (prompt1 || !zflg))
				putchar('\n');
			if (p > linebuf)
				break;
			return(c);
		}
		if ((c &= 0177) == 0)
			continue;
#ifdef TABS
		if (tabc && c == tabc)
			tabf++;
#endif
		*p++ = c;
#ifndef ALLOC
		if (p >= &linebuf[LBSIZE - 2])
#else
		if (p >= &linebuf[lbsize - 2])
#endif
			errmsg(30);
	}
	*p++ = 0;
#ifdef TABS
	if (tabf)
		expand();
#endif
	if (!single && linebuf[0] == '.' && linebuf[1] == 0)
		return(EOF);
	return(0);
}

global(k) {
	register char *gp;
	register charac c;
	register linep *a1;
	register flag globpf;
	char globuf[GBSIZE];

#ifdef HUGE
	if (hugef)
		errmsg(10);
#endif
	if (globp)
		errmsg(37);
	setall();
	nonzero();
	if ((c = getchar()) == '\n')
		peekc = c;
	compile(c);
#ifdef G_VFY
	gaskf = 0;
	if ((peekc = getchar()) == 'v') {
		gaskf++;
		peekc = 0;
	}
#endif
	globpf = pflag;
	if (peekc == '\n')
		globpf = 0;
	pflag = 0;
	gp = globuf;
	if ((peekc = getchar()) < 0)
		errmsg(59);
	if (peekc == '\n') {
		*gp++ = 'p';
		peekc = 0;
	} else
		while ((c = getchar()) != '\n') {
			if (c == '\\') {
				c = getchar();
				if (c != '\n')
					*gp++ = '\\';
			}
			if (c < 0)
				errmsg(59);
			*gp++ = c;
			if (gp >= &globuf[GBSIZE - 2])
				errmsg(38);
		}
	*gp++ = '\n';
	*gp = 0;
	c = 0;
#ifdef LOG
#ifdef G_VFY
	if (gaskf)
		++logstats.lc_gvfy;
#endif
	if (k)
		++logstats.lc_global;
	else
		++logstats.lc_vglobal;
#endif
	for (a1 = zero + 1; a1 <= dol; a1++) {
		*a1 &= ~01;
		s_tmp = 0;
		if (a1 >= addr1 && a1 <= addr2 && execute(0, a1) == k)
#ifdef G_VFY
			if (!gaskf || (c = gask(a1)))
				if (c < 0)
					addr2 = a1;
				else
#endif
					*a1 |= 01;
	}
	for (a1 = zero + 1; a1 <= dol; a1++)
		if (*a1 & 01) {
			*a1 &= ~01;
			dot = a1;
			globp = globuf;
			pflag = globpf;
			commands(1);
			a1 = zero;
		}
}

hangup() {
	signal(SIGHUP, 1);
	if (reading) {
		backup(HUP);
		if (fout != 1) {
			flush_buf();
			fout = 1;
		}
		puts2(errtext[57]);
		delexit(1);
	}
	hupflag++;
}

header() {
	register colnum, number;
	register flag lf;

	number = 0;
	if ((peekc = getchar()) != '\n') {
		white_space();
		number = getnum();
	}
	if (!number)
		number = ccount - (prompt1? 8 : 0);
	newline();
	if (zero != dol)
		dot = addr2;
	lf = listf;
	listf = 0;
	if (prompt1)
		putchar('\t');
	for (colnum = 0; colnum < number / 10; colnum++)
		printf("%10d", colnum + 1);
	putchar('\n');
	if (prompt1)
		putchar('\t');
	for (colnum = 1; colnum <= number; colnum++)
#ifdef TABS
		if (colnum < (TABS << BPWC) &&
		    (tabs[(colnum - 1) >> BPWC] >>
		    ((colnum - 1) & BPWM) & 01))
			putchar('-');
		else
#endif
			putchar(colnum % 10 + '0');
	putchar('\n');
	listf = lf;
}

init() {
	register n;
	register char *p;

	if (tfile > 0)
		close(tfile);
	if (io) {
		close(io);
		io = 0;
		io_w = 0;
	}
#ifdef HELP
	if (doc) {
		close(doc);
		doc = 0;
	}
#endif
#ifdef USE
	if (alt) {
		close(alt);
		alt = 0;
		eflg2 = 0;
	}
#endif
#ifdef PIPE
	if (pfile) {
		close(pfile);
		unlink(pfname);
		pfile = 0;
	}
#endif
	tline = 0;
#ifdef XDEL
	deleted = 0;
	ndeleted = 0;
#endif
	num_reads = 0;
	text_modified = 0;
#ifdef YINT
	yplus = 0;
#endif
#ifdef UNDO
	undo_oldp = 0;
	undo_newp = 0;
#endif
#ifdef V7
	if (overfile) {
		chmod(overfile, overmode);
		overfile = 0;
	}
#endif
	iblock = oblock = -1;
	badf = 0;
	ichanged = 0;
	if ((n = create(tfname, LMODE)) < 0) {
		putsn(errtext[50]);
		exit(1);
	}
	close(n);
	if ((tfile = open(tfname, 2)) < 0) {
		putsn(errtext[36]);
		exit(1);
	}
	addr1 = addr2 = dot = dotdot = lastdot = zero = dol = fendcore;
	endcore = fendcore - 2;
	brk(fendcore);
	n = 26;
	while (--n >= 0) {	/* kill marks */
		names[n] = 0;
#ifdef EXTMARK
		names2[n] = 0;
#endif
	}
	p = globp;
	globp = 0;
	if (chk()) {
		if ((io = open(file, 0)) < 0) {
			io = 0;
			lastc = '\n';
			if (curt)
				putsn("?53");
			else {
				puts2(errtext[53]);
				putsn(file);
			}
			flush_buf();
		} else {
			n = append(getfile, addr1, 0, 0);
			if (prompt2)
				printf((curt? prcntu : errtext[55]),
				    n, (n == 1? null : quote_s));
			if (n) {
				num_reads++;
				text_modified++;
			}
			if (io) {
				close(io);
				io = 0;
				io_w = 0;
			}
		}
		return(1);
	}
	globp = p;
	return(0);
}

istty(fd) {
	register r;
	short buf[3];	/* naughty int's */

	buf[2] = BS1;
	r = 0;
	if (gtty(fd, buf) >= 0)
		++r;
#ifdef CLEAR
	zflg = (buf[2] & BS1) == 0;
#endif
	if (pipef
#ifdef V7
		  || getenv("_NO_STDIO_BUF")
#endif
					    )
		++r;
	return(r);
}

#ifdef XED
join() {
	register linep *a1;
	register char *p1, *p2;
	register charac c, ceof;

	setdot();
	nonzero();
	if (addr1 == addr2)
		--addr1;
	if (addr1 <= zero)
		errmsg(60);

	p2 = rhsbuf;
	if ((c = peekc = getchar()) < 0)
		errmsg(59);
	ceof = 0;
	if (peekc != '\n' && peekc != 'p' && peekc != 'l'
#ifdef PARENS
	    && peekc != 'b'
#endif
#ifdef STRLEN
			    && peekc != 'q'
#endif
					   ) {
		ceof = peekc;
		peekc = 0;
		while ((c = getchar()) != ceof && c != '\n') {
			if (c < 0)
				errmsg(59);
			if (c == '\\')
				if ((c = getchar()) == '\n')
					errmsg(46);
			*p2++ = c;
			if (p2 >= &rhsbuf[LBSIZE / 2 - 2])
				errmsg(42);
		}
		if (c != '\n')
			c = '\0';
		else
			if (prompt2)
				pflag++;
		if (p2 != rhsbuf)
			*p2 = '\0';
		if (*rhsbuf == 0)
			errmsg(27);
#ifdef LOG
		++logstats.lc_jglue;
#endif
	}
#ifdef LOG
	  else
		++logstats.lc_join;
#endif
	peekc = c;
	newline();

	p1 = rhsbuf;
	while (*p1)
		if (*p1++ == '\n')
			errmsg(46);

	p2 = genbuf;
	a1 = addr1;

	while (a1 <= addr2) {
		p1 = getline(*a1++);
		while (*p2++ = *p1++)
#ifndef ALLOC
			if (p2 >= &genbuf[LBSIZE - 2])
#else
			if (p2 >= &genbuf[lbsize - 2])
#endif
				errmsg(12);
		--p2;
		if (ceof && a1 <= addr2) {
			p1 = rhsbuf;
			while (*p2++ = *p1++)
#ifndef ALLOC
				if (p2 >= &genbuf[LBSIZE - 2])
#else
				if (p2 >= &genbuf[lbsize - 2])
#endif
					errmsg(12);
			--p2;
		}
	}
	*p2 = '\0';

	p1 = genbuf;
	p2 = linebuf;
	while (*p2++ = *p1++);

	a1 = putline();
	savemark(*addr1, a1);
	*addr1++ = a1;
	delete();
	dot = --addr1;
	if (dot > dol)
		dot = dol;
}
#endif

#ifdef TABS
listabs() {
	register n;

	if (maxtab < 0)
		errmsg(9);
	n = next_tab(-1);
	printf("tabs: %d", n + 1);
	while ((n = next_tab(n)) >= 0)
		printf(",%d", n + 1);
	putchar('\n');
}
#endif

mail() {
	delexit(1);
}

move(cflag) {
	extern getcopy();
	register linep *adt, *ad1, *ad2;

	setdot();
	nonzero();
	if ((adt = address()) == 0)
		errmsg(31);
	ad1 = addr1;
	ad2 = addr2;
	newline();
	if (cflag) {
		ad1 = dol;
		append(getcopy, ad1++, 0, 0);
		ad2 = dol;
	}
	ad2++;
	if (adt < ad1) {
		dot = adt + (ad2 - ad1);
		if (++adt == ad1)
			return;
		reverse(adt, ad1);
		reverse(ad1, ad2);
		reverse(adt, ad2);
	} else if (adt >= ad2) {
		dot = adt++;
		reverse(ad1, ad2);
		reverse(ad2, adt);
		reverse(ad1, adt);
	} else
		errmsg(28);
}

newline() {
	register charac c;

	if ((c = getchar()) == '\n')
		return;
	if (c == 'p' || c == 'l'
#ifdef PARENS
				 || c == 'b'
#endif
#ifdef STRLEN
					     || c == 'q'
#endif
							) {
		pflag++;
		if (c == 'l')
			listf++;
#ifdef PARENS
		else if (c == 'b')
			parenf++;
#endif
#ifdef STRLEN
		else if (c == 'q')
			quotef++;
#endif
		if (getchar() == '\n')
			return;
	}
	errmsg(25);
}

#ifdef TABS
next_tab(acol) {
	register n;

	if ((n = acol) < maxtab) {
		do {
			n++;
		} while (!(tabs[n >> BPWC] >> (n & BPWM) & 01));
		return(n);
	}
	return(-1);
}
#endif

nonzero() {
	if (addr1 <= zero)
		errmsg(23);
	if (addr2 > dol)
		errmsg(24);
}

#ifdef PIPE
oil_spilled() {
	if (!iflg) {
		signal(SIGINT, 1);
		signal(SIGQIT, 1);
	}
	signal(SIGPIP, 1);
	piperr++;
}
#endif

onintr() {
	register char *p1, *p2;

	signal(SIGINT, onintr);
#ifdef USE
	if (alt) {
		close(alt);
		alt = 0;
		eflg2 = 0;
	}
#endif
	if (io) {
		if (curt)
			putsn("?intI/O");
		else {
			puts2("\7Interrupted I/O!!!\7\nWarning: \"");
			puts2(file);
			if (io_w) {
				putsn("\" is probably truncated.");
				puts2("You should probably re-write it.");
			} else {
				putsn("\" was not read entirely.");
				puts2("If you write the buffer, ");
				puts2("part of the file may be lost.");
			}
		}
		flush_buf();
	}
#ifdef EOL
	prompt3 = 1;
#endif
	if (fout != 1) {
		close(fout);
		fout = 1;
	}
#ifdef YINT
	if (!globp && reading && yflg) {
		globp = ".:\n";
		if (yplus) {
			dot = findmark(yplus, 0);
		} else
			globp++;
		peekc = 0;
		putchar('\n');
		commands(1);
		reset();
	}
#endif
	if (iflg) {
		signal(SIGINT, 1);
		backup(INT);
		delexit(1);
	}
	putchar(lastc = '\n');
	errmsg(29);
}

override() {
	struct stat s;

	if (access(file, 02) < 0) {
		if (stat(file, &s) >= 0 && (s.st_mode & S_IFMT) == S_IFREG) {
			overmode = s.st_mode & 07777;
			if (chmod(file, overmode) < 0)
				return;
			if (immflg == 0) {
				if (curt) {
					puts2("?\"");
					puts2(file);
					puts2("\"  ");
				} else {
					puts2("The file \"");
					puts2(file);
					puts2("\" is write-protected.");
				}
				if (yes_no(curt? "override" :
				    "Do you wish to over-ride the permission",
				    0, 1, 1))
					return;
			}
			overfile = file;
			if (chmod(overfile, overmode | 0200) < 0)
				overfile = 0;
			else {
				putsn("[over-riding]");
#ifdef LOG
				++logstats.lm_overwrite;
#endif
			}
		}
	}
}

#ifdef PAGE
page() {
	register cl, n;
	register char *p, *pp;
	register linep *a, *b;
	register l;

	a = addr1;
	if (addr2 != addr1)
		b = addr2;
	else
		b = dol;
#ifdef PARENS
	parenc[0] = 0;
	parenc[1] = 0;
	parenc[2] = 0;
#endif
	for (n = pcount; n >= 0 && a <= b; n--) {
		pp = p = getline(*a);
		cl = prompt1? 8 : 0;
		l = 0;
		while (*p) {
			if (*p < ' ' || *p > '~')
				switch (*p) {
				case '\b':
					if (listf)
						cl++;
					else {
						cl--;
						if (aflg
#ifdef PARENS
							 && !parenf
#endif
#ifdef STRLEN
							 && !quotef
#endif
								   )
							l++;
					}
					break;
				case '\t':
					if (listf)
						cl++;
					else
						cl += 8 - cl % 8;
					break;
				case '\r':
					if (listf)
						cl += 2;
					else
						cl = 0;
					break;
				case ctrl('L'):	/* ADM-3A's */
					if (listf)
						cl++;
					cl++;
					break;
				default:
					if (listf || zflg)
						cl += 2;
				}
			else
				cl++;
#ifdef PARENS
			if (parenf && paren(*p))
				l++;
#endif
#ifdef STRLEN
			/* if (quotef && (*p == '"' || *p == '\'')) */
			if (quotef && (quotec? *p == quotec :
			    *p == '"' || *p == '\''))
				l++;
#endif
			if (cl < 0)
				cl = 0;
			else if (cl > ccount) {
				cl = 0;
				if (--n < 0)
					goto done;
			}
			p++;
		}
		if (l)
			if (--n < 0)
				goto done;
		col = 0;
		printlno(line_num = a - zero);
		puts(pp);
		a++;
	}
    done:
	dot = a - 1;
}
#endif

#ifdef PARENS
paren(ac) {
	switch (ac) {
	case '(':	return(1);
	case '[':	return(2);
	case '{':	return(3);
	case ')':	return(-1);
	case ']':	return(-2);
	case '}':	return(-3);
	}
	return(0);
}
#endif

place(asp, al1, al2) {
	register char *sp, *l1, *l2;

	sp = asp;
	l1 = al1;
	l2 = al2;
	while (l1 < l2) {
		*sp++ = *l1++;
#ifndef ALLOC
		if (sp >= &genbuf[LBSIZE - 2])
#else
		if (sp >= &genbuf[lbsize - 2])
#endif
			errmsg(42);
	}
	return(sp);
}

/*VARARGS*/
printf(as, aarglist)
char *as;
{
	register *args, w;
	register char *q;
	register flag f;

	args = &aarglist;
	while (*as) {
		if (*as != '%') {
			putchar(*as++);
			continue;
		}
		if (*++as == '\0')
			return;
		w = 0;	/* no default width */
		f = 0;	/* unsigned default */
		while ('0' <= *as && *as <= '9')
			w = w * 10 + *as++ - '0';
		if (*as == '\0')
			return;
		switch (*as++) {
		case 'c':	/* char */
			putchar(*args++);
			continue;
		case 'd':	/* signed decimal */
			f = 1;	/* signed */
		case 'u':	/* unsigned decimal */
			printfn(*args++, 10, f, w);
			continue;
		case 'o':	/* unsigned octal */
			printfn(*args++, 8, 0, w);
			continue;
		case 's':	/* string */
			q = *args++;
			while (*q) {
				putchar(*q++);
				--w;
			}
			while (--w > 0)
				putchar(' ');
			continue;
		default:
			putchar(as[-1]);
			continue;
		}
	}
}

printfn(an, ab, af, aw) {
	register w;
	register unsigned n;
	register char *p;
	char buf[(sizeof(int) != 2? 10 : 6) + 2];

	w = aw;
	p = &buf[sizeof buf];
	*--p = '\0';
	n = an;
	if (af)
		if (an < 0)
			n = -an;
		else
			af = 0;
	do {
		*--p = n % ab + '0';
		--w;
	} while (n /= ab);
	if (af) {
		*--p = '-';
		--w;
	}
	while (--w >= 0)
		putchar(' ');
	while (*p)
		putchar(*p++);
}

printlno(an) {
	register flag l;

	if (prompt1) {
		l = listf;
		listf = 0;
		printf(fmtlno, an - aflg);
		col = 8;
		listf = l;
	}
}

printmarks() {
	register linep *a1;
	register charac c;
	register n, i;

#ifdef HUGE
	i = hugef ^ 01;
#endif
	for (c = 'a' - 'a'; c <= 'z' - 'a'; c++) {
	    if (names[c] == 0)	/* zap marks to deleted lines */
		continue;
	    n = 0;
	    for (a1 = zero + 1; a1 <= dol; a1++)
		if (names[c] == (*a1 |
#ifdef HUGE
					i
#else
					01
#endif
					  )) {
		    n++;
		    printf("%c=%u", c + 'a', a1 - zero);
#ifdef EXTMARK
		    if (names2[c]) {
			for (a1++; a1 <= dol; a1++)
			    if (names2[c] == (*a1 | 01)) {
				printf(",%u", a1 - zero);
				break;
			    }
		    }
#endif
		    putchar(' ');
		    break;
		}
	    if (n == 0)
		names[c] = 0;	/* clear unknown marks */
	}
	putchar('\n');
}

putchar(ac)
charac ac;
{
	register char *lp;
	register charac c;

	lp = linp;
	c = ac;
#ifdef APLMAP
	if (aplmap && fout == 1 && c > ' ')
		c = map_apl[c - (' ' + 1)];
#endif
	if (listf) {
		if (++col >= ccount - 1) {
			col = 1;
			if (c != '\n') {
				*lp++ = '\\';
				*lp++ = '\n';
			}
		}
		if (c == '\t') {
			c = '>';
			goto esc;
		}
		if (c == '\b') {
			c = '<';
		esc:
			*lp++ = '-';
			*lp++ = '\b';
			*lp++ = c;
			goto out;
		}
		if (c < ' ' && c != '\n' || c == 0177) {
			*lp++ = '^';
			*lp++ = c ^ 0100;
			col++;
			goto out;
		}
	}
	*lp++ = c;
out:
	if (lp >= &line[TTSIZE]) {
		linp = line;
		write(fout, line, lp - line);
		return;
	}
	linp = lp;
}

putfile() {
	register char *fp, *lp;
	register nib;
	register linep *a1;

	nib = BLKSIZE;
	fp = genbuf;
	a1 = addr1;
	do {
		lp = getline(*a1++);
		for ever {
			if (--nib < 0) {
				wte(io, genbuf, fp - genbuf);
				nib = BLKSIZE - 1;
				fp = genbuf;
			}
			if ((*fp++ = *lp++) == 0) {
				fp[-1] = '\n';
				break;
			}
		}
	} while (a1 <= addr2);
	wte(io, genbuf, fp - genbuf);
}

putline() {
	register char *bp, *lp;
	register nl, tl;

	lp = linebuf;
	tl = tline;
#ifdef DEBUG
	if (tflg)
		printf("putline:\t%o\n", tl);
#endif
	bp = getblock(tl, WRITE);
#ifdef HUGE
	if (hugef2) {
		hugef2 = 0;
		tl = tline = (tline >> 1) & _6[hugef];
	}
	tl &= _1[hugef];
#else
	tl &= _1;
#endif
	nl = nleft;
	while (*bp = *lp++) {
		if (*bp++ == '\n') {
			*--bp = 0;
			linebp = lp;
			break;
		}
		if (--nl <= 0) {
#ifdef HUGE
			bp = getblock(tl += _2[hugef], WRITE);
#else
			bp = getblock(tl += _2, WRITE);
#endif
			nl = nleft;
		}
	}
	nl = tline;
#ifdef HUGE
	tline += (((lp - linebuf) + 03) >> (1 + hugef)) & _6[hugef];
#else
	tline += (((lp - linebuf) + 03) >> 1) & _6;
#endif
	return(nl);
}

#ifdef PARENS
putparen(an) {
	register c;

	an %= 10 + 26 + 26;
	if (an < 0)
		an += 10 + 26 + 26;
	c = '0';
	if (an > 9 + 26)
		c = 'a' - (10 + 26);
	else if (an > 9)
		c = 'A' - 10;
	putchar(c + an);
}
#endif

puts(as) {
	register n;
	register flag ovp;
	register char *sp, *s;

	sp = as;
	ovp = 0;
	while (*sp) {
		if (aflg &&
#ifdef PARENS
			    !parenf &&
#endif
#ifdef STRLEN
				       !quotef &&
#endif
						  !listf && *sp == '\b') {
			ovp = 1;
			sp += 2;
			continue;
		}
#ifdef PARENS
		if (!ovp && parenf && !listf && paren(*sp))
			ovp = 1;
#endif
#ifdef STRLEN
	/* if (!ovp && quotef && !listf && (*sp == '"' || *sp == '\'')) */
		if (!ovp && quotef && !listf && (
		    quotec? *sp == quotec : *sp == '"' || *sp == '\''))
			ovp = 1;
#endif
		putchar(*sp++);
	}
	sp = as;
	if (aflg && ovp &&
#ifdef PARENS
			   !parenf &&
#endif
#ifdef STRLEN
				      !quotef &&
#endif
						 !listf) {
		putchar('\n');
		if (prompt1)
			putchar('\t');
		for (; *sp; sp++)
			if (sp[1] == '\b' && sp[2]) {
				putchar(sp[2]);
				sp += 2;
			} else if (*sp == '\t')
				putchar('\t');
			else if (*sp >= ' ')
				putchar(' ');
		ovp = 0;
	}
#ifdef PARENS
	if (parenf && ovp && !listf) {
		putchar('\n');
		if (prompt1)
			putchar('\t');
		for (; *sp; sp++)
			if (n = paren(*sp)) {
				if (n < 0) {
					n = -n - 1;
					putparen(parenc[n]--);
				} else
					putparen(++parenc[--n]);
			} else if (*sp == '\t' || *sp >= ' ')
				putchar(*sp == '\t'? '\t' : ' ');
		ovp = 0;
	}
#endif
#ifdef STRLEN
	if (quotef && ovp && !listf) {
		s = null;
		n = 0;
		ovp = 0;
		putchar('\n');
		if (prompt1)
			putchar('\t');
		for (; *sp; sp++) {
		/* if (n == 0 && (*sp == '"' || *sp == '\'') || */
			if (n == 0 && (quotec? *sp == (ovp?
			    quotec : quotec2) : *sp == '"' || *sp == '\'') ||
			    *sp == n) {
				if (ovp) {
					ovp = 0;
					goto not;
				}
				if (n)
					n = 0;
				else {
					s = sp++;
					n = 0;
					while (*sp && *sp != *s) {
						if (*sp == '\\') {
						  if ('0' <= *++sp && *sp <= '7') {
						    if ('0' <= *++sp && *sp <= '7') 
						      if ('0' <= *++sp && *sp <= '7') 
							sp++;
						  } else
							sp++;
						} else
							sp++;
						n++;
					}
					sp = s;
					s = &"00000"[5];
					do {
						*--s = n % 10 + '0';
					} while (n /= 10);
					n = *sp;
				}
			}
    not:		if (*s)
				putchar(*s++);
			else
				putchar(*sp == '\t'? '\t' : ' ');
			if (*sp == '\\')
				ovp ^= 01;
		}
		ovp = 0;
	}
#endif
	putchar('\n');
}

puts2(as) {
	register char *sp;

	sp = as;
	while (*sp)
		putchar(*sp++);
}

#ifdef CKPT
recover() {
	extern etext;
	register filedes f;
	register char *p;
	register func savint, savqit;
	int n;

	savint = signal(SIGINT, 1);
	savqit = signal(SIGQIT, 1);
	if ((f = open(cfname, 0)) < 0 ||
	    read(f, &n, sizeof n) != sizeof n) {
		n = 67;
		goto cerror;
	}
	p = &etext;
	if (brk(p + n) == -1) {
		n = 67;
		goto cerror;
	}
#ifdef pdp11	/* 16 bit byte count only */
	if (n < 0) {
		if (read(f, p, 32256) != 32256) {
			n = 67;
			goto cerror;
		}
		n -= 32256;	/* 63 blocks, since 64 is < 0 */
		p += 32256;
	}
#endif
	if (read(f, p, n) != n ||
	    (tfile = open(tfname, 2)) < 0)
		n = 67;
	else
		n = 0;
	recovry = 1;	/* overwritten by reading data space */
    cerror:
	if (n) {
		puts(errtext[n]);
		exit(1);
	}
	if (f > 0)
		close(f);

	/*
	 * "initialize" special stuff to restore order
	 */

	globp = 0;
	io = 0;
	io_w = 0;
#ifdef HELP
	doc = 0;
#endif
#ifdef USE
	alt = 0;
#endif
#ifdef AGAIN
	agf = 0;
	agp = 0;
#endif
#ifdef EOL
	prompt3 = 1;
#endif
#ifdef PIPE
	if (pfile) {
		unlink(pfname);
		pfile = 0;
	}
#endif

	signal(SIGINT, savint);
	signal(SIGQIT, savqit);
}
#endif

reverse(aa1, aa2) {
	register linep *a1, *a2, t;

	a1 = aa1;
	a2 = aa2;
	for ever {
		t = *--a2;
		if (a2 <= a1)
			return;
		*a2 = *a1;
		*a1++ = t;
	}
}

#ifdef XDEL
saveline() {
	register linep *a1, *a2, *bp;
	register nl, tl;

	a1 = addr1;
	a2 = addr2;
	ndeleted = a2 - a1 + 1;
	tl = tline;
#ifdef DEBUG
	if (tflg)
		printf("saveline:\t%o\n", tl);
#endif
	bp = getblock(tl, WRITE);
#ifdef HUGE
	if (hugef2) {
		hugef2 = 0;
		tl = (tl >> 1) & _6[hugef];
	}
	tl &= _1[hugef];
#else
	tl &= _1;
#endif
	nl = nleft / sizeof(linep);
	while (a1 <= a2) {
		*bp++ = *a1++;
		if (--nl <= 0) {
#ifdef HUGE
			bp = getblock(tl += _2[hugef], WRITE);
			if (hugef2) {
				hugef2 = 0;
				tl = (tl >> 1) & _6[hugef];
			}
#else
			bp = getblock(tl += _2, WRITE);
#endif
			nl = nleft / sizeof(linep);
		}
	}
	deleted = tline;
#ifdef HUGE
	tline += ((a2 - addr1 + 1) << (1 - hugef)) & _6[hugef];
#else
	tline += ((a2 - addr1 + 1) << 1) & _6;
#endif
}
#endif

savemark(p1, p2)
linep p1, p2;
{
	register n;
#ifdef HUGE
	register i;

	i = hugef ^ 01;
	p1 |= i;
	p2 |= i;
#else
	p1 |= 01;
	p2 |= 01;
#endif
	/* save "marks" on lines so marked */
	for (n = 0; n <= 'z' - 'a'; n++) {
		if (names[n] == 0)	/* zap marks to deleted lines */
			continue;
		if (names[n] == p1)
			names[n] = p2;
#ifdef EXTMARK
		if (names2[n] && names2[n] == p1)
			names2[n] = p2;
#endif
	}
#ifdef YINT
	/* don't lose "y+" line either */
	if (yplus == p1)
		yplus = p2;
#endif
#ifdef UNDO
	/* and remember the line for "undo" */
	undo_oldp = p1;
	undo_newp = p2;
#endif
}

scopy(ass, ads)
char *ass, *ads;
{
	register char *p, *q;

	p = ass;
	q = ads;
	while (*q++ = *p++)
		;
	return(ads);
}

setall() {
	if (addr2 == 0) {
		addr1 = zero + (zero != dol);
		addr2 = dol;
	}
	setdot();
}

setchar(adflt) {
	register charac c;

	if ((c = getchar()) == '\\')
		c = getchar();
	if (c < 0)
		errmsg(59);
	if (c == '\n')
		return(adflt);
	newline();
	return(c);
}

setdot() {
	if (addr2 == 0)
		addr1 = addr2 = dot;
	if (addr1 > addr2)
		errmsg(21);
}

setnoaddr() {
	if (addr2)
		errmsg(22);
}

setnum(adflt) {
	register n;

	white_space();
	n = adflt;
	if ('0' <= (peekc = getchar()) && peekc <= '9')
		n = getnum();
	newline();
	return(n);
}

#ifdef TABS
settab(acs) {
	register *p, i, n;

	if (acs == 0) {
		maxtab = -1;
		n = TABS;
		p = tabs;
		while (--n >= 0)
			*p++ = 0;
		return;
	}
	i = 0;
	if (acs < 0) {
		acs = -acs;
		i++;
	}
	if (--acs < TABS * BPW) {
		p = &tabs[acs >> BPWC];
		*p |= 01 << (n = acs & BPWM);
		if (i) {
			*p ^= 01 << n;
			if (acs == maxtab) {	/* find new maxtab */
				for (n = TABS - 1; n >= 0; --n)
					if (tabs[n >> BPWC] >> (n & BPWM) & 01)
						break;
				maxtab = n;
			}
		} else if (acs > maxtab)
			maxtab = acs;
	}
}
#endif

shell() {
#ifdef PIPE
	extern oil_spilled();
#endif
	register pid, wpid;
	register linep *a;
	register dp, df, nopipe, c, e, savint, savqit;
	register linep *dest;
	int retcode, p[2];

#ifdef PIPE
	dp = 0;
	df = 0;
	if (addr2) {
#ifdef LOG
		++logstats.lc_pipe;
#endif
		dest = 0;
		nopipe = 0;
		if ((peekc = getchar()) == '|') {
			nonzero();
			peekc = 0;
			dp++;
			df++;
#ifdef LOG
			--logstats.lc_pipe;
			++logstats.lc_dpipe;
#endif
		}
		if ((peekc = getchar()) == '>') {
			nonzero();
#ifdef LOG
			--logstats.lc_pipe;
			++logstats.lc_pto;
#endif
			goto pi;
		} else if ((peekc = getchar()) == '<') {
			nopipe++;
#ifdef LOG
			--logstats.lc_pipe;
			++logstats.lc_pfrom;
#endif
    pi:			peekc = 0;
			dp++;
			dest = addr2;
		}
		if (dp && '0' <= (peekc = getchar()) && peekc <= '9') {
			c = getnum();
			dest = zero + c;
			if (dest < zero)
				dest = zero + 1;
			if (dest > dol)
				dest = dol;
		}
	}
#ifdef LOG
	  else
		++logstats.lc_shell;
#endif
	piperr = 0;
	if (!iflg)
		if (addr2) {
			savint = signal(SIGINT, oil_spilled);
			savqit = signal(SIGQIT, oil_spilled);
		} else {
#endif
			savint = signal(SIGINT, 1);
			savqit = signal(SIGQIT, 1);
#ifdef PIPE
		}
	if (addr2 && pipe(p) < 0) {
		c = 15;
    errm:	if (!iflg) {
			signal(SIGINT, savint);
			signal(SIGQIT, savqit);
		}
		if (c)
			errmsg(c);
		error();
	}
	if (dp) {
		if ((pfile = create(pfname, LMODE)) < 0) {
			pfile = 0;
			c = 64;
			goto errm;
		}
		if ((io = open(pfname, 0)) < 0) {
			io = 0;
			c = 65;
			goto errm;
		}
	}
#endif
	if ((pid = fork()) < 0) {
#ifdef PIPE
		if (addr2) {
			close(p[0]);
			close(p[1]);
		}
#endif
		c = 14;
		goto errm;
	}
	if (pid == 0) {
/* kid */	if (!iflg) {
/* kid */		signal(SIGHUP, 0);
/* kid */		signal(SIGINT, 0);
/* kid */		signal(SIGQIT, 0);
/* kid */	}
#ifdef PIPE
/* kid */	if (addr2) {
/* kid */		close(0);
/* kid */		dup(p[0]);
/* kid */		close(p[0]);
/* kid */		close(p[1]);
/* kid */		if (dp) {
/* kid */			close(1);
/* kid */			dup(pfile);
/* kid */			close(pfile);
/* kid */		}
/* kid */	}
#endif
/* kid */	execl("/bin/sh", "!sh", "-t", 0);
/* kid */	write(2, "No shell!\n", 10);
/* kid */	exit(0177);
	}
#ifdef CMDS
	if (cmd && !addr2)
		write(cmd, "<UNIX>\n", 7);
#endif
#ifdef PIPE
	if (addr2) {
		signal(SIGPIP, oil_spilled);
		close(p[0]);
		e = eflg2;
		eflg2++;
		fout = p[1];
		while ((c = getchar()) >= 0 && c != '\n')
			putchar(c);
		putchar('\n');
		if (!nopipe) {
			line_num = (a = addr1) - zero - aflg;
			do {
				if (pno >= 0)
					printlno(line_num++);
				puts(getline(*a++));
			} while (a <= addr2 && !piperr);
		}
		flush_buf();
		fout = 1;
		eflg2 = e;
		close(p[1]);
		if (!iflg) {
			signal(SIGINT, 1);
			signal(SIGQIT, 1);
		}
	}
#endif
	while ((wpid = wait(&retcode)) != pid && wpid >= 0);
#ifdef CMDS
	if (cmd)
		lseek(cmd, 0L, 2);
#endif
#ifdef PIPE
	if (addr2)
		signal(SIGPIP, 0);
#endif
	if (c = retcode & 0177) {
		if (0 < c && c < NSTR && status[c])
			puts2(status[c]);
		else
			printf("signal %d", c);
		if (retcode & 0200)
			puts2(" -- core dumped");
		putchar(lastc = '\n');
		c = 0;
		goto errm;
	} else if (df && strict && retcode >> 8) {
		printf("exit status %d\n", retcode >> 8);
		c = 0;
		goto errm;
	}
#ifdef PIPE
	  else if (addr2) {
		if (piperr) {
			putsn(status[SIGPIP]);
			c = 0;
			goto errm;
		}
		if (dp) {
			if (df) {
				c = addr2 != dol;
				delete();
				if (dest == 0)
					dest = dot - c;
			} else if (dest == 0)
				dest = addr2;
			if (dest < zero)
				dest = zero;
			if (c = append(getfile, dest, 0, 0))
				text_modified++;
			if (prompt2)
				printf((curt? prcntu : errtext[55]),
				    c, (c == 1? null : quote_s));
			close(io);
			io = 0;
			unlink(pfname);
			close(pfile);
			pfile = 0;
		}
	}
#endif
	if (!iflg) {
		signal(SIGINT, savint);
		signal(SIGQIT, savqit);
	}
	if (!addr2)
		putsn("!");
#ifdef CLEAR
	istty(1);	/* in case bs1 changed */
#endif
}

signals(ast)
struct sigtab *ast;
{
	register n;
	register struct sigtab *s;

	s = ast - 1;
	while (n = (++s)->s_sig)
		signal(n, s->s_func);
}

skip_rest() {
	register charac c;

	while ((c = getchar()) >= 0 && c != '\n');
}

substitute(inglob) {
	extern getsub();
	register linep *a1, p;
	register nl;
	register flag gsubf;

	gsubf = compsub();		/* 0 or 1 depending on 'g' */
	for (a1 = addr1; a1 <= addr2; a1++) {
		s_tmp = s_cnt;
		if (execute(0, a1) == 0)
			continue;
		inglob |= 01;
		dosub();
		if (gsubf)
			while (*loc2) {
				if (execute(1) == 0)
					break;
				dosub();
			}
		p = *a1;
		*a1 = putline();
		savemark(p, *a1);
		nl = append(getsub, a1, 0, 0);
		a1 += nl;
		addr2 += nl;
	}
	if (inglob == 0)
		errmsg(39);
}

#ifdef XED
tack(aeof, aflag)
char aeof;
{
	register n;
	register char *p1, *p2;
	register linep *a;
	register charac c;

	nonzero();
	setdot();
	p1 = rhsbuf;	/* i/TEXT screwed for long lines */
	while ((c = getchar()) != aeof && c != '\n') {
		if (c == '\\')
			if ((c = getchar()) == '\n')
				break;
		if (c < 0)
			errmsg(59);
		*p1++ = c;
		if (p1 >= &rhsbuf[LBSIZE / 2 - 2])
			errmsg(42);
	}
	if (*rhsbuf == 0)
		errmsg(27);
	if (c == '\n') {
		if (prompt2)
			pflag++;
	} else
		newline();
	if (p1 != rhsbuf)
		*p1 = 0;
	else
		while (*p1)
			p1++;
	n = p1 - rhsbuf;
	for (a = addr1; a <= addr2; a++) {
		getline(*a);
		for (p2 = linebuf; *p2; p2++);
#ifndef ALLOC
		if (p2 + n >= &linebuf[LBSIZE / 2 - 2])
#else
		if (p2 + n >= &linebuf[lbsize / 2 - 2])
#endif
			errmsg(30);
		if (aflag) {	/* $ */
			p1 = rhsbuf;
			while (*p2++ = *p1++);
		} else {	/* ^ */
			p1 = p2 + n;
			*p1 = *p2;
			while (p2 > linebuf)
				*--p1 = *--p2;
			p1 = linebuf;
			p2 = rhsbuf;
			while (*p2)
				*p1++ = *p2++;
		}
		p2 = *a;
		*a = putline();
		savemark(p2, *a);
	}
	dot = addr2;
	return(addr2 - addr1 + 1);
}
#endif

term() {
	signal(SIGTRM, 1);
	if (reading) {
		backup(TRM);
		if (fout != 1) {
			flush_buf();
			fout = 1;
		}
		puts2(errtext[58]);
		delexit(1);
	}
	termflg++;
}

tmpname(as, an)
char *as;
{
	register unsigned n;
	register char *p, c;

	p = as;
	n = an;
	while (*p)
		p++;
	while (--p >= as)
		if ((c = *p) == '0' || (c | 040) == 'x') {
			*p = n % 10 + '0';
			n /= 10;
#ifdef CKPT
			tfnum = p - as;
#endif
		}
	return(as);
}

#ifdef XDEL
undelete() {
	register linep *a1, *a2, *bp;
	register tl, nl, num;

	if ((tl = deleted) == 0)
		errmsg(16);
#ifdef DEBUG
	if (tflg)
		printf("undelete:\t%o\n", tl);
#endif
	setdot();
	a1 = dol + 1;
	a2 = a1 + ndeleted;
	bp = addr2 + 1;
	if (dol + ndeleted > endcore) {
		num = ((ndeleted * 2) + 1023) & ~01777;
		if (sbrk(num) == -1)
			errmsg(33);
		endcore = (int)endcore + num;
	}
	while (a1 > bp)
		*--a2 = *--a1;
	dol += ndeleted;
	a1 = addr2 + 1;
	a2 = a1 + ndeleted;
	bp = getblock(tl, READ);
#ifdef HUGE
	tl &= _1[hugef];
#else
	tl &= _1;
#endif
	nl = nleft / sizeof(linep);
	while (a1 < a2) {
		*a1++ = *bp++;
		if (--nl <= 0) {
#ifdef HUGE
			bp = getblock(tl += _2[hugef], READ);
#else
			bp = getblock(tl += _2, READ);
#endif
			nl = nleft / sizeof(linep);
		}
	}
	dot = a2 - 1;
	return(ndeleted);
}
#endif

#ifdef UNDO
undo() {
	register linep *a, t, o;

	if (undo_newp == 0 || (a = findmark(undo_newp, 0)) == 0)
		errmsg(13);
	t = *a |
#ifdef HUGE
		 hugef ^
#endif
			 01;
	o = undo_oldp;
	savemark(*a, o);
	*a = o;
	undo_newp = o;
	undo_oldp = t;
	dot = a;
	if (text_modified == 0)
		text_modified++;
}
#endif

white_space() {
	register charac c;

	while ((c = getchar()) == ' ' || c == '\t');
	peekc = c;
}

wte(fd, buf, len)
char buf[];
{
	if (write(fd, buf, len) != len)
		errmsg(-32);
}

/*
 * as	a prompt string
 * ay	yes response, !no response
 * ad	default (\n) response, <0 means no default allowed
 * aef	end-of-file response
 */
yes_no(as, ay, ad, aef)
char *as;
{
	register charac c, p;
	register n;
	register flag l;

	l = listf;
	listf = 0;
	p = peekc;
	peekc = 0;
	for ever {
		putchar('\n');
		puts2(as);
		puts2("? ");
		for (n = 0; n < 5; n++) {
			flush_buf();
			if ((c = getchar()) < 0) {
				putchar('\n');
				n = aef;
				goto ret;
			}
			if (c != '\n')
				skip_rest();
			else if (ad >= 0) {
				n = ad;
				goto ret;
			}
			if (c == 'y' || c == 'n') {
				n = ay;
				if (c != 'y')
					n ^= 01;
				goto ret;
			}
			puts2("yes or no? ");
		}
	}
    ret:
	listf = l;
	peekc = p;
	return(n);
}
