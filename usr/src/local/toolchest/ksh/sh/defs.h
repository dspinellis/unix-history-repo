/*

 *      Copyright (c) 1984, 1985, 1986 AT&T
 *      All Rights Reserved

 *      THIS IS UNPUBLISHED PROPRIETARY SOURCE 
 *      CODE OF AT&T.
 *      The copyright notice above does not 
 *      evidence any actual or intended
 *      publication of such source code.

 */
/* @(#)defs.h	1.1 */

/*
 *	UNIX shell
 *	S. R. Bourne
 *	Rewritten by David Korn
 *
 */

#include	<stdio.h>
#include	<setjmp.h>
#include	<signal.h>

#define const 

/* error exits from various parts of shell */
#define ERROR	1
#define SYNBAD	2

#define BYTESPERWORD	(sizeof(char *))
#define ENDARGS	0	/* arg list terminator */
#define	NIL	((char*)0)
#ifndef NULL
#define NULL 0
#endif


/* typedefs used in the shell */
typedef char	BOOL;
typedef char	*ADDRESS;
typedef char	MSG[];
typedef char	*STKPTR;

typedef struct stat	STATBUF;	/* defined in /usr/include/sys/stat.h */
typedef struct blk	*BLKPTR;
typedef struct fileblk	FILEBLK;
typedef struct fileblk	*SHFILE;
typedef struct trenod	*TREPTR;
typedef struct forknod	*FORKPTR;
typedef struct comnod	*COMPTR;
typedef struct swnod	*SWPTR;
typedef struct regnod	*REGPTR;
typedef struct parnod	*PARPTR;
typedef struct ifnod	*IFPTR;
typedef struct whnod	*WHPTR;
typedef struct fornod	*FORPTR;
typedef struct lstnod	*LSTPTR;
typedef struct chnnod	*CHNPTR;
typedef struct dolnod	*DOLPTR;
typedef struct ionod	*IOPTR;
typedef struct Namnod	NAMNOD;
typedef struct Namnod	*NAMPTR;
typedef struct sysnod	*SYSPTR;
typedef struct procnod	*PROCPTR;

#define Rcheat(a)	((unsigned)(a))

#define blank()		putc(SP,output)
#define newline()	putc(NL,output)
#define round(a,b)	(((int)((ADR(a)+b)-1))&~((b)-1))
#define eq(a,b)		(strcmp(a,b)==0)
#define max(a,b)	((a)>(b)?(a):(b))
#define assert(x)	;
#define exitset()	(savexit=exitval)
#define error(s)	failed(s,NIL)
#define		BLK(x)	((BLKPTR)(x))
#define		BYT(x)	((BYTPTR)(x))
#define		STK(x)	((STKPTR)(x))
#define		ADR(x)	((char*)(x))
#ifdef BSD
#define	strchr		index
#define	strrchr		rindex
#endif	/* BSD */
#ifdef  VENIX
#define	strchr		index
#define	strrchr		rindex
#endif	/* VENIX */

/* flags */

typedef long optflag;
#ifdef INT16
# ifndef pdp11
# define _OPTIM_	1
# endif /* pdp11 */
#endif /* INT16 */

#ifdef _OPTIM_
#define _HIGH_	1
#define _LOW_	_HIGH_-1
#define is_option(flag)		((flag)&0xffffL?\
					st.Flags.i[_LOW_]&(unsigned int)(flag):\
					st.Flags.i[_HIGH_]&(unsigned int)((flag)>>16))
#define on_option(flag)		((flag)&0xffffL?\
					(st.Flags.i[_LOW_] |= (unsigned int)(flag)):\
					(st.Flags.i[_HIGH_] |= (unsigned int)((flag)>>16)))
#define off_option(flag)	((flag)&0xffffL?\
					(st.Flags.i[_LOW_] &= ~(unsigned int)(flag)):\
					(st.Flags.i[_HIGH_] &= ~(unsigned int)((flag)>>16)))
#else
#define is_option(flag)		(st.Flags.l & (flag))
#define on_option(flag)		(st.Flags.l |= (flag))
#define off_option(flag)	(st.Flags.l &= ~(flag))
#endif /* _OPTIM_ */

#define Fixflg	1
#define Errflg	2
#define Readpr	3
#define Monitor	4
#define	Intflg	5
#define Rshflg	6
#define Execpr	7
#define Keyflg	8
#define Noset	9
#define Noglob	10
#define Allexp	11
#define Noeof	13
#define Nomatch	14
#define Markdir	15
#define Bgnice	16
#define Editvi	17
#define Viraw	18
#define Oneflg	19
#define Hashall	20
#define Stdflg	21
#define Noexec	22
#define Notify	23
#define Gmacs	24
#define Emacs	25
#define	Privmod 26
#define	Inproc	27

#define FIXFLG	(1<<Fixflg) /* used also as a state */
#define	ERRFLG	(1<<Errflg) /* used also as a state */
#define	READPR	(1<<Readpr) /* used also as a state */
#define MONITOR	(1<<Monitor)/* used also as a state */
#define	INTFLG	(1<<Intflg) /* used also as a state */
#define	RSHFLG	(1L<<Rshflg)
#define	EXECPR	(1L<<Execpr)
#define	KEYFLG	(1L<<Keyflg)
#define NOSET	(1L<<Noset)
#define NOGLOB	(1L<<Noglob)
#define ALLEXP	(1L<<Allexp)
#define NOEOF	(1L<<Noeof)
#define NOMATCH	(1L<<Nomatch)
#define EMACS	(1L<<Emacs)
#define BGNICE	(1L<<Bgnice)
#define EDITVI	(1L<<Editvi)
#define VIRAW	(1L<<Viraw)
#define	ONEFLG	(1L<<Oneflg)
#define HASHALL	(1L<<Hashall)
#define	STDFLG	(1L<<Stdflg)
#define	NOEXEC	(1L<<Noexec)
#define	NOTIFY	(1L<<Notify)
#define GMACS	(1L<<Gmacs)
#define MARKDIR	(1L<<Markdir)
#define PRIVM	(1L<<Privmod)
#define INPROC	(1L<<Inproc)
#define CFLAG	(1L<<30)


/* states */
/* low numbered states are same as flags */
#define	PROMPT		INTFLG
#define	WAITING		0x40
#define	FORKED		0x80
#define	TTYFLG		0x100
#define IS_TMP		0x200	/* set when TMPFD is available */
#define	NO_TMP		0x400	/* set when invalid /tmp/filename */
#define RM_TMP		0x800	/* temp files to remove on exit */
#define FUNCTION 	0x1000	/* set when entering a function */
#define RWAIT		0x2000	/* set when waiting for a read */
#define BUILTIN		0x4000	/* set when processing built-in command */
#define NONSTOP		0x8000	/* set when job cannot be stopped */
#define READC		0x10000	/* only for BSD */
#define VFORKED		0x20000	/* only used with VFORK mode */



/*	fork constant	*/
#define FORKLIM 32
/*	comment delimiter 	*/

#define	COMCHAR	'#'


#define NL	'\n'
#define LITERAL	'\''
#define ENDOF	0
#define STRIP	0377
#define SP	' '
#define NOEXP	1
#define HAT	'^'
#define ESCAPE	'\\'
#define HIGHBIT	0200
#define TO_PRINT 0100	/* bit to set for printing control char */

extern MSG	argcount;
extern MSG	argexp;
extern MSG	arglist;
extern MSG	atline;
extern MSG	baddir;
extern MSG	badexec;
extern MSG	badnum;
extern MSG	badop;
extern MSG	badopt;
extern MSG	badparam;
extern MSG	badsub;
extern MSG	badtrap;
extern MSG	blet;
extern MSG	bltfn;
extern MSG	bset;
extern MSG	bread;
extern MSG	colon;
extern MSG	coredump;
extern MSG	defedit;
extern MSG	defpath;
extern MSG	dot;
extern MSG	endmatch;
extern MSG	fn_hdr;
extern MSG	execpmsg;
extern MSG	intbase;
extern MSG	is_;
extern MSG	is_alias;
extern MSG	is_builtin;
extern MSG	is_function;
extern MSG	is_reserved;
extern MSG	is_talias;
extern MSG	is_xalias;
extern MSG	is_xfunction;
extern MSG	logout;
extern MSG	mailmsg;
extern MSG	minus;
extern MSG	noalias;
extern MSG	nofork;
extern MSG	noquery;
extern MSG	nosign;
extern MSG	nospace;
extern MSG	noswap;
extern MSG	notfound;
extern MSG	nullstr;
extern MSG	off_;
extern MSG	on_;
extern MSG	opt_heading;
extern MSG	parexp;
extern MSG	pexists;
extern MSG	ptrace;
extern MSG	recursive;
extern MSG	restricted;
extern MSG	setpwd;
extern MSG	sptbnl;
extern MSG	stdprompt;
extern MSG	suid_profile;
extern MSG	supprompt;
extern MSG	t_real;
extern MSG	t_sys;
extern MSG	t_user;
extern MSG	toobig;
extern MSG	txtbsy;
extern MSG	unlimited;
extern MSG	unset;
extern MSG	version;
extern MSG	pcsadr;
extern MSG	pidadr;
extern MSG	pwderr;


extern struct Amemory	*alias;	/* for alias names */
extern struct Namnod	*bltin_nodes;

/*
 * Saves the state of the shell
 */

struct State
{
	jmp_buf		jmpbuf;
	union
	{
		long	l;
#ifdef _OPTIM_
		int	i[2];
#endif /* _OPTIM_ */
	}		Flags;
	unsigned	States;
	int		Breakcnt;
	int		Execbrk;
	int		Loopcnt;
	int		Fn_depth;
	int		Peekc;
	int		Peekn;
	int		Aliflg;
	int		Reserv;
	char		*Cmdadr;
	int		Cmdline;
	int		Firstline;
	int		Exec_flag;
	int		Dolc;
	char		**Dolv;
	IOPTR		Iopend;
	int		Ioset;
	IOPTR		Iotemp;
	int		Linked;
	SHFILE		Standin;
	FILE		*Standout;
	FILE		*Cpipe[2];
	int		Cpid;
	int		Wdset;
};

extern struct State st;

#define flags		st.Flags.l

#define states		st.States
#define breakcnt	st.Breakcnt
#define loopcnt		st.Loopcnt
#define execbrk		st.Execbrk
#define fn_depth	st.Fn_depth
#define peekc		st.Peekc
#define peekn		st.Peekn
#define aliflg		st.Aliflg
#define reserv		st.Reserv
#define cmdadr		st.Cmdadr
#define cmdline		st.Cmdline
#define firstline	st.Firstline
#define exec_flag	st.Exec_flag
#define dolc		st.Dolc
#define dolv		st.Dolv
#define linked		st.Linked
#define iotemp		st.Iotemp
#define ioset		st.Ioset
#define iopend		st.Iopend
#define	standout	st.Standout
#define	standin		st.Standin
#define	cpipe		st.Cpipe
#define	cpid		st.Cpid
#define wdset		st.Wdset

extern char	*comdiv;
extern int	errno;
extern int	exitval;
extern char	*lastarg;
extern long	mailchk;
extern int	oldexit;
extern FILE	*output;
extern long	ppid;
extern struct Amemory *prnames;	/* for function names */
extern int	savexit;
extern int	topfd;
extern char	*trapcom[];
extern BOOL	trapnote;
extern BOOL	login_sh;
extern int 	userid;
#ifdef pdp11
# ifndef INT16
# define INT16
# endif /* INT16 */
#endif	/* pdp11 */

#ifdef INT16
/* save space */
#undef putc
#define putc fputc
#endif	/* INT16 */
