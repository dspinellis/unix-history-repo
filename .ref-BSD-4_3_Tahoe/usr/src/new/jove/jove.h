/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/

/* jove.h header file to be included by EVERYONE */ 


#include <setjmp.h>

#ifndef TUNED
#   include "tune.h"
#endif

#ifndef MAC
#	include <sys/types.h>
#else
#	include <types.h>
#endif


#ifdef MSDOS
#include <string.h>
#endif


#if !(defined(MSDOS) || defined(MAC))
#define void int
#endif

#if !(defined(IBMPC) || defined(MAC))
#	define TERMCAP
#	define ASCII
#endif

#ifdef ASCII	/* seven bit characters */
#	define NCHARS 0200
#else
#	define NCHARS 0400
#endif
#define CHARMASK (NCHARS -1)

#define private	static

#ifndef BSD4_2
#   ifdef MENLO_JCL
#	ifndef EUNICE
#		define signal	sigset
#	endif
#   endif /* MENLO_JCL */
#endif

#define EOF	-1

#ifdef MSDOS
#	define NULL	((char *)0)
#	define NIL	((char *)0)
#else
#	ifdef MAC
#		define NULL 0L
#		define NIL 0L
#	else
#		define NULL 0
#		define NIL 0
#	endif /* MAC */
#endif /* MSDOS */
/* kinds of regular expression compiles */
#define NORM	0	/* nothing special */
#define OKAY_RE	1	/* allow regular expressions */
#define IN_CB	2	/* in curly brace; implies OKAY_RE */

/* return codes for command completion (all < 0 because >= 0 are
   legitimate offsets into array of strings */

#define AMBIGUOUS	-2	/* matches more than one at this point */
#define UNIQUE		-3	/* matches only one string */
#define ORIGINAL	-4	/* matches no strings at all! */
#define NULLSTRING	-5	/* just hit return without typing anything */

/* values for the `flags' argument to complete */
#define NOTHING		0	/* opposite of RET_STATE */
#define RET_STATE	1	/* return state when we hit return */
#define RCOMMAND	2	/* we are reading a joverc file */
#define CASEIND		4	/* map all to lower case */

#define SAVE		01	/* this macro needs saving to a file */

#define	LBSIZE		BUFSIZ	/* same as a logical disk block */
#ifndef MSDOS
#define FILESIZE	256
#else /* MSDOS */
#define FILESIZE	64
#endif /* MSDOS */

#define FORWARD		1
#define BACKWARD	-1

#define CTL(c)		(c & 037)
#define META(c)		(c | 0200)
#define RUBOUT		'\177'
#define LF		CTL('J')
#define CR		CTL('M')
#define BS		CTL('H')
#define ESC		'\033'

#define HALF(wp)	((wp->w_height - 1) / 2)
#define IsModified(b)	(b->b_modified)
#define SIZE(wp)	(wp->w_height - 1)
#define SavLine(a, b)	(a->l_dline = putline(b))
#define SetLine(line)	DotTo(line, 0)
#define bobp()		(firstp(curline) && bolp())
#define bolp()		(curchar == 0)
#define eobp()		(lastp(curline) && eolp())
#define eolp()		(linebuf[curchar] == '\0')
#define firstp(line)	(line == curbuf->b_first)
#define getDOT()	getline(curline->l_dline, linebuf)
#define isdirty(line)	(line->l_dline & DIRTY)
#define lastp(line)	(line == curbuf->b_last)
#define makedirty(line)	line->l_dline |= DIRTY
#define one_windp()	(fwind->w_next == fwind)

#define CharUpcase(c)	(CaseEquiv[c])

extern int	BufSize;

#define ARG_CMD		1
#define LINECMD		2
#define KILLCMD		3	/* so we can merge kills */
#define YANKCMD		4	/* so we can do ESC Y (yank-pop) */

/* Buffer type */

#define B_SCRATCH	1	/* for internal things, e.g. minibuffer ... */
#define B_FILE		2	/* normal file (We Auto-save these.) */
#define B_PROCESS	3	/* process output in this buffer */

/* Major modes */
#define FUNDAMENTAL	0	/* Fundamental mode */
#define TEXT		1	/* Text mode */
#define CMODE		2	/* C mode */
#ifdef LISP
#	define LISPMODE		3	/* Lisp mode */
#	define NMAJORS		4
#else
#	define NMAJORS	3
#endif

/* Minor Modes */
#define Indent		(1 << 0)	/* indent same as previous line after return */
#define ShowMatch	(1 << 1)	/* paren flash mode */
#define Fill		(1 << 2)	/* text fill mode */
#define OverWrite	(1 << 3)	/* over write mode */
#define Abbrev		(1 << 4)	/* abbrev mode */

#define BufMinorMode(b, x)	(b->b_minor & x)

#define MinorMode(x)	BufMinorMode(curbuf, x)
#define MajorMode(x)	(curbuf->b_major == x)
#define SetMajor(x)	((curbuf->b_major = x), UpdModLine = YES)

extern char	CharTable[NMAJORS][NCHARS];
extern char	CaseEquiv[NCHARS];

/* setjmp/longjmp args for DoKeys() mainjmp */
#define FIRSTCALL	0
#define ERROR		1
#define COMPLAIN	2	/* do the error without a getDOT */
#define QUIT		3	/* leave this level of recursion */

#define QUIET		1	/* sure, why not? */

#define YES		1
#define NO		0
#define TRUE		1
#define FALSE		0
#define ON		1
#define OFF		0
#define YES_NODIGIT	2

#define INT_OKAY	0
#define INT_BAD		-1

extern char	*Mainbuf,
		*HomeDir,	/* home directory */
		key_strokes[],	/* strokes that make up current command */
		*Inputp;

extern int	HomeLen;	/* length of home directory */

extern char	NullStr[];

#if defined(VMUNIX)||defined(MSDOS)
extern char	genbuf[LBSIZE],
		linebuf[LBSIZE],
		iobuff[LBSIZE];
#else
extern char	*genbuf,	/* scratch pad points at s_genbuf (see main()) */
		*linebuf,	/* points at s_linebuf */
		*iobuff;	/* for file reading ... points at s_iobuff */
#endif

extern int	InJoverc,
		Interactive;

#define	READ	0
#define	WRITE	1
extern int	errno;

extern jmp_buf	mainjmp;

#ifdef IPROCS
typedef struct process	Process;
#endif
typedef struct window	Window;
typedef struct position	Bufpos;
typedef struct mark	Mark;
typedef struct buffer	Buffer;
typedef struct line	Line;
typedef struct iobuf	IOBUF;
typedef struct data_obj {
	int	Type;
	char	*Name;
} data_obj;	/* points to cmd, macro, or variable */

typedef data_obj	*keymap[NCHARS];

struct line {
	Line	*l_prev,		/* pointer to prev */
		*l_next;		/* pointer to next */
	disk_line	l_dline;	/* pointer to disk location */
};

#ifdef IPROCS
struct process {
	Process	*p_next;
#ifdef PIPEPROCS
	int	p_toproc,	/* read p_fromproc and write p_toproc */
		p_portpid,	/* pid of child (the portsrv) */
		p_pid;		/* pid of real child i.e. not portsrv */
#else
	int	p_fd,		/* file descriptor of pty? opened r/w */
		p_pid;		/* pid of child (the shell) */
#endif
	Buffer	*p_buffer;	/* add output to end of this buffer */
	char	*p_name;	/* ... */
	char	p_state,	/* State */
		p_howdied,	/* Killed? or Exited? */
		p_reason;	/* If signaled, p_reason is the signal; else
				   it is the the exit code */
	Mark	*p_mark;	/* where output left us */
	data_obj
		*p_cmd;		/* command to call when process dies */
};
#endif /* IPROCS */

struct window {
	Window	*w_prev,	/* circular list */
		*w_next;
	Buffer	*w_bufp;	/* buffer associated with this window */
	Line	*w_top,		/* top line */
		*w_line;	/* current line */
	int	w_char,
		w_height,	/* window height */
		w_topnum,	/* line number of the topline */
		w_dotcol,	/* UpdWindow sets this ... */
		w_dotline,	/* ... and this */
		w_flags,
#define	W_TOPGONE	01
#define	W_CURGONE	02	/* topline (curline) of window has been deleted
				   since the last time a redisplay was called */
#define W_VISSPACE	04
#define W_NUMLINES	010
		w_LRscroll;	/* amount of LeftRight scrolling in window */
#ifdef MAC
	int	w_topline;	/* row number of top line in window */
	char **w_control;	/* scroll bar for window */
#endif
};

extern Window	*fwind,		/* first window in list */
		*curwind;	/* current window */

struct position {
	Line	*p_line;
	int	p_char;
};

struct mark {
	Line	*m_line;
	int	m_char;
	Mark	*m_next;	/* list of marks */
#define M_FIXED		00
#define M_FLOATER	01
#define M_BIG_DELETE	02
	char	m_flags;	/* FLOATERing mark? */
};

struct buffer {
#ifdef MAC
	int Type;		/* kludge... to look like a data_obj */
	char *Name;		/* Name will not be used */
#endif	
	Buffer	*b_next;		/* next buffer in chain */
	char	*b_name,		/* buffer name */
		*b_fname;		/* file name associated with buffer */
	dev_t	b_dev;			/* device of file name. */
	ino_t	b_ino;			/* inode of file name */
	time_t	b_mtime;		/* last modify time ...
					   to detect two people writing
					   to the same file */
	Line	*b_first,		/* pointer to first line in list */
		*b_dot,			/* current line */
		*b_last;		/* last line in list */
	int	b_char;			/* current character in line */

#define NMARKS	8			/* number of marks in the ring */

	Mark	*b_markring[NMARKS],	/* new marks are pushed here */
		*b_marks;		/* all the marks for this buffer */
	char	b_themark,		/* current mark (in b_markring) */
		b_type,			/* file, scratch, process, iprocess */
		b_ntbf,			/* needs to be found when we
					   first select? */
		b_modified;		/* is the buffer modified? */
	int	b_major,		/* major mode */
		b_minor;		/* and minor mode */
	keymap	*b_keybinds;		/* local bindings (if any) */
#ifdef IPROCS
	Process	*b_process;		/* process we're attached to */
#endif
};

struct macro {
	int	Type;		/* in this case a macro */
	char	*Name;		/* name is always second ... */
	int	m_len,		/* length of macro so we can use ^@ */
		m_buflen,	/* memory allocated for it */
		m_flags;
	char	*m_body;	/* actual body of the macro */
	struct macro
		*m_nextm;
};

struct variable {
	int	Type;		/* in this case a variable */
	char	*Name;		/* name is always second */
	int	*v_value,
		v_flags;
};

struct cmd {
	int	Type;
	char	*Name;
#ifdef MAC
	void (*c_proc)();
#else
	int (*c_proc)();
#endif
#ifdef MAC
	char c_map;			/* prefix map for About Jove... */
	char c_key;			/* key binding for About Jove... */
#endif
};

extern keymap	mainmap,	/* various key maps */
		pref1map,
		pref2map,
		miscmap;
#ifdef MAC					/* used in About Jove... */		
	#define F_MAINMAP '\001'
	#define F_PREF1MAP '\002'
	#define F_PREF2MAP '\003'
#endif

extern data_obj	*LastCmd;	/* last command invoked */

extern char	*ProcFmt;

extern struct cmd	commands[];
extern struct macro	*macros;
extern struct variable	variables[];

extern struct macro
	*macstack[],
	KeyMacro;

#define FUNCTION	1
#define VARIABLE	2
#define MACRO		3
#ifdef MAC
#	define BUFFER		6	/* menus can point to buffers, too */
#	define STRING		7	/* a menu string or divider */
#endif

#define TYPEMASK	07
#define MAJOR_MODE	010
#define MINOR_MODE	020
#define DefMajor(x)	(FUNCTION|MAJOR_MODE|(x << 8))
#define DefMinor(x)	(FUNCTION|MINOR_MODE|(x << 8))

extern Buffer	*world,		/* first buffer */
		*curbuf;	/* pointer into world for current buffer */

#define curline	curbuf->b_dot
#define curchar curbuf->b_char

#define NUMKILLS	10	/* number of kills saved in the kill ring */

#ifdef MAC		/* when doing ~DIRTY, we need high bits set */
#	define DIRTY	(disk_line) 01	/* just needs updating for some reason */
#else
#	define DIRTY	01	/* just needs updating for some reason */
#endif
#define MODELINE	02	/* this is a modeline */
#define L_MOD		04	/* this line has been modified internally */

struct scrimage {
	int	s_offset,	/* offset to start printing at */
		s_flags,	/* various flags */
		s_id,		/* which buffer line */
		s_vln;		/* Visible Line Number */
	Line	*s_lp;		/* so we can turn off red bit */
	Window	*s_window;	/* window that contains this line */
};

extern struct scrimage
	*DesiredScreen,		/* what we want */
	*PhysScreen;		/* what we got */

/* Variable flags (that can be set). */
#define V_BASE10	01	/* is integer in base 10 */
#define V_BASE8		02	/* is integer in base 8 */
#define V_BOOL		04	/* is a boolean */
#define V_STRING	010	/* is a string */
#define V_CHAR		020	/* is a character */
#define V_FILENAME	040	/* a file name (implies V_STRING) */
#define V_TYPEMASK	077	/* mask off the extra bits */
#define V_MODELINE	0100	/* update modeline */
#define V_CLRSCREEN	0200	/* clear and redraw screen */
#define V_TTY_RESET	0400	/* redo the tty modes */

#ifdef MAC
#ifdef TXT_TO_C
int		/* kludge, so setmaps will compile with variables */
#else
extern int
#endif /* TXT_TO_C */
#else
extern int
#endif	/* MAC */

	OKXonXoff,		/* disable start/stop characters */
	MetaKey,		/* this terminal has a meta key */
	VisBell,		/* use visible bell (if possible) */
	WrapScan,		/* make searches wrap */
#ifndef MAC
	phystab,		/* terminal's tabstop settings */ 
#endif
	tabstop,		/* expand tabs to this number of spaces */
#ifdef BACKUPFILES
	BkupOnWrite,		/* make backup files when writing */
#endif
	RMargin,		/* right margin */
	LMargin,		/* left margin */
	ScrollStep,		/* how should we scroll */
#ifndef MAC
	WtOnMk,			/* write files on compile-it command */
#endif
	EndWNewline,		/* end files with a blank line */
	MarkThresh,		/* moves greater than MarkThresh will SetMark */
	PDelay,			/* paren flash delay in tenths of a second */
	CIndIncrmt,		/* how much each indentation level pushes
				   over in C mode */
	CreatMode,		/* default mode for creat'ing files */
	CaseIgnore,		/* case ignore search */
#ifdef ABBREV
	AutoCaseAbbrev,		/* automatically do case on abbreviations */
#endif
	MarksShouldFloat,	/* adjust marks on insertion/deletion */
	UseRE,			/* use regular expressions in search */
	SyncFreq,		/* how often to sync the file pointers */
	BriteMode,		/* make the mode line inverse? */
	OkayBadChars,		/* allow bad characters in files created
				   by JOVE */
	UpdFreq,		/* how often to update modeline */
	UseBuffers,		/* use buffers with Typeout() */
#ifdef BIFF
	BiffChk,		/* turn off/on biff with entering/exiting jove */
#endif
	MailInt,		/* mail check interval */
#ifdef ID_CHAR
	UseIC,			/* whether or not to use i/d char
				   processesing */
#endif
	SExitChar,		/* type this to stop i-search */
	AbortChar,		/* cancels command input */
	IntChar,		/* ttysets this to generate QUIT */
	DoEVexpand,		/* treat $foo as environment variable */
#ifdef F_COMPLETION
 	DispBadFs,		/* display filenames with bad extensions? */
#endif	
#ifdef IBMPC
	Fgcolor,
	Bgcolor,
	Mdcolor,
#endif /* IBMPC */
#ifdef F_COMPLETION
	DispBadFs,		/* display filenames with bad extensions? */
#endif
	ScrollAll,		/* we current line scrolls, scroll whole window? */
#ifndef MAC
	EWSize;			/* size to make the error window */
#else	
	Macmode,	/* see mac.c */
	Keyonly,
	Bufchange,
	Modechange,
	Windchange,
	EventCmd;
#endif	/* MAC */

#ifdef MAC
#	ifdef TXT_TO_C	/* kludge, for setmaps with variables */
char
#	else
extern char
#	endif /* TXT_TO_C */
#else
extern char
#endif /* MAC */

#ifndef MAC
	ErrFmtStr[256],		/* format string for parse errors */
#endif
#ifdef IPROCS
	proc_prompt[128],	/* process prompt */
#endif
#ifdef F_COMPLETION
	BadExtensions[128],	/* extensions (e.g., ".o" to ignore) */
#endif
#ifdef CMT_FMT
	CmtFmt[80],
#endif
	ModeFmt[120],		/* mode line format string */
#ifdef UNIX
	Mailbox[FILESIZE],		/* mailbox name */
#endif /* UNIX */
	TmpFilePath[FILESIZE],	/* directory/device to store tmp files */
	TagFile[FILESIZE],		/* default tag file */
	Shell[FILESIZE];		/* shell to use */

extern int
	TOabort,	/* flag set by Typeout() */
	io,		/* file descriptor for reading and writing files */
	errormsg,	/* last message was an error message
			   so don't erase the error before it
			   has been read */
	this_cmd,	/* ... */
	last_cmd,	/* last command ... to implement appending
			   to kill buffer */
	RecDepth,	/* recursion depth */
	InputPending,	/* nonzero if there is input waiting to
			   be processed */
 	killptr,	/* index into killbuf */
	CanScroll,	/* can this terminal scroll? */
	Crashing,	/* we are in the middle of crashing */
	Asking,		/* are we on read a string from the terminal? */
	inIOread;	/* so we know whether we can do a redisplay. */

extern char	Minibuf[LBSIZE];

#define curmark		(curbuf->b_markring[curbuf->b_themark])
#define b_curmark(b)	(b->b_markring[b->b_themark])

extern Line	*killbuf[NUMKILLS];	/* array of pointers to killed stuff */

#define MESG_SIZE 128
extern char	mesgbuf[MESG_SIZE];

struct screenline {
	char	*s_line,
		*s_length;
};

extern int
	LastKeyStruck;

extern int
	InMacDefine,	/* are we defining a macro right now? */

	CapLine,	/* cursor line and cursor column */
	CapCol,

	UpdModLine,	/* whether we want to update the mode line */
	UpdMesg;	/* update the message line */

#define CATCH \
{\
	jmp_buf	sav_jmp; \
\
	push_env(sav_jmp); \
	if (setjmp(mainjmp) == 0) {

#define ONERROR \
	} else { \

#define ENDCATCH \
	} \
	pop_env(sav_jmp); \
}

#include "externs.h"
