/* vi.h */

/* Author:
 *	Steve Kirkendall
 *	14407 SW Teal Blvd. #C
 *	Beaverton, OR 97005
 *	kirkenda@cs.pdx.edu
 */

#define VERSION "ELVIS 1.5, by Steve Kirkendall (23 March 1992)"
#define COPYING	"This version of ELVIS is freely redistributable."

#include <errno.h>
extern int errno;
#if TOS && !defined(__GNUC__)
#define ENOENT (-AEFILNF)
#endif

#if TOS || VMS
# include <types.h>
# define O_RDONLY	0
# define O_WRONLY	1
# define O_RDWR		2
# ifdef __GNUC__
#  define S_IJDIR	S_IFDIR
# endif
#else
# if OSK
#  include <modes.h>
#  define O_RDONLY	S_IREAD
#  define O_WRONLY	S_IWRITE
#  define O_RDWR	(S_IREAD | S_IWRITE)
#  define ENOENT	E_PNNF
#  define sprintf	Sprintf
# else
#  include <sys/types.h>
#  if COHERENT
#   include <sys/fcntl.h>
#  else
#   include <fcntl.h>
#  endif
# endif
#endif

#ifndef O_BINARY
# define O_BINARY	0
#endif

#include "curses.h"

#include <signal.h>

/*------------------------------------------------------------------------*/
/* Miscellaneous constants.						  */

#define INFINITY	2000000001L	/* a very large integer */
#define LONGKEY		10		/* longest possible raw :map key */
#ifndef MAXRCLEN
# define MAXRCLEN	1000		/* longest possible :@ command */
#endif

/*------------------------------------------------------------------------*/
/* These describe how temporary files are divided into blocks             */

#define MAXBLKS	(BLKSIZE / sizeof(unsigned short))
typedef union
{
	char		c[BLKSIZE];	/* for text blocks */
	unsigned short	n[MAXBLKS];	/* for the header block */
}
	BLK;

/*------------------------------------------------------------------------*/
/* These are used manipulate BLK buffers.                                 */

extern BLK	hdr;		/* buffer for the header block */
extern BLK	*blkget();	/* given index into hdr.c[], reads block */
extern BLK	*blkadd();	/* inserts a new block into hdr.c[] */

/*------------------------------------------------------------------------*/
/* These are used to keep track of various flags                          */
extern struct _viflags
{
	short	file;		/* file flags */
}
	viflags;

/* file flags */
#define NEWFILE		0x0001	/* the file was just created */
#define READONLY	0x0002	/* the file is read-only */
#define HADNUL		0x0004	/* the file contained NUL characters */
#define MODIFIED	0x0008	/* the file has been modified, but not saved */
#define NOFILE		0x0010	/* no name is known for the current text */
#define ADDEDNL		0x0020	/* newlines were added to the file */
#define HADBS		0x0040	/* backspace chars were lost from the file */
#define UNDOABLE	0x0080	/* file has been modified */
#define NOTEDITED	0x0100	/* the :file command has been used */

/* macros used to set/clear/test flags */
#define setflag(x,y)	viflags.x |= y
#define clrflag(x,y)	viflags.x &= ~y
#define tstflag(x,y)	(viflags.x & y)
#define initflags()	viflags.file = 0;

/* The options */
extern char	o_autoindent[1];
extern char	o_autoprint[1];
extern char	o_autotab[1];
extern char	o_autowrite[1];
extern char	o_columns[3];
extern char	o_directory[30];
extern char	o_edcompatible[1];
extern char	o_equalprg[80];
extern char	o_errorbells[1];
extern char	o_exrefresh[1];
extern char	o_ignorecase[1];
extern char	o_keytime[3];
extern char	o_keywordprg[80];
extern char	o_lines[3];
extern char	o_list[1];
extern char	o_number[1];
extern char	o_readonly[1];
extern char	o_remap[1];
extern char	o_report[3];
extern char	o_scroll[3];
extern char	o_shell[60];
extern char	o_shiftwidth[3];
extern char	o_sidescroll[3];
extern char	o_sync[1];
extern char	o_tabstop[3];
extern char	o_term[30];
extern char	o_flash[1];
extern char	o_warn[1];
extern char	o_wrapscan[1];

#ifndef CRUNCH
extern char	o_beautify[1];
extern char	o_exrc[1];
extern char	o_mesg[1];
extern char	o_more[1];
extern char	o_novice[1];
extern char	o_prompt[1];
extern char	o_taglength[3];
extern char	o_terse[1];
extern char	o_window[3];
extern char	o_wrapmargin[3];
extern char	o_writeany[1];
#endif

#ifndef NO_ERRLIST
extern char	o_cc[30];
extern char	o_make[30];
#endif

#ifndef NO_CHARATTR
extern char	o_charattr[1];
#endif

#ifndef NO_DIGRAPH
extern char	o_digraph[1];
extern char	o_flipcase[80];
#endif

#ifndef NO_SENTENCE
extern char	o_hideformat[1];
#endif

#ifndef NO_EXTENSIONS
extern char	o_inputmode[1];
extern char	o_ruler[1];
#endif

#ifndef NO_MAGIC
extern char	o_magic[1];
#endif

#ifndef NO_MODELINES
extern char	o_modelines[1];
#endif

#ifndef NO_SENTENCE
extern char	o_paragraphs[30];
extern char	o_sections[30];
#endif

#if MSDOS
extern char	o_pcbios[1];
#endif

#ifndef NO_SHOWMATCH
extern char	o_showmatch[1];
#endif

#ifndef	NO_SHOWMODE
extern char	o_smd[1];
#endif

/*------------------------------------------------------------------------*/
/* These help support the single-line multi-change "undo" -- shift-U      */

extern char	U_text[BLKSIZE];
extern long	U_line;

/*------------------------------------------------------------------------*/
/* These are used to refer to places in the text 			  */

typedef long	MARK;
#define markline(x)	(long)((x) / BLKSIZE)
#define markidx(x)	(int)((x) & (BLKSIZE - 1))
#define MARK_UNSET	((MARK)0)
#define MARK_FIRST	((MARK)BLKSIZE)
#define MARK_LAST	((MARK)(nlines * BLKSIZE))
#define MARK_AT_LINE(x)	((MARK)(x) * BLKSIZE)

#define NMARKS	29
extern MARK	mark[NMARKS];	/* marks a-z, plus mark ' and two temps */
extern MARK	cursor;		/* mark where line is */

/*------------------------------------------------------------------------*/
/* These are used to keep track of the current & previous files.	  */

extern long	origtime;	/* modification date&time of the current file */
extern char	origname[256];	/* name of the current file */
extern char	prevorig[256];	/* name of the preceding file */
extern long	prevline;	/* line number from preceding file */

/*------------------------------------------------------------------------*/
/* misc housekeeping variables & functions				  */

extern int	tmpfd;		/* fd used to access the tmp file */
extern int	tmpnum;		/* counter used to generate unique filenames */
extern long	lnum[MAXBLKS];	/* last line# of each block */
extern long	nlines;		/* number of lines in the file */
extern char	args[BLKSIZE];	/* file names given on the command line */
extern int	argno;		/* the current element of args[] */
extern int	nargs;		/* number of filenames in args */
extern long	changes;	/* counts changes, to prohibit short-cuts */
extern int	significant;	/* boolean: was a *REAL* change made? */
extern BLK	tmpblk;		/* a block used to accumulate changes */
extern long	topline;	/* file line number of top line */
extern int	leftcol;	/* column number of left col */
#define		botline	 (topline + LINES - 2)
#define		rightcol (leftcol + COLS - (*o_number ? 9 : 1))
extern int	physcol;	/* physical column number that cursor is on */
extern int	physrow;	/* physical row number that cursor is on */
extern int	exwrote;	/* used to detect verbose ex commands */
extern int	doingdot;	/* boolean: are we doing the "." command? */
extern int	doingglobal;	/* boolean: are doing a ":g" command? */
extern long	rptlines;	/* number of lines affected by a command */
extern char	*rptlabel;	/* description of how lines were affected */
extern char	*fetchline();	/* read a given line from tmp file */
extern char	*parseptrn();	/* isolate a regexp in a line */
extern MARK	paste();	/* paste from cut buffer to a given point */
extern char	*wildcard();	/* expand wildcards in filenames */
extern MARK	input();	/* inserts characters from keyboard */
extern char	*linespec();	/* finds the end of a /regexp/ string */
#define		ctrl(ch) ((ch)&037)
#ifndef NO_RECYCLE
extern long	allocate();	/* allocate a free block of the tmp file */
#endif
extern int	trapint();	/* trap handler for SIGINT */
extern int	deathtrap();	/* trap handler for deadly signals */
extern void	blkdirty();	/* marks a block as being "dirty" */
extern void	blkflush();	/* writes a single dirty block to the disk */
extern void	blksync();	/* forces all "dirty" blocks to disk */
extern void	blkinit();	/* resets the block cache to "empty" state */
extern void	beep();		/* rings the terminal's bell */
extern void	exrefresh();	/* writes text to the screen */
extern void	msg();		/* writes a printf-style message to the screen */
extern void	endmsgs();	/* if "manymsgs" is set, then scroll up 1 line */
extern void	garbage();	/* reclaims any garbage blocks */
extern void	redraw();	/* updates the screen after a change */
extern void	resume_curses();/* puts the terminal in "cbreak" mode */
extern void	beforedo();	/* saves current revision before a new change */
extern void	afterdo();	/* marks end of a beforedo() change */
extern void	abortdo();	/* like "afterdo()" followed by "undo()" */
extern int	undo();		/* restores file to previous undo() */
extern void	dumpkey();	/* lists key mappings to the screen */
extern void	mapkey();	/* defines a new key mapping */
extern void	savekeys();	/* lists key mappings to a file */
extern void	redrawrange();	/* records clues from modify.c */
extern void	cut();		/* saves text in a cut buffer */
extern void	delete();	/* deletes text */
extern void	add();		/* adds text */
extern void	change();	/* deletes text, and then adds other text */
extern void	cutswitch();	/* updates cut buffers when we switch files */
extern void	do_abbr();	/* defines or lists abbreviations */
extern void	do_digraph();	/* defines or lists digraphs */
extern void	exstring();	/* execute a string as EX commands */
extern void	dumpopts();
extern void	setopts();
extern void	saveopts();
extern void	savedigs();
extern void	saveabbr();
extern void	savecolor();
extern void	cutname();
extern void	cutname();
extern void	initopts();
extern void	cutend();
#ifndef CRUNCH
extern int	wset;		/* boolean: has the "window" size been set? */
#endif

/*------------------------------------------------------------------------*/
/* macros that are used as control structures                             */

#define BeforeAfter(before, after) for((before),bavar=1;bavar;(after),bavar=0)
#define ChangeText	BeforeAfter(beforedo(FALSE),afterdo())

extern int	bavar;		/* used only in BeforeAfter macros */

/*------------------------------------------------------------------------*/
/* These are the movement commands.  Each accepts a mark for the starting */
/* location & number and returns a mark for the destination.		  */

extern MARK	m_updnto();		/* k j G */
extern MARK	m_right();		/* h */
extern MARK	m_left();		/* l */
extern MARK	m_tocol();		/* | */
extern MARK	m_front();		/* ^ */
extern MARK	m_rear();		/* $ */
extern MARK	m_fword();		/* w */
extern MARK	m_bword();		/* b */
extern MARK	m_eword();		/* e */
extern MARK	m_paragraph();		/* { } [[ ]] */
extern MARK	m_match();		/* % */
#ifndef NO_SENTENCE
 extern MARK	m_sentence();		/* ( ) */
#endif
extern MARK	m_tomark();		/* 'm */
#ifndef NO_EXTENSIONS
extern MARK	m_wsrch();		/* ^A */
#endif
extern MARK	m_nsrch();		/* n */
extern MARK	m_Nsrch();		/* N */
extern MARK	m_fsrch();		/* /regexp */
extern MARK	m_bsrch();		/* ?regexp */
#ifndef NO_CHARSEARCH
 extern MARK	m__ch();		/* ; , */
 extern MARK	m_fch();		/* f */
 extern MARK	m_tch();		/* t */
 extern MARK	m_Fch();		/* F */
 extern MARK	m_Tch();		/* T */
#endif
extern MARK	m_row();		/* H L M */
extern MARK	m_z();			/* z */
extern MARK	m_scroll();		/* ^B ^F ^E ^Y ^U ^D */

/* Some stuff that is used by movement functions... */

extern MARK	adjmove();		/* a helper fn, used by move fns */

/* This macro is used to set the default value of cnt */
#define DEFAULT(val)	if (cnt < 1) cnt = (val)

/* These are used to minimize calls to fetchline() */
extern int	plen;	/* length of the line */
extern long	pline;	/* line number that len refers to */
extern long	pchgs;	/* "changes" level that len refers to */
extern char	*ptext;	/* text of previous line, if valid */
extern void	pfetch();
extern char	digraph();

/* This is used to build a MARK that corresponds to a specific point in the
 * line that was most recently pfetch'ed.
 */
#define buildmark(text)	(MARK)(BLKSIZE * pline + (int)((text) - ptext))


/*------------------------------------------------------------------------*/
/* These are used to handle EX commands.				  */

#define  CMD_NULL	0	/* NOT A VALID COMMAND */
#define  CMD_ABBR	1	/* "define an abbreviation" */
#define  CMD_ARGS	2	/* "show me the args" */
#define  CMD_APPEND	3	/* "insert lines after this line" */
#define  CMD_AT		4	/* "execute a cut buffer's contents via EX" */
#define  CMD_BANG	5	/* "run a single shell command" */
#define  CMD_CC		6	/* "run `cc` and then do CMD_ERRLIST" */
#define  CMD_CD		7	/* "change directories" */
#define  CMD_CHANGE	8	/* "change some lines" */
#define	 CMD_COLOR	9	/* "change the default colors" */
#define  CMD_COPY	10	/* "copy the selected text to a given place" */
#define  CMD_DELETE	11	/* "delete the selected text" */
#define  CMD_DIGRAPH	12	/* "add a digraph, or display them all" */
#define  CMD_EDIT	13	/* "switch to a different file" */
#define  CMD_EQUAL	14	/* "display a line number" */
#define  CMD_ERRLIST	15	/* "locate the next error in a list" */
#define  CMD_FILE	16	/* "show the file's status" */
#define  CMD_GLOBAL	17	/* "globally search & do a command" */
#define  CMD_INSERT	18	/* "insert lines before the current line" */
#define  CMD_JOIN	19	/* "join the selected line & the one after" */
#define  CMD_LIST	20	/* "print lines, making control chars visible" */
#define  CMD_MAKE	21	/* "run `make` and then do CMD_ERRLIST" */
#define  CMD_MAP	22	/* "adjust the keyboard map" */
#define  CMD_MARK	23	/* "mark this line" */
#define  CMD_MKEXRC	24	/* "make a .exrc file" */
#define  CMD_MOVE	25	/* "move the selected text to a given place" */
#define  CMD_NEXT	26	/* "switch to next file in args" */
#define  CMD_NUMBER	27	/* "print lines from the file w/ line numbers" */
#define  CMD_PRESERVE	28	/* "act as though vi crashed" */
#define  CMD_PREVIOUS	29	/* "switch to the previous file in args" */
#define  CMD_PRINT	30	/* "print the selected text" */
#define  CMD_PUT	31	/* "insert any cut lines before this line" */
#define  CMD_QUIT	32	/* "quit without writing the file" */
#define  CMD_READ	33	/* "append the given file after this line */
#define  CMD_RECOVER	34	/* "recover file after vi crashes" - USE -r FLAG */
#define  CMD_REWIND	35	/* "rewind to first file" */
#define  CMD_SET	36	/* "set a variable's value" */
#define  CMD_SHELL	37	/* "run some lines through a command" */
#define  CMD_SHIFTL	38	/* "shift lines left" */
#define  CMD_SHIFTR	39	/* "shift lines right" */
#define  CMD_SOURCE	40	/* "interpret a file's contents as ex commands" */
#define  CMD_STOP	41	/* same as CMD_SUSPEND */
#define  CMD_SUBAGAIN	42	/* "repeat the previous substitution" */
#define  CMD_SUBSTITUTE	43	/* "substitute text in this line" */
#define  CMD_SUSPEND	44	/* "suspend the vi session" */
#define  CMD_TR		45	/* "transliterate chars in the selected lines" */
#define  CMD_TAG	46	/* "go to a particular tag" */
#define  CMD_UNABBR	47	/* "remove an abbreviation definition" */
#define  CMD_UNDO	48	/* "undo the previous command" */
#define  CMD_UNMAP	49	/* "remove a key sequence map */
#define  CMD_VERSION	50	/* "describe which version this is" */
#define  CMD_VGLOBAL	51	/* "apply a cmd to lines NOT containing an RE" */
#define  CMD_VISUAL	52	/* "go into visual mode" */
#define  CMD_WQUIT	53	/* "write this file out (any case) & quit" */
#define  CMD_WRITE	54	/* "write the selected(?) text to a given file" */
#define  CMD_XIT	55	/* "write this file out (if modified) & quit" */
#define  CMD_YANK	56	/* "copy the selected text into the cut buffer" */
#ifdef DEBUG
# define CMD_DEBUG	57	/* access to internal data structures */
# define CMD_VALIDATE	58	/* check for internal consistency */
#endif
typedef int CMD;

extern void	ex();
extern void	vi();
extern void	doexcmd();

extern void	cmd_append();
extern void	cmd_args();
#ifndef NO_AT
extern void	cmd_at();
#endif
extern void	cmd_cd();
#ifndef NO_COLOR
extern void	cmd_color();
#endif
extern void	cmd_delete();
#ifndef NO_DIGRAPH
extern void	cmd_digraph();
#endif
extern void	cmd_edit();
#ifndef NO_ERRLIST
extern void	cmd_errlist();
#endif
extern void	cmd_file();
extern void	cmd_global();
extern void	cmd_join();
extern void	cmd_mark();
#ifndef NO_ERRLIST
extern void	cmd_make();
#endif
extern void	cmd_map();
#ifndef NO_MKEXRC
extern void	cmd_mkexrc();
#endif
extern void	cmd_next();
extern void	cmd_print();
extern void	cmd_put();
extern void	cmd_read();
extern void	cmd_set();
extern void	cmd_shell();
extern void	cmd_shift();
extern void	cmd_source();
extern void	cmd_substitute();
extern void	cmd_tag();
extern void	cmd_undo();
extern void	cmd_version();
extern void	cmd_write();
extern void	cmd_xit();
extern void	cmd_move();
#ifdef DEBUG
extern void	cmd_debug();
extern void	cmd_validate();
#endif
#ifdef SIGTSTP
extern void	cmd_suspend();
#endif

/*----------------------------------------------------------------------*/
/* These are used to handle VI commands 				*/

extern MARK	v_1ex();	/* : */
extern MARK	v_mark();	/* m */
extern MARK	v_quit();	/* Q */
extern MARK	v_redraw();	/* ^L ^R */
extern MARK	v_ulcase();	/* ~ */
extern MARK	v_undo();	/* u */
extern MARK	v_xchar();	/* x X */
extern MARK	v_replace();	/* r */
extern MARK	v_overtype();	/* R */
extern MARK	v_selcut();	/* " */
extern MARK	v_paste();	/* p P */
extern MARK	v_yank();	/* y Y */
extern MARK	v_delete();	/* d D */
extern MARK	v_join();	/* J */
extern MARK	v_insert();	/* a A i I o O */
extern MARK	v_change();	/* c C */
extern MARK	v_subst();	/* s */
extern MARK	v_lshift();	/* < */
extern MARK	v_rshift();	/* > */
extern MARK	v_reformat();	/* = */
extern MARK	v_filter();	/* ! */
extern MARK	v_status();	/* ^G */
extern MARK	v_switch();	/* ^^ */
extern MARK	v_tag();	/* ^] */
extern MARK	v_xit();	/* ZZ */
extern MARK	v_undoline();	/* U */
extern MARK	v_again();	/* & */
#ifndef NO_EXTENSIONS
 extern MARK	v_keyword();	/* K */
 extern MARK	v_increment();	/* * */
#endif
#ifndef NO_ERRLIST
 extern MARK	v_errlist();	/* * */
#endif
#ifndef NO_AT
 extern MARK	v_at();		/* @ */
#endif
#ifdef SIGTSTP
 extern MARK	v_suspend();	/* ^Z */
#endif
#ifndef NO_POPUP
 extern MARK	v_popup();	/* \ */
#endif

/*----------------------------------------------------------------------*/
/* These flags describe the quirks of the individual visual commands */
#define NO_FLAGS	0x00
#define	MVMT		0x01	/* this is a movement command */
#define PTMV		0x02	/* this can be *part* of a movement command */
#define FRNT		0x04	/* after move, go to front of line */
#define INCL		0x08	/* include last char when used with c/d/y */
#define LNMD		0x10	/* use line mode of c/d/y */
#define NCOL		0x20	/* this command can't change the column# */
#define NREL		0x40	/* this is "non-relative" -- set the '' mark */
#define SDOT		0x80	/* set the "dot" variables, for the "." cmd */
#ifndef NO_VISIBLE
# define VIZ		0x100	/* commands which can be used with 'v' */
#else
# define VIZ		0
#endif

/* This variable is zeroed before a command executes, and later ORed with the
 * command's flags after the command has been executed.  It is used to force
 * certain flags to be TRUE for *some* invocations of a particular command.
 * For example, "/regexp/+offset" forces the LNMD flag, and sometimes a "p"
 * or "P" command will force FRNT.
 */
extern int	force_flags;

/*----------------------------------------------------------------------*/
/* These describe what mode we're in */

#define MODE_EX		1	/* executing ex commands */
#define	MODE_VI		2	/* executing vi commands */
#define	MODE_COLON	3	/* executing an ex command from vi mode */
#define	MODE_QUIT	4
extern int	mode;

#define WHEN_VICMD	1	/* getkey: we're reading a VI command */
#define WHEN_VIINP	2	/* getkey: we're in VI's INPUT mode */
#define WHEN_VIREP	4	/* getkey: we're in VI's REPLACE mode */
#define WHEN_EX		8	/* getkey: we're in EX mode */
#define WHEN_MSG	16	/* getkey: we're at a "more" prompt */
#define WHEN_POPUP	32	/* getkey: we're in the pop-up menu */
#define WHEN_REP1	64	/* getkey: we're getting a single char for 'r' */
#define WHEN_CUT	128	/* getkey: we're getting a cut buffer name */
#define WHEN_MARK	256	/* getkey: we're getting a mark name */
#define WHEN_CHAR	512	/* getkey: we're getting a destination for f/F/t/T */
#define WHEN_INMV	4096	/* in input mode, interpret the key in VICMD mode */
#define WHEN_FREE	8192	/* free the keymap after doing it once */
#define WHENMASK	(WHEN_VICMD|WHEN_VIINP|WHEN_VIREP|WHEN_REP1|WHEN_CUT|WHEN_MARK|WHEN_CHAR)

#ifndef NO_VISIBLE
extern MARK	V_from;
extern int	V_linemd;
extern MARK	v_start();
#endif

#ifdef DEBUG
# define malloc(size)	dbmalloc(size, __FILE__, __LINE__)
# define free(ptr)	dbfree(ptr, __FILE__, __LINE__)
extern char *dbmalloc();
#endif
