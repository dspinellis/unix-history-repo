/* vi.h */

/* Author:
 *	Steve Kirkendall
 *	14407 SW Teal Blvd. #C
 *	Beaverton, OR 97005
 *	kirkenda@cs.pdx.edu
 */


/* This is the header file for my version of vi. */

#define VERSION "ELVIS 1.4, by Steve Kirkendall"
#define COPYING	"This version of ELVIS is freely redistributable."

#include <errno.h>
extern int errno;
#if TOS
#define ENOENT (-AEFILNF)
#endif

#if TOS
# include <types.h>
# define O_RDONLY	0
# define O_WRONLY	1
# define O_RDWR		2
#else
# if OSK
#  include <modes.h>
#  define O_RDONLY	S_IREAD
#  define O_WRONLY	S_IWRITE
#  define O_RDWR	(S_IREAD | S_IWRITE)
#  define ENOENT	E_PNNF
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

/*------------------------------------------------------------------------*/
/* Miscellaneous constants.						  */

#define INFINITY	2000000001L	/* a very large integer */
#define LONGKEY		10		/* longest possible raw :map key */
#ifndef MAXRCLEN
# define MAXRCLEN	1000		/* longest possible .exrc file */
#endif

/*------------------------------------------------------------------------*/
/* These describe how temporary files are divided into blocks             */

#define BLKSIZE	1024		/* size of blocks */
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
#define MODIFIED	0x0008	/* the file has been modified */
#define NOFILE		0x0010	/* no name is known for the current text */
#define ADDEDNL		0x0020	/* newlines were added to the file */

/* macros used to set/clear/test flags */
#define setflag(x,y)	viflags.x |= y
#define clrflag(x,y)	viflags.x &= ~y
#define tstflag(x,y)	(viflags.x & y)
#define initflags()	viflags.file = 0;

/* The options */
extern char	o_autoindent[1];
extern char	o_autoprint[1];
extern char	o_autowrite[1];
#ifndef NO_ERRLIST
extern char	o_cc[30];
#endif
#ifndef NO_CHARATTR
extern char	o_charattr[1];
#endif
extern char	o_columns[3];
extern char	o_digraph[1];
extern char	o_directory[30];
extern char	o_edcompatible[1];
extern char	o_errorbells[1];
extern char	o_exrefresh[1];
#ifndef NO_DIGRAPH
extern char	o_flipcase[80];
#endif
#ifndef NO_SENTENCE
extern char	o_hideformat[1];
#endif
extern char	o_ignorecase[1];
#ifndef NO_EXTENSIONS
extern char	o_inputmode[1];
#endif
extern char	o_keytime[3];
extern char	o_keywordprg[80];
extern char	o_lines[3];
extern char	o_list[1];
#ifndef NO_MAGIC
extern char	o_magic[1];
#endif
#ifndef NO_ERRLIST
extern char	o_make[30];
#endif
#ifndef NO_MODELINE
extern char	o_modeline[1];
#endif
#ifndef NO_SENTENCE
extern char	o_paragraphs[30];
#endif
#if MSDOS
extern char	o_pcbios[1];
#endif
extern char	o_readonly[1];
extern char	o_report[3];
extern char	o_scroll[3];
#ifndef NO_SENTENCE
extern char	o_sections[30];
#endif
extern char	o_shell[60];
#ifndef NO_SHOWMATCH
extern char	o_showmatch[1];
#endif
#ifndef NO_SHOWMODE
extern char	o_smd[1];
#endif
extern char	o_shiftwidth[3];
extern char	o_sidescroll[3];
extern char	o_sync[1];
extern char	o_tabstop[3];
extern char	o_term[30];
extern char	o_vbell[1];
extern char	o_warn[1];
extern char	o_wrapmargin[3];
extern char	o_wrapscan[1];

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
#define MARK_AT_LINE(x)	((MARK)((x) * BLKSIZE))

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
extern long	lnum[MAXBLKS];	/* last line# of each block */
extern long	nlines;		/* number of lines in the file */
extern char	args[BLKSIZE];	/* file names given on the command line */
extern int	argno;		/* the current element of args[] */
extern int	nargs;		/* number of filenames in args */
extern long	changes;	/* counts changes, to prohibit short-cuts */
extern int	significant;	/* boolean: was a *REAL* change made? */
extern int	mustredraw;	/* boolean: force total redraw of screen? */
extern BLK	tmpblk;		/* a block used to accumulate changes */
extern long	topline;	/* file line number of top line */
extern int	leftcol;	/* column number of left col */
#define		botline	 (topline + LINES - 2)
#define		rightcol (leftcol + COLS - 1)
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
extern void	blkdirty();	/* marks a block as being "dirty" */
extern void	blkflush();	/* writes a single dirty block to the disk */
extern void	blksync();	/* forces all "dirty" blocks to disk */
extern void	blkinit();	/* resets the block cache to "empty" state */
extern void	beep();		/* rings the terminal's bell */
extern void	exrefresh();	/* writes text to the screen */
extern void	msg();		/* writes a printf-style message to the screen */
extern void	reset_msg();	/* resets the "manymsgs" flag */
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
#ifndef NO_DIGRAPH
extern void	savedigs();
#endif
#ifndef NO_ABBR
extern void	saveabbr();
#endif
extern void	cutname();
extern void	cutname();
extern void	initopts();
extern void	cutend();

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
extern MARK	m_fWord();		/* W */
extern MARK	m_bWord();		/* B */
extern MARK	m_eWord();		/* E */
extern MARK	m_fparagraph();		/* } */
extern MARK	m_bparagraph();		/* { */
extern MARK	m_fsection();		/* ]] */
extern MARK	m_bsection();		/* [[ */
extern MARK	m_match();		/* % */
#ifndef NO_SENTENCE
 extern MARK	m_fsentence();		/* ) */
 extern MARK	m_bsentence();		/* ( */
#endif
extern MARK	m_tomark();		/* 'm */
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
#define  CMD_COPY	9	/* "copy the selected text to a given place" */
#define  CMD_DELETE	10	/* "delete the selected text" */
#define  CMD_DIGRAPH	11	/* "add a digraph, or display them all" */
#define  CMD_EDIT	12	/* "switch to a different file" */
#define  CMD_EQUAL	13	/* "display a line number" */
#define  CMD_ERRLIST	14	/* "locate the next error in a list" */
#define  CMD_FILE	15	/* "show the file's status" */
#define  CMD_GLOBAL	16	/* "globally search & do a command" */
#define  CMD_INSERT	17	/* "insert lines before the current line" */
#define  CMD_JOIN	18	/* "join the selected line & the one after" */
#define  CMD_LIST	19	/* "print lines, making control chars visible" */
#define  CMD_MAKE	20	/* "run `make` and then do CMD_ERRLIST" */
#define  CMD_MAP	21	/* "adjust the keyboard map" */
#define  CMD_MARK	22	/* "mark this line" */
#define  CMD_MKEXRC	23	/* "make a .exrc file" */
#define  CMD_MOVE	24	/* "move the selected text to a given place" */
#define  CMD_NEXT	25	/* "switch to next file in args" */
#define  CMD_NUMBER	26	/* "print lines from the file w/ line numbers" */
#define  CMD_PRESERVE	27	/* "act as though vi crashed" */
#define  CMD_PREVIOUS	28	/* "switch to the previous file in args" */
#define  CMD_PRINT	29	/* "print the selected text" */
#define  CMD_PUT	30	/* "insert any cut lines before this line" */
#define  CMD_QUIT	31	/* "quit without writing the file" */
#define  CMD_READ	32	/* "append the given file after this line */
#define  CMD_RECOVER	33	/* "recover file after vi crashes" - USE -r FLAG */
#define  CMD_REWIND	34	/* "rewind to first file" */
#define  CMD_SET	35	/* "set a variable's value" */
#define  CMD_SHELL	36	/* "run some lines through a command" */
#define  CMD_SHIFTL	37	/* "shift lines left" */
#define  CMD_SHIFTR	38	/* "shift lines right" */
#define  CMD_SOURCE	39	/* "interpret a file's contents as ex commands" */
#define  CMD_STOP	40	/* same as CMD_SUSPEND */
#define  CMD_SUBAGAIN	41	/* "repeat the previous substitution" */
#define  CMD_SUBSTITUTE	42	/* "substitute text in this line" */
#define  CMD_SUSPEND	43	/* "suspend the vi session" */
#define  CMD_TR		44	/* "transliterate chars in the selected lines" */
#define  CMD_TAG	45	/* "go to a particular tag" */
#define  CMD_UNABBR	46	/* "remove an abbreviation definition" */
#define  CMD_UNDO	47	/* "undo the previous command" */
#define  CMD_UNMAP	48	/* "remove a key sequence map */
#define  CMD_VERSION	49	/* "describe which version this is" */
#define  CMD_VGLOBAL	50	/* "apply a cmd to lines NOT containing an RE" */
#define  CMD_VISUAL	51	/* "go into visual mode" */
#define  CMD_WQUIT	52	/* "write this file out (any case) & quit" */
#define  CMD_WRITE	53	/* "write the selected(?) text to a given file" */
#define  CMD_XIT	54	/* "write this file out (if modified) & quit" */
#define  CMD_YANK	55	/* "copy the selected text into the cut buffer" */
#ifdef DEBUG
# define CMD_DEBUG	56	/* access to internal data structures */
# define CMD_VALIDATE	57	/* check for internal consistency */
#endif
typedef int CMD;

extern void	ex();
extern void	vi();
extern void	doexcmd();

#ifndef NO_ABBR
extern void	cmd_abbr();
#endif
extern void	cmd_append();
extern void	cmd_args();
#ifndef NO_AT
extern void	cmd_at();
#endif
extern void	cmd_cd();
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
extern void	cmd_visual();
extern void	cmd_write();
extern void	cmd_xit();
extern void	cmd_move();
#ifdef DEBUG
extern void	cmd_debug();
extern void	cmd_validate();
#endif

/*----------------------------------------------------------------------*/
/* These are used to handle VI commands 				*/

extern MARK	v_1ex();	/* : */
extern MARK	v_mark();	/* m */
extern MARK	v_quit();	/* Q */
extern MARK	v_redraw();	/* ^L ^R */
extern MARK	v_ulcase();	/* ~ */
extern MARK	v_undo();	/* u */
extern MARK	v_xchar();	/* x */
extern MARK	v_Xchar();	/* X */
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
extern MARK	v_filter();	/* ! */
extern MARK	v_status();	/* ^G */
extern MARK	v_switch();	/* ^^ */
extern MARK	v_tag();	/* ^] */
extern MARK	v_xit();	/* ZZ */
extern MARK	v_undoline();	/* U */
extern MARK	v_again();	/* & */
#ifndef NO_EXTENSIONS
 extern MARK	v_keyword();	/* ^K */
 extern MARK	v_increment();	/* * */
#endif
#ifndef NO_ERRLIST
 extern MARK	v_errlist();	/* * */
#endif
#ifndef NO_AT
 extern MARK	v_at();		/* @ */
#endif

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
#define WHEN_INMV	256	/* in input mode, interpret the key in VICMD mode */
