/*-
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *	@(#)screen.h	8.1 (Berkeley) 6/9/93
 */

/*
 * There are minimum values that vi has to have to display a screen.  The
 * row minimum is fixed at 1 line for the text, and 1 line for any error
 * messages.  The column calculation is a lot trickier.  For example, you
 * have to have enough columns to display the line number, not to mention
 * guaranteeing that tabstop and shiftwidth values are smaller than the
 * current column value.  It's a lot simpler to have a fixed value and not
 * worry about it.
 *
 * XXX
 * MINIMUM_SCREEN_COLS is probably wrong.
 */
#define	MINIMUM_SCREEN_ROWS	 2
#define	MINIMUM_SCREEN_COLS	20

enum confirmation { YES, NO, QUIT };	/* Confirmation routine interface. */
					/* Line operations. */
enum operation { LINE_APPEND, LINE_DELETE, LINE_INSERT, LINE_RESET };
					/* Standard continue message. */
#define	CONTMSG	"Enter return to continue: "

/*
 * Structure for building argc/argv vector of ex arguments.
 */
typedef struct _args {
	char	*bp;			/* Buffer. */
	size_t	 len;			/* Buffer length. */

#define	A_ALLOCATED	0x01		/* If allocated space. */
	u_char	 flags;
} ARGS;

/*
 * Structure for mapping lines to the screen.  An SMAP is an array of
 * structures, one per screen line, holding a physical line and screen
 * offset into the line.  For example, the pair 2:1 would be the first
 * screen of line 2, and 2:2 would be the second.  If doing left-right
 * scrolling, all of the offsets will be the same, i.e. for the second
 * screen, 1:2, 2:2, 3:2, etc.  If doing the standard, but unbelievably
 * stupid, vi scrolling, it will be staggered, i.e. 1:1, 1:2, 1:3, 2:1,
 * 3:1, etc.
 *
 * The SMAP is always as large as the physical screen, so that when split
 * screens close, there is room to add in the newly available lines.
 */
					/* Map positions. */
enum position { P_BOTTOM, P_FILL, P_MIDDLE, P_TOP };

typedef struct _smap {
	recno_t lno;			/* 1-N: Physical file line number. */
	size_t off;			/* 1-N: Screen offset in the line. */
} SMAP;

/*
 * scr --
 *	The screen structure.  Most of the traditional ex/vi options and
 *	values follow the screen, and therefore are kept here.  For those
 *	of you that didn't follow that sentence, read "dumping ground".
 *	For example, things like tags and mapped character sequences are
 *	stored here.  Each new screen that is added to the editor will
 *	almost certainly have to keep its own stuff in here as well.
 */
struct _exf;
typedef struct _scr {
/* INITIALIZED AT SCREEN CREATE. */
	struct _scr	*next, *prev;	/* Linked list of screens. */

					/* Underlying file information. */
	struct _exf	*ep;		/* Screen's current file. */
	struct _exf	*enext;		/* Next file to edit. */
	struct _exf	*eprev;		/* Last file edited. */

					/* Split screen information. */
	struct _scr	*child;		/* Child screen. */
	struct _scr	*parent;	/* Parent screen. */
	struct _scr	*snext;		/* Next screen to display. */

					/* Physical screen information. */
	struct _smap	*h_smap;	/* First entry in screen/row map. */
	struct _smap	*t_smap;	/*  Last entry in screen/row map. */

	recno_t	 lno;			/* 1-N:     cursor file line. */
	recno_t	 olno;			/* 1-N: old cursor file line. */
	size_t	 cno;			/* 0-N:     file cursor column. */
	size_t	 ocno;			/* 0-N: old file cursor column. */
	size_t	 sc_row;		/* 0-N: logical screen cursor row. */
	size_t	 sc_col;		/* 0-N: logical screen cursor column. */

	size_t	 rows;			/* 1-N:      rows per screen. */
	size_t	 cols;			/* 1-N:   columns per screen. */
	size_t	 t_rows;		/* 1-N: text rows per screen. */
	size_t	 w_rows;		/* 1-N:      rows per window. */
	size_t	 s_off;			/* 0-N: row offset in window. */

	struct _msg	*msgp;		/* User message list. */

#define	L_ADDED		 0		/* Added lines. */
#define	L_CHANGED	 1		/* Changed lines. */
#define	L_COPIED	 2		/* Copied lines. */
#define	L_DELETED	 3		/* Deleted lines. */
#define	L_JOINED	 4		/* Joined lines. */
#define	L_MOVED		 5		/* Moved lines. */
#define	L_PUT		 6		/* Put lines. */
#define	L_READ		 7		/* Read lines. */
#define	L_LSHIFT	 8		/* Read lines. */
#define	L_RSHIFT	 9		/* Read lines. */
#define	L_YANKED	10		/* Yanked lines. */
	recno_t	 rptlines[L_YANKED + 1];/* Ex/vi: lines changed by last op. */

	size_t	 rcm;			/* Vi: 0-N: Column suck. */
#define	RCM_FNB		0x01		/* Column suck: first non-blank. */
#define	RCM_LAST	0x02		/* Column suck: last. */
	u_int	 rcmflags;

	struct _args	*args;		/* Ex/vi: argument buffers. */
	int	 argscnt;		/* Argument count. */
	char   **argv;			/* Arguments. */
	
					/* Ex/vi: interface between ex/vi. */
	FILE	*stdfp;			/* Ex output file pointer. */
	size_t	 exlinecount;		/* Ex/vi overwrite count. */
	size_t	 extotalcount;		/* Ex/vi overwrite count. */
	size_t	 exlcontinue;		/* Ex/vi line continue value. */

					/* FWOPEN_NOT_AVAILABLE */
	int	 trapped_fd;		/* Ex/vi trapped file descriptor. */

	u_int	 nkeybuf;		/* # of keys in the input buffer. */
	char	*mappedkey;		/* Mapped key return. */
	u_int	 nextkey;		/* Index of next key in keybuf. */
	char	 keybuf[256];		/* Key buffer. */

	fd_set	 rdfd;			/* Ex/vi: read fd select mask. */

	struct _hdr	 bhdr;		/* Ex/vi: line input. */
	struct _hdr	 txthdr;	/* Vi: text input TEXT header. */

	char	*ibp;			/* Ex: line input buffer. */
	size_t	 ibp_len;		/* Line input buffer length. */

	struct _excmdlist *lastcmd;	/* Ex: last command. */

/* PARTIALLY OR COMPLETELY COPIED FROM PREVIOUS SCREEN. */
	struct _gs	*gp;		/* Pointer to global area. */

	char	*rep;			/* Vi: input replay buffer. */
	size_t	 rep_len;		/* Vi: input replay buffer length. */

	char	*VB;			/* Visual bell termcap string. */

	char	*lastbcomm;		/* Ex/vi: last bang command. */

	char	*altfname;		/* Ex/vi: alternate file name. */

	u_char	 inc_lastch;		/* Vi: Last increment character. */
	long	 inc_lastval;		/* Vi: Last increment value. */

	struct _cb cuts[UCHAR_MAX + 2];	/* Ex/vi: cut buffers. */

	char	*paragraph;		/* Vi: Paragraph search list. */

	struct _hdr	taghdr;		/* Ex/vi: tag stack. */
	struct _tagf   **tfhead;	/* List of tag files. */
	char	*tlast;			/* Saved last tag. */

					/* Ex/vi: search/substitute info. */
	enum direction	searchdir;	/* File search direction. */
	regex_t	 sre;			/* Last search RE. */
	enum cdirection	csearchdir;	/* Character search direction. */
	u_char	 lastckey;		/* Last search character. */
	regmatch_t     *match;		/* Substitute match array. */
	size_t	 matchsize;		/* Substitute match array size. */
	char	*repl;			/* Substitute replacement. */
	size_t	 repl_len;		/* Substitute replacement length.*/
	size_t	*newl;			/* Newline offset array. */
	size_t	 newl_len;		/* Newline array size. */
	size_t	 newl_cnt;		/* Newlines in replacement. */

	struct _chname *cname;		/* Display names of characters. */
	u_char	 special[UCHAR_MAX];	/* Special character array. */

					/* Ex/vi: mapped chars, abbrevs. */
	struct _hdr	 seqhdr;	/* Linked list of all sequences. */
	struct _hdr	 seq[UCHAR_MAX];/* Linked character sequences. */

					/* Ex/vi: executed buffers. */
	char	*atkey_buf;		/* At key buffer. */
	char	*atkey_cur;		/* At key current pointer. */
	int	 atkey_len;		/* Remaining keys in at buffer. */
					/* At key stack. */
	u_char	 atkey_stack[UCHAR_MAX + 1];
	int	 exat_recurse;		/* Ex at recursion count. */
	int	 exat_lbuf;		/* Ex at last buffer. */
					/* Ex at key stack. */
	u_char	 exat_stack[UCHAR_MAX + 1];

	OPTION	 opts[O_OPTIONCOUNT];	/* Ex/vi: options. */

/*
 * SCREEN SUPPORT ROUTINES.
 * This is the set of routines that have to be written to add a screen.
 */
	void	 (*s_bell) __P((struct _scr *));
	int	 (*s_busy_cursor) __P((struct _scr *, char *));
	int	 (*s_change) __P((struct _scr *,
		     struct _exf *, recno_t, enum operation));
	size_t	 (*s_chposition) __P((struct _scr *,
		     struct _exf *, recno_t, size_t));
	enum confirmation
		 (*s_confirm) __P((struct _scr *,
		     struct _exf *, struct _mark *, struct _mark *));
	int	 (*s_down) __P((struct _scr *,
		     struct _exf *, struct _mark *, recno_t, int));
	int	 (*s_ex_cmd) __P((struct _scr *, struct _exf *,
		     struct _excmdarg *, struct _mark *));
	int	 (*s_ex_run) __P((struct _scr *, struct _exf *,
		     struct _mark *));
	int	 (*s_ex_write) __P((void *, const char *, int));
	int	 (*s_fill) __P((struct _scr *,
		     struct _exf *, recno_t, enum position));
	int	 (*s_get) __P((struct _scr *, struct _exf *, struct _hdr *,
		     int, u_int));
	int	 (*s_position) __P((struct _scr *,
		     struct _exf *, recno_t *, u_long, enum position));
	int	 (*s_refresh) __P((struct _scr *, struct _exf *));
	size_t	 (*s_relative) __P((struct _scr *, struct _exf *, recno_t));
	int	 (*s_split) __P((struct _scr *, struct _exf *));
	int	 (*s_suspend) __P((struct _scr *));
	int	 (*s_up) __P((struct _scr *,
		     struct _exf *, struct _mark *, recno_t, int));

/* Editor screens (implies edit mode, as well). */
#define	S_MODE_EX	0x0000001	/* Ex mode. */
#define	S_MODE_VI	0x0000002	/* Vi mode. */

/* Major screen/file changes. */
#define	S_EXIT		0x0000004	/* Exiting (not forced). */
#define	S_EXIT_FORCE	0x0000008	/* Exiting (forced). */
#define	S_FSWITCH	0x0000010	/* Switch files (not forced). */
#define	S_FSWITCH_FORCE	0x0000020	/* Switch files (forced). */
#define	S_SSWITCH	0x0000040	/* Switch screens. */
#define	__S_UNUSED	0x0000080	/* Unused. */
#define	S_MAJOR_CHANGE			/* Screen or file changes. */	\
	(S_EXIT | S_EXIT_FORCE | S_FSWITCH | S_FSWITCH_FORCE | S_SSWITCH)

#define	S_ABBREV	0x0000100	/* If have abbreviations. */
#define	S_AUTOPRINT	0x0000200	/* Autoprint flag. */
#define	S_BELLSCHED	0x0000400	/* Bell scheduled. */
#define	S_CUR_INVALID	0x0000800	/* Cursor position is incalculable. */
#define	S_GLOBAL	0x0001000	/* Doing a global command. */
#define	S_GLOBAL_QUIT	0x0002000	/* Quitting a global command. */
#define	S_INPUT		0x0004000	/* Doing text input. */
#define	S_ISFROMTTY	0x0008000	/* Reading from a tty. */
#define	S_MSGREENTER	0x0010000	/* If msg routine reentered. */
#define	S_RE_SET	0x0020000	/* The file's RE has been set. */
#define	S_REDRAW	0x0040000	/* Redraw the screen. */
#define	S_REFORMAT	0x0080000	/* Reformat the screen. */
#define	S_REFRESH	0x0100000	/* Refresh the screen. */
#define	S_RESIZE	0x0200000	/* Resize the screen. */
#define	S_UPDATE_MODE	0x0400000	/* Don't repaint modeline. */

#define	S_SCREEN_RETAIN			/* Retain at screen create. */	\
	(S_MODE_EX | S_MODE_VI | S_ISFROMTTY)

	u_int flags;
} SCR;

/* Public interfaces to the screens. */
int	scr_end __P((struct _scr *));
int	scr_init __P((struct _scr *, struct _scr *));
int	sex __P((struct _scr *, struct _exf *, struct _scr **));
int	svi __P((struct _scr *, struct _exf *, struct _scr **));
