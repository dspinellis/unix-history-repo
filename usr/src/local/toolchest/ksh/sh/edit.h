/*

 *      Copyright (c) 1984, 1985, 1986 AT&T
 *      All Rights Reserved

 *      THIS IS UNPUBLISHED PROPRIETARY SOURCE 
 *      CODE OF AT&T.
 *      The copyright notice above does not 
 *      evidence any actual or intended
 *      publication of such source code.

 */

/* @(#)edit.h	1.1 */

/*
 *  edit.h -  common data structure for vi and emacs edit options
 *
 *   David Korn
 *   AT&T Bell Laboratories
 *   Room 5D-112
 *   Murray Hill, N. J. 07974
 *   Tel. x7975
 *
 */

#define LOOKAHEAD	80
#ifdef VENIX
#define READAHEAD	1
#else
#define READAHEAD	LOOKAHEAD
#endif	/* VENIX */

#ifdef MULTIBYTE
# ifndef ESS_MAXCHAR
# include	"national.h"
# endif	/* ESS_MAXCHAR */
# if ESS_MAXCHAR<=2
typedef unsigned short genchar;
# else
typedef long genchar;
# endif
#define CHARSIZE	2
#else
typedef char genchar;
#define CHARSIZE	1
#endif /* MULTIBYTE */

struct edit
{
	int	e_kill;
	int	e_erase;
	int	e_eof;
	int	e_fchar;
	char	e_plen;		/* length of prompt string */
	char	e_crlf;		/* zero if cannot return to beginning of line */
	jmp_buf e_env;
	int	e_llimit;	/* line length limit */
	int	e_hline;	/* current history line number */
	int	e_hloff;	/* line number offset for command */
	int	e_hismin;	/* minimum history line number */
	int	e_hismax;	/* maximum history line number */
	int	e_raw;		/* set when in raw mode or alt mode */
	int	e_cur;		/* current line position */
	int	e_eol;		/* end-of-line position */
	int	e_pcur;		/* current physical line position */
	int	e_peol;		/* end of physical line position */
	int	e_mode;		/* edit mode */
	int	e_index;	/* index in look-ahead buffer */
	int	e_repeat;
	int	e_saved;
	int	e_fcol;		/* first column */
	int	e_ucol;		/* column for undo */
	int	e_addnl;	/* set if new-line must be added */
	int	e_wsize;	/* width of display window */
	unsigned char	*e_outptr;	/* pointer with output buffer */
	genchar	*e_inbuf;	/* pointer to input buffer */
	char	*e_prompt;	/* pointer to buffer containing the prompt */
	genchar	*e_ubuf;	/* pointer to the undo buffer */
	genchar	*e_tmp;		/* temporary workspace buffer */
	char	*e_search;	/* temporary workspace buffer */
	genchar	*e_Ubuf;	/* temporary workspace buffer */
	genchar	*e_physbuf;	/* temporary workspace buffer */
	int	e_lbuf[LOOKAHEAD];/* pointer to look-ahead buffer */
	int	e_fd;		/* file descriptor */
	int	e_ttyspeed;	/* line speed, also indicates tty parms are valid */
	int	*e_globals;	/* global variables */
	genchar	*e_window;	/* display window  image */
	char	e_inmacro;	/* processing macro expansion */
};

#define FEMAX		50	/* maximum number of file matches for q_expand */
#define MAXWINDOW	160	/* maximum width window */
#define MINWINDOW	20	/* minimum width window */
#define DFLTWINDOW	80	/* default window width */
#define	MAXPAT		100	/* maximum length for pattern word */
#define	YES	1
#define NO	0
#define FAST	2
#define SLOW	1
#define RAWMODE	1
#define ALTMODE	2
#define DELETE	'\177'
#define BELL	'\7'
#define ESC	033
#define	UEOF	-2			/* user eof char synonym */
#define	UERASE	-3			/* user erase char synonym */
#define	UINTR	-4			/* user intr char synonym */
#define	UKILL	-5			/* user kill char synonym */
#define	UQUIT	-6			/* user quit char synonym */

#define	cntl(x)		('x'&037)

#ifndef KSHELL
#define		STRIP		0377
#define		TO_PRINT	0100
#define 	GMACS	1
#define 	EMACS	2
#define		VIRAW	4
#define		EDITVI	8
#define		NOHIST	16
#define		EDITMASK	15
#define		is_option(m)	(opt_flag&(m))
extern char opt_flag;
#define read(fd,buff,n)		syscall(3,fd,buff,n)
#endif	/* KSHELL */

extern void setcooked();
extern struct edit editb;
extern MSG	big_vi;
extern MSG	version;
