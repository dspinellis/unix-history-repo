/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)2648.h	5.1 (Berkeley) 4/26/85
 */

/*
 * lib2648: routines to deal directly with the HP 2648 Graphics terminal.
 */

#include <stdio.h>

#define ESC	'\033'	/* Escape */

/* Normal/inverse video */
#define NORMAL	0	/* not inverse video */
#define INVERSE	1	/* inverse video */

/* Kinds of lines we can draw */
#define MX	10	/* exclusive or what's on screen */
#define MC	11	/* clear what's on screen */
#define MS	12	/* set what's on screen */

/* Escape sequence modes the terminal might be in */
#define NONE	20	/* not in an escape sequence */
#define ESCD	21	/* in an escape * d sequence */
#define ESCP	22	/* in an escape * p sequence */
#define ESCM	23	/* in an escape * m sequence */
#define TEXT	24	/* in graphics text mode */

/*
 * Constants for 2648 ^E/^F handshaking.
 */
#define ENQ	5	/* ^E sent by system to terminal */
#define ACK	6	/* ^F reply by terminal to system */
#define TBLKSIZ	32	/* Max # chars between handshakes */

/*
 * Misc. variables used by lib2648.
 */
int	_on2648;	/* true if getenv("TERM") is hp2648 */
int	_video;		/* are we in inverse video mode? */
int	_actsmode;	/* line type mode screen actually in */
int	_supsmode;	/* line type mode screen supposed to be in */
int	_escmode;	/* flavor of escape sequence currently in */
int	_cursoron;	/* true if cursor is on */

int	_outcount;	/* # of consecutive chars without read sent */
char	_pushback[BUFSIZ]; /* queue of chars pushed back onto the input */
char	*_pb_front, *_pb_back;

int	_penx, _peny;	/* where pen is really at */
int	_curx, _cury;	/* where cursor is really at */
int	_supx, _supy;	/* where pen and cursor are supposed to be */

#ifdef TRACE
FILE	*trace;		/* trace file for debugging */
#endif
