/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)ttydefaults.h	1.2 (Berkeley) %G%
 */

/*
 * System wide defaults of terminal state.
 */
#ifndef _TTYDEFAULTS_
#define	_TTYDEFAULTS_

#define CTRL(x)	(x&037)
/*
 * Control Character Defaults
 */
#define	CEOF		CTRL('d')
#define	CEOL		_POSIX_VDISABLE
#ifdef DECSTYLE_DEFAULTS
#define	CERASE		0177
#define	CINTR		CTRL('c')
#else
#define	CERASE		CTRL('h')
#define	CINTR		0177
#endif
#define	CERASE2		_POSIX_VDISABLE
#define	CKILL		CTRL('u')
#define	CMIN		1
#define	CQUIT		034	/* FS, ^\ */
#define	CSUSP		CTRL('z')
#define	CTIME		0
#define	CDSUSP		CTRL('y')
#define	CSTART		CTRL('q')
#define	CSTOP		CTRL('s')
#define	CLNEXT		CTRL('v')
#define	CFLUSHO 	CTRL('o')
#define	CWERASE 	CTRL('w')
#define	CREPRINT 	CTRL('r')
#define CQUOTE		'\\'
#define	CEOT		CEOF

/* COMPAT_43 */
#define	CBRK		CEOL
#define CRPRNT		CREPRINT
#define CFLUSH		CFLUSHO

/*
 * Settings on first open of a tty.
 */
#define	TTYDEF_IFLAG	(BRKINT | ISTRIP | IMAXBEL | IEXTEN | IXON)
#define TTYDEF_OFLAG	(0)
#define TTYDEF_LFLAG	(ECHO | ICANON | ISIG)
#define TTYDEF_CFLAG	(CREAD | CS7 | PARENB)
#define TTYDEF_SPEED	(B9600)

#endif /*_TTYDEFAULTS_*/

/*
 * define TTYDEFCHARS to include an array of default control characters.
 */
#ifdef TTYDEFCHARS
u_char	ttydefchars[NCC] = {
	CEOF,	CEOL,	CEOL,	CERASE, CWERASE, CKILL, CREPRINT, CQUOTE,
	CINTR,	CQUIT,	CSUSP,	CDSUSP,	CSTART,	CSTOP,	CLNEXT,
	CFLUSHO, CMIN,	CTIME, CERASE2, _POSIX_VDISABLE
};
#endif /*TTYDEFCHARS*/
