/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)ttydefaults.h	1.1 (Berkeley) %G%
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
#define	CEOL		POSIX_V_DISABLE
#ifdef DECSTYLE_DEFAULTS
#define	CERASE		0177
#define	CINTR		CTRL('c')	
#define	CKILL		CTRL('u')
#else
#define	CERASE		CTRL('h')
#define	CINTR		0177
#define	CKILL		CTRL('x')
#endif
#define	CMIN		1
#define	CQUIT		034	/* FS, ^\ */
#define	CSUSP		CTRL('z')
#define	CTIME		1
#define	CDSUSP		CTRL('y')
#define	CSTART		CTRL('q')
#define	CSTOP		CTRL('s')
#define	CLNEXT		CTRL('v')
#define	CFLUSHO 	CTRL('o')
#define	CWERASE 	CTRL('w')
#define	CREPRINT 	CTRL('r')
#define CQUOTE		'\\'
#define	CEOT		CEOF

#ifdef COMPAT_43
#define	CBRK		CEOL
#define CRPRNT		CREPRINT
#define CFLUSH		CFLUSHO
#endif

/*
 * Settings on first open of a tty.
 */
#define	TTYDEF_IFLAG	(BRKINT | ISTRIP | IMAXBEL | IEXTEN)
#define TTYDEF_OFLAG	(0)
#define TTYDEF_LFLAG	(ECHO | ICANON | ISIG)
#define TTYDEF_CFLAG	(CREAD | CS7 | PARENB)
#define TTYDEF_SPEED	(B9600)

#endif /*_TTYDEFAULTS_*/

/*
 * Define TTYDEFCHARS to include an array of default control characters.
 */
#ifdef TTYDEFCHARS
u_char	ttydefchars[NCC] = {
	CEOF,	CEOL,	CEOL,	CERASE, CWERASE, CKILL, CREPRINT, CQUOTE,
	CINTR,	CQUIT,	CSUSP,	CDSUSP,	CSTART,	CSTOP,	CLNEXT,
	CFLUSHO, CMIN,	CTIME, POSIX_V_DISABLE, POSIX_V_DISABLE
};
#endif /*TTYDEFCHARS*/
