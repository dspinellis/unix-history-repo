/*-
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)tty.c	5.15 (Berkeley) %G%";
#endif /* not lint */

/*
 * Terminal initialization routines.
 */
#include <sys/ioctl.h>

#include <curses.h>
#include <termios.h>
#include <unistd.h>

struct termios __orig_termios;
static struct termios norawt, rawt;
static int useraw;

#ifndef	OXTABS
#ifdef	XTABS			/* SMI uses XTABS. */
#define	OXTABS	XTABS
#else
#define	OXTABS	0
#endif
#endif

/*
 * gettmode --
 *	Do terminal type initialization.
 */
int
gettmode()
{
	useraw = 0;
	
	if (tcgetattr(STDIN_FILENO, &__orig_termios))
		return (ERR);

	GT = (__orig_termios.c_oflag & OXTABS) == 0;
	NONL = (__orig_termios.c_oflag & ONLCR) == 0;

	norawt = __orig_termios;
	norawt.c_oflag &= ~OXTABS;

	/*
	 * XXX
	 * System V and SMI systems overload VMIN and VTIME, such that
	 * VMIN is the same as the VEOF element, and VTIME is the same
	 * as the VEOL element.  This means that, if VEOF was ^D, the
	 * default VMIN is 4.  Majorly stupid.
	 */
	rawt = norawt;
	cfmakeraw(&rawt);
	rawt.c_cc[VMIN] = 1;
	rawt.c_cc[VTIME] = 0;

	return (tcsetattr(STDIN_FILENO, TCSADRAIN, &norawt) ? ERR : OK);
}

int
raw()
{
	useraw = __pfast = __rawmode = 1;
	return (tcsetattr(STDIN_FILENO, TCSADRAIN, &rawt));
}

int
noraw()
{
	useraw = __pfast = __rawmode = 0;
	return (tcsetattr(STDIN_FILENO, TCSADRAIN, &norawt));
}

int
cbreak()
{
	rawt.c_lflag &= ~ICANON;
	norawt.c_lflag &= ~ICANON;

	__rawmode = 1;
	if (useraw)
		return (tcsetattr(STDIN_FILENO, TCSADRAIN, &rawt));
	else
		return (tcsetattr(STDIN_FILENO, TCSADRAIN, &norawt));
}

int
nocbreak()
{
	rawt.c_lflag |= ICANON;
	norawt.c_lflag |= ICANON;

	__rawmode = 0;
	if (useraw) 
		return (tcsetattr(STDIN_FILENO, TCSADRAIN, &rawt));
	else
		return (tcsetattr(STDIN_FILENO, TCSADRAIN, &norawt));
}
	
int
echo()
{
	rawt.c_lflag |= ECHO;
	norawt.c_lflag |= ECHO;
	
	__echoit = 1;
	if (useraw) 
		return (tcsetattr(STDIN_FILENO, TCSADRAIN, &rawt));
	else
		return (tcsetattr(STDIN_FILENO, TCSADRAIN, &norawt));
}

int
noecho()
{
	rawt.c_lflag &= ~ECHO;
	norawt.c_lflag &= ~ECHO;
	
	__echoit = 0;
	if (useraw) 
		return (tcsetattr(STDIN_FILENO, TCSADRAIN, &rawt));
	else
		return (tcsetattr(STDIN_FILENO, TCSADRAIN, &norawt));
}

int
nl()
{
	rawt.c_iflag |= ICRNL;
	rawt.c_oflag |= ONLCR;
	norawt.c_iflag |= ICRNL;
	norawt.c_oflag |= ONLCR;

	__pfast = __rawmode;
	if (useraw) 
		return (tcsetattr(STDIN_FILENO, TCSADRAIN, &rawt));
	else
		return (tcsetattr(STDIN_FILENO, TCSADRAIN, &norawt));
}

int
nonl()
{
	rawt.c_iflag &= ~ICRNL;
	rawt.c_oflag &= ~ONLCR;
	norawt.c_iflag &= ~ICRNL;
	norawt.c_oflag &= ~ONLCR;

	__pfast = 1;
	if (useraw) 
		return (tcsetattr(STDIN_FILENO, TCSADRAIN, &rawt));
	else
		return (tcsetattr(STDIN_FILENO, TCSADRAIN, &norawt));
}

void
__startwin()
{
	(void)fflush(stdout);
	(void)setvbuf(stdout, NULL, _IOFBF, 0);

	tputs(TI, 0, __cputchar);
	tputs(VS, 0, __cputchar);
}

int
endwin()
{
	if (curscr != NULL) {
		if (curscr->flags & __WSTANDOUT) {
			tputs(SE, 0, __cputchar);
			curscr->flags &= ~__WSTANDOUT;
		}
		__mvcur(curscr->cury, curscr->cury, curscr->maxy - 1, 0, 0);
	}

	(void)tputs(VE, 0, __cputchar);
	(void)tputs(TE, 0, __cputchar);
	(void)fflush(stdout);
	(void)setvbuf(stdout, NULL, _IOLBF, 0);

	return (tcsetattr(STDIN_FILENO, TCSADRAIN, &__orig_termios));
}

/*
 * The following routines, savetty and resetty are completely useless and
 * are left in only as stubs.  If people actually use them they will almost
 * certainly screw up the state of the world.
 */
static struct termios savedtty;
int
savetty()
{
	return (tcgetattr(STDIN_FILENO, &savedtty));
}

int
resetty()
{
	return (tcsetattr(STDIN_FILENO, TCSADRAIN, &savedtty));
}
