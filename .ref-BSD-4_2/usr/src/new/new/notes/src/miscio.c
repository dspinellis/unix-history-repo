static char *sccsid = "@(#)miscio.c	1.3 2/3/83";

/*
 * miscio stuff:
 *
 * ttystrt/ttystop	switch back and forth to character-at-a-time mode
 * catchem		makes our program ignore kills and coredumps.
 * getnum		is a char at a time input routine
 * gchar		sucks in 1 character and masks off parity.
 */

#include "parms.h"
#include "structs.h"

#ifdef	UNIX4.0
#include <termio.h>
#else
#include <sgtty.h>
#endif

/* #include <sys/ioctl.h> */

#include <signal.h>
#include <errno.h>

static int modeset = 0;			/* == 1 if ttyflags messed with */
static char t_erase, t_kill;
short ospeed;				/* for tputs padding */
int replot;

#ifdef	UNIX4.0
static struct termio tty, otty;
#else
static int oldmode;			/* prev mode bits */
static struct sgttyb tty;
#endif

ttystrt()
{
	static int initialized = 0;

	if (!initialized) {
#ifdef	UNIX4.0
		x (ioctl(0, TCGETA, &tty) < 0, "ttystrt: gtty");
		otty = tty;
		t_erase = tty.c_cc[VERASE];
		t_kill = tty.c_cc[VKILL];
		ospeed = tty.c_cflag & CBAUD;
#else	UNIX4.0
#ifdef	V6
		x (gtty(0, &tty) < 0, "ttystrt: gtty");
#else	V6
		x (ioctl(0, TIOCGETP, &tty) < 0, "ttystrt: gtty");
#endif	V6
		oldmode = tty.sg_flags;
		t_erase = tty.sg_erase;
		t_kill = tty.sg_kill;
		ospeed = tty.sg_ospeed;
#endif	UNIX4.0
		initialized = 1;
	}

	if (!modeset) {
#ifdef	UNIX4.0
		tty.c_lflag &= ~(ECHO|ICANON);
		tty.c_cc[VEOF] = 1;
		tty.c_cc[VEOL] = 1;
		x (ioctl(0, TCSETA, &tty) < 0, "ttystrt: stty");
#else	UNIX4.0
		tty.sg_flags |= CBREAK;
		tty.sg_flags &= ~ECHO;
#ifdef	V6
		x (stty(0, &tty) < 0, "ttystrt: stty");
#else	V6
		x (ioctl(0, TIOCSETN, &tty) < 0, "ttystrt: stty");
#endif	V6
#endif	UNIX4.0
		replot = 1;
		modeset = 1;
	}
}

ttystop()
{
	if (modeset) {
#ifndef	UNIX4.0
		tty.sg_flags = oldmode;
#ifdef	V6
		if (stty(0, &tty) < 0)
#else	V6
		if (ioctl(0, TIOCSETN, &tty) < 0)
#endif	V6
#else	UNIX4.0
		if (ioctl(0, TCSETA, &otty) < 0)
#endif	UNIX4.0
			printf("ttystop: stty");
			/* can't use x() cause it calls us */
		modeset = 0;
	}
}

/*
 * return 1 if there is input from the terminal,
 * 0 otherwise.  systems without the appropriate
 * call should always return 0.
 */
isinput()
{
#ifndef	UNIX4.0
	long retval;

	if (ioctl(0,FIONREAD,&retval) == 0)
		return(retval != 0);
#endif
	return(0);
}

int ignsigs;
struct notesenv curenv;

static int (*osigint)();
static int (*osigquit)();			/* hold signal status */
#ifdef	SIGTSTP
static int (*osigtstp)();			/* control-z job stop */
#endif

/*
 * the new fancy signal catcher.
 * for interrupts and quits.
 */
catchint(signo)
{
	signal(signo, catchint);
	if (ignsigs)
		return;
	if (replot == 0)
		warn("Interrupt");
	longjmp(jenv, 1);
}

#ifdef	SIGTSTP

/*
 * the new fancy signal handler
 * for ^Z/SIGTSTP
 */
catchz()
{
	int wasset;

	signal(SIGTSTP, catchz);
	if (ignsigs)
		return;
	if ((wasset = modeset) != 0) {		/* want assignment */
		ttystop();			/* fix it up for user */
		at(0, 1);
		fflush(stdout);
	}
	signal(SIGTSTP, SIG_DFL);		/* make sure it nabs us */
	kill(0, SIGTSTP);			/* halt myself */
	signal(SIGTSTP, catchz);		/* ready to catch again */
	if (wasset)
		ttystrt();			/* fix his tty */
	longjmp(jenv);
}
#endif

catchem()
{
	osigint = signal(SIGINT, catchint);		/* interrupts */
	osigquit = signal(SIGQUIT, catchint);		/* quits */
#ifdef	SIGTSTP
	osigtstp = signal(SIGTSTP, catchz);		/* control Z */
#endif
}

uncatchem()
{
	signal(SIGINT, osigint);
	signal(SIGQUIT, osigquit);
#ifdef	SIGTSTP
	signal(SIGTSTP, osigtstp);
#endif
}

/*
 * get the next character from the terminal,
 * stripping the parity bit
 */
gchar()
{
	char c;
	int ret;
	extern int errno;

	fflush (stdout);		/* get rid of what's there */
	while ((ret = read (0, &c, 1)) <= 0)
		/* try again only if interrupted */
		x (ret == 0 || errno != EINTR, "gchar: read error");
	return(c & 0177);
}

/*
 * get a number from the user.
 * c is the first digit (already typed).
 */
getnum(c)
int c;
{
	int num, numin;
	int x, y;

	num = c - '0';
	curpos(&x, &y);
	numin = 1;
	putch(c);
	for (; ;) {
		c = gchar();
		if (c == '\n' || c == '\r')
			return(num);			/* done */
		if (c == t_erase) {
			if (numin > 0) {
				num /= 10;
				numin--;
				putstr("\b \b");
			}
			continue;
		}
		if (c == t_kill) {
			num = 0;
			numin = 0;
			at(x, y);
			clear_eol();
			continue;
		}
		if ('0' <= c && c <= '9') {		/* valid character */
			putch(c);
			numin++;
			num = 10 * num + (c - '0');
		} else
			putch('\07');
	}
	/*NOT REACHED*/
}

/*
 * gline( p, i) - suck a maximum of i characters from the tty.
 * do erase and kill processing.
 * The line is terminated by the user typing a <cr> or <nl>. This
 * character is converted to null and left on the end of the
 * string returned. The count of characters (including the null
 * terminator) is returned.
 * The array passed in is assumed to have i+1 elements
 * (enough for the characters plus the terminator)
 *
 * Original Coding:	Ray Essick	December 1981
 */
gline(p, max)
char *p;
{
	register int numin;
	register char *q;
	register int c;
	int x, y;

	q = p;	
	numin = 0;
	curpos(&x, &y);
	for (;;) {
		c = gchar();
		if (c == t_erase) {
			if (numin > 0) {
				numin--;
				q--;
				putstr("\b \b");
				if (*q < 040 || *q == 0177)
					putstr("\b \b");
			}
			continue;
		}
		if (c == t_kill) {
			at(x, y);
			clear_eol();
			q = p;
			numin = 0;
			continue;
		}
		switch (c) {
		case '\n': 
		case '\r': 
			if (numin >= max) {
				p[max] = '\0';
				return(max + 1);
			}
			*q = '\0';
			numin++;
			return(numin);
		case 'V'&037:			/* ^V */
		case '\\': 			/* escape character */
			putstr("^\b");		/* back space to it */
			c = gchar();		/* grab escaped character */
			/* FALL INTO ... */
		default:
			if (numin < max) {
				*q++ = c;
				numin++;
				mapch(c);
			} else
				putch('\007');
				/* show him I ignored char */
			break;
		}
	}
	/*NOT REACHED*/
}

/*
 * returns y or n to the asked question
 */
askyn()
{
	int c;
	int x, y;

	curpos(&x, &y);
	for (; ;) {
		c = gchar();
		if (c == 'y' || c == 'n') {
			putch(c);
			putstr("  (working)");
			fflush(stdout);
			return(c);
		}
		putchar('\007');
		putstr("  (y/n)");
		at(x, y);
	}
	/*NOT REACHED*/
}

wfchar()
{
	printf("--Hit any key to continue--");
	gchar();
}

/*VARARGS1*/
warn(s, a1, a2, a3)
char *s;
{
	char buf[BUFSIZ];

	at(0, 1);
	sprintf(buf, s, a1, a2, a3);
	standout(1);
	putstr(buf);
	standout(0);
	clear_eol();
}

/*VARARGS1*/
prompt(s, a1, a2, a3)
char *s;
{
	char buf[BUFSIZ];

	at(0, 1);
	sprintf(buf, s, a1, a2, a3);
	putstr(buf);
	clear_eol();
}

cmdprompt()
{
#ifdef BERKELEY
	int set = LFLUSHO;

	/* fflush(stdout);	/* sigh... want a synchronous write here */
	ioctl(0, TIOCLBIC, &set);
#endif BERKELEY

	at(-1, 1);
#ifdef PROMPT
	putstr(PROMPT);
#endif PROMPT
}

putstr(s)
char *s;
{
	while (*s)
		putch(*s++);
}

/*
 * a putchar with all the fancy control
 * character mapping
 */
mapch(c)
{
	if (c < 040 || c == 0177) {
		putch('^');
		if (c < 040)
			c |= 0100;
		else
			c = '?';
	}
	putch(c);
}
