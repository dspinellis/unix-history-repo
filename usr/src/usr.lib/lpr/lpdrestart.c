/*	lpdrestart.c	1.4	83/01/05	*/

#include "lp.local.h"
#include <sys/types.h>
#include <signal.h>
#include <stdio.h>
#include <ctype.h>
#include "local/uparm.h"
#include <sys/stat.h>

/*
 * Restart line printer daemons after a reboot
 */

#define MAXHOP	32	/* max number of tc= indirections */
#define PRINTCAP

#ifdef PRINTCAP
#define tgetent	pgetent
#define tskip	pskip
#define tgetstr	pgetstr
#define tdecode pdecode
#define tgetnum	pgetnum
#define	tgetflag pgetflag
#define tdecode pdecode
#define tnchktc	pnchktc
#define	tnamatch pnamatch
#undef E_TERMCAP
#define E_TERMCAP "/etc/printcap"
#endif

char	*DN;		/* daemon name */
char	*LP;		/* printer device */
char	*SD;		/* spool directory */
char	*LO;		/* lock file name */
char	*LF;		/* log file */
char	*RM;		/* remote machine name */
char	*name;		/* name of current printer being worked on */

FILE	*lfd;		/* open file descriptor for LF */

char *tskip();
char *tgetstr();
char *tdecode();

main(argc, argv)
	char *argv[];
{
	char buf[BUFSIZ/2], b[BUFSIZ], *bp, lbuf[256];
	register char *Bp;
	char namebuf[30];
	struct stat statb;
	register int pid;

	while (getpr(b)) {
		bp = buf;
		if ((DN = pgetstr("dn", &bp)) == NULL)
			DN = DEFDAEMON;
		if ((LP = pgetstr("lp", &bp)) == NULL)
			LP = DEFDEVLP;
		if ((SD = pgetstr("sd", &bp)) == NULL)
			SD = DEFSPOOL;
		if ((LO = pgetstr("lo", &bp)) == NULL)
			LO = DEFLOCK;
		if ((LF = pgetstr("lf", &bp)) == NULL)
			LF = DEFLOGF;
		RM = pgetstr("rm", &bp);
		for (name = namebuf, Bp = b; *Bp && *Bp != ':' && *Bp != '|';)
			*name++ = *Bp++;
		*name = '\0';
		name = namebuf;
		if ((lfd = fopen(LF, "a")) == NULL)
			lfd = stderr;		/* expected to be console */
		/*
		 * If we are spooling remotely, let the daemon try to establish
		 *  a connection; otherwise, check the printer.
		 */
		if (*LP && RM == NULL) {
			if (stat(LP, (char *)&statb) < 0) {
				log("%s: can't stat %s", name, LP);
				continue;
			}
			if (statb.st_mode == 0) {
				log("%s: printer marked down", name);
				continue;
			}
		}
		/*
		 * Remove a lock file if it's present, then
		 *  restart the daemon
		 */
		sprintf(lbuf, "%s/%s", SD, LO);
		if (stat(lbuf, (char *)&statb) >= 0)
			if (unlink(lbuf)) {
				log("%s: can't unlink lock file in %s", name, SD);
				continue;
			}
		if ((pid = fork()) < 0)
			log("%s: can't fork", name);
		else if (!pid) {
			execl(DN, bp = rindex(DN, '/') ? bp+1 : DN, name, 0);
			log("%s: can't execl %s", name, DN);
			exit(1);
		}
		else
			fclose(lfd);
	}
}

/*VARARGS1*/
log(message, a1, a2, a3)
char *message;
{
	short console = isatty(fileno(lfd));

	fprintf(lfd, console ? "\r\n%s: " : "%s: ", name);
	fprintf(lfd, message, a1, a2, a3);
	if (console)
		putc('\r', lfd);
	putc('\n', lfd);
	fclose(lfd);
}

/*VARARGS*/
fatal(fmt, arg)
	char *fmt;
{
	fprintf(stderr, "lpdrestart: ");
	fprintf(stderr, fmt, arg);
	putc('\n', stderr);
	exit(1);
}

/*
 * Routines to perform a sequential pass through the printer
 *  data base.  Things are slightly complicated by interactions
 *  with the printcap routines.
 */

static int pfd = -1;		/* for reading from data base */
static long nextloc = -1L;	/* seek offset */
static char *tbuf;
static int hopcount;

getpr(bp)
	char *bp;
{
	register char *cp, *Bp;
	register int i = 0, cnt = 0, c;
	char ibuf[BUFSIZ];

	if (pfd < 0) {
		if ((pfd = open(E_TERMCAP, 0)) < 0)
			fatal("can't open %s", E_TERMCAP);
		nextloc = 0L;
	}
	else
		lseek(pfd, nextloc, 0);		/* move sequentially */
	/*
	 * This code is (for the most part) taken from tgetent()
	 */
	tbuf = bp;
	for (;;) {
		cp = bp;
		for (;;) {
			if (i == cnt) {
				cnt = read(pfd, ibuf, BUFSIZ);
				if (cnt <= 0) {
					close(pfd);
					nextloc = 0L;
					return(NULL);
				}
				i = 0;
			}
			c = ibuf[i++];
			if (c == '\n') {
				if (cp > bp && cp[-1] == '\\') {
					cp--;
					continue;
				}
				break;
			}
			if (cp >= bp+BUFSIZ)
				fatal("printcap entry too long");
			*cp++ = c;
		}
		*cp = 0;
		/*
		 * Interpret the printer description and adjust
		 *  nextloc so that we'll start scanning again
		 *  after this record.
		 */
		nextloc += i;		/* amount taken from read buffer */
		Bp = tbuf;
		if (*Bp != '#' && *Bp != '\0') {
			/*
			 * For now, we won't support "tc" chaining
			 */
			return(1);
		}
	}
}

/*
 * Skip to the next field.  Notice that this is very dumb, not
 * knowing about \: escapes or any such.  If necessary, :'s can be put
 * into the termcap file in octal.
 */
static char *
tskip(bp)
	register char *bp;
{

	while (*bp && *bp != ':')
		bp++;
	if (*bp == ':')
		bp++;
	return (bp);
}

/*
 * Get a string valued option.
 * These are given as
 *	cl=^Z
 * Much decoding is done on the strings, and the strings are
 * placed in area, which is a ref parameter which is updated.
 * No checking on area overflow.
 */
char *
tgetstr(id, area)
	char *id, **area;
{
	register char *bp = tbuf;

	for (;;) {
		bp = tskip(bp);
		if (!*bp)
			return (0);
		if (*bp++ != id[0] || *bp == 0 || *bp++ != id[1])
			continue;
		if (*bp == '@')
			return(0);
		if (*bp != '=')
			continue;
		bp++;
		return (tdecode(bp, area));
	}
}

/*
 * Tdecode does the grung work to decode the
 * string capability escapes.
 */
static char *
tdecode(str, area)
	register char *str;
	char **area;
{
	register char *cp;
	register int c;
	register char *dp;
	int i;

	cp = *area;
	while ((c = *str++) && c != ':') {
		switch (c) {

		case '^':
			c = *str++ & 037;
			break;

		case '\\':
			dp = "E\033^^\\\\::n\nr\rt\tb\bf\f";
			c = *str++;
nextc:
			if (*dp++ == c) {
				c = *dp++;
				break;
			}
			dp++;
			if (*dp)
				goto nextc;
			if (isdigit(c)) {
				c -= '0', i = 2;
				do
					c <<= 3, c |= *str++ - '0';
				while (--i && isdigit(*str));
			}
			break;
		}
		*cp++ = c;
	}
	*cp++ = 0;
	str = *area;
	*area = cp;
	return (str);
}
