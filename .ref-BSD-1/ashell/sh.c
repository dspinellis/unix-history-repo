#include "sh.h"
/*
 * Shell
 *
 * Modified by Bill Joy
 * UC Berkeley 1976/1977
 *

	Features remaining to be fixed up and/or implemented
	======== ========= == == ===== == === == ===========

 * Commands insert and delete on argument lists should be
 * implemented also set 3= when have 2 arguments should be
 * handled somehow (perhaps optionally valid).
 * Also note that $12 should really be done correctly.

 * A notion of terse ?

 * time time ... time time date etc. are funny
 * These should be implemented correctly with the internal versions
 * of exit repeat if goto etc.  Probably want to maintain a timed
 * process number list and check this when processes die (this uses
 * less processes than forking a time).

 * Better facilities for string manipulation ?

 * The exit status variable should be implemented (status)
 * and set too by the internal commands.  When do this get
 * rid of kludge about prompt not set for exit on error or
 * fix up in some way.  Also should rewrite the argument
 * processing section to be cleaner and make some sense.

 * Do verbose more cleanly.

 * Implement next (recursively!)

 * Implement shell filters and <<. This probably involves some more
 * modularization of the scanner and deciphering of the two different
 * options available in the bell shell.

 * Other stuff from Bell shell -
 *
 *	Any more control facilities ?
 *	|| &&
 *	<>, ><, >2, 2 >, etc.
 *	Terminate
 *	Can we build high level commands out of gotos which have
 *	    optional defaults and ifs that set variables which one
 *	    can test ? Think so !
 *	    but what about "for" ?
 *	Prompt for more input ?
 *	Command substitution `   ` ?
 *	Trap ?
 *	Exec ?
 *	Eval ?
 *	.acct ?
 *	Some way to set stdin to be input when filters can happen.

 * What about file name substitution and quoting in set.

 * Conditional expressions should be fixed up with set name@
 * and to allow quoting at least between ? ... : ... }.

 * internal if, repeat, nohup, goto, exit, echo, echononl
 * Repeat, time, nohup, etc. should allow
 *
 *	repeat 10 { sleep 5; echo hi } &
 *	time { pc -l *.p ; obj }
 * or perhaps with ('s since that is illegal now anyways
 * although the above is more like if.

 * Changes to glob to allow ~, prevent too long path, and prevent
 * perhaps running out of directories.  Also change it so it doesn't
 * exit so ungracefully, i.e. on:
 *	chdir /asdf/adfas/a*
 * In error diagnostics the command name is often not set or set
 * wrong on calls to bferr.  Could have more calls to bferr2.
 * Should be able to distinguish between ``No more processes'' and
 * ``Too many processes.''

 * Shell flags to exit on error, make undefined variables an error.

 * Interrupted waits and errors in glob cost storage.
 * Scratch should go away.
 * Set a="abc" should print back similarly.

 * Last, but certainly not least, some of the INTERLISP redo etc features.

 */

char	prompt[]	"prompt";
char	pcs[]		"pcs";
char	shell[]		"shell";
char	pid[]		"pid";
char	home[]		"home";
char	path[]		"path";
char	n_args[]	"nargs";
char	tim[]		"time";

char *narginp, nonelflg, nverbose;

main(c, av)
	int c;
	char **av;
{
	register f;
	register char **v, *cp;

	settimes();
	v = av;
	uid = getuid();
	loginsh = **v == '-';
	set(home, gethome() == 0 ? savestr(hentry.home) : ".");
	set1("0404", "/usr/bin/px", &interps);
	set(prompt, uid == 0 ? "#\240" : "\246\240");
	set(shell, "/usr/bin/ashell");
	set(pid, putn(getpid()));
	set1(tim, "tyme", &aliases);
	for (f = 3; f < 15; f++)
		close(f);
	if (c > 1) {
		if (*(cp = v[1]) == '-') {
			do
				switch (*cp++) {
					case 'V':
						verbose = 1;
					case 'v':
						nverbose = 1;
						break;
					case 'c':
						if (c > 2)
							narginp = v[2];
						goto l1;
					case 't':
						nonelflg = 2;
					case '\0':
l1:
						set(prompt, "");
						unsetv(prompt);
					case 'i':
						**v = '-';
						nofile++;
						break;
				}
			while (*cp);
			v++;
			c--;
		}
		if (nofile == 0 && c > 1) {
			set(prompt, "");
			unsetv(prompt);
			close(0);
			if (open(cp = v[1], 0) < 0) {
				prs(v[1]);
				err(": Cannot open");
				exit(1);
			}
		}
	}
	if (gtty(0, scratch) == 0 && gtty(1, scratch) == 0)
		**v = '-';
	c--;
	v++;
	if (c == 0) {
		c = 1;
		v = av;
	}
	setargs(c, v);
	set(path, "-/bin-/usr/bin");
	pfile("/.profile");
	if (loginsh)
		pfile("/.login");
	arginp = narginp;
	onelflg = nonelflg;
	verbose = nverbose;
	if (**av == '-') {
		setintr++;
		signal(QUIT, 1);
		signal(INTR, 1);
	}
	/* mask the name ?? */
	process(1);
	if (loginsh)
		prs("logout\n");
	goodbye(loginsh);
}

pfile(cp)
	char *cp;
{
	int oldinput, c;

	strcpy(scratch, hentry.home);
	strcat(scratch, cp);
	oldinput = dup(0);
	close(0);
	c = open(scratch, 0);
	if (c == 0) {
		process(0);
		close(0);
	}
	dup(oldinput);
	close(oldinput);
}

goodbye(f)
	int f;
{

	if (f) {
		signal(QUIT, 1);
		signal(INTR, 1);
		setintr = 0;
		pfile("/.logout");
	}
	exit(0);
}

process(pro)
{
	register char *cp;

	setexit();
	if (doneinp) {
		doneinp = 0;
		return;
	}
	for (;;) {
		cp = value(prompt);
		if (pro)
			prs(cp);
		for (; *cp; cp++)
			echo(*cp);
		main1();
	}
}

main1()
{
	register int *t;

	error = 0;
	lex(&paraml);
	if (error) {
		err(error);
		freelex(&paraml);
		return;
	}
	t = syntax(paraml.next, &paraml);
	if (error)
		err(error);
	else
		execute(t);
	freesyn(t);
	freelex(&paraml);
}

echo(c)
	char c;
{

	c =& 0177;
	if (verbose)
		write(2, &c, 1);
	return (c);
}

err(s)
	char *s;
{

	prs(s);
	prs("\n");
	s = value(prompt);
	if (*s == 0) {
		seek(0, 0, 2);
		exit(1);
	}
}

prs(os)
	register char *os;
{
	register char *s;

	s = os;
	if (s != 0) {
		while (*s)
			s++;
		write(2, os, s-os);
	}
}

digit(c)
	char c;
{
	c =& 0177;
	return (c >= '0' && c <= '9');
}

letter(c)
	char c;
{
	c =& 0177;
	return (c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z');
}

any(c, s)
	int c;
	register char *s;
{

	while (*s)
		if (*s++ == c)
			return(1);
	return(0);
}

char	htmp[]	"/etc/htmp";
int	tty;

gethome()
{
	char buf[100];
	register int i;
	register char *bufp, *cp;

	if (loginsh)
		exechome();
	tty = ttyn(2);
	if (readhome() == 0 && hentry.uid == uid)
		return (0);
	if (getpw(uid, buf) && sleep(15), getpw(uid, buf))
		return (-1);
	bufp = buf;
	for (i = 1; i < 6; i++) {
		while (*bufp != ':')
			if (*bufp++ == 0)
				return (-1);
		bufp++;
	}
/* 	printf("buf %s bufp %s", buf, bufp); */
	for (cp = bufp; *cp && *cp != ':'; cp++)
		continue;
	*cp = 0;
/* 	printf("bufp %s\n", bufp); */
	hentry.uid == uid;
	for (i = 0; i < (sizeof hentry.home - 1); i++)
		hentry.home[i] = *bufp ? *bufp++ : 0;
	return (0);
}

readhome()
{
	int htmpf;

	htmpf = open(htmp, 0);
	if (htmpf < 0)
		return (-1);
	if (seek(htmpf, tty * sizeof hentry, 0) < 0) {
		close(htmpf);
		return (-1);
	}
	if (read(htmpf, &hentry, sizeof hentry) != sizeof hentry) {
		close(htmpf);
		return (-1);
	}
	close (htmpf);
	return (0);
}

exechome()
{
	int status;
	static reenter;

	if (reenter)
		return;
	reenter = 1;
/*
	if (fork()) {
		wait(&status);
		if (tty == 'x')
			tty = status >> 8;
	} else {
		execl("/usr/bin/sethome", loginsh ? "-" : "x", 0);
		exit(0177);
	}
*/
}

getpw(uid, buf)
	int uid;
	char buf[];
{
	char pbuf[512];
	register int n;
	register char *bp, *pbufp;
	int pwf, m, pbufc;

	pwf = open("/etc/passwd", 0);
	if (pwf < 0)
		return (1);
	pbufc = 0;
	for (;;) {
		bp = buf;
		do {
			if (pbufc == 0) {
				pbufc = read(pwf, &pbuf, 512);
				if (pbufc <= 0)
					return (1);
				pbufp = pbuf;
			}
			pbufc--;
		} while ((*bp++ = *pbufp++) != '\n');
		*bp++ = '\0';
/* 		printf("uid %d xid %d buf %s", uid, xid(buf), buf); */
		if (xid(buf) == uid)
			return (0);
	}
}

xid(buf)
	char buf[];
{
	register int i;
	int uid;
	register char *bp;
	register c;

	bp = buf;
	for (i = 1; i < 3; i++) {
		while (*bp != ':')
			if (*bp++ == 0)
				return (-1);
		bp++;
	}
	i = 0;
	while ((c = *bp++) != ':')
		if (c == 0)
			return (-1);
		else if (digit(c))
			i = i * 10 + c - '0';
	uid = i;
	i = 0;
	while ((c = *bp++) != ':')
		if (c == 0)
			return (-1);
		else if (digit(c))
			i = i * 10 + c - '0';
	return (i << 8 | uid);
}
