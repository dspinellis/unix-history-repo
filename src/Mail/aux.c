/* Copyright (c) 1979 Regents of the University of California */
#

#include "rcv.h"
#include <sys/stat.h>
#include <sgtty.h>

/*
 * Mail -- a mail program
 *
 * Auxiliary functions.
 */

/*
 * Return a pointer to a dynamic copy of the argument.
 */

char *
savestr(str)
	char *str;
{
	register char *cp, *cp2, *top;

	for (cp = str; *cp; cp++)
		;
	top = salloc(cp-str + 1);
	if (top == NOSTR)
		return(NOSTR);
	for (cp = str, cp2 = top; *cp; cp++)
		*cp2++ = *cp;
	*cp2 = 0;
	return(top);
}

/*
 * Copy the name from the passed header line into the passed
 * name buffer.  Null pad the name buffer.
 */

copyname(linebuf, nbuf)
	char *linebuf, *nbuf;
{
	register char *cp, *cp2;

	for (cp = linebuf + 5, cp2 = nbuf; *cp != ' ' && cp2-nbuf < 8; cp++)
		*cp2++ = *cp;
	while (cp2-nbuf < 8)
		*cp2++ = 0;
}

/*
 * Announce a fatal error and die.
 */

panic(str)
	char *str;
{
	prs("panic: ");
	prs(str);
	prs("\n");
	exit(1);
}

/*
 * Catch stdio errors and report them more nicely.
 */

_error(str)
	char *str;
{
	prs("Stdio Error: ");
	prs(str);
	prs("\n");
	abort();
}

/*
 * Print a string on diagnostic output.
 */

prs(str)
	char *str;
{
	register char *s;

	for (s = str; *s; s++)
		;
	write(2, str, s-str);
}

/*
 * Touch the named message by setting its MTOUCH flag.
 * Touched messages have the effect of not being sent
 * back to the system mailbox on exit.
 */

touch(mesg)
{
	if (mesg >= 1 && mesg <= msgCount)
		message[mesg-1].m_flag |= MTOUCH;
}

/*
 * Test to see if the passed file name is a directory.
 * Return true if it is.
 */

isdir(name)
	char name[];
{
	struct stat sbuf;

	if (stat(name, &sbuf) < 0)
		return(0);
	return((sbuf.st_mode & S_IFMT) == S_IFDIR);
}

/*
 * Compute the size in characters of the passed message
 */

unsigned int
msize(messp)
	struct message *messp;
{
	register struct message *mp;

	mp = messp;
	return(mp->m_size);
}

/*
 * Count the number of arguments in the given string raw list.
 */

argcount(argv)
	char **argv;
{
	register char **ap;

	for (ap = argv; *ap != NOSTR; ap++)
		;	
	return(ap-argv);
}

/*
 * Given a file address, determine the
 * block number it represents.
 */

blockof(off)
	off_t off;
{
	off_t a;

	a = off >> 9;
	a &= 077777;
	return((int) a);
}

/*
 * Take a file address, and determine
 * its offset in the current block.
 */

offsetof(off)
	off_t off;
{
	off_t a;

	a = off & 0777;
	return((int) a);
}

/*
 * Determine if the passed file is actually a tty, via a call to
 * gtty.  This is not totally reliable, but . . .
 */

isatty(f)
{
	struct sgttyb buf;

	if (gtty(f, &buf) < 0)
		return(0);
	return(1);
}

/*
 * Return the desired header line from the passed message
 * pointer (or NOSTR if the desired header field is not available.
 */

char *
hfield(field, mp)
	char field[];
	struct message *mp;
{
	FILE *ibuf;
	char linebuf[LINESIZE];
	register char *cp, *cp2;
	int hfc;

	ibuf = setinput(mp);
	hfc = 0;
	while (readline(ibuf, linebuf) > 0 && hfc < HDRFIELDS) {
		if (equal(linebuf, ""))
			return(NOSTR);
		cp = linebuf;
		cp2 = field;
		while (raise(*cp++) == raise(*cp2++))
			;
		if (*--cp == ':' && *--cp2 == '\0') {
			cp++;
			while (any(*cp, " \t"))
				cp++;
			return(savestr(cp));
		}
		hfc++;
	}
	return(NOSTR);
}

/*
 * The following code deals with input stacking to do source
 * commands.  All but the current file pointer are saved on
 * the stack.
 */

static	int	ssp = -1;		/* Top of file stack */
static	FILE	*sstack[_NFILE];	/* Saved input files */

/*
 * Pushdown current input file and switch to a new one.
 * Set the global flag "sourcing" so that others will realize
 * that they are no longer reading from a tty (in all probability).
 */

source(name)
	char name[];
{
	register FILE *fi;

	if ((fi = fopen(name, "r")) == NULL) {
		perror(name);
		return(1);
	}
	if (ssp >= _NFILE-2) {
		printf("Too much \"sourcing\" going on.\n");
		fclose(fi);
		return(1);
	}
	sstack[++ssp] = input;
	input = fi;
	sourcing++;
	return(0);
}

/*
 * Source a file, but do nothing if the file cannot be opened.
 */

source1(name)
	char name[];
{
	register int f;

	if ((f = open(name, 0)) < 0)
		return(0);
	close(f);
	source(name);
}

/*
 * Pop the current input back to the previous level.
 * Update the "sourcing" flag as appropriate.
 */

unstack()
{
	if (ssp < 0) {
		printf("\"Source\" stack over-pop.\n");
		sourcing = 0;
		return(1);
	}
	fclose(input);
	input = sstack[ssp--];
	if (ssp < 0)
		sourcing = 0;
	return(0);
}

/*
 * Touch the indicated file.
 * This is nifty for the shell.
 */

alter(name)
	char name[];
{
	register int pid, f;
	char w;

	if ((pid = fork()) != 0)
		return;
	clrbuf(stdout);
	clrbuf(stderr);
	clrbuf(stdin);
	sleep(1);
	if ((f = open(name, 0)) < 0)
		exit(1);
	read(f, &w, 1);
	exit(0);
}

/*
 * Examine the passed line buffer and
 * return true if it is all blanks and tabs.
 */

blankline(linebuf)
	char linebuf[];
{
	register char *cp;

	for (cp = linebuf; *cp; cp++)
		if (!any(*cp, " \t"))
			return(0);
	return(1);
}

/*
 * Fetch the sender's name from the passed message.
 */

char *
nameof(mp)
	register struct message *mp;
{
	static char namebuf[NAMESIZE];
	char linebuf[LINESIZE];
	register char *cp, *cp2;
	register FILE *ibuf;

	ibuf = setinput(mp);
	copy("", namebuf);
	if (readline(ibuf, linebuf) <= 0)
		return(namebuf);
	for (cp = linebuf; *cp != ' '; cp++)
		;
	while (any(*cp, " \t"))
		cp++;
	for (cp2 = namebuf; *cp && !any(*cp, " \t") &&
	    cp2-namebuf < NAMESIZE-1; *cp2++ = *cp++)
		;
	*cp2 = '\0';
	return(namebuf);
}
