#

#include "rcv.h"
#include <sys/stat.h>
#include <sgtty.h>
#include <ctype.h>

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
 * pointer (or NOSTR if the desired header field is not available).
 */

char *
hfield(field, mp)
	char field[];
	struct message *mp;
{
	register FILE *ibuf;
	char linebuf[LINESIZE];
	register int lc;

	ibuf = setinput(mp);
	if ((lc = mp->m_lines) <= 0)
		return(NOSTR);
	if (readline(ibuf, linebuf) < 0)
		return(NOSTR);
	lc--;
	do {
		lc = gethfield(ibuf, linebuf, lc);
		if (lc == -1)
			return(NOSTR);
		if (ishfield(linebuf, field))
			return(savestr(hcontents(linebuf)));
	} while (lc > 0);
	return(NOSTR);
}

/*
 * Return the next header field found in the given message.
 * Return > 0 if something found, <= 0 elsewise.
 * Must deal with \ continuations & other such fraud.
 */

gethfield(f, linebuf, rem)
	register FILE *f;
	char linebuf[];
	register int rem;
{
	char line2[LINESIZE];
	register char *cp, *cp2;

	for (;;) {
		if (rem <= 0)
			return(-1);
		if (readline(f, linebuf) < 0)
			return(-1);
		rem--;
		if (strlen(linebuf) == 0)
			return(-1);
		if (isspace(linebuf[0]))
			return(-1);
		if (linebuf[0] == '>')
			continue;
		cp = index(linebuf, ':');
		if (cp == NOSTR)
			return(-1);
		for (cp2 = linebuf; cp2 < cp; cp2++)
			if (isdigit(*cp2))
				continue;
		
		/*
		 * I guess we got a headline.
		 * Handle \ wraparounding
		 */
		
		for (;;) {
			cp = &linebuf[strlen(linebuf) - 1];
			if (*cp != '\\')
				break;
			if (rem < 0)
				break;
			if (readline(f, line2) < 0)
				break;
			rem--;
			if (strlen(linebuf) + strlen(line2) >= LINESIZE-2)
				break;
			if (cp > linebuf)
				--cp;
			if (!isspace(*cp))
				*++cp = ' ';
			for (cp2 = line2; *cp2 != 0 && isspace(*cp2); cp2++)
				;
			strcpy(++cp, cp2);
		}
		return(rem);
	}
	/* NOTREACHED */
}

/*
 * Check whether the passed line is a header line of
 * the desired breed.
 */

ishfield(linebuf, field)
	char linebuf[], field[];
{
	register char *cp;
	register int c;

	if ((cp = index(linebuf, ':')) == NOSTR)
		return(0);
	if (cp == linebuf)
		return(0);
	cp--;
	while (cp > linebuf && isspace(*cp))
		cp--;
	c = *++cp;
	*cp = 0;
	if (icequal(linebuf ,field)) {
		*cp = c;
		return(1);
	}
	*cp = c;
	return(0);
}

/*
 * Extract the non label information from the given header field
 * and return it.
 */

char *
hcontents(hfield)
	char hfield[];
{
	register char *cp;

	if ((cp = index(hfield, ':')) == NOSTR)
		return(NOSTR);
	cp++;
	while (*cp && isspace(*cp))
		cp++;
	return(cp);
}

/*
 * Compare two strings, ignoring case.
 */

icequal(s1, s2)
	register char *s1, *s2;
{

	while (raise(*s1++) == raise(*s2))
		if (*s2++ == 0)
			return(1);
	return(0);
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
	int first = 1;

	if ((cp = hfield("from", mp)) != NOSTR) {
		strcpy(namebuf, cp);
		return(namebuf);
	}
	ibuf = setinput(mp);
	copy("", namebuf);
	if (readline(ibuf, linebuf) <= 0)
		return(namebuf);
newname:
	for (cp = linebuf; *cp != ' '; cp++)
		;
	while (any(*cp, " \t"))
		cp++;
	for (cp2 = &namebuf[strlen(namebuf)]; *cp && !any(*cp, " \t") &&
	    cp2-namebuf < NAMESIZE-1; *cp2++ = *cp++)
		;
	*cp2 = '\0';
	if (readline(ibuf, linebuf) <= 0)
		return(namebuf);
	if ((cp = index(linebuf, 'F')) == NULL)
		return(namebuf);
	if (strncmp(cp, "From", 4) != 0)
		return(namebuf);
	while ((cp = index(cp, 'r')) != NULL) {
		if (strncmp(cp, "remote", 6) == 0) {
			if ((cp = index(cp, 'f')) == NULL)
				break;
			if (strncmp(cp, "from", 4) != 0)
				break;
			if ((cp = index(cp, ' ')) == NULL)
				break;
			cp++;
			if (first) {
				copy(cp, namebuf);
				first = 0;
			} else
				strcpy(rindex(namebuf, '!')+1, cp);
			strcat(namebuf, "!");
			goto newname;
		}
		cp++;
	}
	return(namebuf);
}

/*
 * Find the rightmost pointer to an instance of the
 * character in the string and return it.
 */

char *
rindex(str, c)
	char str[];
	register int c;
{
	register char *cp, *cp2;

	for (cp = str, cp2 = NOSTR; *cp; cp++)
		if (c == *cp)
			cp2 = cp;
	return(cp2);
}

/*
 * See if the string is a number.
 */

numeric(str)
	char str[];
{
	register char *cp = str;

	while (*cp)
		if (!isdigit(*cp++))
			return(0);
	return(1);
}

/*
 * Are any of the characters in the two strings the same?
 */

anyof(s1, s2)
	register char *s1, *s2;
{
	register int c;

	while (c = *s1++)
		if (any(c, s2))
			return(1);
	return(0);
}

/*
 * Determine the leftmost index of the character
 * in the string.
 */

char *
index(str, ch)
	char *str;
{
	register char *cp;
	register int c;

	for (c = ch, cp = str; *cp; cp++)
		if (*cp == c)
			return(cp);
	return(NOSTR);
}

/*
 * String compare two strings of bounded length.
 */

strncmp(as1, as2, an)
	char *as1, *as2;
{
	register char *s1, *s2;
	register int n;

	s1 = as1;
	s2 = as2;
	n = an;
	while (--n >= 0 && *s1 == *s2++)
		if (*s1++ == '\0')
			return(0);
	return(n<0 ? 0 : *s1 - *--s2);
}

