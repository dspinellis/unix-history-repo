# include <stdio.h>
# include <pwd.h>
# include <sys/types.h>
# include <sys/stat.h>
# include <sysexits.h>
# include <errno.h>
# include <ctype.h>
# include "sendmail.h"
# include "conf.h"

SCCSID(@(#)util.c	3.40		%G%);

/*
**  STRIPQUOTES -- Strip quotes & quote bits from a string.
**
**	Runs through a string and strips off unquoted quote
**	characters and quote bits.  This is done in place.
**
**	Parameters:
**		s -- the string to strip.
**		qf -- if set, remove actual `` " '' characters
**			as well as the quote bits.
**
**	Returns:
**		none.
**
**	Side Effects:
**		none.
**
**	Called By:
**		deliver
*/

stripquotes(s, qf)
	char *s;
	bool qf;
{
	register char *p;
	register char *q;
	register char c;

	if (s == NULL)
		return;

	for (p = q = s; (c = *p++) != '\0'; )
	{
		if (c != '"' || !qf)
			*q++ = c & 0177;
	}
	*q = '\0';
}
/*
**  QSTRLEN -- give me the string length assuming 0200 bits add a char
**
**	Parameters:
**		s -- the string to measure.
**
**	Reurns:
**		The length of s, including space for backslash escapes.
**
**	Side Effects:
**		none.
*/

qstrlen(s)
	register char *s;
{
	register int l = 0;
	register char c;

	while ((c = *s++) != '\0')
	{
		if (bitset(0200, c))
			l++;
		l++;
	}
	return (l);
}
/*
**  CAPITALIZE -- return a copy of a string, properly capitalized.
**
**	Parameters:
**		s -- the string to capitalize.
**
**	Returns:
**		a pointer to a properly capitalized string.
**
**	Side Effects:
**		none.
*/

char *
capitalize(s)
	register char *s;
{
	static char buf[50];
	register char *p;

	p = buf;

	for (;;)
	{
		while (!isalpha(*s) && *s != '\0')
			*p++ = *s++;
		if (*s == '\0')
			break;
		*p++ = toupper(*s++);
		while (isalpha(*s))
			*p++ = *s++;
	}

	*p = '\0';
	return (buf);
}
/*
**  XALLOC -- Allocate memory and bitch wildly on failure.
**
**	THIS IS A CLUDGE.  This should be made to give a proper
**	error -- but after all, what can we do?
**
**	Parameters:
**		sz -- size of area to allocate.
**
**	Returns:
**		pointer to data region.
**
**	Side Effects:
**		Memory is allocated.
*/

char *
xalloc(sz)
	register int sz;
{
	register char *p;

	p = malloc(sz);
	if (p == NULL)
	{
		syserr("Out of memory!!");
		exit(EX_UNAVAILABLE);
	}
	return (p);
}
/*
**  COPYPLIST -- copy list of pointers.
**
**	This routine is the equivalent of newstr for lists of
**	pointers.
**
**	Parameters:
**		list -- list of pointers to copy.
**			Must be NULL terminated.
**		copycont -- if TRUE, copy the contents of the vector
**			(which must be a string) also.
**
**	Returns:
**		a copy of 'list'.
**
**	Side Effects:
**		none.
*/

char **
copyplist(list, copycont)
	char **list;
	bool copycont;
{
	register char **vp;
	register char **newvp;

	for (vp = list; *vp != NULL; vp++)
		continue;

	vp++;

	newvp = (char **) xalloc((vp - list) * sizeof *vp);
	bmove((char *) list, (char *) newvp, (vp - list) * sizeof *vp);

	if (copycont)
	{
		for (vp = newvp; *vp != NULL; vp++)
			*vp = newstr(*vp);
	}

	return (newvp);
}
/*
**  PRINTAV -- print argument vector.
**
**	Parameters:
**		av -- argument vector.
**
**	Returns:
**		none.
**
**	Side Effects:
**		prints av.
*/

# ifdef DEBUG
printav(av)
	register char **av;
{
	while (*av != NULL)
	{
		if (tTd(0, 44))
			printf("\n\t%08x=", *av);
		else
			putchar(' ');
		xputs(*av++);
	}
	putchar('\n');
}
# endif DEBUG
/*
**  LOWER -- turn letter into lower case.
**
**	Parameters:
**		c -- character to turn into lower case.
**
**	Returns:
**		c, in lower case.
**
**	Side Effects:
**		none.
*/

char
lower(c)
	register char c;
{
	if (isascii(c) && isupper(c))
		c = c - 'A' + 'a';
	return (c);
}
/*
**  XPUTS -- put string doing control escapes.
**
**	Parameters:
**		s -- string to put.
**
**	Returns:
**		none.
**
**	Side Effects:
**		output to stdout
*/

# ifdef DEBUG
xputs(s)
	register char *s;
{
	register char c;

	if (s == NULL)
	{
		printf("<null>");
		return;
	}
	putchar('"');
	while ((c = *s++) != '\0')
	{
		if (!isascii(c))
		{
			putchar('\\');
			c &= 0177;
		}
		if (iscntrl(c))
		{
			putchar('^');
			c |= 0100;
		}
		putchar(c);
	}
	putchar('"');
	(void) fflush(stdout);
}
# endif DEBUG
/*
**  MAKELOWER -- Translate a line into lower case
**
**	Parameters:
**		p -- the string to translate.  If NULL, return is
**			immediate.
**
**	Returns:
**		none.
**
**	Side Effects:
**		String pointed to by p is translated to lower case.
**
**	Called By:
**		parse
*/

makelower(p)
	register char *p;
{
	register char c;

	if (p == NULL)
		return;
	for (; (c = *p) != '\0'; p++)
		if (isascii(c) && isupper(c))
			*p = c - 'A' + 'a';
}
/*
**  SAMEWORD -- return TRUE if the words are the same
**
**	Ignores case.
**
**	Parameters:
**		a, b -- the words to compare.
**
**	Returns:
**		TRUE if a & b match exactly (modulo case)
**		FALSE otherwise.
**
**	Side Effects:
**		none.
*/

bool
sameword(a, b)
	register char *a, *b;
{
	while (lower(*a) == lower(*b))
	{
		if (*a == '\0')
			return (TRUE);
		a++;
		b++;
	}
	return (FALSE);
}
/*
**  CLEAR -- clear a block of memory
**
**	Parameters:
**		p -- location to clear.
**		l -- number of bytes to clear.
**
**	Returns:
**		none.
**
**	Side Effects:
**		none.
*/

clear(p, l)
	register char *p;
	register int l;
{
	while (l-- > 0)
		*p++ = 0;
}
/*
**  FULLNAME -- extract full name from a passwd file entry.
**
**	Parameters:
**		pw -- password entry to start from.
**		buf -- buffer to store result in.
**
**	Returns:
**		none.
**
**	Side Effects:
**		none.
*/

fullname(pw, buf)
	register struct passwd *pw;
	char *buf;
{
	register char *bp = buf;
	register char *p = pw->pw_gecos;

	if (*p == '*')
		p++;
	while (*p != '\0' && *p != ',' && *p != ';' && *p != '%')
	{
		if (*p == '&')
		{
			(void) strcpy(bp, pw->pw_name);
			*bp = toupper(*bp);
			while (*bp != '\0')
				bp++;
			p++;
		}
		else
			*bp++ = *p++;
	}
	*bp = '\0';
}
/*
**  SAFEFILE -- return true if a file exists and is safe for a user.
**
**	Parameters:
**		fn -- filename to check.
**		uid -- uid to compare against.
**		mode -- mode bits that must match.
**
**	Returns:
**		TRUE if fn exists, is owned by uid, and matches mode.
**		FALSE otherwise.
**
**	Side Effects:
**		none.
*/

bool
safefile(fn, uid, mode)
	char *fn;
	int uid;
	int mode;
{
	struct stat stbuf;

	if (stat(fn, &stbuf) >= 0 && stbuf.st_uid == uid &&
	    (stbuf.st_mode & mode) == mode)
		return (TRUE);
	return (FALSE);
}
/*
**  FIXCRLF -- fix <CR><LF> in line.
**
**	Looks for the <CR><LF> combination and turns it into the
**	UNIX canonical <NL> character.  It only takes one line,
**	i.e., it is assumed that the first <NL> found is the end
**	of the line.
**
**	Parameters:
**		line -- the line to fix.
**		stripnl -- if true, strip the newline also.
**
**	Returns:
**		none.
**
**	Side Effects:
**		line is changed in place.
*/

fixcrlf(line, stripnl)
	char *line;
	bool stripnl;
{
	register char *p;

	p = index(line, '\n');
	if (p == NULL)
		return;
	if (p[-1] == '\r')
		p--;
	if (!stripnl)
		*p++ = '\n';
	*p = '\0';
}
/*
**  SYSLOG -- fake entry to fool lint
*/

# ifdef LOG
# ifdef lint

/*VARARGS2*/
syslog(pri, fmt, args)
	int pri;
	char *fmt;
{
	pri = *fmt;
	args = pri;
	pri = args;
}

# endif lint
# endif LOG
/*
**  DFOPEN -- determined file open
**
**	This routine has the semantics of fopen, except that it will
**	keep trying a few times to make this happen.  The idea is that
**	on very loaded systems, we may run out of resources (inodes,
**	whatever), so this tries to get around it.
*/

FILE *
dfopen(filename, mode)
	char *filename;
	char *mode;
{
	register int tries;
	register FILE *fp;

	for (tries = 0; tries < 10; tries++)
	{
		sleep(10 * tries);
		errno = 0;
		fp = fopen(filename, mode);
		if (fp != NULL)
			break;
		if (errno != ENFILE && errno != EINTR)
			break;
	}
	return (fp);
}
/*
**  PUTLINE -- put a line like fputs obeying SMTP conventions
**
**	This routine always guarantees outputing a newline (or CRLF,
**	as appropriate) at the end of the string.
**
**	Parameters:
**		l -- line to put.
**		fp -- file to put it onto.
**		m -- the mailer used to control output.
**
**	Returns:
**		none
**
**	Side Effects:
**		output of l to fp.
*/

# define SMTPLINELIM	990	/* maximum line length */

putline(l, fp, m)
	register char *l;
	FILE *fp;
	MAILER *m;
{
	register char *p;
	char svchar;

	do
	{
		/* find the end of the line */
		p = index(l, '\n');
		if (p == NULL)
			p = &l[strlen(l)];

		/* check for line overflow */
		while (bitset(M_LIMITS, m->m_flags) && (p - l) > SMTPLINELIM)
		{
			register char *q = &l[SMTPLINELIM - 1];

			svchar = *q;
			*q = '\0';
			if (l[0] == '.' && bitset(M_XDOT, m->m_flags))
				fputc('.', fp);
			fputs(l, fp);
			fputc('!', fp);
			fputs(crlf(m), fp);
			*q = svchar;
			l = q;
		}

		/* output last part */
		svchar = *p;
		*p = '\0';
		if (l[0] == '.' && bitset(M_XDOT, m->m_flags))
			fputc('.', fp);
		fputs(l, fp);
		fputs(crlf(m), fp);
		*p = svchar;
		l = p;
		if (*l == '\n')
			l++;
	} while (l[0] != '\0');
}
/*
**  XUNLINK -- unlink a file, doing logging as appropriate.
**
**	Parameters:
**		f -- name of file to unlink.
**
**	Returns:
**		none.
**
**	Side Effects:
**		f is unlinked.
*/

xunlink(f)
	char *f;
{
	register int i;

# ifdef LOG
	if (LogLevel > 20)
		syslog(LOG_DEBUG, "%s: unlink %s\n", CurEnv->e_id, f);
# endif LOG

	i = unlink(f);
# ifdef LOG
	if (i < 0 && LogLevel > 21)
		syslog(LOG_DEBUG, "%s: unlink-fail %d", f, errno);
# endif LOG
}
/*
**  SFGETS -- "safe" fgets -- times out.
**
**	Parameters:
**		buf -- place to put the input line.
**		siz -- size of buf.
**		fp -- file to read from.
**
**	Returns:
**		NULL on error (including timeout).
**		buf otherwise.
**
**	Side Effects:
**		none.
*/

static bool	TimeoutFlag;

char *
sfgets(buf, siz, fp)
	char *buf;
	int siz;
	FILE *fp;
{
	register EVENT *ev = NULL;
	register char *p;
	extern readtimeout();

	if (ReadTimeout != 0)
		ev = setevent(ReadTimeout, readtimeout, 0);
	TimeoutFlag = FALSE;
	do
	{
		errno = 0;
		p = fgets(buf, siz, fp);
	} while (!(p != NULL || TimeoutFlag || errno != EINTR));
	clrevent(ev);
	LineNumber++;
	if (TimeoutFlag)
		syserr("sfgets: timeout on read (mailer may be hung)");
	return (p);
}

static
readtimeout()
{
	TimeoutFlag = TRUE;
}
/*
**  FGETFOLDED -- like fgets, but know about folded lines.
**
**	Parameters:
**		buf -- place to put result.
**		n -- bytes available.
**		f -- file to read from.
**
**	Returns:
**		buf on success, NULL on error or EOF.
**
**	Side Effects:
**		buf gets lines from f, with continuation lines (lines
**		with leading white space) appended.  CRLF's are mapped
**		into single newlines.  Any trailing NL is stripped.
*/

char *
fgetfolded(buf, n, f)
	char *buf;
	register int n;
	FILE *f;
{
	register char *p = buf;
	register int i;

	n--;
	while (fgets(p, n, f) != NULL)
	{
		LineNumber++;
		fixcrlf(p, TRUE);
		i = fgetc(f);
		if (i != EOF)
			ungetc(i, f);
		if (i != ' ' && i != '\t')
			return (buf);
		i = strlen(p);
		p += i;
		*p++ = '\n';
		n -= i + 1;
	}
	return (NULL);
}
/*
**  CURTIME -- return current time.
**
**	Parameters:
**		none.
**
**	Returns:
**		the current time.
**
**	Side Effects:
**		none.
*/

time_t
curtime()
{
	auto time_t t;

	(void) time(&t);
	return (t);
}
/*
**  ATOBOOL -- convert a string representation to boolean.
**
**	Defaults to "TRUE"
**
**	Parameters:
**		s -- string to convert.  Takes "tTyY" as true,
**			others as false.
**
**	Returns:
**		A boolean representation of the string.
**
**	Side Effects:
**		none.
*/

bool
atobool(s)
	register char *s;
{
	if (*s == '\0' || index("tTyY", *s) != NULL)
		return (TRUE);
	return (FALSE);
}
/*
**  ATOOCT -- convert a string representation to octal.
**
**	Parameters:
**		s -- string to convert.
**
**	Returns:
**		An integer representing the string interpreted as an
**		octal number.
**
**	Side Effects:
**		none.
*/

atooct(s)
	register char *s;
{
	register int i = 0;

	while (*s >= '0' && *s <= '7')
		i = (i << 3) | (*s++ - '0');
	return (i);
}
/*
**  WAITFOR -- wait for a particular process id.
**
**	Parameters:
**		pid -- process id to wait for.
**
**	Returns:
**		status of pid.
**		-1 if pid never shows up.
**
**	Side Effects:
**		none.
*/

waitfor(pid)
	int pid;
{
	auto int st;
	int i;

	do
	{
		errno = 0;
		i = wait(&st);
	} while ((i >= 0 || errno == EINTR) && i != pid);
	if (i < 0)
		st = -1;
	return (st);
}
/*
**  CLOSEALL -- close all extraneous file descriptors
**
**	Parameters:
**		none.
**
**	Returns:
**		none.
**
**	Side Effects:
**		Closes all file descriptors except zero, one, and two.
*/

closeall()
{
	int i;

	for (i = 3; i < 50; i++)
		(void) close(i);
}
