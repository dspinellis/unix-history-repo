/*-
 * Copyright (c) 1992 Diomidis Spinellis.
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Diomidis Spinellis of Imperial College, University of London.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)process.c	5.4 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/ioctl.h>
#include <sys/uio.h>

#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <regex.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "defs.h"
#include "extern.h"

typedef struct {
	char *space;		/* Current space pointer. */
	size_t len;		/* Current length. */
	int deleted;		/* If deleted. */
	char *back;		/* Backing memory. */
	size_t blen;		/* Backing memory length. */
} SPACE;
static SPACE HS, PS, SS;
#define	pd		PS.deleted
#define	ps		PS.space
#define	psl		PS.len
#define	hs		HS.space
#define	hsl		HS.len

static inline int	 applies __P((struct s_command *));
static void		 cspace __P((SPACE *, char *, size_t, int));
static void		 flush_appends __P((void));
static void		 lputs __P((char *));
static inline int	 regexec_e __P((regex_t *, const char *,
			    size_t, regmatch_t [], int));
static void		 regsub __P((regmatch_t *, char *, char *, SPACE *));
static int		 substitute __P((struct s_command *));

struct s_appends *appends;	/* Array of pointers to strings to append. */
static int appendx;		/* Index into appends array. */
int appendnum;			/* Size of appends array. */

static int lastaddr;		/* Set by applies if last address of a range. */
static int sdone;		/* If any substitutes since last line input. */
				/* Iov structure for 'w' commands. */
static struct iovec iov[2] = { NULL, 0, "\n", 1 };

static regex_t *defpreg;
static size_t defnmatch;

void
process()
{
	struct s_command *cp;
	SPACE tspace;
	size_t len;
	char oldc, *p;

	for (linenum = 0; ps = mf_fgets(&psl);) {
		pd = 0;
		cp = prog;
redirect:
		while (cp != NULL) {
			if (!applies(cp)) {
				cp = cp->next;
				continue;
			}
			switch (cp->code) {
			case '{':
				cp = cp->u.c;
				goto redirect;
			case 'a':
				if (appendx >= appendnum)
					appends = xrealloc(appends,
					    sizeof(struct s_appends) *
					    (appendnum *= 2));
				appends[appendx].type = AP_STRING;
				appends[appendx].s = cp->t;
				appendx++;
				break;
			case 'b':
				cp = cp->u.c;
				goto redirect;
			case 'c':
				pd = 1;
				psl = 0;
				if (cp->a2 == NULL || lastaddr)
					(void)printf("%s", cp->t);
				break;
			case 'd':
				if (pd)
					goto new;
				pd = 1;
				goto new;
			case 'D':
				if (pd)
					goto new;
				if ((p = strchr(ps, '\n')) == NULL)
					pd = 1;
				else {
					psl -= (p - ps) - 1;
					memmove(ps, p + 1, psl);
				}
				goto new;
			case 'g':
				ps = hs;
				psl = hsl;
				break;
			case 'G':
				cspace(&PS, hs, hsl, 1);
				break;
			case 'h':
				cspace(&HS, ps, psl, 0);
				break;
			case 'H':
				cspace(&HS, ps, psl, 1);
				break;
			case 'i':
				(void)printf("%s", cp->t);
				break;
			case 'l':
				lputs(ps);
				break;
			case 'n':
				if (!nflag && !pd)
					(void)printf("%s\n", ps);
				flush_appends();
				ps = mf_fgets(&psl);
#ifdef HISTORIC_PRACTICE
				if (ps == NULL)
					exit(0);
#endif
				pd = 0;
				break;
			case 'N':
				flush_appends();
				if (ps != PS.back)
					cspace(&PS, NULL, 0, 0);
				if ((p = mf_fgets(&len)) == NULL) {
					if (!nflag && !pd)
						(void)printf("%s\n", ps);
					exit(0);
				}
				cspace(&PS, p, len, 1);
				break;
			case 'p':
				if (pd)
					break;
				(void)printf("%s\n", ps);
				break;
			case 'P':
				if (pd)
					break;
				if ((p = strchr(ps, '\n')) != NULL) {
					oldc = *p;
					*p = '\0';
				}
				(void)printf("%s\n", ps);
				if (p != NULL)
					*p = oldc;
				break;
			case 'q':
				if (!nflag && !pd)
					(void)printf("%s\n", ps);
				flush_appends();
				exit(0);
			case 'r':
				if (appendx >= appendnum)
					appends = xrealloc(appends,
					    sizeof(struct s_appends) *
					    (appendnum *= 2));
				appends[appendx].type = AP_FILE;
				appends[appendx].s = cp->t;
				appendx++;
				break;
			case 's':
				sdone = substitute(cp);
				break;
			case 't':
				if (sdone) {
					sdone = 0;
					cp = cp->u.c;
					goto redirect;
				}
				break;
			case 'w':
				if (pd)
					break;
				if (cp->u.fd == -1 && (cp->u.fd = open(cp->t,
				    O_WRONLY|O_APPEND|O_CREAT|O_TRUNC,
				    DEFFILEMODE)) == -1)
					err(FATAL, "%s: %s\n",
					    cp->t, strerror(errno));
				iov[0].iov_base = ps;
				iov[0].iov_len = psl;
				if (writev(cp->u.fd, iov, 2) != psl + 1)
					err(FATAL, "%s: %s\n",
					    cp->t, strerror(errno));
				break;
			case 'x':
				tspace = PS;
				PS = HS;
				HS = tspace;
				break;
			case 'y':
				if (pd)
					break;
				for (p = ps, len = psl; len--; ++p)
					*p = cp->u.y[*p];
				break;
			case ':':
			case '}':
				break;
			case '=':
				(void)printf("%lu\n", linenum);
			}
			cp = cp->next;
		} /* for all cp */

new:		if (!nflag && !pd)
			(void)printf("%s\n", ps);
		flush_appends();
	} /* for all lines */
}

/*
 * TRUE if the address passed matches the current program state
 * (lastline, linenumber, ps).
 */
#define	MATCH(a)							\
	(a)->type == AT_RE ?						\
	    regexec_e((a)->u.r, ps, 0, NULL, 0) :			\
	    (a)->type == AT_LINE ? linenum == (a)->u.l : lastline

/*
 * Return TRUE if the command applies to the current line.  Sets the inrange
 * flag to process ranges.  Interprets the non-select (``!'') flag.
 */
static inline int
applies(cp)
	struct s_command *cp;
{
	int r;

	lastaddr = 0;
	if (cp->a1 == NULL && cp->a2 == NULL)
		r = 1;
	else if (cp->a2)
		if (cp->inrange) {
			if (MATCH(cp->a2)) {
				cp->inrange = 0;
				lastaddr = 1;
			}
			r = 1;
		} else if (MATCH(cp->a1)) {
			/*
			 * If the second address is a number less than or
			 * equal to the line number first selected, only
			 * one line shall be selected.
			 *	-- POSIX 1003.2
			 */
			if (cp->a2->type == AT_LINE &&
			    linenum >= cp->a2->u.l)
				lastaddr = 1;
			else
				cp->inrange = 1;
			r = 1;
		} else
			r = 0;
	else
		r = MATCH(cp->a1);
	return (cp->nonsel ? ! r : r);
}

/*
 * substitute --
 *	Do substitutions in the pattern space.  Currently, we build a
 *	copy of the new pattern space in the substitute space structure
 *	and then swap them.
 */
static int
substitute(cp)
	struct s_command *cp;
{
	SPACE tspace;
	regex_t *re;
	size_t nsub;
	int n, re_off;
	char *endp, *s;

	s = ps;
	re = cp->u.s->re;
	if (re == NULL) {
		nsub = 1;
		if (defpreg != NULL && cp->u.s->maxbref > defnmatch) {
			linenum = cp->u.s->linenum;
			err(COMPILE, "\\%d not defined in the RE",
			    cp->u.s->maxbref);
		}
	} else
		nsub = re->re_nsub + 1;
	if (!regexec_e(re, s, nsub, cp->u.s->pmatch, 0))
		return (0);

	SS.len = 0;				/* Clean substitute space. */
	n = cp->u.s->n;
	switch (n) {
	case 0:					/* Global */
		do {
			/* Locate start of replaced string. */
			re_off = cp->u.s->pmatch[0].rm_so;
			/* Locate end of replaced string + 1. */
			endp = s + cp->u.s->pmatch[0].rm_eo;
			/* Copy leading retained string. */
			cspace(&SS, s, re_off, 0);
			/* Add in regular expression. */
			regsub(cp->u.s->pmatch, s, cp->u.s->new, &SS);
			/* Move past this match. */
			s += cp->u.s->pmatch[0].rm_eo;
		} while(regexec_e(re, s, nsub, cp->u.s->pmatch, REG_NOTBOL));
		/* Copy trailing retained string. */
		cspace(&SS, s, strlen(s), 0);
		break;
	default:				/* Nth occurrence */
		while (--n) {
			s += cp->u.s->pmatch[0].rm_eo;
			if (!regexec_e(re,
			    s, nsub, cp->u.s->pmatch, REG_NOTBOL))
				return (0);
		}
		/* FALLTHROUGH */
	case 1:					/* 1st occurrence */
		/* Locate start of replaced string. */
		re_off = cp->u.s->pmatch[0].rm_so + s - ps;
		/* Copy leading retained string. */
		cspace(&SS, ps, re_off, 0);
		/* Add in regular expression. */
		regsub(cp->u.s->pmatch, s, cp->u.s->new, &SS);
		/* Copy trailing retained string. */
		s += cp->u.s->pmatch[0].rm_eo;
		cspace(&SS, s, strlen(s), 0);
		break;
	}

	/*
	 * Swap the substitute space and the pattern space, and make sure
	 * that any leftover pointers into stdio memory get lost.
	 */
	tspace = PS;
	PS = SS;
	SS = tspace;
	SS.space = SS.back;

	/* Handle the 'p' flag. */
	if (cp->u.s->p)
		(void)printf("%s\n", ps);

	/* Handle the 'w' flag. */
	if (cp->u.s->wfile && !pd) {
		if (cp->u.s->wfd == -1 && (cp->u.s->wfd = open(cp->u.s->wfile,
		    O_WRONLY|O_APPEND|O_CREAT|O_TRUNC, DEFFILEMODE)) == -1)
			err(FATAL, "%s: %s\n", cp->u.s->wfile, strerror(errno));
		iov[0].iov_base = ps;
		iov[0].iov_len = psl;	
		if (writev(cp->u.s->wfd, iov, 2) != psl + 1)
			err(FATAL, "%s: %s\n", cp->u.s->wfile, strerror(errno));
	}
	return (1);
}

/*
 * Flush append requests.  Always called before reading a line,
 * therefore it also resets the substitution done (sdone) flag.
 */
static void
flush_appends()
{
	FILE *f;
	int count, i;
	char buf[8 * 1024];

	for (i = 0; i < appendx; i++) 
		switch (appends[i].type) {
		case AP_STRING:
			(void)printf("%s", appends[i].s);
			break;
		case AP_FILE:
			/*
			 * Read files probably shouldn't be cached.  Since
			 * it's not an error to read a non-existent file,
			 * it's possible that another program is interacting
			 * with the sed script through the file system.  It
			 * would be truly bizarre, but possible.  It's probably
			 * not that big a performance win, anyhow.
			 */
			if ((f = fopen(appends[i].s, "r")) == NULL)
				break;
			while (count = fread(buf, 1, sizeof(buf), f))
				(void)fwrite(buf, 1, count, stdout);
			(void)fclose(f);
			break;
		}
	if (ferror(stdout))
		err(FATAL, "stdout: %s", strerror(errno ? errno : EIO));
	appendx = 0;
	sdone = 0;
}

static void
lputs(s)
	register char *s;
{
	register int count;
	register char *escapes, *p;
	struct winsize win;
	static int termwidth = -1;

	if (termwidth == -1)
		if (p = getenv("COLUMNS"))
			termwidth = atoi(p);
		else if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &win) == 0 &&
		    win.ws_col > 0)
			termwidth = win.ws_col;
		else
			termwidth = 60;

	for (count = 0; *s; ++s) { 
		if (count >= termwidth) {
			(void)printf("\\\n");
			count = 0;
		}
		if (isascii(*s) && isprint(*s) && *s != '\\') {
			(void)putchar(*s);
			count++;
		} else {
			escapes = "\\\a\b\f\n\r\t\v";
			(void)putchar('\\');
			if (p = strchr(escapes, *s)) {
				(void)putchar("\\abfnrtv"[p - escapes]);
				count += 2;
			} else {
				(void)printf("%03o", (u_char)*s);
				count += 4;
			}
		}
	}
	(void)putchar('$');
	(void)putchar('\n');
	if (ferror(stdout))
		err(FATAL, "stdout: %s", strerror(errno ? errno : EIO));
}

static inline int
regexec_e(preg, string, nmatch, pmatch, eflags)
	regex_t *preg;
	const char *string;
	size_t nmatch;
	regmatch_t pmatch[];
	int eflags;
{
	int eval;

	if (preg == NULL) {
		if (defpreg == NULL)
			err(FATAL, "first RE may not be empty");
	} else {
		defpreg = preg;
		defnmatch = nmatch;
	}

	eval = regexec(defpreg,
	    string, pmatch == NULL ? 0 : defnmatch, pmatch, eflags);
	switch(eval) {
	case 0:
		return (1);
	case REG_NOMATCH:
		return (0);
	}
	err(FATAL, "RE error: %s", strregerror(eval, defpreg));
	/* NOTREACHED */
}

/*
 * regsub - perform substitutions after a regexp match
 * Based on a routine by Henry Spencer
 */
static void
regsub(pmatch, string, src, sp)
	regmatch_t *pmatch;
	char *string, *src;
	SPACE *sp;
{
	register int len, no;
	register char c, *dst;

#define	NEEDSP(reqlen)							\
	if (sp->len >= sp->blen - (reqlen) - 1) {			\
		sp->blen += (reqlen) + 1024;				\
		sp->space = sp->back = xrealloc(sp->back, sp->blen);	\
		dst = sp->space + sp->len;				\
	}

	dst = sp->space + sp->len;
	while ((c = *src++) != '\0') {
		if (c == '&')
			no = 0;
		else if (c == '\\' && isdigit(*src))
			no = *src++ - '0';
		else
			no = -1;
		if (no < 0) {		/* Ordinary character. */
 			if (c == '\\' && (*src == '\\' || *src == '&'))
 				c = *src++;
			NEEDSP(1);
 			*dst++ = c;
			++sp->len;
 		} else if (pmatch[no].rm_so != -1 && pmatch[no].rm_eo != -1) {
			len = pmatch[no].rm_eo - pmatch[no].rm_so;
			NEEDSP(len);
			memmove(dst, string + pmatch[no].rm_so, len);
			dst += len;
			sp->len += len;
		}
	}
	NEEDSP(1);
	*dst = '\0';
}

/*
 * aspace --
 *	Append the source space to the destination space, allocating new
 *	space as necessary.
 */
static void
cspace(sp, p, len, append)
	SPACE *sp;
	char *p;
	size_t len;
	int append;
{
	size_t tlen;
	int needcopy;

	/* Current pointer may point to something else at the moment. */
	needcopy = sp->space != sp->back;

	/*
	 * Make sure SPACE has enough memory and ramp up quickly.
	 * Add in two extra bytes, one for the newline, one for a
	 * terminating NULL.
	 */
	tlen = sp->len + len + 2;
	if (tlen > sp->blen) {
		sp->blen = tlen + 1024;
		sp->back = xrealloc(sp->back, sp->blen);
	}

	if (needcopy)
		memmove(sp->back, sp->space, sp->len + 1);
	sp->space = sp->back;

	/* May just be copying out of a stdio buffer. */
	if (len == NULL)
		return;

	/* Append a separating newline. */
	if (append)
		sp->space[sp->len++] = '\n';

	/* Append the new stuff, plus its terminating NULL. */
	memmove(sp->space + sp->len, p, len + 1);
	sp->len += len;
}

/*
 * Close all cached opened files and report any errors
 */
void
cfclose(cp)
	register struct s_command *cp;
{

	for (; cp != NULL; cp = cp->next)
		switch(cp->code) {
		case 's':
			if (cp->u.s->wfd != -1 && close(cp->u.s->wfd))
				err(FATAL,
				    "%s: %s", cp->u.s->wfile, strerror(errno));
			cp->u.s->wfd = -1;
			break;
		case 'w':
			if (cp->u.fd != -1 && close(cp->u.fd))
				err(FATAL, "%s: %s", cp->t, strerror(errno));
			cp->u.fd = -1;
			break;
		case '{':
			cfclose(cp->u.c);
			break;
		}
}
