/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Guido van Rossum.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)glob.c	5.8 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

/*
 * Glob: the interface is a superset of the one defined in POSIX 1003.2,
 * draft 9.
 *
 * The [!...] convention to negate a range is supported (SysV, Posix, ksh).
 *
 * Optional extra services, controlled by flags not defined by POSIX:
 *
 * GLOB_QUOTE:
 *	Escaping convention: \ inhibits any special meaning the following
 *	character might have (except \ at end of string is retained).
 * GLOB_MAGCHAR:
 *	Set in gl_flags if pattern contained a globbing character.
 * gl_matchc:
 *	Number of matches in the current invocation of glob.
 */

#include <sys/cdefs.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <dirent.h>
#include <glob.h>
#include <ctype.h>
#include <errno.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

static int glob1(), glob2(), glob3(), globextend(), match();

#define	DOLLAR		'$'
#define	DOT		'.'
#define	EOS		'\0'
#define	LBRACKET	'['
#define	NOT		'!'
#define	QUESTION	'?'
#define	QUOTE		'\\'
#define	RANGE		'-'
#define	RBRACKET	']'
#define	SEP		'/'
#define	STAR		'*'
#define	TILDE		'~'
#define	UNDERSCORE	'_'

#define	SHORT_STRINGS
#ifdef SHORT_STRINGS
typedef u_short shortchar_t;
#define	METABIT		0x8000
#define	M_MASK		0xffff
#else
typedef char shortchar_t;
#define	METABIT		0x80
#define	M_MASK		0xff
#endif

#define	META(c)		((c)|METABIT)
#define	M_ALL		META('*')
#define	M_END		META(']')
#define	M_NOT		META('!')
#define	M_ONE		META('?')
#define	M_RNG		META('-')
#define	M_SET		META('[')
#define	ismeta(c)	(((c)&METABIT) != 0)

#ifdef SHORT_STRINGS
static DIR *
Opendir(str)
register shortchar_t *str;
{
	register char *dc;
	char buf[MAXPATHLEN];

	if (!*str)
		return(opendir("."));
	for (dc = buf; *dc++ = *str++;);
	return(opendir(buf));
}

static int
Lstat(fn, sb)
register shortchar_t *fn;
struct stat *sb;
{
	register char *dc;
	char buf[MAXPATHLEN];

	for (dc = buf; *dc++ = *fn++;);
	return(lstat(buf, sb));
}

static int
Stat(fn, sb)
register shortchar_t *fn;
struct stat *sb;
{
	register char *dc;
	char buf[MAXPATHLEN];

	for (dc = buf; *dc++ = *fn++;);
	return(stat(buf, sb));
}
#else
#define	Opendir	opendir
#define	Stat	stat
#define	Lstat	lstat
#endif

static int
compare(p, q)
	void **p, **q;
{
	return(strcmp(*(char **)p, *(char **)q));
}

/*
 * The main glob() routine: compiles the pattern (optionally processing
 * quotes), calls glob1() to do the real pattern matching, and finally
 * sorts the list (unless unsorted operation is requested).  Returns 0
 * if things went well, nonzero if errors occurred.  It is not an error
 * to find no matches.
 */
glob(pattern, flags, errfunc, pglob)
	const char *pattern;
	int flags;
	int (*errfunc) __P((char *, int));
	glob_t *pglob;
{
	int err, oldpathc;
	shortchar_t *bufnext, *bufend, *compilebuf;
	const char *compilepat, *patnext;
	char c;
	shortchar_t patbuf[MAXPATHLEN+1];

	patnext = pattern;
	if (!(flags & GLOB_APPEND)) {
		pglob->gl_pathc = 0;
		pglob->gl_pathv = NULL;
		if (!(flags & GLOB_DOOFFS))
			pglob->gl_offs = 0;
	}
	pglob->gl_flags = flags & ~GLOB_MAGCHAR;
	pglob->gl_errfunc = errfunc;
	oldpathc = pglob->gl_pathc;
	pglob->gl_matchc = 0;

	bufnext = patbuf;
	bufend = bufnext+MAXPATHLEN;

	compilebuf = bufnext;
	compilepat = patnext;
	while (bufnext < bufend && (c = *patnext++) != EOS) {
		switch (c) {
		case LBRACKET:
			pglob->gl_flags |= GLOB_MAGCHAR;
			c = *patnext;
			if (c == NOT)
				++patnext;
			if (*patnext == EOS ||
			    strchr(patnext+1, RBRACKET) == NULL) {
				*bufnext++ = LBRACKET;
				if (c == NOT)
					--patnext;
				break;
			}
			*bufnext++ = M_SET;
			if (c == NOT)
				*bufnext++ = M_NOT;
			c = *patnext++;
			do {
				/* todo: quoting */
				*bufnext++ = c;
				if (*patnext == RANGE &&
				    (c = patnext[1]) != RBRACKET) {
					*bufnext++ = M_RNG;
					*bufnext++ = c;
					patnext += 2;
				}
			} while ((c = *patnext++) != RBRACKET);
			*bufnext++ = M_END;
			break;
		case QUESTION:
			pglob->gl_flags |= GLOB_MAGCHAR;
			*bufnext++ = M_ONE;
			break;
		case QUOTE:
			if (!(flags & GLOB_QUOTE))
				*bufnext++ = QUOTE;
			else {
				if ((c = *patnext++) == EOS) {
					c = QUOTE;
					--patnext;
				}
				*bufnext++ = c;
			}
			break;
		case STAR:
			pglob->gl_flags |= GLOB_MAGCHAR;
			*bufnext++ = M_ALL;
			break;
		default:
			*bufnext++ = c;
			break;
		}
	}
	*bufnext = EOS;

	if ((err = glob1(patbuf, pglob)) != 0)
		return(err);

	if (pglob->gl_pathc == oldpathc && flags & GLOB_NOCHECK) {
		if (!(flags & GLOB_QUOTE)) {
			shortchar_t *dp = compilebuf;
			const char *sp = compilepat;
			while (*dp++ = (u_char)*sp++);
		}
		else {
			/*
			 * copy pattern, interpreting quotes; this is slightly
			 * different than the interpretation of quotes above
			 * -- which should prevail?
			 */
			while (*compilepat != EOS) {
				if (*compilepat == QUOTE) {
					if (*++compilepat == EOS)
						--compilepat;
				}
				*compilebuf++ = (u_char)*compilepat++;
			}
			*compilebuf = EOS;
		}
		return(globextend(patbuf, pglob));
	} else if (!(flags & GLOB_NOSORT)) 
		qsort((char*)(pglob->gl_pathv + pglob->gl_offs + oldpathc),
		    pglob->gl_pathc - oldpathc, sizeof(char*), compare);
	return(0);
}

static
glob1(pattern, pglob)
	shortchar_t *pattern;
	glob_t *pglob;
{
	shortchar_t pathbuf[MAXPATHLEN+1];

	/*
	 * a null pathname is invalid -- POSIX 1003.1 sect. 2.4. 
	 */
	if (*pattern == EOS)
		return(0);
	return(glob2(pathbuf, pathbuf, pattern, pglob));
}

/*
 * functions glob2 and glob3 are mutually recursive; there is one level
 * of recursion for each segment in the pattern that contains one or
 * more meta characters.
 */
static
glob2(pathbuf, pathend, pattern, pglob)
	shortchar_t *pathbuf, *pathend, *pattern;
	glob_t *pglob;
{
	struct stat sbuf;
	shortchar_t *p, *q;
	int anymeta;

	/*
	 * loop over pattern segments until end of pattern or until
	 * segment with meta character found.
	 */
	for (anymeta = 0;;) {
		if (*pattern == EOS) {		/* end of pattern? */
			*pathend = EOS;
			if (Lstat(pathbuf, &sbuf))
				return(0);
		
			if (((pglob->gl_flags & GLOB_MARK) &&
			    pathend[-1] != SEP) && (S_ISDIR(sbuf.st_mode)
			    || (S_ISLNK(sbuf.st_mode) &&
			    (Stat(pathbuf, &sbuf) == 0) &&
			    S_ISDIR(sbuf.st_mode)))) {
				*pathend++ = SEP;
				*pathend = EOS;
			}
			++pglob->gl_matchc;
			return(globextend(pathbuf, pglob));
		}

		/* find end of next segment, copy tentatively to pathend */
		q = pathend;
		p = pattern;
		while (*p != EOS && *p != SEP) {
			if (ismeta(*p))
				anymeta = 1;
			*q++ = *p++;
		}

		if (!anymeta) {		/* no expansion, do next segment */
			pathend = q;
			pattern = p;
			while (*pattern == SEP)
				*pathend++ = *pattern++;
		} else			/* need expansion, recurse */
			return(glob3(pathbuf, pathend, pattern, p, pglob));
	}
	/* NOTREACHED */
}


static
glob3(pathbuf, pathend, pattern, restpattern, pglob)
	shortchar_t *pathbuf, *pathend, *pattern, *restpattern;
	glob_t *pglob;
{
	extern int errno;
	DIR *dirp;
	struct dirent *dp;
	int len, err;

	*pathend = EOS;
	errno = 0;
	    
	if (!(dirp = Opendir(pathbuf)))
		/* todo: don't call for ENOENT or ENOTDIR? */
		if (pglob->gl_errfunc &&
		    (*pglob->gl_errfunc)(pathbuf, errno) ||
		    (pglob->gl_flags & GLOB_ERR))
			return(GLOB_ABEND);
		else
			return(0);

	err = 0;

	/* search directory for matching names */
	while ((dp = readdir(dirp))) {
		register char *sc;
		register shortchar_t *dc;
		/* initial DOT must be matched literally */
		if (dp->d_name[0] == DOT && *pattern != DOT)
			continue;
		for (sc = dp->d_name, dc = pathend; 
		     *dc++ = (u_char)*sc++;);
		if (!match(pathend, pattern, restpattern)) {
			*pathend = EOS;
			continue;
		}
		err = glob2(pathbuf, --dc, restpattern, pglob);
		if (err)
			break;
	}
	/* todo: check error from readdir? */
	(void)closedir(dirp);
	return(err);
}


/*
 * Extend the gl_pathv member of a glob_t structure to accomodate a new item,
 * add the new item, and update gl_pathc.
 *
 * This assumes the BSD realloc, which only copies the block when its size
 * crosses a power-of-two boundary; for v7 realloc, this would cause quadratic
 * behavior.
 *
 * Return 0 if new item added, error code if memory couldn't be allocated.
 *
 * Invariant of the glob_t structure:
 *	Either gl_pathc is zero and gl_pathv is NULL; or gl_pathc > 0 and
 *	 gl_pathv points to (gl_offs + gl_pathc + 1) items.
 */
static int
globextend(path, pglob)
	shortchar_t *path;
	glob_t *pglob;
{
	register char **pathv;
	register int i;
	u_int newsize;
	char *copy;
	shortchar_t *p;

	newsize = sizeof(*pathv) * (2 + pglob->gl_pathc + pglob->gl_offs);
	pathv = (char **)realloc((char *)pglob->gl_pathv, newsize);
	if (pathv == NULL)
		return(GLOB_NOSPACE);

	if (pglob->gl_pathv == NULL && pglob->gl_offs > 0) {
		/* first time around -- clear initial gl_offs items */
		pathv += pglob->gl_offs;
		for (i = pglob->gl_offs; --i >= 0; )
			*--pathv = NULL;
	}
	pglob->gl_pathv = pathv;

	for (p = path; *p++;);
	if ((copy = malloc(p - path)) != NULL) {
		register char *dc = copy;
		register shortchar_t *sc = path;
		while (*dc++ = *sc++);
		pathv[pglob->gl_offs + pglob->gl_pathc++] = copy;
	}
	pathv[pglob->gl_offs + pglob->gl_pathc] = NULL;
	return((copy == NULL) ? GLOB_NOSPACE : 0);
}


/*
 * pattern matching function for filenames.  Each occurrence of the *
 * pattern causes a recursion level.
 */
static
match(name, pat, patend)
	register shortchar_t *name, *pat, *patend;
{
	int ok, negate_range;
	shortchar_t c, k;

	while (pat < patend) {
		c = *pat++;
		switch (c & M_MASK) {
		case M_ALL:
			if (pat == patend)
				return(1);
			for (; *name != EOS; ++name) {
				if (match(name, pat, patend))
					return(1);
			}
			return(0);
		case M_ONE:
			if (*name++ == EOS)
				return(0);
			break;
		case M_SET:
			ok = 0;
			k = *name++;
			if (negate_range = ((*pat & M_MASK) == M_NOT))
				++pat;
			while (((c = *pat++) & M_MASK) != M_END) {
				if ((*pat & M_MASK) == M_RNG) {
					if (c <= k && k <= pat[1])
						ok = 1;
					pat += 2;
				}
				else if (c == k)
					ok = 1;
			}
			if (ok == negate_range)
				return(0);
			break;
		default:
			if (*name++ != c)
				return(0);
			break;
		}
	}
	return(*name == EOS);
}

/* free allocated data belonging to a glob_t structure */
void
globfree(pglob)
	glob_t *pglob;
{
	register int i;
	register char **pp;

	if (pglob->gl_pathv != NULL) {
		pp = pglob->gl_pathv + pglob->gl_offs;
		for (i = pglob->gl_pathc; i--; ++pp)
			if (*pp)
				free((void *)*pp);
		free((void *)pglob->gl_pathv);
	}
}
