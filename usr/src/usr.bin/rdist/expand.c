#ifndef lint
static	char *sccsid = "@(#)expand.c	4.9 (Berkeley) 83/12/19";
#endif

#include "defs.h"

#define LC '{'
#define RC '}'

static char	shchars[] = "${[*?";

static int	which;		/* bit mask of types to expand */
static int	argc;		/* expanded arg count */
static char	**argv;		/* expanded arg vectors */
static char	*path;
static char	*pathp;
static char	*lastpathp;
static char	*tilde;		/* "~user" if not expanding tilde, else "" */
static char	*tpathp;
static int	nleft;

static int	expany;		/* any expansions done? */
static char	*entp;
static char	**sortbase;

char	*index();
struct block *copy();

/*
 * Take a list of names and expand any macros, etc.
 * wh = E_VARS if expanding variables.
 * wh = E_SHELL if expanding shell characters.
 * wh = E_TILDE if expanding `~'.
 * or any of these or'ed together.
 */
struct block *
expand(list, wh)
	struct block *list;
	int wh;
{
	register struct block *bp, *prev;
	register int n;
	char pathbuf[BUFSIZ];
	char *argvbuf[GAVSIZ];

	if (debug) {
		printf("expand(%x, %d)\nlist = ", list, wh);
		prnames(list);
	}

	if (wh == 0)
		return(list);

	which = wh;
	path = tpathp = pathp = pathbuf;
	*pathp = '\0';
	lastpathp = &path[sizeof pathbuf - 2];
	tilde = "";
	argc = 0;
	argv = sortbase = argvbuf;
	*argv = 0;
	nleft = NCARGS - 4;
	/*
	 * Walk the block list and expand names into argv[];
	 */
	for (bp = list; bp != NULL; bp = bp->b_next) {
		expstr(bp->b_name);
		free(bp->b_name);
		free(bp);
	}
	/*
	 * Take expanded list of names from argv[] and build a block list.
	 */
	list = prev = NULL;
	for (n = 0; n < argc; n++) {
		bp = ALLOC(block);
		if (bp == NULL)
			fatal("ran out of memory\n");
		bp->b_type = NAME;
		bp->b_next = bp->b_args = NULL;
		bp->b_name = argv[n];
		if (prev == NULL)
			list = prev = bp;
		else {
			prev->b_next = bp;
			prev = bp;
		}
	}
	if (debug) {
		printf("expanded list = ");
		prnames(list);
	}
	return(list);
}

expstr(s)
	char *s;
{
	register char *cp, *cp1;
	register struct block *tp;
	char *tail;
	char buf[BUFSIZ];
	int savec, oargc;
	extern char homedir[];

	if (s == NULL || *s == '\0')
		return;

	if ((which & E_VARS) && (cp = index(s, '$')) != NULL) {
		*cp++ = '\0';
		if (*cp == '\0') {
			error("no variable name after '$'\n");
			return;
		}
		if (*cp == LC) {
			cp++;
			if ((tail = index(cp, RC)) == NULL) {
				error("unmatched %c\n", *cp);
				return;
			}
			*tail++ = savec = '\0';
			if (*cp == '\0') {
				error("no variable name after '$'\n");
				return;
			}
		} else {
			tail = cp + 1;
			savec = *tail;
			*tail = '\0';
		}
		tp = lookup(cp, NULL, 0);
		if (savec != '\0')
			*tail = savec;
		if ((tp = tp->b_args) != NULL) {
			for (; tp != NULL; tp = tp->b_next) {
				sprintf(buf, "%s%s%s", s, tp->b_name, tail);
				expstr(buf);
			}
			return;
		}
		sprintf(buf, "%s%s", s, tail);
		expstr(buf);
		return;
	}
	if ((which & ~E_VARS) == 0 || !strcmp(s, "{") || !strcmp(s, "{}")) {
		Cat(s, "");
		sort();
		return;
	}
	if (*s == '~') {
		cp = ++s;
		if (*cp == '\0' || *cp == '/') {
			tilde = "~";
			cp1 = homedir;
		} else {
			tilde = cp1 = buf;
			*cp1++ = '~';
			do
				*cp1++ = *cp++;
			while (*cp && *cp != '/');
			*cp1 = '\0';
			if (pw == NULL || strcmp(pw->pw_name, buf+1) != 0) {
				if ((pw = getpwnam(buf+1)) == NULL) {
					error("unknown user %s\n", buf+1);
					return;
				}
			}
			cp1 = pw->pw_dir;
			s = cp;
		}
		for (cp = path; *cp++ = *cp1++; )
			;
		tpathp = pathp = cp - 1;
	} else {
		tpathp = pathp = path;
		tilde = "";
	}
	*pathp = '\0';
	if (!(which & E_SHELL)) {
		if (which & E_TILDE)
			Cat(path, s);
		else
			Cat(tilde, s);
		sort();
		return;
	}
	oargc = argc;
	expany = 0;
	expsh(s);
	if (argc != oargc)
		sort();
}

/*
 * Bubble sort any new entries
 */
sort()
{
	register char **p1, **p2, *c;
	char **ap = &argv[argc];

	p1 = sortbase;
	while (p1 < ap-1) {
		p2 = p1;
		while (++p2 < ap)
			if (strcmp(*p1, *p2) > 0)
				c = *p1, *p1 = *p2, *p2 = c;
		p1++;
	}
	sortbase = ap;
}

/*
 * If there are any Shell meta characters in the name,
 * expand into a list, after searching directory
 */
expsh(s)
	char *s;
{
	register char *cp;
	register char *spathp, *oldcp;
	struct stat stb;

	spathp = pathp;
	cp = s;
	while (!any(*cp, shchars)) {
		if (*cp == '\0') {
			if (!expany || stat(path, &stb) >= 0) {
				if (which & E_TILDE)
					Cat(path, "");
				else
					Cat(tilde, tpathp);
			}
			goto endit;
		}
		addpath(*cp++);
	}
	oldcp = cp;
	while (cp > s && *cp != '/')
		cp--, pathp--;
	if (*cp == '/')
		cp++, pathp++;
	*pathp = '\0';
	if (*oldcp == '{') {
		execbrc(cp, NULL);
		return;
	}
	matchdir(cp);
endit:
	pathp = spathp;
	*pathp = '\0';
}

matchdir(pattern)
	char *pattern;
{
	struct stat stb;
	register struct direct *dp;
	DIR *dirp;

	dirp = opendir(path);
	if (dirp == NULL) {
		if (expany)
			return;
		goto patherr2;
	}
	if (fstat(dirp->dd_fd, &stb) < 0)
		goto patherr1;
	if (!ISDIR(stb.st_mode)) {
		errno = ENOTDIR;
		goto patherr1;
	}
	while ((dp = readdir(dirp)) != NULL)
		if (match(dp->d_name, pattern)) {
			if (which & E_TILDE)
				Cat(path, dp->d_name);
			else {
				strcpy(pathp, dp->d_name);
				Cat(tilde, tpathp);
				*pathp = '\0';
			}
		}
	closedir(dirp);
	return;

patherr1:
	closedir(dirp);
patherr2:
	error("%s: %s\n", path, sys_errlist[errno]);
}

execbrc(p, s)
	char *p, *s;
{
	char restbuf[BUFSIZ + 2];
	register char *pe, *pm, *pl;
	int brclev = 0;
	char *lm, savec, *spathp;

	for (lm = restbuf; *p != '{'; *lm++ = *p++)
		continue;
	for (pe = ++p; *pe; pe++)
		switch (*pe) {

		case '{':
			brclev++;
			continue;

		case '}':
			if (brclev == 0)
				goto pend;
			brclev--;
			continue;

		case '[':
			for (pe++; *pe && *pe != ']'; pe++)
				continue;
			if (!*pe)
				error("Missing ]\n");
			continue;
		}
pend:
	if (brclev || !*pe)
		fatal("Missing }\n");
	for (pl = pm = p; pm <= pe; pm++)
		switch (*pm & (QUOTE|TRIM)) {

		case '{':
			brclev++;
			continue;

		case '}':
			if (brclev) {
				brclev--;
				continue;
			}
			goto doit;

		case ',':
			if (brclev)
				continue;
doit:
			savec = *pm;
			*pm = 0;
			strcpy(lm, pl);
			strcat(restbuf, pe + 1);
			*pm = savec;
			if (s == 0) {
				spathp = pathp;
				expsh(restbuf);
				pathp = spathp;
				*pathp = 0;
			} else if (amatch(s, restbuf))
				return (1);
			sort();
			pl = pm + 1;
			continue;

		case '[':
			for (pm++; *pm && *pm != ']'; pm++)
				continue;
			if (!*pm)
				error("Missing ]\n");
			continue;
		}
	return (0);
}

match(s, p)
	char *s, *p;
{
	register int c;
	register char *sentp;
	char sexpany = expany;

	if (*s == '.' && *p != '.')
		return (0);
	sentp = entp;
	entp = s;
	c = amatch(s, p);
	entp = sentp;
	expany = sexpany;
	return (c);
}

amatch(s, p)
	register char *s, *p;
{
	register int scc;
	int ok, lc;
	char *spathp;
	struct stat stb;
	int c, cc;

	expany = 1;
	for (;;) {
		scc = *s++ & TRIM;
		switch (c = *p++) {

		case '{':
			return (execbrc(p - 1, s - 1));

		case '[':
			ok = 0;
			lc = 077777;
			while (cc = *p++) {
				if (cc == ']') {
					if (ok)
						break;
					return (0);
				}
				if (cc == '-') {
					if (lc <= scc && scc <= *p++)
						ok++;
				} else
					if (scc == (lc = cc))
						ok++;
			}
			if (cc == 0)
				fatal("Missing ]\n");
			continue;

		case '*':
			if (!*p)
				return (1);
			if (*p == '/') {
				p++;
				goto slash;
			}
			for (s--; *s; s++)
				if (amatch(s, p))
					return (1);
			return (0);

		case '\0':
			return (scc == '\0');

		default:
			if (c != scc)
				return (0);
			continue;

		case '?':
			if (scc == '\0')
				return (0);
			continue;

		case '/':
			if (scc)
				return (0);
slash:
			s = entp;
			spathp = pathp;
			while (*s)
				addpath(*s++);
			addpath('/');
			if (stat(path, &stb) == 0 && ISDIR(stb.st_mode))
				if (*p == '\0') {
					if (which & E_TILDE)
						Cat(path, "");
					else
						Cat(tilde, tpathp);
				} else
					expsh(p);
			pathp = spathp;
			*pathp = '\0';
			return (0);
		}
	}
}

smatch(s, p)
	register char *s, *p;
{
	register int scc;
	int ok, lc;
	int c, cc;

	for (;;) {
		scc = *s++ & TRIM;
		switch (c = *p++) {

		case '[':
			ok = 0;
			lc = 077777;
			while (cc = *p++) {
				if (cc == ']') {
					if (ok)
						break;
					return (0);
				}
				if (cc == '-') {
					if (lc <= scc && scc <= *p++)
						ok++;
				} else
					if (scc == (lc = cc))
						ok++;
			}
			if (cc == 0)
				fatal("Missing ]\n");
			continue;

		case '*':
			if (!*p)
				return (1);
			for (s--; *s; s++)
				if (smatch(s, p))
					return (1);
			return (0);

		case '\0':
			return (scc == '\0');

		default:
			if ((c & TRIM) != scc)
				return (0);
			continue;

		case '?':
			if (scc == 0)
				return (0);
			continue;

		}
	}
}

Cat(s1, s2)
	register char *s1, *s2;
{
	int len = strlen(s1) + strlen(s2) + 1;
	register char *s;

	nleft -= len;
	if (nleft <= 0 || ++argc >= GAVSIZ)
		error("Arguments too long\n");
	argv[argc] = 0;
	argv[argc - 1] = s = malloc(len);
	if (s == NULL)
		fatal("ran out of memory\n");
	while (*s++ = *s1++ & TRIM)
		;
	s--;
	while (*s++ = *s2++ & TRIM)
		;
}

addpath(c)
	char c;
{

	if (pathp >= lastpathp)
		fatal("Pathname too long\n");
	*pathp++ = c;
	*pathp = '\0';
}

/*
 * Expand file names beginning with `~' into the
 * user's home directory path name. Return a pointer in buf to the
 * part corresponding to `file'.
 */
char *
exptilde(buf, file)
	char buf[];
	register char *file;
{
	register char *s1, *s2, *s3;
	extern char homedir[];

	if (*file != '~') {
		strcpy(buf, file);
		return(buf);
	}
	if (*++file == '\0') {
		s2 = homedir;
		s3 = NULL;
	} else if (*file == '/') {
		s2 = homedir;
		s3 = file;
	} else {
		s3 = file;
		while (*s3 && *s3 != '/')
			s3++;
		if (*s3 == '/')
			*s3 = '\0';
		else
			s3 = NULL;
		if (pw == NULL || strcmp(pw->pw_name, file) != 0) {
			if ((pw = getpwnam(file)) == NULL) {
				error("unknown user %s\n", file);
				if (s3 != NULL)
					*s3 = '/';
				return(NULL);
			}
		}
		if (s3 != NULL)
			*s3 = '/';
		s2 = pw->pw_dir;
	}
	for (s1 = buf; *s1++ = *s2++; )
		;
	s2 = --s1;
	if (s3 != NULL) {
		s2++;
		while (*s1++ = *s3++)
			;
	}
	return(s2);
}
