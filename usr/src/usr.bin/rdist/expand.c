#ifndef lint
static	char *sccsid = "@(#)expand.c	4.6 (Berkeley) 83/11/01";
#endif

#include "defs.h"

char	shchars[] = "{[*?";

int	argc;
char	**argv;
char	*path, *pathp, *lastpathp;
int	nleft;

int	argcnt;
int	expany;		/* any expansions done? */
char	*entp;
char	**sortbase;

char	*index();
struct block *copy();

/*
 * Take a list of names and expand any macros, etc.
 */
struct block *
expand(list, noshexp)
	struct block *list;
	int noshexp;
{
	register struct block *prev, *bp, *tp;
	register char *cp;
	register int n;
	char *tail;
	int c;
	char pathbuf[BUFSIZ];
	char *argvbuf[GAVSIZ];

	if (debug) {
		printf("expand(%x, %d)\nlist = ", list, noshexp);
		prnames(list);
	}

	for (prev = NULL, bp = list; bp != NULL; prev = bp, bp = bp->b_next) {
	again:
		cp = index(bp->b_name, '$');
		if (cp == NULL)
			continue;
		*cp++ = '\0';
		if (*cp == '\0')
			fatal("no variable name after '$'");
		if (*cp == '{') {
			cp++;
			if ((tail = index(cp, '}')) == NULL)
				fatal("missing '}'");
			*tail++ = c = '\0';
			if (*cp == '\0')
				fatal("no variable name after '$'");
		} else {
			tail = cp + 1;
			c = *tail;
			*tail = '\0';
		}
		tp = lookup(cp, NULL, 0);
		if (c != '\0')
			*tail = c;
		if ((tp = tp->b_args) != NULL) {
			struct block *first = bp;

			for (bp = prev; tp != NULL; tp = tp->b_next) {
				if (bp == NULL)
					list = bp = copy(tp, first->b_name, tail);
				else {
					bp->b_next = copy(tp, first->b_name, tail);
					bp = bp->b_next;
				}
			}
			bp->b_next = first->b_next;
			free(first->b_name);
			free(first);
			if (prev == NULL)
				bp = list;
			else
				bp = prev->b_next;
			goto again;
		} else {
			if (prev == NULL)
				list = tp = bp->b_next;
			else
				prev->b_next = tp = bp->b_next;
			free(bp->b_name);
			free(bp);
			if (tp != NULL) {
				bp = tp;
				goto again;
			}
			break;
		}
	}

	if (noshexp)
		return(list);

	if (debug) {
		printf("shexpand ");
		prnames(list);
	}

	path = pathp = pathbuf;
	*pathp = '\0';
	lastpathp = &path[sizeof pathbuf - 2];
	argc = 0;
	argv = sortbase = argvbuf;
	*argv = 0;
	nleft = NCARGS - 4;
	argcnt = 0;
	for (bp = list; bp != NULL; bp = bp->b_next)
		expsh(bp->b_name);
	for (bp = list; bp != NULL; bp = tp) {
		tp = bp->b_next;
		free(bp->b_name);
		free(bp);
	}
	prev = NULL;
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
	return(list);
}

/*
 * Return a new NAME block named "head, bp->b_name, tail"
 */
struct block *
copy(bp, head, tail)
	struct block *bp;
	char *head, *tail;
{
	register int n;
	register char *cp;
	register struct block *np;

	np = ALLOC(block);
	if (np == NULL)
		fatal("ran out of memory\n");
	np->b_type = NAME;
	np->b_next = bp->b_args = NULL;
	n = strlen(bp->b_name) + strlen(head) + strlen(tail) + 1;
	np->b_name = cp = malloc(n);
	if (cp == NULL)
		fatal("ran out of memory");
	sprintf(cp, "%s%s%s", head, bp->b_name, tail);
	return(np);
}

/*
 * If there are any Shell meta characters in the name,
 * expand into a list, after searching directory
 */
expsh(s)
	register char *s;
{
	register int oargc = argc;

	if (!strcmp(s, "{") || !strcmp(s, "{}")) {
		Cat(s, "");
		sort();
		return;
	}

	pathp = path;
	*pathp = 0;
	expany = 0;
	expstr(s);
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

expstr(s)
	char *s;
{
	register char *cp;
	register char *spathp, *oldcp;
	struct stat stb;

	spathp = pathp;
	cp = s;
	while (!any(*cp, shchars)) {
		if (*cp == '\0') {
			if (!expany)
				Cat(path, "");
			else if (stat(path, &stb) >= 0) {
				Cat(path, "");
				argcnt++;
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
			Cat(path, dp->d_name);
			argcnt++;
		}
	closedir(dirp);
	return;

patherr1:
	closedir(dirp);
patherr2:
	fatal("%s: %s\n", path, sys_errlist[errno]);
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
				fatal("Missing ]\n");
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
				expstr(restbuf);
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
				fatal("Missing ]\n");
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
					Cat(path, "");
					argcnt++;
				} else
					expstr(p);
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
		fatal("Arguments too long\n");
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
