/* $Header: /home/hyperion/mu/christos/src/sys/tcsh-6.01/RCS/sh.dir.c,v 3.8 1991/12/19 22:34:14 christos Exp $ */
/*
 * sh.dir.c: Directory manipulation functions
 */
/*-
 * Copyright (c) 1980, 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */
#include "sh.h"

RCSID("$Id: sh.dir.c,v 3.8 1991/12/19 22:34:14 christos Exp $")

/*
 * C Shell - directory management
 */

static	struct directory	*dfind		__P((Char *));
static	Char 			*dfollow	__P((Char *));
static	void 	 	 	 printdirs	__P((void));
static	Char 			*dgoto		__P((Char *));
static	void 	 	 	 dnewcwd	__P((struct directory *));
static	void 	 	 	 dset		__P((Char *));

struct directory dhead;		/* "head" of loop */
int     printd;			/* force name to be printed */

#ifdef CSHDIRS
int     bequiet = 0;		/* do not print dir stack -strike */

#endif
static int dirflag = 0;

/*
 * dinit - initialize current working directory
 */
void
dinit(hp)
    Char   *hp;
{
    register char *tcp;
    register Char *cp;
    register struct directory *dp;
    char    path[MAXPATHLEN];
    static char *emsg = "tcsh: Trying to start from \"%s\"\n";

    /* Don't believe the login shell home, because it may be a symlink */
    tcp = getwd(path);		/* see ngetwd.c for System V version */
    if (tcp == NULL || *tcp == '\0') {
	(void) xprintf("tcsh: %s\n", path);
	if (hp && *hp) {
	    tcp = short2str(hp);
	    (void) xprintf(emsg, tcp);
	    if (chdir(tcp) == -1)
		cp = NULL;
	    else
		cp = hp;
	}
	else
	    cp = NULL;
	if (cp == NULL) {
	    (void) xprintf(emsg, "/");
	    if (chdir("/") == -1)
		/* I am not even try to print an error message! */
		xexit(1);
	    cp = SAVE("/");
	}
    }
    else {
#ifdef S_IFLNK
	struct stat swd, shp;

	/*
	 * See if $HOME is the working directory we got and use that
	 */
	if (hp && *hp &&
	    stat(tcp, &swd) != -1 && stat(short2str(hp), &shp) != -1 &&
	    DEV_DEV_COMPARE(swd.st_dev, shp.st_dev)  &&
		swd.st_ino == shp.st_ino)
	    cp = hp;
	else {
	    char   *cwd;

	    /*
	     * use PWD if we have it (for subshells)
	     */
	    if (cwd = getenv("PWD")) {
		if (stat(cwd, &shp) != -1 && 
			DEV_DEV_COMPARE(swd.st_dev, shp.st_dev) &&
		    swd.st_ino == shp.st_ino)
		    tcp = cwd;
	    }
	    cp = dcanon(SAVE(tcp), STRNULL);
	}
#else				/* S_IFLNK */
	cp = dcanon(SAVE(tcp), STRNULL);
#endif				/* S_IFLNK */
    }

    dp = (struct directory *) xcalloc(sizeof(struct directory), 1);
    dp->di_name = Strsave(cp);
    dp->di_count = 0;
    dhead.di_next = dhead.di_prev = dp;
    dp->di_next = dp->di_prev = &dhead;
    printd = 0;
    dnewcwd(dp);
}

static void
dset(dp)
Char *dp;
{
    /*
     * Don't call set() directly cause if the directory contains ` or
     * other junk characters glob will fail. 
     */
    register Char **vec = (Char **) xmalloc((size_t) (2 * sizeof(Char **)));

    vec[0] = Strsave(dp);
    vec[1] = 0;
    setq(STRcwd, vec, &shvhed);
    Setenv(STRPWD, dp);
}

#define DIR_LONG 1
#define DIR_VERT 2
#define DIR_LINE 4

static void
skipargs(v, str)
    Char ***v;
    char   *str;
{
    Char  **n = *v, *s;

    dirflag = 0;
    for (n++; *n != NULL && (*n)[0] == '-'; n++)
	for (s = &((*n)[1]); *s; s++)
	    switch (*s) {
	    case 'l':
		dirflag |= DIR_LONG;
		break;
	    case 'v':
		dirflag |= DIR_VERT;
		break;
	    case 'n':
		dirflag |= DIR_LINE;
		break;
	    default:
		stderror(ERR_DIRUS, short2str(**v), str);
		break;
	    }
    *v = n;
}

/*
 * dodirs - list all directories in directory loop
 */
/*ARGSUSED*/
void
dodirs(v, c)
    Char  **v;
    struct command *c;
{
    skipargs(&v, "");

    if (*v != NULL)
	stderror(ERR_DIRUS, "dirs", "");
    printdirs();
}

static void
printdirs()
{
    register struct directory *dp;
    Char   *s, *hp = value(STRhome);
    int     idx, len, cur;
    extern int T_Cols;

    if (*hp == '\0')
	hp = NULL;
    dp = dcwd;
    idx = 0;
    cur = 0;
    do {
	if (dp == &dhead)
	    continue;
	if (dirflag & DIR_VERT) {
	    xprintf("%d\t", idx++);
	    cur = 0;
	}
	if (!(dirflag & DIR_LONG) && hp != NULL && !eq(hp, STRslash) &&
	    prefix(hp, dp->di_name))
	    len = Strlen(s = (dp->di_name + Strlen(hp))) + 2;
	else
	    len = Strlen(s = dp->di_name) + 1;

	cur += len;
	if ((dirflag & DIR_LINE) && cur >= T_Cols - 1 && len < T_Cols) {
	    xprintf("\n");
	    cur = len;
	}
	xprintf(s != dp->di_name ? "~%s%c" : "%s%c",
		short2str(s), (dirflag & DIR_VERT) ? '\n' : ' ');
    } while ((dp = dp->di_prev) != dcwd);
    if (!(dirflag & DIR_VERT))
	xprintf("\n");
}

void
dtildepr(home, dir)
    register Char *home, *dir;
{

    if (!eq(home, STRslash) && prefix(home, dir))
	xprintf("~%s", short2str(dir + Strlen(home)));
    else
	xprintf("%s", short2str(dir));
}

void
dtilde()
{
    struct directory *d = dcwd;

    do {
	if (d == &dhead)
	    continue;
	d->di_name = dcanon(d->di_name, STRNULL);
    } while ((d = d->di_prev) != dcwd);

    dset(dcwd->di_name);
}


/* dnormalize():
 *	If the name starts with . or .. then we might need to normalize
 *	it depending on the symbolic link flags
 */
Char   *
dnormalize(cp)
    Char   *cp;
{

#define TRM(a) ((a) & TRIM)
#define ISDOT(c) (TRM((c)[0]) == '.' && ((TRM((c)[1]) == '\0') || \
		  (TRM((c)[1]) == '/')))
#define ISDOTDOT(c) (TRM((c)[0]) == '.' && ISDOT(&((c)[1])))

    if (TRM(cp[0]) == '/')
	return (Strsave(cp));

#ifdef S_IFLNK
    if (adrof(STRignore_symlinks)) {
	int     dotdot = 0;
	Char   *dp, *cwd;
#ifdef apollo
	bool slashslash;
#endif

	cwd = (Char *) xmalloc((size_t) ((Strlen(dcwd->di_name) + 3) *
					 sizeof(Char)));
	(void) Strcpy(cwd, dcwd->di_name);
#ifdef apollo
	slashslash = cwd[0] == '/' && cwd[1] == '/';
#endif

	/*
	 * Ignore . and count ..'s
	 */
	while (*cp) {
	    if (ISDOT(cp)) {
		if (*++cp)
		    cp++;
	    }
	    else if (ISDOTDOT(cp)) {
		dotdot++;
		cp += 2;
		if (*cp)
		    cp++;
	    }
	    else
		break;
	}
	while (dotdot > 0) 
	    if ((dp = Strrchr(cwd, '/'))) {
#ifdef apollo
		if (dp == &cwd[1]) 
		    slashslash = 1;
#endif
		*dp = '\0';
		dotdot--;
	    }
	    else
		break;

	if (*cp) {
	    if ((TRM(cwd[(dotdot = Strlen(cwd)) - 1])) != '/')
		cwd[dotdot++] = '/';
	    cwd[dotdot] = '\0';
	    dp = Strspl(cwd, cp);
	    xfree((ptr_t) cwd);
	    return dp;
	}
	else {
	    if (!*cwd) {
		cwd[0] = '/';
#ifdef apollo
		cwd[1] = '/';
		cwd[2] = '\0';
#else
		cwd[1] = '\0';
#endif
	    }
#ifdef apollo
	    else if (slashslash && cwd[1] == '\0') {
		cwd[1] = '/';
		cwd[2] = '\0';
	    }
#endif
	    return cwd;
	}
    }
#endif
    return Strsave(cp);
}

/*
 * dochngd - implement chdir command.
 */
/*ARGSUSED*/
void
dochngd(v, c)
    Char  **v;
    struct command *c;
{
    register Char *cp;
    register struct directory *dp;

    skipargs(&v, " [<dir>]");
    printd = 0;
    if (*v == NULL) {
	if ((cp = value(STRhome)) == NULL || *cp == 0)
	    stderror(ERR_NAME | ERR_NOHOMEDIR);
	if (chdir(short2str(cp)) < 0)
	    stderror(ERR_NAME | ERR_CANTCHANGE);
	cp = Strsave(cp);
    }
    else if (v[1] != NULL) {
	stderror(ERR_NAME | ERR_TOOMANY);
	/* NOTREACHED */
	return;
    }
    else if ((dp = dfind(*v)) != 0) {
	char   *tmp;

	printd = 1;
	if (chdir(tmp = short2str(dp->di_name)) < 0)
	    stderror(ERR_SYSTEM, tmp, strerror(errno));
	dcwd->di_prev->di_next = dcwd->di_next;
	dcwd->di_next->di_prev = dcwd->di_prev;
	dfree(dcwd);
	dnewcwd(dp);
	return;
    }
    else
	if ((cp = dfollow(*v)) == NULL)
	    return;
    dp = (struct directory *) xcalloc(sizeof(struct directory), 1);
    dp->di_name = cp;
    dp->di_count = 0;
    dp->di_next = dcwd->di_next;
    dp->di_prev = dcwd->di_prev;
    dp->di_prev->di_next = dp;
    dp->di_next->di_prev = dp;
    dfree(dcwd);
    dnewcwd(dp);
}

static Char *
dgoto(cp)
    Char   *cp;
{
    Char   *dp;

    if (*cp != '/') {
	register Char *p, *q;
	int     cwdlen;

	for (p = dcwd->di_name; *p++;);
	if ((cwdlen = p - dcwd->di_name - 1) == 1)	/* root */
	    cwdlen = 0;
	for (p = cp; *p++;);
	dp = (Char *) xmalloc((size_t)((cwdlen + (p - cp) + 1) * sizeof(Char)));
	for (p = dp, q = dcwd->di_name; *p++ = *q++;);
	if (cwdlen)
	    p[-1] = '/';
	else
	    p--;		/* don't add a / after root */
	for (q = cp; *p++ = *q++;);
	xfree((ptr_t) cp);
	cp = dp;
	dp += cwdlen;
    }
    else
	dp = cp;

    cp = dcanon(cp, dp);
    return cp;
}

/*
 * dfollow - change to arg directory; fall back on cdpath if not valid
 */
static Char *
dfollow(cp)
    register Char *cp;
{
    register Char *dp;
    struct varent *c;
    char    ebuf[MAXPATHLEN];
    int serrno;

    cp = globone(cp, G_ERROR);
#ifdef apollo
    if (Strchr(cp, '`')) {
	char *dptr, *ptr;
	if (chdir(dptr = short2str(cp)) < 0) 
	    stderror(ERR_SYSTEM, dptr, strerror(errno));
	else if ((ptr = getwd(ebuf)) && *ptr != '\0') {
		xfree((ptr_t) cp);
		cp = Strsave(str2short(ptr));
		return dgoto(cp);
	}
	else 
	    stderror(ERR_SYSTEM, dptr, ebuf);
    }
#endif
	    
    /*
     * if we are ignoring symlinks, try to fix relatives now.
     */
    dp = dnormalize(cp);
    if (chdir(short2str(dp)) >= 0) {
	xfree((ptr_t) cp);
	return dgoto(dp);
    }
    else {
	xfree((ptr_t) dp);
	if (chdir(short2str(cp)) >= 0) 
	    return dgoto(cp);
	serrno = errno;
    }

    if (cp[0] != '/' && !prefix(STRdotsl, cp) && !prefix(STRdotdotsl, cp)
	&& (c = adrof(STRcdpath))) {
	Char  **cdp;
	register Char *p;
	Char    buf[MAXPATHLEN];

	for (cdp = c->vec; *cdp; cdp++) {
	    for (dp = buf, p = *cdp; *dp++ = *p++;);
	    dp[-1] = '/';
	    for (p = cp; *dp++ = *p++;);
	    if (chdir(short2str(buf)) >= 0) {
		printd = 1;
		xfree((ptr_t) cp);
		cp = Strsave(buf);
		return dgoto(cp);
	    }
	}
    }
    dp = value(cp);
    if ((dp[0] == '/' || dp[0] == '.') && chdir(short2str(dp)) >= 0) {
	xfree((ptr_t) cp);
	cp = Strsave(dp);
	printd = 1;
	return dgoto(cp);
    }
    (void) strcpy(ebuf, short2str(cp));
    xfree((ptr_t) cp);
#ifdef CSHDIRS
    /*
     * on login source of ~/.cshdirs, errors are eaten. the dir stack is all
     * directories we could get to.
     */
    if (!bequiet)
	stderror(ERR_SYSTEM, ebuf, strerror(serrno));
    else
	return (NULL);
#else
    stderror(ERR_SYSTEM, ebuf, strerror(serrno));
#endif
    /* NOTREACHED */
    return (NULL);
}


/*
 * dopushd - push new directory onto directory stack.
 *	with no arguments exchange top and second.
 *	with numeric argument (+n) bring it to top.
 */
/*ARGSUSED*/
void
dopushd(v, c)
    Char  **v;
    struct command *c;
{
    register struct directory *dp;
    register Char *cp;

    skipargs(&v, " [<dir>|+<n>]");
    printd = 1;
    if (*v == NULL) {
	if (adrof(STRpushdtohome)) {
	    if ((cp = value(STRhome)) == NULL || *cp == 0)
		stderror(ERR_NAME | ERR_NOHOMEDIR);
	    if (chdir(short2str(cp)) < 0)
		stderror(ERR_NAME | ERR_CANTCHANGE);
	    cp = Strsave(cp);	/* hmmm... PWP */
	    if ((cp = dfollow(cp)) == NULL)
		return;
	    dp = (struct directory *) xcalloc(sizeof(struct directory), 1);
	    dp->di_name = cp;
	    dp->di_count = 0;
	    dp->di_prev = dcwd;
	    dp->di_next = dcwd->di_next;
	    dcwd->di_next = dp;
	    dp->di_next->di_prev = dp;
	}
	else {
	    char   *tmp;

	    if ((dp = dcwd->di_prev) == &dhead)
		dp = dhead.di_prev;
	    if (dp == dcwd)
		stderror(ERR_NAME | ERR_NODIR);
	    if (chdir(tmp = short2str(dp->di_name)) < 0)
		stderror(ERR_SYSTEM, tmp, strerror(errno));
	    dp->di_prev->di_next = dp->di_next;
	    dp->di_next->di_prev = dp->di_prev;
	    dp->di_next = dcwd->di_next;
	    dp->di_prev = dcwd;
	    dcwd->di_next->di_prev = dp;
	    dcwd->di_next = dp;
	}
    }
    else if (v[1] != NULL) {
	stderror(ERR_NAME | ERR_TOOMANY);
	/* NOTREACHED */
	return;
    }
    else if (dp = dfind(*v)) {
	char   *tmp;

	if (chdir(tmp = short2str(dp->di_name)) < 0)
	    stderror(ERR_SYSTEM, tmp, strerror(errno));
	/*
	 * kfk - 10 Feb 1984 - added new "extraction style" pushd +n
	 */
	if (adrof(STRdextract))
	    dextract(dp);
    }
    else {
	register Char *ccp;

	if ((ccp = dfollow(*v)) == NULL)
	    return;
	dp = (struct directory *) xcalloc(sizeof(struct directory), 1);
	dp->di_name = ccp;
	dp->di_count = 0;
	dp->di_prev = dcwd;
	dp->di_next = dcwd->di_next;
	dcwd->di_next = dp;
	dp->di_next->di_prev = dp;
    }
    dnewcwd(dp);
}

/*
 * dfind - find a directory if specified by numeric (+n) argument
 */
static struct directory *
dfind(cp)
    register Char *cp;
{
    register struct directory *dp;
    register int i;
    register Char *ep;

    if (*cp++ != '+')
	return (0);
    for (ep = cp; Isdigit(*ep); ep++)
	continue;
    if (*ep)
	return (0);
    i = getn(cp);
    if (i <= 0)
	return (0);
    for (dp = dcwd; i != 0; i--) {
	if ((dp = dp->di_prev) == &dhead)
	    dp = dp->di_prev;
	if (dp == dcwd)
	    stderror(ERR_NAME | ERR_DEEP);
    }
    return (dp);
}

/*
 * dopopd - pop a directory out of the directory stack
 *	with a numeric argument just discard it.
 */
/*ARGSUSED*/
void
dopopd(v, c)
    Char  **v;
    struct command *c;
{
    register struct directory *dp, *p = NULL;

    skipargs(&v, " [+<n>]");
    printd = 1;
    if (*v == NULL)
	dp = dcwd;
    else if (v[1] != NULL) {
	stderror(ERR_NAME | ERR_TOOMANY);
	/* NOTREACHED */
	return;
    }
    else if ((dp = dfind(*v)) == 0)
	stderror(ERR_NAME | ERR_BADDIR);
    if (dp->di_prev == &dhead && dp->di_next == &dhead)
	stderror(ERR_NAME | ERR_EMPTY);
    if (dp == dcwd) {
	char   *tmp;

	if ((p = dp->di_prev) == &dhead)
	    p = dhead.di_prev;
	if (chdir(tmp = short2str(p->di_name)) < 0)
	    stderror(ERR_SYSTEM, tmp, strerror(errno));
    }
    dp->di_prev->di_next = dp->di_next;
    dp->di_next->di_prev = dp->di_prev;
    if (dp == dcwd)
	dnewcwd(p);
    else {
	printdirs();
    }
    dfree(dp);
}

/*
 * dfree - free the directory (or keep it if it still has ref count)
 */
void
dfree(dp)
    register struct directory *dp;
{

    if (dp->di_count != 0) {
	dp->di_next = dp->di_prev = 0;
    }
    else {
	xfree((ptr_t) dp->di_name);
	xfree((ptr_t) dp);
    }
}

/*
 * dcanon - canonicalize the pathname, removing excess ./ and ../ etc.
 *	we are of course assuming that the file system is standardly
 *	constructed (always have ..'s, directories have links)
 */
Char   *
dcanon(cp, p)
    register Char *cp, *p;
{
    register Char *sp;
    register Char *p1, *p2;	/* general purpose */
    bool    slash;
#ifdef apollo
    bool    slashslash;
#endif

#ifdef S_IFLNK			/* if we have symlinks */
    Char    link[MAXPATHLEN];
    char    tlink[MAXPATHLEN];
    int     cc;
    Char   *newcp;
#endif				/* S_IFLNK */

    /*
     * christos: if the path given does not start with a slash prepend cwd. If
     * cwd does not start with a slash or the result would be too long abort().
     */
    if (*cp != '/') {
	Char    tmpdir[MAXPATHLEN];

	p1 = value(STRcwd);
	if (p1 == NULL || *p1 != '/')
	    abort();
	if (Strlen(p1) + Strlen(cp) + 1 >= MAXPATHLEN)
	    abort();
	(void) Strcpy(tmpdir, p1);
	(void) Strcat(tmpdir, STRslash);
	(void) Strcat(tmpdir, cp);
	xfree((ptr_t) cp);
	cp = p = Strsave(tmpdir);
    }

#ifdef COMMENT
    if (*cp != '/')
	abort();
#endif

#ifdef apollo
    slashslash = (cp[0] == '/' && cp[1] == '/');
#endif

    while (*p) {		/* for each component */
	sp = p;			/* save slash address */
	while (*++p == '/')	/* flush extra slashes */
	    ;
	if (p != ++sp)
	    for (p1 = sp, p2 = p; *p1++ = *p2++;);
	p = sp;			/* save start of component */
	slash = 0;
	while (*++p)		/* find next slash or end of path */
	    if (*p == '/') {
		slash = 1;
		*p = 0;
		break;
	    }

#ifdef apollo
	if (&cp[1] == sp && sp[0] == '.' && sp[1] == '.' && sp[2] == '\0')
	    slashslash = 1;
#endif
	if (*sp == '\0')	/* if component is null */
	    if (--sp == cp)	/* if path is one char (i.e. /) */ 
		break;
	    else
		*sp = '\0';
	else if (sp[0] == '.' && sp[1] == 0) {
	    if (slash) {
		for (p1 = sp, p2 = p + 1; *p1++ = *p2++;);
		p = --sp;
	    }
	    else if (--sp != cp)
		*sp = '\0';
	}
	else if (sp[0] == '.' && sp[1] == '.' && sp[2] == 0) {
	    /*
	     * We have something like "yyy/xxx/..", where "yyy" can be null or
	     * a path starting at /, and "xxx" is a single component. Before
	     * compressing "xxx/..", we want to expand "yyy/xxx", if it is a
	     * symbolic link.
	     */
	    *--sp = 0;		/* form the pathname for readlink */
#ifdef S_IFLNK			/* if we have symlinks */
	    if (sp != cp && !adrof(STRignore_symlinks) &&
		(cc = readlink(short2str(cp), tlink,
			       sizeof tlink)) >= 0) {
		(void) Strcpy(link, str2short(tlink));
		link[cc] = '\0';

		if (slash)
		    *p = '/';
		/*
		 * Point p to the '/' in "/..", and restore the '/'.
		 */
		*(p = sp) = '/';
		/*
		 * find length of p
		 */
		for (p1 = p; *p1++;);
		if (*link != '/') {
		    /*
		     * Relative path, expand it between the "yyy/" and the
		     * "/..". First, back sp up to the character past "yyy/".
		     */
		    while (*--sp != '/');
		    sp++;
		    *sp = 0;
		    /*
		     * New length is "yyy/" + link + "/.." and rest
		     */
		    p1 = newcp = (Char *) xmalloc((size_t)
						(((sp - cp) + cc + (p1 - p)) *
						 sizeof(Char)));
		    /*
		     * Copy new path into newcp
		     */
		    for (p2 = cp; *p1++ = *p2++;);
		    for (p1--, p2 = link; *p1++ = *p2++;);
		    for (p1--, p2 = p; *p1++ = *p2++;);
		    /*
		     * Restart canonicalization at expanded "/xxx".
		     */
		    p = sp - cp - 1 + newcp;
		}
		else {
		    /*
		     * New length is link + "/.." and rest
		     */
		    p1 = newcp = (Char *) xmalloc((size_t)
					    ((cc + (p1 - p)) * sizeof(Char)));
		    /*
		     * Copy new path into newcp
		     */
		    for (p2 = link; *p1++ = *p2++;);
		    for (p1--, p2 = p; *p1++ = *p2++;);
		    /*
		     * Restart canonicalization at beginning
		     */
		    p = newcp;
		}
		xfree((ptr_t) cp);
		cp = newcp;
		continue;	/* canonicalize the link */
	    }
#endif				/* S_IFLNK */
	    *sp = '/';
	    if (sp != cp)
		while (*--sp != '/');
	    if (slash) {
		for (p1 = sp + 1, p2 = p + 1; *p1++ = *p2++;);
		p = sp;
	    }
	    else if (cp == sp)
		*++sp = '\0';
	    else
		*sp = '\0';
	}
	else {			/* normal dir name (not . or .. or nothing) */

#ifdef S_IFLNK			/* if we have symlinks */
	    if (sp != cp && adrof(STRchase_symlinks) &&
		!adrof(STRignore_symlinks) &&
		(cc = readlink(short2str(cp), tlink,
			       sizeof tlink)) >= 0) {
		(void) Strcpy(link, str2short(tlink));
		link[cc] = '\0';

		/*
		 * restore the '/'.
		 */
		if (slash)
		    *p = '/';

		/*
		 * point sp to p (rather than backing up).
		 */
		sp = p;

		/*
		 * find length of p
		 */
		for (p1 = p; *p1++;);
		if (*link != '/') {
		    /*
		     * Relative path, expand it between the "yyy/" and the
		     * remainder. First, back sp up to the character past
		     * "yyy/".
		     */
		    while (*--sp != '/');
		    sp++;
		    *sp = 0;
		    /*
		     * New length is "yyy/" + link + "/.." and rest
		     */
		    p1 = newcp = (Char *) xmalloc((size_t)
						  (((sp - cp) + cc + (p1 - p))
						   * sizeof(Char)));
		    /*
		     * Copy new path into newcp
		     */
		    for (p2 = cp; *p1++ = *p2++;);
		    for (p1--, p2 = link; *p1++ = *p2++;);
		    for (p1--, p2 = p; *p1++ = *p2++;);
		    /*
		     * Restart canonicalization at expanded "/xxx".
		     */
		    p = sp - cp - 1 + newcp;
		}
		else {
		    /*
		     * New length is link + the rest
		     */
		    p1 = newcp = (Char *) xmalloc((size_t)
					    ((cc + (p1 - p)) * sizeof(Char)));
		    /*
		     * Copy new path into newcp
		     */
		    for (p2 = link; *p1++ = *p2++;);
		    for (p1--, p2 = p; *p1++ = *p2++;);
		    /*
		     * Restart canonicalization at beginning
		     */
		    p = newcp;
		}
		xfree((ptr_t) cp);
		cp = newcp;
		continue;	/* canonicalize the link */
	    }
#endif				/* S_IFLNK */
	    if (slash)
		*p = '/';
	}
    }

    /*
     * fix home...
     */
#ifdef S_IFLNK
    p1 = value(STRhome);
    cc = Strlen(p1);
    /*
     * See if we're not in a subdir of STRhome
     */
    if (p1 && *p1 == '/' &&
	(Strncmp(p1, cp, cc) != 0 || (cp[cc] != '/' && cp[cc] != '\0'))) {
	static ino_t home_ino = -1;
	static dev_t home_dev = -1;
	static Char *home_ptr = NULL;
	struct stat statbuf;

	/*
	 * Get dev and ino of STRhome
	 */
	if (home_ptr != p1 &&
	    stat(short2str(p1), &statbuf) != -1) {
	    home_dev = statbuf.st_dev;
	    home_ino = statbuf.st_ino;
	    home_ptr = p1;
	}
	/*
	 * Start comparing dev & ino backwards
	 */
	p2 = Strcpy(link, cp);
	for (sp = NULL; *p2 && stat(short2str(p2), &statbuf) != -1;) {
	    if (DEV_DEV_COMPARE(statbuf.st_dev, home_dev) &&
			statbuf.st_ino == home_ino) {
			sp = (Char *) - 1;
			break;
	    }
	    if (sp = Strrchr(p2, '/'))
		*sp = '\0';
	}
	/*
	 * See if we found it
	 */
	if (*p2 && sp == (Char *) -1) {
	    /*
	     * Use STRhome to make '~' work
	     */
	    newcp = Strspl(p1, cp + Strlen(p2));
	    xfree((ptr_t) cp);
	    cp = newcp;
	}
    }
#endif				/* S_IFLNK */

#ifdef apollo
    if (slashslash) {
	if (cp[1] != '/') {
	    p = (Char *) xmalloc((size_t) (Strlen(cp) + 2) * sizeof(Char));
	    *p = '/';
	    (void) Strcpy(&p[1], cp);
	    xfree((ptr_t) cp);
	    cp = p;
	}
    }
    if (cp[1] == '/' && cp[2] == '/') 
	(void) Strcpy(&cp[1], &cp[2]);
#endif
    return cp;
}


/*
 * dnewcwd - make a new directory in the loop the current one
 */
static void
dnewcwd(dp)
    register struct directory *dp;
{
    dcwd = dp;
    dset(dcwd->di_name);
    if (printd && !(adrof(STRpushdsilent))	/* PWP: pushdsilent */
#ifdef CSHDIRS
	&& !bequiet		/* be quite while restoring stack -strike */
#endif
	)
	printdirs();
    cwd_cmd();			/* PWP: run the defined cwd command */
}

/*
 * getstakd - added by kfk 17 Jan 1984
 * Support routine for the stack hack.  Finds nth directory in
 * the directory stack, or finds last directory in stack.
 */
int
getstakd(s, cnt)
    Char   *s;
    int     cnt;
{
    struct directory *dp;

    dp = dcwd;
    if (cnt < 0) {		/* < 0 ==> last dir requested. */
	dp = dp->di_next;
	if (dp == &dhead)
	    dp = dp->di_next;
    }
    else {
	while (cnt-- > 0) {
	    dp = dp->di_prev;
	    if (dp == &dhead)
		dp = dp->di_prev;
	    if (dp == dcwd)
		return (0);
	}
    }
    (void) Strcpy(s, dp->di_name);
    return (1);
}

/*
 * Karl Kleinpaste - 10 Feb 1984
 * Added dextract(), which is used in pushd +n.
 * Instead of just rotating the entire stack around, dextract()
 * lets the user have the nth dir extracted from its current
 * position, and pushes it onto the top.
 */
void
dextract(dp)
    struct directory *dp;
{
    if (dp == dcwd)
	return;
    dp->di_next->di_prev = dp->di_prev;
    dp->di_prev->di_next = dp->di_next;
    dp->di_next = dcwd->di_next;
    dp->di_prev = dcwd;
    dp->di_next->di_prev = dp;
    dcwd->di_next = dp;
}

#ifdef CSHDIRS
/*
 * create a file called ~/.cshdirs which has a sequence
 * of pushd commands which will restore the dir stack to
 * its state before exit/logout. remember that the order
 * is reversed in the file because we are pushing.
 * -strike
 */
void
recdirs()
{
    int     fp, ftmp, oldidfds;
    int     cdflag = 0;
    extern int fast;
    Char    buf[BUFSIZE];

    if (!fast) {
	if (!adrof(STRsavedirs))/* does it exist */
	    return;
	(void) Strcpy(buf, value(STRhome));
	(void) Strcat(buf, STRsldtdirs);
	if ((fp = creat(short2str(buf), 0666)) == -1)
	    return;
	oldidfds = didfds;
	didfds = 0;
	ftmp = SHOUT;
	SHOUT = fp;
	{
	    extern struct directory dhead;
	    extern struct directory *dcwd;
	    struct directory *dp = dcwd->di_next;

	    do {
		if (dp == &dhead)
		    continue;
		if (cdflag == 0)
		    cdflag++, xprintf("cd %s\n",
				      short2str(dp->di_name));
		else
		    xprintf("pushd %s\n",
			    short2str(dp->di_name));
	    } while ((dp = dp->di_next) != dcwd->di_next);
	}
	xprintf("dirs\n");	/* show the dir stack */

	(void) close(fp);
	SHOUT = ftmp;
	didfds = oldidfds;
    }
}

#endif
