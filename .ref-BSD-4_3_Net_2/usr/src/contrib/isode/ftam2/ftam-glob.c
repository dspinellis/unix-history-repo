/* ftam-glob.c - interactive initiator FTAM -- globbing */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/ftam2/RCS/ftam-glob.c,v 7.2 91/02/22 09:23:43 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/ftam2/RCS/ftam-glob.c,v 7.2 91/02/22 09:23:43 mrose Interim $
 *
 *
 * $Log:	ftam-glob.c,v $
 * Revision 7.2  91/02/22  09:23:43  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/11/04  19:15:15  mrose
 * update
 * 
 * Revision 7.0  89/11/23  21:54:19  mrose
 * Release 6.0
 * 
 */

/*
 *				  NOTICE
 *
 *    Acquisition, use, and distribution of this module and related
 *    materials are subject to the restrictions of a license agreement.
 *    Consult the Preface in the User's Manual for the full terms of
 *    this agreement.
 *
 */


/*
 * C-shell glob for random programs.
 *
 * Modified for FTAM (and linted)
 *
 * Used by permission.
 */

#include <stdio.h>
#include <errno.h>
#include <pwd.h>
#include "ftamuser.h"


#define	QUOTE 0200
#define	TRIM 0177
#define	eq(a,b)		(strcmp(a, b)==0)
#define	GAVSIZ		(NCARGS/6)
#define	isdir(d)	((d.st_mode & S_IFMT) == S_IFDIR)

static	char **gargv;		/* Pointer to the (stack) arglist */
static	int    gargc;		/* Number args in gargv */
static	int    gnleft;
static	int    gflag;
static	int tglob();
char	**glob();
char	*globerr;
static char *home;
struct	passwd *getpwnam();
extern	int errno;
static	char *strspl(), **copyblk(), *strend();
char	*malloc(), *strcpy(), *strcat();

static	int globcnt;

static
char	*globchars = "`{[*?";

static	char *gpath, *gpathp, *lastgpathp;
static	int globbed;
static	char *entp;
static	char **sortbas;


int	chkldir (), chkrdir ();
static int (*chkdir) () = chkldir;

int	getldir (), getrdir ();
static int (*gethdir) () = getldir;

int	matchldir (), matchrdir ();
static int (*matchdir) () = matchldir;


static
char **
glob(v)
	register char *v;
{
	char agpath[BUFSIZ];
	char *agargv[GAVSIZ];
	char *vv[2];
	vv[0] = malloc ((unsigned) (strlen (v) + 1));
	if (vv[0] == (char *)0)
		fatal("out of memory");
	(void) strcpy (vv[0], v);
	v = vv[0];
	vv[1] = 0;
	gflag = 0;
	rscan(vv, tglob);
	if (gflag == 0)
		return (copyblk(vv));

	globerr = 0;
	gpath = agpath; gpathp = gpath; *gpathp = 0;
	lastgpathp = &gpath[sizeof agpath - 2];
	ginit(agargv); globcnt = 0;
	collect(v);
	free(v);
	if (globcnt == 0 && (gflag&1)) {
		blkfree(gargv), gargv = 0;
		return (0);
	} else
		return (gargv = copyblk(gargv));
}

static
ginit(agargv)
	char **agargv;
{

	agargv[0] = 0; gargv = agargv; sortbas = agargv; gargc = 0;
	gnleft = NCARGS - 4;
}

static
collect(as)
	register char *as;
{
	if (eq(as, "{") || eq(as, "{}")) {
		Gcat(as, "");
		sort();
	} else
		acollect(as);
}

static
acollect(as)
	register char *as;
{
	register int ogargc = gargc;

	gpathp = gpath; *gpathp = 0; globbed = 0;
	expand(as);
	if (gargc != ogargc)
		sort();
}

static
sort()
{
	register char **p1, **p2, *c;
	char **Gvp = &gargv[gargc];

	p1 = sortbas;
	while (p1 < Gvp-1) {
		p2 = p1;
		while (++p2 < Gvp)
			if (strcmp(*p1, *p2) > 0)
				c = *p1, *p1 = *p2, *p2 = c;
		p1++;
	}
	sortbas = Gvp;
}

static
expand(as)
	char *as;
{
	register char *cs,
		      *sgpathp,
		      *oldcs;
	char   *csstr;
	struct stat stb;

	sgpathp = gpathp;
	cs = csstr = strdup (as);
	if (*cs == '~' && gpathp == gpath) {
		addpath('~');
		for (cs++; letter(*cs) || digit(*cs) || *cs == '-';)
			addpath(*cs++);
		if (!*cs || *cs == '/') {
			if (gpathp != gpath + 1) {
				*gpathp = 0;
				if ((*gethdir) (gpath + 1))
					globerr = "Unknown user name after ~";
				(void) strcpy(gpath, gpath + 1);
			} else
				(void) strcpy(gpath, home);
			gpathp = strend(gpath);
		}
	}
	while (!any(*cs, globchars)) {
		if (*cs == 0) {
			if (!globbed)
				Gcat(gpath, "");
			else if (stat(gpath, &stb) >= 0) {
				Gcat(gpath, "");
				globcnt++;
			}
			goto endit;
		}
		addpath(*cs++);
	}
	oldcs = cs;
	while (cs > as && *cs != '/')
		cs--, gpathp--;
	if (*cs == '/')
		cs++, gpathp++;
	*gpathp = 0;
	if (*oldcs == '{') {
		(void) execbrc(cs, ((char *)0));
		return;
	}
	(*matchdir) (cs);
endit:
	gpathp = sgpathp;
	*gpathp = 0;

	if (csstr)
	    free (csstr);
}

static
matchldir(pattern)
	char *pattern;
{
	char	pat[MAXPATHLEN];
	struct stat stb;
	register struct dirent *dp;
	DIR *dirp;

	(void) strcpy (pat, pattern);

	dirp = opendir(*gpath ? gpath : ".");
	if (dirp == NULL) {
		if (globbed)
			return;
		goto patherr2;
	}
	if (fstat(dirp->dd_fd, &stb) < 0)
		goto patherr1;
	if (!isdir(stb)) {
		errno = ENOTDIR;
		goto patherr1;
	}
	for (errno = 0; dp = readdir(dirp); errno = 0) {
		if (dp->d_ino == 0)
			continue;
		if (match(dp->d_name, pat)) {
			Gcat(gpath, dp->d_name);
			globcnt++;
		}
	}
	if (errno)
	    globerr = "corrupted directory";
	(void) closedir(dirp);
	return;

patherr1:
	(void) closedir(dirp);
patherr2:
	globerr = "Bad directory components";
}

static
execbrc(p, s)
	char *p, *s;
{
	char restbuf[BUFSIZ + 2];
	register char *pe, *pm, *pl;
	int brclev = 0;
	char *lm, savec, *sgpathp;

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
		continue;
	}
pend:
	brclev = 0;
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

	case ','|QUOTE:
	case ',':
		if (brclev)
			continue;
doit:
		savec = *pm;
		*pm = 0;
		(void) strcpy(lm, pl);
		(void) strcat(restbuf, pe + 1);
		*pm = savec;
		if (s == 0) {
			sgpathp = gpathp;
			expand(restbuf);
			gpathp = sgpathp;
			*gpathp = 0;
		} else if (amatch(s, restbuf))
			return (1);
		sort();
		pl = pm + 1;
		if (brclev)
			return (0);
		continue;

	case '[':
		for (pm++; *pm && *pm != ']'; pm++)
			continue;
		if (!*pm)
			pm--;
		continue;
	}
	if (brclev)
		goto doit;
	return (0);
}

static
match(s, p)
	char *s, *p;
{
	register int c;
	register char *sentp;
	char sglobbed = globbed;

	if (*s == '.' && *p != '.')
		return (0);
	sentp = entp;
	entp = s;
	c = amatch(s, p);
	entp = sentp;
	globbed = sglobbed;
	return (c);
}

static
amatch(s, p)
	register char *s, *p;
{
	register int scc;
	int ok, lc;
	char *sgpathp;
	struct stat stb;
	int c, cc;

	globbed = 1;
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
				if (ok)
					p--;
				else
					return 0;
			continue;

		case '*':
			if (!*p)
				return (1);
			if (*p == '/') {
				p++;
				goto slash;
			}
			s--;
			do {
				if (amatch(s, p))
					return (1);
			} while (*s++);
			return (0);

		case 0:
			return (scc == 0);

		default:
			if (c != scc)
				return (0);
			continue;

		case '?':
			if (scc == 0)
				return (0);
			continue;

		case '/':
			if (scc)
				return (0);
slash:
			s = entp;
			sgpathp = gpathp;
			while (*s)
				addpath(*s++);
			addpath('/');
			if ((*chkdir) (gpath, &stb)) 
				if (*p == 0) {
					Gcat(gpath, "");
					globcnt++;
				} else
					expand(p);
			gpathp = sgpathp;
			*gpathp = 0;
			return (0);
		}
	}
}

static
chkldir (path, st)
char   *path;
struct stat *st;
{
    return (stat (path, st) == 0 && (st -> st_mode & S_IFMT) == S_IFDIR);
}

static
Gmatch(s, p)
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
				if (ok)
					p--;
				else
					return 0;
			continue;

		case '*':
			if (!*p)
				return (1);
			for (s--; *s; s++)
				if (Gmatch(s, p))
					return (1);
			return (0);

		case 0:
			return (scc == 0);

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

static
Gcat(s1, s2)
	register char *s1, *s2;
{
	register int len = strlen(s1) + strlen(s2) + 1;

	if (len >= gnleft || gargc >= GAVSIZ - 1)
		globerr = "Arguments too long";
	else {
		gargc++;
		gnleft -= len;
		gargv[gargc] = 0;
		gargv[gargc - 1] = strspl(s1, s2);
	}
}

static
addpath(c)
	char c;
{

	if (gpathp >= lastgpathp)
		globerr = "Pathname too long";
	else {
		*gpathp++ = c;
		*gpathp = 0;
	}
}

static
rscan(t, f)
	register char **t;
	int (*f)();
{
	register char *p, c;

	while (p = *t++) {
		if (f == tglob)
			if (*p == '~')
				gflag |= 2;
			else if (eq(p, "{") || eq(p, "{}"))
				continue;
		while (c = *p++)
			(*f)(c);
	}
}

#ifdef	notdef
static
scan(t, f)
	register char **t;
	int (*f)();
{
	register char *p, c;

	while (p = *t++)
		while (c = *p)
			*p++ = (*f)(c);
}
#endif

static
tglob(c)
	register char c;
{

	if (any(c, globchars))
		gflag |= c == '{' ? 2 : 1;
	return (c);
}

#ifdef	notdef
static
trim(c)
	char c;
{

	return (c & TRIM);
}
#endif

static
letter(c)
	register char c;
{

	return (c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' || c == '_');
}

static
digit(c)
	register char c;
{

	return (c >= '0' && c <= '9');
}

static
any(c, s)
	register int c;
	register char *s;
{

	while (*s)
		if (*s++ == c)
			return(1);
	return(0);
}
blklen(av)
	register char **av;
{
	register int i = 0;

	while (*av++)
		i++;
	return (i);
}

char **
blkcpy(oav, bv)
	char **oav;
	register char **bv;
{
	register char **av = oav;

	while (*av++ = *bv++)
		continue;
	return (oav);
}

blkfree(av0)
	char **av0;
{
	register char **av = av0;

	while (*av)
		free(*av++);
	free((char *)av0);
}

static
char *
strspl(cp, dp)
	register char *cp, *dp;
{
	register char *ep = malloc((unsigned)(strlen(cp) + strlen(dp) + 1));

	if (ep == (char *)0)
		fatal("out of memory");
	(void) strcpy(ep, cp);
	(void) strcat(ep, dp);
	return (ep);
}

static
char **
copyblk(v)
	register char **v;
{
	register char **nv = (char **)malloc((unsigned)((blklen(v) + 1) *
						sizeof(char **)));
	if (nv == (char **)0)
		fatal("out of memory");

	return (blkcpy(nv, v));
}

static
char *
strend(cp)
	register char *cp;
{

	while (*cp)
		cp++;
	return (cp);
}
/*
 * Extract a home directory from the password file
 * The argument points to a buffer where the name of the
 * user whose home directory is sought is currently.
 * We write the home directory of the user back there.
 */
static
getldir(hdir)
	char *hdir;
{
	register struct passwd *pp = getpwnam(hdir);

	if (pp == 0)
		return (1);
	(void) strcpy(hdir, pp->pw_dir);
	return (0);
}

/*  */

#undef	isdir


int	xglobbed;

static OID   matchoid;

/*  */

char   *xglob1val (v, remote)
char   *v;
int	remote;
{
    register char **gp;
    char   *cp,
           *gb[2];

    gb[0] = v;
    gb[1] = NULLCP;

    if ((gp = xglob (gb, remote)) == NULLVP)
	return NULLCP;

    if (gp[1]) {
	advise (NULLCP, "%s: ambiguous", v);
	blkfree (gp);
	return NULLCP;
    }

    cp = *gp;
    free ((char *) gp);

    return cp;
}

/*  */

char  **xglob (v, remote)
char  **v;
int	remote;
{
    register int    i;
    register char  *cp,
		  **gp,
                  **vp;
    char   *loses;

    xglobbed = 0;

    if (!globbing) {
	register char *dp;

	for (gp = vp = copyblk (v); *gp; gp++) {
	    cp = remote ? str2file (*gp) : *gp;
	    if ((dp = malloc ((unsigned) (strlen (cp) + 1))) == NULLCP)
		fatal ("out of memory");
	    (void) strcpy (dp, cp);
	    *gp = dp;
	}

	return vp;
    }

    if (remote) {
	switch (realstore) {
	    case RFS_UNKNOWN: 
		advise (NULLCP, "%s", rs_unknown);
		return NULLVP;

	    case RFS_UNIX: 
		home = "~";
		chkdir = chkrdir;
		gethdir = getrdir;
		matchdir = matchrdir;
		break;

	    default: 
		advise (NULLCP, "%s", rs_support);
		return NULLVP;
	}
    }
    else {
	home = myhome;
	chkdir = chkldir;
	gethdir = getldir;
	matchdir = matchldir;
    }

    for (i = 0, loses = NULL, vp = NULLVP; cp = *v++; ) {
	if ((gp = glob (remote ? str2file (cp) : cp)) == NULLVP) {
	    if (!loses && globerr)
		loses = globerr;
	    continue;
	}

	if (vp) {
	    register int    j;
	    register char **xp,
			  **yp;

	    if ((j = blklen (gp)) > 1)
		xglobbed++;

	    if ((vp = (char **) realloc ((char *) vp,
				    ((unsigned) (i + j + 1)) * sizeof *vp))
		    == NULLVP)
		fatal ("out of memory");

	    for (xp = vp + i, yp = gp; *xp = *yp; xp++, yp++)
		continue;
	    i += j;

	    free ((char *) gp);
	}
	else
	    if ((i = blklen (vp = gp)) > 1)
		xglobbed++;
    }

    if (vp == NULLVP || *vp == NULLCP) {
	if (!loses)
	    loses = "no files match specification";
	advise (NULLCP, "%s", loses);

	if (vp) {
	    blkfree (vp);
	    vp = NULLVP;
	}
    }

    if (vp && debug)
	for (gp = vp; *gp; gp++)
	    printf ("%d: \"%s\"\n", gp - vp, *gp);

    return vp;
}

/*  */

static	matchrdir (pattern)
char   *pattern;
{
    register char  *cp;
    char    cwd[MAXPATHLEN],
	    pat[MAXPATHLEN];
    struct FADUidentity faduids;
    register struct FADUidentity *faduid = &faduids;
    register struct filent *fi, *gi;
    
    (void) strcpy (pat, pattern);

    switch (isdir (gpath, cwd, 1)) {
	case NOTOK:
	case OK:
	    if (!globbed)
		globerr = "Bad directory components";
	    return;

	default:
	    if (cwd[0] == NULL)
		(void) strcpy (cwd, gpath);
	    cp = cwd + strlen (cwd) - 1;
	    if (*cp == '/')
		*cp = NULL;
	    else {
		*++cp = '/';
		*++cp = NULL;
	    }
	    cp = cwd;
	    break;
    }
    
    faduid -> fa_type = FA_FIRSTLAST;
    faduid -> fa_firstlast = FA_FIRST;

    (void) fdffnx (NOTOK, (struct PSAPdata *) 0, 1);
    (void) getvf (cp, NULLCP, faduid, &vfs[VFS_FDF], fdffnx);

    fi = gi = filents, filents = NULL;
    (void) fdffnx (NOTOK, (struct PSAPdata *) 0, 0);

    {
	register int len = strlen (cp);

	for (fi = gi; fi; fi = fi -> fi_next)
	    if (strncmp (fi -> fi_name, cp, len) == 0)
		fi -> fi_entry = fi -> fi_name + len;
    }

    for (fi = gi; fi; fi = fi -> fi_next) {
	matchoid = fi -> fi_oid;
	if (match (fi -> fi_entry, pat)) {
	    Gcat (gpath, fi -> fi_entry);
	    globcnt++;
	}
    }

    filents = gi;
    (void) fdffnx (NOTOK, (struct PSAPdata *) 0, 0);
}

/*  */

/* ARGSUSED */

static	chkrdir (path, st)
char   *path;
struct stat *st;
{
    return (oid_cmp (vfs[VFS_FDF].vf_oid, matchoid) == 0);
}

/*  */

static getrdir (hdir)
char    *hdir;
{
    char    buffer[BUFSIZ];

    (void) sprintf (buffer, "~%s", hdir);

    return (isdir (buffer, hdir, 1) != DONE);
}

/*  */

static int  fatal (s)
char   *s;
{
    adios (NULLCP, "%s", s);
}

/*  */

int	f_echo (vec)
char  **vec;
{
    char  **gb,
          **gp,
		   *gs;

    if (*++vec && (gp = gb = xglob (vec, 1))) {
	char   *cp;

	for (cp = ""; *gp; gp++, cp = " "){
		gs = rindex (*gp, '/');
		if (gs == NULL)
	    	printf ("%s%s", cp, *gp);
		else
	    	printf ("%s%s", cp, ++gs);
	}
	printf ("\n");

	blkfree (gb);
    }

    return OK;
}
