#include "sh.h"

int gimme(), normal(); 
int chngd(), shift(), doset(), goodbye(), await(), dozip();
int rewind(), doalias(), unalias(), dotimes(), doalloc();
int dointerp(), uninterp(), noquit(), quit(), abort();

#define INF	127
#define	FRET	1

static struct biltins {
	char	*bname;
	int	(*bfunct)();
	char	minargs, maxargs;
	int	flags;
} bfunc[] {
	"chdir",	&chngd,		0,	1,	0,
	"wait",		&await,		0,	1,	0,
	"shift",	&shift,		0,	1,	0,
	"rewind",	&rewind,	0,	0,	0,
	"set",		&doset,		0,	INF,	0,
	"unset",	&unset,		1,	INF,	0,
	"alias",	&doalias,	0,	2,	0,
	"unalias",	&unalias,	1,	INF,	0,
	"interp",	&dointerp,	0,	2,	0,
	"uninterp",	&uninterp,	1,	INF,	0,
	"logout",	&goodbye,	0,	0,	0,
	"quit",		&quit,		0,	0,	0,
	"noquit",	&noquit,	0,	0,	0,
	"alloc",	&doalloc,	0,	0,	0,
	"abort",	&abort,		0,	0,	0,
	":",		&dozip,		0,	INF,	0,
	/* "verbose" */
	"tyme",		&dotimes,	0,	INF,	FRET,
	/* "nohup" */
	/* "repeat" */
	/* "if" */
	/* "goto" */
	/* "exit" */
	0,		0,		0,	0,	0
};
static char *bname;

func(t)
	register int *t;
{
	register struct biltins *bp;
	register int *tp;
	int i;

	bp = bfunc;
	for (;;) {
		if (bp->bname == 0)
			return(0);
		if (strcmp(bp->bname, t[DCOM]) == 0)
			break;
		bp++;
	}
	bname = bp->bname;
	i = 0;
	for (tp = &t[DCOM+1]; *tp; tp++)
		i++;
	if (i < bp->minargs) {
		bferr(": Too few arguments");
		return (1);
	}
	if (i > bp->maxargs) {
		bferr(": Too many arguments");
		return (1);
	}
	i = (*bp->bfunct)(&t[DCOM]);
	return (bp->flags & FRET ? i : 1);
}

noquit()
{

	signal(QUIT, 1);
}

quit()
{

	signal(QUIT, 0);
}

abort()
{

	abort();
}

dozip()
{
	;
}

chngd(vp)
	register char **vp;
{
	register char *dp;

	vp++;
	dp = *vp++;
	if (dp) {
		dp = globone(dp);
		if (dp == 0)
			return;
	} else {
		dp = value(home);
		if (*dp == 0) {
			bferr(": No home");
			return;
		}
	}
	if (chdir(dp) < 0)
		switch (errno) {
			case ENOTDIR:
				bferr2(dp, ": Not a directory");
				return;
			case ENOENT:
				bferr2(dp, ": No such file or directory");
				return;
			case EACCES:
				bferr2(dp, ": No execute access");
				return;
			default:
				prs("errno = ");
				prn(errno);
				prs("\n");
				bferr2(dp, ": bad directory !?!");
				return;
		}
}

prvars()
{

	plist(&shvhed);
}
/*
prvars()
{
	register struct shvar *vp;
	register char *cp;
	register int col;

	vp = shvhed.next;
	if (vp == 0)
		return;
	col = 0;
	cp = unquote(vp->value);
	for(;;) {
		prs(vp->name);
		col =+ strlen(vp->name);
		if (cp[0]) {
			prs("=");
			prs(cp);
			col =+ strlen(cp) + 1;
		}
		xfree(cp);
		col++;
		vp = vp->next;
		if (vp == 0)
			break;
		cp = unquote(vp->value);
		if (col + strlen(vp->name) + strlen(cp) + 1 > 79) {
			prs("\n");
			col = 0;
		} else
			prs(" ");
	}
	if (col)
		prs("\n");
}
*/

unquote(s)
	char *s;
{
	char *s2;
	int i;
	register char *cp, *dp;
	register c;

	i = 0;
	for (cp = s; c = *cp++;) {
		if ((c & QUOTE) == 0)
			continue;
		c =& 0177;
		i++;
		if (c == '"' || (*cp & QUOTE) == 0)
			continue;
		while ((c = *cp) && (c & QUOTE)) {
			c =& 0177;
			if (c == '"')
				break;
			cp++;
		}
		i++;
	}
	s2 = calloc(1, strlen(s) + i + 1);
	for (dp = s2, cp = s; c = *cp++;) {
		if ((c & QUOTE) == 0) {
			*dp++ = c;
			continue;
		}
		c =& 0177;
		if (c == '"' || (*cp & QUOTE) == 0) {
			*dp++ = '\\';
			*dp++ = c;
			continue;
		}
		*dp++ = '"';
		*dp++ = c;
		while ((c = *cp) && (c & QUOTE)) {
			c =& 0177;
			if (c == '"')
				break;
			cp++;
			*dp++ = c;
		}
		*dp++ = '"';
	}
	return (s2);
}

setname(cp)
	char *cp;
{

	bname = cp;
}

bferr(cp)
	char *cp;
{
	prs(bname);
	err(cp);
}

bferr2(cp, cp2)
	char *cp, *cp2;
{
	prs(cp);
	err(cp2);
}

doalias(v)
	register char *v[];
{
	register struct shvar *vp;
	register char *p;

	v++;
	p = *v++;
	if (p == 0)
		plist(&aliases);
	else if (*v == 0) {
		p = value1(p, &aliases);
		if (*p != 0) {
			prs(p);
			prs("\n");
		}
	} else
		set1(p, savestr(*v), &aliases);
}

unalias(v)
	register char *v[];
{

	unset1(v, &aliases);
}

dointerp(v)
	register char *v[];
{
	register int i;
	register char *p;

	v++;
	p = *v++;
	if (p == 0) {
		plist(&interps);
		return;
	}
	if (*v == 0) {
		p = value1(p, &interps);
		if (*p != 0) {
			prs(p);
			prs("\n");
		}
		return;
	}
	i = getn(p);
	if (i == 0)
		return;
	set1(p, savestr(*v), &interps);
}

uninterp(v)
	register char *v[];
{
	unset1(v, &interps);
}

interp(w)
	int w;
{
	register struct shvar *vp;

	for (vp = interps.next; vp != 0; vp = vp->next)
		if (getn(vp->name) == w)
			return (vp->value);
	return (0);
}

plist(vp)
	register struct shvar *vp;
{
	register char *cp;

	for (vp = vp->next; vp != 0; vp = vp->next) {
		prs(vp->name);
		if (vp->value[0]) {
			cp = unquote(vp->value);
			prs("\t");
			prs(cp);
			xfree(cp);
		}
		prs("\n");
	}
}

alias(t)
	int *t;
{
	register struct shvar *ap;

	for (ap = aliases.next; ap != 0; ap = ap->next)
		if (strcmp(ap->name, *t) == 0) {
			xfree(*t);
			if (ap->value[0] == 0) {
				bferr2(*t, ": Restricted");
				*t = ":";
			} else
				*t = savestr(ap->value);
			return (1);
		}
	return (0);
}

extern char end[];

doalloc()
{
	char *cp;

	cp = sbrk(0);
	prn(cp - &end);
	prs("\n");
}
