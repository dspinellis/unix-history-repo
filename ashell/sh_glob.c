#include "sh.h"

#define	STRSIZ	522
#define	PTHSIZ	100

static	char ab[STRSIZ];
static	char *ava[200];
static	char **av;
static	char *string;
static	int ncoll;

static	char gpath[100], *gpathp;
static	int globbed;
static	char *entp;

static	int stbuff[18];

char **
glob(v)
	register char *v[];
{

	if (adrof("noglob"))
		return (v);
	ginit();
	while (*v)
		collect(*v++);
	*av++ = 0;
	return (ncoll == 0 ? 0 : &ava[2]);
}

ginit()
{
	av = &ava[2];
	string = ab;
	ncoll = 0;
	globbed = 0;
	gpathp = gpath;
}

collect(as)
{
	char **oav;

	oav = av;
	globbed = 0;
	expand(as);
	sort(oav);
}

sort(oav)
char **oav;
{
	register char **p1, **p2, **c;

	p1 = oav;
	while (p1 < av-1) {
		p2 = p1;
		while(++p2 < av) {
			if (strcmp(*p1, *p2) > 0) {
				c = *p1;
				*p1 = *p2;
				*p2 = c;
			}
		}
		p1++;
	}
}

expand(as)
char *as;
{
	register char *cs;
	char *sgpathp;
	register int dirf;
	static struct {
		int	ino;
		char	name[16];
	} entry;

	sgpathp = gpathp;
	cs = as;
	while (*cs != '*' && *cs != '?' && *cs != '[') {
		if (gpathp >= &gpath[PTHSIZ])
			gpatherr();
		else if ((*gpathp++ = *cs++) == 0) {
			if (!globbed)
				*av++ = cat(as, "");
			else if (stat(gpath, &stbuff) >= 0) {
				*av++ = cat(gpath, "");
				ncoll++;
			}
			goto endit;
		}
	}
	gpathp = sgpathp;
	cs--;
	while (cs >= as && *cs != '/')
		cs--;
	while (as <= cs)
		if (gpathp >= &gpath[PTHSIZ])
			gpatherr();
		else
			*gpathp++ = *as++;
	*gpathp = 0;
	dirf = open(gpath, 0);
	if (dirf<0)
		if (globbed)
			goto endit;
		else {
			prs(gpath);
			panic(": cannot open");
		}
	globbed++;
	cs++;
	while (read(dirf, &entry, 16) == 16) {
		if (entry.ino==0)
			continue;
		if (match(entry.name, cs)) {
			*av++ = cat(gpath, entry.name);
			ncoll++;
		}
	}
	close(dirf);
endit:
	gpathp = sgpathp;
	*gpathp = 0;
}

toolong()
{
	panic("Arg list too long");
}

gpatherr()
{
	prs("Path too long: ");
	panic(gpath);
}

match(s, p)
char *s, *p;
{
	register c, sentp;

	if (*s == '.' && *p != '.')
		return(0);
	sentp = entp;
	entp = s;
	c = amatch(s, p);
	entp = sentp;
	return(c);
}

amatch(as, ap)
char *as, *ap;
{
	register char *s, *p;
	register scc;
	int c, cc, ok, lc;
	char *sgpathp;

	s = as;
	p = ap;
nextc:
	if(scc = *s++ & 0177)
		if ((scc =& 0177) == 0)
			scc = 0200;
	switch (c = *p++) {
		case '[':
			ok = 0;
			lc = 077777;
			while (cc = *p++) {
				if (cc == ']') {
					if (ok)
						goto nextc;
					else
						return(0);
				} else if (cc == '-') {
					if (lc <= scc && scc <= *p++)
						ok++;
				} else
					if (scc == (lc = cc))
						ok++;
			}
			panic("missing ]");
		case '*':
			if(*p == '\0')
				return(1);
			else if (*p == '/') {
				p++;
				goto slash;
			}
			s--;
			while(*s)
				if (amatch(s, p))
					return(1);
				else
					s++;
			return(0);
		case '\0':
			return(scc == '\0');
		default:
			if (c == scc)
				goto nextc;
			else
				return(0);
		case '?':
			if (scc != '\0')
				goto nextc;
			else
				return(0);
		case '/':
			if (scc == '\0') {
slash:
				s = entp;
				sgpathp = gpathp;
				while (*gpathp = *s++)
					gpathp++;
				*gpathp++ = '/';
				*gpathp = 0;
				if (stat(gpath, &stbuff) == 0)
					if ((stbuff[2] & 060000) == 040000)
						if (*p == 0) {
							*av++ = cat(gpath, "");
							ncoll++;
						} else
							expand(p);
				gpathp = sgpathp;
				*gpathp = 0;
			}
			return(0);
	}
}

cat(as1, as2)
char *as1, *as2;
{
	register char *s1, *s2;

	s2 = string;
	s1 = as1;
	while (*s2++ = (*s1++ & 0177))
		if (s2 >= &ab[STRSIZ])
			toolong();
	s1 = as2;
	s2--;
	while (*s2++ = *s1++)
		if (s2 > &ab[STRSIZ])
			toolong();
	s1 = string;
	string = s2;
	return(s1);
}

panic(str)
{
	err(str);
	reset();
}
