#

/*
 * glob1 libs command arg ...
 *
 * Modified by: Bill Joy (UCB) Oct 1976
 *
 *  takes library name string from the shell
 *  allows patterns to match directory names, i.e. "/mnt/c*?/.q"
 *  knows about pascal object and forks px's
 */

char	usagestr[] "usage: glob2 libs command arg ...";

char	shell[] "/bin/sh";
char	px[]	"/bin/px";

#define	PCXN	0404
#define	E2BIG	7
#define	ENOEXEC	8
#define	ENOENT	2

#define	STRSIZ	522
#define	PTHSIZ	100

char	*lib;

char	ab[STRSIZ];
char	*ava[200];
char	**av &ava[1];
char	*string ab;
int	ncoll;

char	path[100];
char	*pathp path;
int	globbed;
char	*entp;

int	errno;
int	stbuff[18];

main(argc, argv)
char *argv[];
{
	register char *cp1, *cp2, *cpl;

	if (argc < 4)
		panic(usagestr);
	argv++;
	argc--;
	lib = *argv++;
	argc--;
	*av++ = *argv++;
	argc--;
	while (argc)
	{
		collect(*argv++);
		argc--;
	}
	if (ncoll==0)
		panic("No match");
	cp1 = ava[1];
	while(*cp1 && *cp1 != '/')
		cp1++;
	if (*cp1 || getuid())
		execute(ava[1], &ava[1]);
	if (!*cp1)
	{
		if (cpl = lib)
			for(;;)
			{
				while ((*cpl & 0177) == ' ')
					cpl++;
				if (!*cpl)
					break;
				cp1 = path;
				while (*cpl =& 0177)
					if (*cpl == ' ')
						break;
					else if (cp1 >= &path[PTHSIZ-2])
						patherr();
					else
						*cp1++ = *cpl++;
				*cp1++ = '/';
				cp2 = ava[1];
				while(*cp1++ = *cp2++)
					if (cp1 >= &path[PTHSIZ])
						patherr();
				execute(path, &ava[1]);
			}
		cp1 = path;
		cp2 = "/usr/bin/";
		while(*cp1++ = *cp2++)
			;
		cp1--;
		cp2 = ava[1];
		while(*cp1++ = *cp2++)
			if (cp1 >= &path[PTHSIZ])
				patherr();
		execute(path+4, &ava[1]);
		execute(path, &ava[1]);
	}
	prs(ava[1]);
	panic(": not found");
}

execute(afile, aarg)
char *afile;
char **aarg;
{
	register char *file, **arg;
	register i;
	int w;

	arg = aarg;
	file = afile;
	execv(file, arg);
	if (errno==ENOEXEC) {
		arg[0] = file;
		*--arg = shell;
		i = open(file, 0);
		if (i >= 0) {
			if (read(i, &w, 2) == 2 && w == PCXN)
				*arg = px;
			close(i);
		}
		execv(*arg, arg);
		prs("no ");
		prs(*arg);
		panic("!!");
	} else if (errno==E2BIG)
		toolong();
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
			if (compar(*p1, *p2) > 0) {
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
	char *spathp;
	register int dirf;
	static struct {
		int	ino;
		char	name[16];
	} entry;

	spathp = pathp;
	cs = as;
	while (*cs != '*' && *cs != '?' && *cs != '[') {
		if (pathp >= &path[PTHSIZ])
			patherr();
		else if ((*pathp++ = *cs++) == 0) {
			if (!globbed)
				*av++ = cat(as, "");
			else if (stat(path, &stbuff) >= 0) {
				*av++ = cat(path, "");
				ncoll++;
			}
			goto endit;
		}
	}
	pathp = spathp;
	cs--;
	while (cs >= as && *cs != '/')
		cs--;
	while (as <= cs)
		if (pathp >= &path[PTHSIZ])
			patherr();
		else
			*pathp++ = *as++;
	*pathp = 0;
	dirf = open(path, 0);
	if (dirf<0)
		if (globbed)
			goto endit;
		else {
			prs(path);
			panic(": cannot open");
		}
	globbed++;
	cs++;
	while (read(dirf, &entry, 16) == 16) {
		if (entry.ino==0)
			continue;
		if (match(entry.name, cs)) {
			*av++ = cat(path, entry.name);
			ncoll++;
		}
	}
	close(dirf);
endit:
	pathp = spathp;
	*pathp = 0;
}

toolong()
{
	panic("Arg list too long");
}

patherr()
{
	prs("Path too long: ");
	panic(path);
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
	char *spathp;

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
				spathp = pathp;
				while (*pathp = *s++)
					pathp++;
				*pathp++ = '/';
				*pathp = 0;
				if (stat(path, &stbuff) == 0)
					if ((stbuff[2] & 060000) == 040000)
						if (*p == 0) {
							*av++ = cat(path, "");
							ncoll++;
						} else
							expand(p);
				pathp = spathp;
				*pathp = 0;
			}
			return(0);
	}
}

compar(as1, as2)
char *as1, *as2;
{
	register char *s1, *s2;

	s1 = as1;
	s2 = as2;
	while (*s1++ == *s2)
		if (*s2++ == 0)
			return(0);
	if (*--s1 == '/')
		return(-1);
	else if (*s2 == '/')
		return(1);
	else
		return (*s1 - *s2);
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

prs(str)
char *str;
{
	register char *cp;

	cp = str;
	while(*cp)
		cp++;
	write(2, str, cp-str);
}

panic(str)
{
	prs(str);
	write(2, &"\n", 1);
	exit(1);
}
