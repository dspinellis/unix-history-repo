#
/* global command --

   glob params

   "*" in params matches r.e ".*"
   "?" in params matches r.e. "."
   "[...]" in params matches character class
   "[...a-z...]" in params matches a through z.

   perform command with argument list
  constructed as follows:
     if param does not contain "*", "[", or "?", use it as is
     if it does, find all files in current directory
     which match the param, sort them, and use them

   prepend the command name with "/bin" or "/usr/bin"
   as required.
*/

#define	E2BIG	7
#define	ENOEXEC	8
#define	ENOENT	2

#define	STRSIZ	522
char	ab[STRSIZ];		/* generated characters */
char	*ava[200];		/* generated arguments */
char	**av &ava[1];
char	*string ab;
int	errno;
int	ncoll;

main(argc, argv)
char *argv[];
{
	register char *cp;

	if (argc < 3) {
		write(2, "Arg count\n", 10);
		return;
	}
	argv++;
	*av++ = *argv;
	while (--argc >= 2)
		expand(*++argv);
	if (ncoll==0) {
		write(2, "No match\n", 9);
		return;
	}
	execute(ava[1], &ava[1]);
	cp = cat("/usr/bin/", ava[1]);
	execute(cp+4, &ava[1]);
	execute(cp, &ava[1]);
	write(2, "Command not found.\n", 19);
}

expand(as)
char *as;
{
	register char *s, *cs;
	register int dirf;
	char **oav;
	static struct {
		int	ino;
		char	name[16];
	} entry;

	s = cs = as;
	while (*cs!='*' && *cs!='?' && *cs!='[') {
		if (*cs++ == 0) {
			*av++ = cat(s, "");
			return;
		}
	}
	for (;;) {
		if (cs==s) {
			dirf = open(".", 0);
			s = "";
			break;
		}
		if (*--cs == '/') {
			*cs = 0;
			dirf = open(s==cs? "/": s, 0);
			*cs++ = 0200;
			break;
		}
	}
	if (dirf<0) {
		write(2, "No directory\n", 13);
		exit();
	}
	oav = av;
	while (read(dirf, &entry, 16) == 16) {
		if (entry.ino==0)
			continue;
		if (match(entry.name, cs)) {
			*av++ = cat(s, entry.name);
			ncoll++;
		}
	}
	close(dirf);
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

execute(afile, aarg)
char *afile;
char **aarg;
{
	register char *file, **arg;

	arg = aarg;
	file = afile;
	execv(file, arg);
	if (errno==ENOEXEC) {
		arg[0] = file;
		*--arg = "/bin/sh";
		execv(*arg, arg);
	}
	if (errno==E2BIG)
		toolong();
}

toolong()
{
	write(2, "Arg list too long\n", 18);
	exit();
}

match(s, p)
char *s, *p;
{
	if (*s=='.' && *p!='.')
		return(0);
	return(amatch(s, p));
}

amatch(as, ap)
char *as, *ap;
{
	register char *s, *p;
	register scc;
	int c, cc, ok, lc;

	s = as;
	p = ap;
	if (scc = *s++)
		if ((scc =& 0177) == 0)
			scc = 0200;
	switch (c = *p++) {

	case '[':
		ok = 0;
		lc = 077777;
		while (cc = *p++) {
			if (cc==']') {
				if (ok)
					return(amatch(s, p));
				else
					return(0);
			} else if (cc=='-') {
				if (lc<=scc && scc<=(c = *p++))
					ok++;
			} else
				if (scc == (lc=cc))
					ok++;
		}
		return(0);

	default:
		if (c!=scc)
			return(0);

	case '?':
		if (scc)
			return(amatch(s, p));
		return(0);

	case '*':
		return(umatch(--s, p));

	case '\0':
		return(!scc);
	}
}

umatch(s, p)
char *s, *p;
{
	if(*p==0)
		return(1);
	while(*s)
		if (amatch(s++,p))
			return(1);
	return(0);
}

compar(as1, as2)
char *as1, *as2;
{
	register char *s1, *s2;

	s1 = as1;
	s2 = as2;
	while (*s1++ ==  *s2)
		if (*s2++ == 0)
			return(0);
	return (*--s1 - *s2);
}

cat(as1, as2)
char *as1, *as2;
{
	register char *s1, *s2;
	register int c;

	s2 = string;
	s1 = as1;
	while (c = *s1++) {
		if (s2 > &ab[STRSIZ])
			toolong();
		c =& 0177;
		if (c==0) {
			*s2++ = '/';
			break;
		}
		*s2++ = c;
	}
	s1 = as2;
	do {
		if (s2 > &ab[STRSIZ])
			toolong();
		*s2++ = c = *s1++;
	} while (c);
	s1 = string;
	string = s2;
	return(s1);
}
