#include "sh.h"

int	tglob(), trim();
static	int gflag;
static	char *exerr, *expath;

char	nic[]	"nice";

doexec(t)
	int *t;
{
	register char *cp, *pp;
	register char **av;
	char slash, pathc;
	char *vp;

	if (adrof(nic))
		nice(getn(value(nic)));
	cp = globone(t[DCOM]);
	if (cp == 0)
		exit(1);
	exerr = 0;
	expath = t[DCOM] = cp;
	slash = gflag || any ('/', cp);
	gflag = 0;
	av = &t[DCOM+1];
	scan(av, &tglob);
	if (gflag) {
		av = glob(av);
		if (av == 0) {
			prs("No match\n");
			exit(1);
		}
	}
	*--av = t[DCOM];
	scan(av, &trim);
	pp = value(path);
	if (*pp == 0 || slash)
		pp = "";
	do {
		for (cp = pp; *pp && *pp != '-' ; pp++)
			*pp =& ~QUOTE;
		pathc = *pp;
		*pp = 0;
		vp = strcpy(calloc(1, strlen(cp) + strlen(*av) + 2), cp);
		if (cp == pp || slash == 0) {
			if (cp[0])
				strcat(vp, "/");
			strcat(vp, *av);
			texec(vp, av-DCOM);
			xfree(vp);
		}
	} while ((*pp++ = pathc) != 0);
	prs(expath);
	if (exerr) {
		err(exerr);
		xfree(exerr);
	} else
		err(": Cannot find");
	exit(1);
}

texec(f, t)
	register int *t;
{
	extern errno;
	register int *t1;
	register i;
	int w;

	execv(f, (t1 = &t[DCOM]));
	switch (errno) {
		case ENOEXEC:
			*t1 = f;
			i = open(f, 0);
			if (i < 0) {
				prs(*t1);
				prs(": Cannot open");
				exit(1);
			}
			w = 0;
			read(i, &w, 2);
			close(i);
			f = interp(w);
			if (f != 0)
				*--t1 = f;
			else if (w & 0100200) {
				prs(*t1);
				prs(": No interpreter\n");
				exit(1);
			} else
				*--t1 = value(shell);
			execv(*t1, t1);
			prs(*t1);
			/*
			 * should give better message but can't goto
			 * top because could loop
			 */
			err(": Execute failed");
			exit(1);
		case ENOMEM:
			prs(*t1);
			err(": Not enough core");
			exit(1);
		case EACCES:
			if (exerr == 0) {
				expath = savestr(f);
				exerr = ": No execute access";
			}
			return;
	}
}


scan(t, f)
	register int *t;
	int (*f)();
{
	register char *p, c;

	while (p = *t++)
		while (c = *p)
			*p++ = (*f)(c);
}

tglob(c)
	char c;
{
	if (any(c, "[?*"))
		gflag = 1;
	return (c);
}

trim(c)
	char c;
{

	return (c & 0177);
}

prn(n)
	int n;
{
	register a;

	a = n / 10;
	if (a != 0)
		prn(a);
	n = n % 10 + '0';
	write(2, &n, 1);
}

globone(str)
	register char *str;
{
	int gv[2];
	register char **gvp;

	setname(str);
	gv[0] = str;
	gv[1] = 0;
	gflag = 0;
	scan(gv, &tglob);
	if (gflag) {
		gvp = glob(gv);
		if (gvp == 0) {
			bferr(": No match");
			return (0);
		}
		str = *gvp++;
		if (*gvp) {
			bferr(": Ambiguous");
			return (0);
		}
	} else {
		scan(gv, &trim);
		str = gv[0];
	}
	return (str);
}
