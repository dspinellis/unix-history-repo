/* Copyright (c) 1979 Regents of the University of California */
#include "sh.h"

/*
 * C shell
 */

/*
 * System level search and execute of a command.
 * We look in each directory for the specified command name.
 * If the name contains a '/' then we execute only the full path name.
 * If there is no search path then we execute only full path names.
 */

/* 
 * As we search for the command we note the first non-trivial error
 * message for presentation to the user.  This allows us often
 * to show that a file has the wrong mode/no access when the file
 * is not in the last component of the search path, so we must
 * go on after first detecting the error.
 */
char	*exerr;			/* Execution error message */
char	*expath;		/* Path for exerr */

/*
 * Xhash is an array of HSHSIZ chars, which are used to hash execs.
 * If it is allocated, then to tell whether ``name'' is (possibly)
 * present in the i'th component of the variable path, you look at
 * the i'th bit of xhash[hash("name")].  This is setup automatically
 * after .login is executed, and recomputed whenever ``path'' is
 * changed.
 */
int	havhash;
#define	HSHSIZ	511
char	xhash[HSHSIZ];
int	hits, misses;

/* Dummy search path for just absolute search when no path */
char	*justabs[] =	{ "", 0 };

doexec(t)
	register struct command *t;
{
	char *sav;
	register char *dp, **pv, **av;
	register struct varent *v;
	bool slash = any('/', t->t_dcom[0]);
	int hashval, i;
	char *blk[2];

	/*
	 * Glob the command name.  If this does anything, then we
	 * will execute the command only relative to ".".  One special
	 * case: if there is no PATH, then we execute only commands
	 * which start with '/'.
	 */
	dp = globone(t->t_dcom[0]);
	sav = t->t_dcom[0];
	exerr = 0; expath = t->t_dcom[0] = dp;
	xfree(sav);
	v = adrof("path");
	if (v == 0 && expath[0] != '/')
		pexerr();
	slash |= gflag;

	/*
	 * Glob the argument list, if necessary.
	 * Otherwise trim off the quote bits.
	 */
	gflag = 0; av = &t->t_dcom[1];
	rscan(av, tglob);
	if (gflag) {
		av = glob(av);
		if (av == 0)
			error("No match");
	}
	blk[0] = t->t_dcom[0];
	blk[1] = 0;
	av = blkspl(blk, av);
#ifdef VFORK
	Vav = av;
#endif
	scan(av, trim);

	xechoit(av);		/* Echo command if -x */
	closech();		/* Close random fd's */

	/*
	 * If no path, no words in path, or a / in the filename
	 * then restrict the command search.
	 */
	if (v == 0 || v->vec[0] == 0 || slash)
		pv = justabs;
	else
		pv = v->vec;
	sav = strspl("/", *av);		/* / command name for postpending */
#ifdef VFORK
	Vsav = sav;
#endif
	if (havhash)
		hashval = xhash[hash(*av)];
	i = 0;
	hits++;
	do {
		if (!slash && pv[0][0] == '/' && havhash && (hashval & (1 << (i % 8))) == 0)
			goto cont;
		if (pv[0][0] == 0 || eq(pv[0], "."))	/* don't make ./xxx */
			texec(*av, av);
		else {
			dp = strspl(*pv, sav);
#ifdef VFORK
			Vdp = dp;
#endif
			texec(dp, av);
#ifdef VFORK
			Vdp = 0;
#endif
			xfree(dp);
		}
		misses++;
cont:
		pv++;
		i++;
	} while (*pv);
	hits--;
#ifdef VFORK
	Vsav = 0;
	Vav = 0;
#endif
	xfree(sav);
	xfree(av);
	pexerr();
}

pexerr()
{

	/* Couldn't find the damn thing */
	setname(expath);
	/* xfree(expath); */
	if (exerr)
		bferr(exerr);
	bferr("Command not found");
}

/* Last resort shell */
char	*lastsh[] =	{ SHELLPATH, 0 };

/*
 * Execute command f, arg list t.
 * Record error message if not found.
 * Also do shell scripts here.
 */
texec(f, t)
	char *f;
	register char **t;
{
	register struct varent *v;
	register char **vp;
	extern char *sys_errlist[];

	execv(f, t);
	switch (errno) {

	case ENOEXEC:
		/*
		 * If there is an alias for shell, then
		 * put the words of the alias in front of the
		 * argument list replacing the command name.
		 * Note no interpretation of the words at this point.
		 */
		v = adrof1("shell", &aliases);
		if (v == 0) {
#ifdef OTHERSH
			register int ff = open(f, 0);
			char ch;
#endif

			vp = lastsh;
			vp[0] = adrof("shell") ? value("shell") : SHELLPATH;
#ifdef OTHERSH
			if (ff != -1 && read(ff, &ch, 1) == 1 && ch != '#')
				vp[0] = OTHERSH;
			close(ff);
#endif
		} else
			vp = v->vec;
		t[0] = f;
		t = blkspl(vp, t);		/* Splice up the new arglst */
		f = *t;
		execv(f, t);
		xfree(t);
		/* The sky is falling, the sky is falling! */

	case ENOMEM:
		Perror(f);

	case ENOENT:
		break;

	default:
		if (exerr == 0) {
			exerr = sys_errlist[errno];
			expath = savestr(f);
		}
	}
}

execash(t, kp)
	register struct command *kp;
{

	didcch++;
	lshift(kp->t_dcom, 1);
	doexec(kp);
	/*NOTREACHED*/
}

xechoit(t)
	char **t;
{

	if (adrof("echo")) {
		flush();
		haderr = 1;
		blkpr(t), printf("\n");
		haderr = 0;
	}
}

dohash()
{
	struct stat stb;
	struct direct dirbuf[BUFSIZ / sizeof (struct direct)];
	char d_name[DIRSIZ + 1];
	register int dirf, cnt;
	int i = 0;
	struct varent *v = adrof("path");
	char **pv;

	havhash = 1;
	for (cnt = 0; cnt < HSHSIZ; cnt++)
		xhash[cnt] = 0;
	if (v == 0)
		return;
	for (pv = v->vec; *pv; pv++, i = (i + 1) % 8) {
		if (pv[0][0] != '/')
			continue;
		dirf = open(*pv, 0);
		if (dirf < 0)
			continue;
		if (fstat(dirf, &stb) < 0 || !isdir(stb)) {
			close(dirf);
			continue;
		}
		while ((cnt = read(dirf, (char *) dirbuf, sizeof dirbuf)) >= sizeof dirbuf[0]) {
			register struct direct *ep = dirbuf;

			for (cnt /= sizeof(struct direct); cnt > 0; cnt--, ep++) {
				if (ep->d_ino == 0)
					continue;
				copdent(d_name, ep->d_name);
				xhash[hash(d_name)] |= (1 << i);
			}
		}
		close(dirf);
	}
}

dounhash()
{

	havhash = 0;
}

hashstat()
{

	if (hits+misses)
	printf("%d hits, %d misses, %2d%%\n", hits, misses, 100 * hits / (hits + misses));
}

hash(cp)
	register char *cp;
{
	register int hash = 0;

	while (*cp)
		hash += hash + *cp++;
	return (hash % HSHSIZ);
}
