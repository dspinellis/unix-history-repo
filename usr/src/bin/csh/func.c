static	char *sccsid = "@(#)func.c 4.5 81/06/19";

#include "sh.h"
#include <sys/ioctl.h>

/*
 * C shell
 */

struct biltins *
isbfunc(t)
	register struct command *t;
{
	register char *cp = t->t_dcom[0];
	register char *dp;
	register struct biltins *bp;
	int dolabel(), dofg1(), dobg1();
	static struct biltins label = { "", dolabel, 0, 0 };
	static struct biltins foregnd = { "%job", dofg1, 0, 0 };
	static struct biltins backgnd = { "%job &", dobg1, 0, 0 };

	if (lastchr(cp) == ':') {
		label.bname = cp;
		return (&label);
	}
	if (*cp == '%') {
		if (t->t_dflg & FAND) {
			t->t_dflg &= ~FAND;
			backgnd.bname = cp;
			return (&backgnd);
		} 
		foregnd.bname = cp;
		return (&foregnd);
	}
	for (bp = bfunc; dp = bp->bname; bp++) {
		if (dp[0] == cp[0] && eq(dp, cp))
			return (bp);
		if (dp[0] > cp[0])
			break;
	}
	return (0);
}

func(t, bp)
	register struct command *t;
	register struct biltins *bp;
{
	int i;

	xechoit(t->t_dcom);
	setname(bp->bname);
	i = blklen(t->t_dcom) - 1;
	if (i < bp->minargs)
		bferr("Too few arguments");
	if (i > bp->maxargs)
		bferr("Too many arguments");
	(*bp->bfunct)(t->t_dcom, t);
}

dolabel()
{

}

doonintr(v)
	char **v;
{
	register char *cp;
	register char *vv = v[1];

	if (parintr == SIG_IGN)
		return;
	if (setintr && intty)
		bferr("Can't from terminal");
	cp = gointr, gointr = 0, xfree(cp);
	if (vv == 0) {
		if (setintr)
			sighold(SIGINT);
		else
			sigset(SIGINT, SIG_DFL);
		gointr = 0;
	} else if (eq((vv = strip(vv)), "-")) {
		sigset(SIGINT, SIG_IGN);
		gointr = "-";
	} else {
		gointr = savestr(vv);
		sigset(SIGINT, pintr);
	}
}

donohup()
{

	if (intty)
		bferr("Can't from terminal");
	if (setintr == 0) {
		signal(SIGHUP, SIG_IGN);
#ifdef CC
		submit(getpid());
#endif
	}
}

dozip()
{

	;
}

prvars()
{

	plist(&shvhed);
}

doalias(v)
	register char **v;
{
	register struct varent *vp;
	register char *p;

	v++;
	p = *v++;
	if (p == 0)
		plist(&aliases);
	else if (*v == 0) {
		vp = adrof1(strip(p), &aliases);
		if (vp)
			blkpr(vp->vec), printf("\n");
	} else {
		if (eq(p, "alias") || eq(p, "unalias")) {
			setname(p);
			bferr("Too dangerous to alias that");
		}
		set1(strip(p), saveblk(v), &aliases);
	}
}

unalias(v)
	char **v;
{

	unset1(v, &aliases);
}

dologout()
{

	islogin();
	goodbye();
}

dologin(v)
	char **v;
{

	islogin();
	signal(SIGTERM, parterm);
	execl("/bin/login", "login", v[1], 0);
	untty();
	exit(1);
}

donewgrp(v)
	char **v;
{

	if (chkstop == 0 && setintr)
		panystop(0);
	signal(SIGTERM, parterm);
	execl("/bin/newgrp", "newgrp", v[1], 0);
	execl("/usr/bin/newgrp", "newgrp", v[1], 0);
	untty();
	exit(1);
}

islogin()
{

	if (chkstop == 0 && setintr)
		panystop(0);
	if (loginsh)
		return;
	error("Not login shell");
}

doif(v, kp)
	char **v;
	struct command *kp;
{
	register int i;
	register char **vv;

	v++;
	i = exp(&v);
	vv = v;
	if (*vv == NOSTR)
		bferr("Empty if");
	if (eq(*vv, "then")) {
		if (*++vv)
			bferr("Improper then");
		setname("then");
		/*
		 * If expression was zero, then scan to else,
		 * otherwise just fall into following code.
		 */
		if (!i)
			search(ZIF, 0);
		return;
	}
	/*
	 * Simple command attached to this if.
	 * Left shift the node in this tree, munging it
	 * so we can reexecute it.
	 */
	if (i) {
		lshift(kp->t_dcom, vv - kp->t_dcom);
		reexecute(kp);
		donefds();
	}
}

/*
 * Reexecute a command, being careful not
 * to redo i/o redirection, which is already set up.
 */
reexecute(kp)
	register struct command *kp;
{

	kp->t_dflg &= FSAVE;
	kp->t_dflg |= FREDO;
	/*
	 * If tty is still ours to arbitrate, arbitrate it;
	 * otherwise dont even set pgrp's as the jobs would
	 * then have no way to get the tty (we can't give it
	 * to them, and our parent wouldn't know their pgrp, etc.
	 */
	execute(kp, tpgrp > 0 ? tpgrp : -1);
}

doelse()
{

	search(ZELSE, 0);
}

dogoto(v)
	char **v;
{
	register struct whyle *wp;
	char *lp;

	/*
	 * While we still can, locate any unknown ends of existing loops.
	 * This obscure code is the WORST result of the fact that we
	 * don't really parse.
	 */
	for (wp = whyles; wp; wp = wp->w_next)
		if (wp->w_end == 0) {
			search(ZBREAK, 0);
			wp->w_end = btell();
		} else
			bseek(wp->w_end);
	search(ZGOTO, 0, lp = globone(v[1]));
	xfree(lp);
	/*
	 * Eliminate loops which were exited.
	 */
	wfree();
}

doswitch(v)
	register char **v;
{
	register char *cp, *lp;

	v++;
	if (!*v || *(*v++) != '(')
		goto syntax;
	cp = **v == ')' ? "" : *v++;
	if (*(*v++) != ')')
		v--;
	if (*v)
syntax:
		error("Syntax error");
	search(ZSWITCH, 0, lp = globone(cp));
	xfree(lp);
}

dobreak()
{

	if (whyles)
		toend();
	else
		bferr("Not in while/foreach");
}

doexit(v)
	char **v;
{

	if (chkstop == 0)
		panystop(0);
	/*
	 * Don't DEMAND parentheses here either.
	 */
	v++;
	if (*v) {
		set("status", putn(exp(&v)));
		if (*v)
			bferr("Expression syntax");
	}
	btoeof();
	if (intty)
		close(SHIN);
}

doforeach(v)
	register char **v;
{
	register char *cp;
	register struct whyle *nwp;

	v++;
	cp = strip(*v);
	while (*cp && letter(*cp))
		cp++;
	if (*cp || strlen(*v) >= 20)
		bferr("Invalid variable");
	cp = *v++;
	if (v[0][0] != '(' || v[blklen(v) - 1][0] != ')')
		bferr("Words not ()'ed");
	v++;
	gflag = 0, rscan(v, tglob);
	v = glob(v);
	if (v == 0)
		bferr("No match");
	nwp = (struct whyle *) calloc(1, sizeof *nwp);
	nwp->w_fe = nwp->w_fe0 = v; gargv = 0;
	nwp->w_start = btell();
	nwp->w_fename = savestr(cp);
	nwp->w_next = whyles;
	whyles = nwp;
	/*
	 * Pre-read the loop so as to be more
	 * comprehensible to a terminal user.
	 */
	if (intty)
		preread();
	doagain();
}

dowhile(v)
	char **v;
{
	register int status;
	register bool again = whyles != 0 && whyles->w_start == lineloc &&
	    whyles->w_fename == 0;

	v++;
	/*
	 * Implement prereading here also, taking care not to
	 * evaluate the expression before the loop has been read up
	 * from a terminal.
	 */
	if (intty && !again)
		status = !exp0(&v, 1);
	else
		status = !exp(&v);
	if (*v)
		bferr("Expression syntax");
	if (!again) {
		register struct whyle *nwp = (struct whyle *) calloc(1, sizeof (*nwp));

		nwp->w_start = lineloc;
		nwp->w_end = 0;
		nwp->w_next = whyles;
		whyles = nwp;
		if (intty) {
			/*
			 * The tty preread
			 */
			preread();
			doagain();
			return;
		}
	}
	if (status)
		/* We ain't gonna loop no more, no more! */
		toend();
}

preread()
{

	whyles->w_end = -1;
	if (setintr)
		sigrelse(SIGINT);
	search(ZBREAK, 0);
	if (setintr)
		sighold(SIGINT);
	whyles->w_end = btell();
}

doend()
{

	if (!whyles)
		bferr("Not in while/foreach");
	whyles->w_end = btell();
	doagain();
}

docontin()
{

	if (!whyles)
		bferr("Not in while/foreach");
	doagain();
}

doagain()
{

	/* Repeating a while is simple */
	if (whyles->w_fename == 0) {
		bseek(whyles->w_start);
		return;
	}
	/*
	 * The foreach variable list actually has a spurious word
	 * ")" at the end of the w_fe list.  Thus we are at the
	 * of the list if one word beyond this is 0.
	 */
	if (!whyles->w_fe[1]) {
		dobreak();
		return;
	}
	set(whyles->w_fename, savestr(*whyles->w_fe++));
	bseek(whyles->w_start);
}

dorepeat(v, kp)
	char **v;
	struct command *kp;
{
	register int i;

	i = getn(v[1]);
	if (setintr)
		sighold(SIGINT);
	lshift(v, 2);
	while (i > 0) {
		if (setintr)
			sigrelse(SIGINT);
		reexecute(kp);
		--i;
	}
	donefds();
	if (setintr)
		sigrelse(SIGINT);
}

doswbrk()
{

	search(ZBRKSW, 0);
}

srchx(cp)
	register char *cp;
{
	register struct srch *sp;

	for (sp = srchn; sp->s_name; sp++)
		if (eq(cp, sp->s_name))
			return (sp->s_value);
	return (-1);
}

char	Stype;
char	*Sgoal;

/*VARARGS2*/
search(type, level, goal)
	int type;
	register int level;
	char *goal;
{
	char wordbuf[BUFSIZ];
	register char *aword = wordbuf;
	register char *cp;

	Stype = type; Sgoal = goal;
	if (type == ZGOTO)
		bseek(0l);
	do {
		if (intty && fseekp == feobp)
			printf("? "), flush();
		aword[0] = 0, getword(aword);
		switch (srchx(aword)) {

		case ZELSE:
			if (level == 0 && type == ZIF)
				return;
			break;

		case ZIF:
			while (getword(aword))
				continue;
			if ((type == ZIF || type == ZELSE) && eq(aword, "then"))
				level++;
			break;

		case ZENDIF:
			if (type == ZIF || type == ZELSE)
				level--;
			break;

		case ZFOREACH:
		case ZWHILE:
			if (type == ZBREAK)
				level++;
			break;

		case ZEND:
			if (type == ZBREAK)
				level--;
			break;

		case ZSWITCH:
			if (type == ZSWITCH || type == ZBRKSW)
				level++;
			break;

		case ZENDSW:
			if (type == ZSWITCH || type == ZBRKSW)
				level--;
			break;

		case ZLABEL:
			if (type == ZGOTO && getword(aword) && eq(aword, goal))
				level = -1;
			break;

		default:
			if (type != ZGOTO && (type != ZSWITCH || level != 0))
				break;
			if (lastchr(aword) != ':')
				break;
			aword[strlen(aword) - 1] = 0;
			if (type == ZGOTO && eq(aword, goal) || type == ZSWITCH && eq(aword, "default"))
				level = -1;
			break;

		case ZCASE:
			if (type != ZSWITCH || level != 0)
				break;
			getword(aword);
			if (lastchr(aword) == ':')
				aword[strlen(aword) - 1] = 0;
			cp = strip(Dfix1(aword));
			if (Gmatch(goal, cp))
				level = -1;
			xfree(cp);
			break;

		case ZDEFAULT:
			if (type == ZSWITCH && level == 0)
				level = -1;
			break;
		}
		getword(NOSTR);
	} while (level >= 0);
}

getword(wp)
	register char *wp;
{
	register int found = 0;
	register int c, d;

	c = readc(1);
	d = 0;
	do {
		while (c == ' ' || c == '\t')
			c = readc(1);
		if (c == '#')
			do
				c = readc(1);
			while (c >= 0 && c != '\n');
		if (c < 0)
			goto past;
		if (c == '\n') {
			if (wp)
				break;
			return (0);
		}
		unreadc(c);
		found = 1;
		do {
			c = readc(1);
			if (c == '\\' && (c = readc(1)) == '\n')
				c = ' ';
			if (any(c, "'\""))
				if (d == 0)
					d = c;
				else if (d == c)
					d = 0;
			if (c < 0)
				goto past;
			if (wp)
				*wp++ = c;
		} while ((d || c != ' ' && c != '\t') && c != '\n');
	} while (wp == 0);
	unreadc(c);
	if (found)
		*--wp = 0;
	return (found);

past:
	switch (Stype) {

	case ZIF:
		bferr("then/endif not found");

	case ZELSE:
		bferr("endif not found");

	case ZBRKSW:
	case ZSWITCH:
		bferr("endsw not found");

	case ZBREAK:
		bferr("end not found");

	case ZGOTO:
		setname(Sgoal);
		bferr("label not found");
	}
	/*NOTREACHED*/
}

toend()
{

	if (whyles->w_end == 0) {
		search(ZBREAK, 0);
		whyles->w_end = btell() - 1;
	} else
		bseek(whyles->w_end);
	wfree();
}

wfree()
{
	long o = btell();

	while (whyles) {
		register struct whyle *wp = whyles;
		register struct whyle *nwp = wp->w_next;

		if (o >= wp->w_start && (wp->w_end == 0 || o < wp->w_end))
			break;
		if (wp->w_fe0)
			blkfree(wp->w_fe0);
		if (wp->w_fename)
			xfree(wp->w_fename);
		xfree((char *)wp);
		whyles = nwp;
	}
}

doecho(v)
	char **v;
{

	echo(' ', v);
}

doglob(v)
	char **v;
{

	echo(0, v);
	flush();
}

echo(sep, v)
	char sep;
	register char **v;
{
	register char *cp;
	int nonl = 0;

	if (setintr)
		sigrelse(SIGINT);
	v++;
	if (*v == 0)
		return;
	gflag = 0; rscan(v, tglob);
	if (gflag) {
		v = glob(v);
		if (v == 0)
			bferr("No match");
	} else
		scan(v, trim);
	if (sep == ' ' && !strcmp(*v, "-n"))
		nonl++, v++;
	while (cp = *v++) {
		register int c;

		while (c = *cp++)
			putchar(c | QUOTE);
		if (*v)
			putchar(sep | QUOTE);
	}
	if (sep && nonl == 0)
		putchar('\n');
	else
		flush();
	if (setintr)
		sighold(SIGINT);
	if (gargv)
		blkfree(gargv), gargv = 0;
}

char	**environ;

dosetenv(v)
	register char **v;
{
	char *lp = globone(v[2]);

	setenv(v[1], lp);
	if (eq(v[1], "PATH")) {
		importpath(lp);
		dohash();
	}
	xfree(lp);
}

dounsetenv(v)
	register char **v;
{

	v++;
	do
		unsetenv(*v++);
	while (*v);
}

setenv(name, value)
	char *name, *value;
{
	register char **ep = environ;
	register char *cp, *dp;
	char *blk[2], **oep = ep;

	for (; *ep; ep++) {
		for (cp = name, dp = *ep; *cp && *cp == *dp; cp++, dp++)
			continue;
		if (*cp != 0 || *dp != '=')
			continue;
		cp = strspl("=", value);
		xfree(*ep);
		*ep = strspl(name, cp);
		xfree(cp);
		scan(ep, trim);
		return;
	}
	blk[0] = strspl(name, "="); blk[1] = 0;
	environ = blkspl(environ, blk);
	xfree((char *)oep);
	setenv(name, value);
}

unsetenv(name)
	char *name;
{
	register char **ep = environ;
	register char *cp, *dp;
	char **oep = ep;

	for (; *ep; ep++) {
		for (cp = name, dp = *ep; *cp && *cp == *dp; cp++, dp++)
			continue;
		if (*cp != 0 || *dp != '=')
			continue;
		cp = *ep;
		*ep = 0;
		environ = blkspl(environ, ep+1);
		*ep = cp;
		xfree(cp);
		xfree((char *)oep);
		return;
	}
}

doumask(v)
	register char **v;
{
	register char *cp = v[1];
	register int i;

	if (cp == 0) {
		i = umask(0);
		umask(i);
		printf("%o\n", i);
		return;
	}
	i = 0;
	while (digit(*cp) && *cp != '8' && *cp != '9')
		i = i * 8 + *cp++ - '0';
	if (*cp || i < 0 || i > 0777)
		bferr("Improper mask");
	umask(i);
}

#include <sys/vlimit.h>

struct limits {
	int	limconst;
	char	*limname;
	int	limdiv;
	char	*limscale;
} limits[] = {
	LIM_NORAISE,	"noraise",	1,	"",
	LIM_CPU,	"cputime",	1,	"seconds",
	LIM_FSIZE,	"filesize",	1024,	"kbytes",
	LIM_DATA,	"datasize",	1024,	"kbytes",
	LIM_STACK,	"stacksize",	1024,	"kbytes",
	LIM_CORE,	"coredumpsize",	1024,	"kbytes",
	LIM_MAXRSS,	"memoryuse",	1024,	"kbytes",
	-1,		0,
};

struct limits *
findlim(cp)
	char *cp;
{
	register struct limits *lp, *res;

	res = 0;
	for (lp = limits; lp->limconst >= 0; lp++)
		if (prefix(cp, lp->limname)) {
			if (res)
				bferr("Ambiguous");
			res = lp;
		}
	if (res)
		return (res);
	bferr("No such limit");
}

dolimit(v)
	register char **v;
{
	register struct limits *lp;
	register int limit;

	v++;
	if (*v == 0) {
		for (lp = limits+1; lp->limconst >= 0; lp++)
			plim(lp);
		if (vlimit(LIM_NORAISE, -1) && getuid())
			printf("Limits cannot be raised\n");
		return;
	}
	lp = findlim(v[0]);
	if (v[1] == 0) {
		plim(lp);
		return;
	}
	limit = getval(lp, v+1);
	setlim(lp, limit);
}

getval(lp, v)
	register struct limits *lp;
	char **v;
{
	register float f;
	double atof();
	char *cp = *v++;

	f = atof(cp);
	while (digit(*cp) || *cp == '.' || *cp == 'e' || *cp == 'E')
		cp++;
	if (*cp == 0) {
		if (*v == 0)
			return ((int)(f+0.5) * lp->limdiv);
		cp = *v;
	}
	if (lp->limconst == LIM_NORAISE)
		goto badscal;
	switch (*cp) {

	case ':':
		if (lp->limconst != LIM_CPU)
			goto badscal;
		return ((int)(f * 60.0 + atof(cp+1)));

	case 'h':
		if (lp->limconst != LIM_CPU)
			goto badscal;
		limtail(cp, "hours");
		f *= 3600.;
		break;

	case 'm':
		if (lp->limconst == LIM_CPU) {
			limtail(cp, "minutes");
			f *= 60.;
			break;
		}
	case 'M':
		if (lp->limconst == LIM_CPU)
			goto badscal;
		*cp = 'm';
		limtail(cp, "megabytes");
		f *= 1024.*1024.;
		break;

	case 's':
		if (lp->limconst != LIM_CPU)
			goto badscal;
		limtail(cp, "seconds");
		break;

	case 'k':
		if (lp->limconst == LIM_CPU)
			goto badscal;
		limtail(cp, "kbytes");
		f *= 1024;
		break;

	case 'u':
		limtail(cp, "unlimited");
		return (INFINITY);

	default:
badscal:
		bferr("Improper or unknown scale factor");
	}
	return ((int)(f+0.5));
}

limtail(cp, str0)
	char *cp, *str0;
{
	register char *str = str0;

	while (*cp && *cp == *str)
		cp++, str++;
	if (*cp)
		error("Bad scaling; did you mean ``%s''?", str0);
}

plim(lp)
	register struct limits *lp;
{
	register int lim;

	printf("%s \t", lp->limname);
	lim = vlimit(lp->limconst, -1);
	if (lim == INFINITY)
		printf("unlimited");
	else if (lp->limconst == LIM_CPU)
		psecs((long)lim);
	else
		printf("%d %s", lim / lp->limdiv, lp->limscale);
	printf("\n");
}

dounlimit(v)
	register char **v;
{
	register struct limits *lp;

	v++;
	if (*v == 0) {
		for (lp = limits+1; lp->limconst >= 0; lp++)
			setlim(lp, INFINITY);
		return;
	}
	while (*v) {
		lp = findlim(*v++);
		setlim(lp, INFINITY);
	}
}

setlim(lp, limit)
	register struct limits *lp;
{

	if (vlimit(lp->limconst, limit) < 0)
		Perror(bname);
}

dosuspend()
{
	int old, ldisc;
	short ctpgrp;

	if (loginsh)
		error("Can't suspend a login shell (yet)");
	untty();
	old = sigsys(SIGTSTP, SIG_DFL);
	kill(0, SIGTSTP);
	/* the shell stops here */
	sigsys(SIGTSTP, old);
	if (tpgrp != -1) {
retry:
		ioctl(FSHTTY, TIOCGPGRP, &ctpgrp);
		if (ctpgrp != opgrp) {
			old = sigsys(SIGTTIN, SIG_DFL);
			kill(0, SIGTTIN);
			sigsys(SIGTTIN, old);
			goto retry;
		}
		ioctl(FSHTTY, TIOCSPGRP, &shpgrp);
		setpgrp(0, shpgrp);
	}
	ioctl(FSHTTY, TIOCGETD, &oldisc);
	if (oldisc != NTTYDISC) {
		printf("Switching to new tty driver...\n");
		ldisc = NTTYDISC;
		ioctl(FSHTTY, TIOCSETD, &ldisc);
	}
}

doeval(v)
	char **v;
{
	char **oevalvec = evalvec;
	char *oevalp = evalp;
	jmp_buf osetexit;
	int reenter;
	char **gv = 0;

	v++;
	if (*v == 0)
		return;
	gflag = 0; rscan(v, tglob);
	if (gflag) {
		gv = v = glob(v);
		gargv = 0;
		if (v == 0)
			error("No match");
		v = copyblk(v);
	} else
		scan(v, trim);
	getexit(osetexit);
	reenter = 0;
	setexit();
	reenter++;
	if (reenter == 1) {
		evalvec = v;
		evalp = 0;
		process(0);
	}
	evalvec = oevalvec;
	evalp = oevalp;
	doneinp = 0;
	if (gv)
		blkfree(gv);
	resexit(osetexit);
	if (reenter >= 2)
		error(NOSTR);
}
