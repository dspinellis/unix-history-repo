/* Copyright (c) 1979 Regents of the University of California */
#include "sh.h"

/*
 * C shell
 */

execute(t, pipein, pipeout)
	register struct command *t;
	int *pipein, *pipeout;
{
	int pid, flags, pv[2];
	register struct command *t1;
	register char *cp;
	bool forked = 0;
	bool shudint, shudhup;
#ifdef VFORK
	int (*savint)(), vffree();
	int ochild, osetintr, ohaderr, otimflg, odidfds, odidcch;
	int oSHIN, oSHOUT, oSHDIAG, oOLDSTD;
	int isvfork = 0;
#endif

	if (t == 0)
		return;
	switch (t->t_dtyp) {

	case TCOM:
		cp = t->t_dcom[0];
		if ((cp[0] & (QUOTE|TRIM)) == QUOTE)
			strcpy(cp, cp + 1);
		if ((t->t_dflg & FREDO) == 0)
			Dfix(t);		/* $ " ' \ */
		/* fall into... */

	case TPAR:
		flags = t->t_dflg;
		if (flags & FPOU)
			mypipe(pipeout);
		/*
		 * A child will be interruptible only under very
		 * certain conditions:
		 *	we must be monkeying with interrupts
		 *	the child must not be &'ed
		 *	we must not have had an "onintr -"
		 */
		shudint = setintr && (flags & FINT) == 0 && (!gointr || !eq(gointr, "-"));
		shudhup = (flags & FAND) == 0;

		/*
		 * Must do << early so parent will know
		 * where input pointer should be
		 */
		if (flags & FHERE)
			close(0), heredoc(t->t_dlef);

		/*
		 * If not executing commands then
		 * all we must do is read forward in the input to
		 * account for << redirection if present.
		 */
		if (noexec) {
			if (flags & FHERE)
				close(0);
			return;
		}

		set("status", "0");
		pid = 0;

		/*
		 * Built-in functions
		 */
		if (t->t_dtyp == TCOM && isbfunc(t->t_dcom[0])) {
			/*
			 * If output is piped, or running & and we would
			 * eventually fork for non-builtin commands,
			 * then do it now, so we won't block.
			 */
			if ((flags & (FPOU|FAND)) && (flags & FPAR) == 0)
				pid = dofork(shudint, shudhup), forked++;

			/*
			 * If the builtin is actually executed (some, e.g.
			 * time and nice may refuse to execute here)
			 * then either exit (if we forked) or close i/o
			 * and continue execution (if we didn't).
			 */
			if (pid == 0) {
				doio(t, pipein, pipeout);
				if (flags & FPOU) {
					close(pipeout[0]), close(pipeout[1]);
					pipeout[0] = pipeout[1] = -1;
				}
				if (setintr && forked) {
					if (shudint)
						signal(SIGINT, SIG_DFL), signal(SIGQUIT, SIG_DFL);
					signal(SIGTERM, parterm);
					if (flags & FINT)
						setintr = 0;
				}
				if (func(t, pipein, pipeout)) {
					if (forked)
						exitstat();
					if (didfds && !(t->t_dflg & FREDO))
						donefds();
					return;
				}
			}
		}

		/*
		 * Now, we must make a new process since either the
		 * command is non-builtin, a parenthesized list,
		 * or builtin such as time or nice which really
		 * requires a child.
		 */
		if (!forked && (flags & FPAR) == 0)
#ifdef VFORK
			if (t->t_dtyp == TPAR || (flags&FREDO) ||
			    eq(t->t_dcom[0], "nice") || eq(t->t_dcom[0], "nohup"))
#endif
				pid = dofork(shudint, shudhup);
#ifdef VFORK
			else {
				savint = signal(SIGINT, SIG_IGN);
				ochild = child; osetintr = setintr;
				ohaderr = haderr; otimflg = timflg;
				odidfds = didfds; odidcch = didcch;
				oSHIN = SHIN; oSHOUT = SHOUT;
				oSHDIAG = SHDIAG; oOLDSTD = OLDSTD;
				Vsav = Vdp = 0; Vav = 0;
				isvfork++;
				pid = vfork();
				if (pid < 0) {
					signal(SIGINT, savint);
					error("No more processes");
				}
				if (pid == 0) {
					child++;
					signal(SIGINT, shudint ? SIG_DFL : savint);
					if (!shudhup)
						signal(SIGHUP, SIG_IGN);
				} else {
					child = ochild; setintr = osetintr;
					haderr = ohaderr; timflg = otimflg;
					didfds = odidfds; didcch = odidcch;
					SHIN = oSHIN; SHOUT = oSHOUT;
					SHDIAG = oSHDIAG; OLDSTD = oOLDSTD;
					xfree(Vsav), Vsav = 0;
					xfree(Vdp), Vdp = 0;
					xfree(Vav), Vav = 0;
					signal(SIGINT, savint);
				}
			}
#endif
		if (pid != 0) {
			/*
			 * The parent path (or nobody does this if
			 * (flags & FPAR), i.e. date in (set;date))
			 */
			if (didfds == 0 && (flags & FPIN))
				close(pipein[0]), close(pipein[1]);
			if (didfds && !(t->t_dflg & FREDO))
				donefds();
			if (flags & FPRS)
				printf("%d\n", pid), set("child", putn(pid));
			/*
			 * Unless output is piped or command is &
			 * wait for it.
			 */
			if (t->t_dtyp == TCOM)
				cadd(pid, t->t_dcom[0]);
			else
				cadd(pid, "()");
			if ((flags & (FPOU|FAND)) == 0)
				pwait(pid);
			return;
		}

		/*
		 * Insure that this (child) shell doesn't muck on
		 */
		child++;

		/*
		 * If havent yet, finally set up the file descriptors.
		 */
		doio(t, pipein, pipeout);
		if (flags & FPOU)
			close(pipeout[0]), close(pipeout[1]);

		/*
		 * If mucking with interrupts fix interrupt, quit,
		 * and terminate handling ... in any case set setintr
		 * to 0 if we are not interruptible so that no further
		 * interrupt mucking occurs.
		 */
		if (setintr) {
			if (shudint) {
				signal(SIGQUIT, SIG_DFL);
#ifdef VFORK
				if (isvfork)
					signal(SIGINT, vffree);
				else
#endif
					signal(SIGINT, SIG_DFL);
			}
			signal(SIGTERM, parterm);
			if (flags & FINT)
				setintr = 0;
		}

		/*
		 * For () commands must put new 0,1,2 in FSH* and recurse
		 */
		if (t->t_dtyp == TPAR) {
			t1 = t->t_dspr;
			t1->t_dflg |= flags & FINT;
			OLDSTD = dcopy(0, FOLDSTD);
			SHOUT = dcopy(1, FSHOUT);
			SHDIAG = dcopy(2, FSHDIAG);
			close(SHIN), SHIN = -1;
			didcch = 0, didfds = 0;
			execute(t1);
			exitstat();
		}
		if (eq(t->t_dcom[0], "nice")) {
/* sigh...
			nice(20);
			nice(-10);
*/
			cp = t->t_dcom[1];
			if (any(cp[0], "+-"))
				nice(getn(cp)), lshift(t->t_dcom, 2);
			else
				nice(4), lshift(t->t_dcom, 1);
			t->t_dflg = FPAR | FREDO;
			execute(t);
			exitstat();
		}
		if (eq(t->t_dcom[0], "nohup")) {
			if (setintr == 0)
				signal(SIGHUP, SIG_IGN);
			signal(SIGTERM, SIG_IGN);
			lshift(t->t_dcom, 1);
			t->t_dflg = FPAR | FREDO;
			execute(t);
			exitstat();
		}
		doexec(t);
		/* no return */

	case TFIL:
		flags = t->t_dflg;
		t1 = t->t_dcar;
		t1->t_dflg |= FPOU | (flags & (FPIN|FINT|FPRS|FDIAG));
		execute(t1, pipein, pv);
		t1 = t->t_dcdr;
		t1->t_dflg |= FPIN | (flags & (FPOU|FINT|FAND|FPRS|FPAR));
		execute(t1, pv, pipeout);
		return;

	case TLST:
		flags = t->t_dflg & FINT;
		if (t1 = t->t_dcar)
			t1->t_dflg |= flags, execute(t1);
		if (t1 = t->t_dcdr)
			t1->t_dflg |= t->t_dflg & (FINT|FPAR), execute(t1);
		return;

	case TOR:
	case TAND:
		flags = t->t_dflg & FINT;
		if (t1 = t->t_dcar) {
			t1->t_dflg |= flags, execute(t1);
			if ((getn(value("status")) == 0) == (t->t_dtyp == TAND))
				return;
		}
		if (t1 = t->t_dcdr)
			t1->t_dflg |= t->t_dflg & (FINT|FPAR), execute(t1);
		return;
	}
}

#ifdef VFORK
vffree()
{
	register char **v;

	if (v = gargv)
		gargv = 0, xfree(gargv);
	if (v = pargv)
		pargv = 0, xfree(pargv);
	_exit(1);
}
#endif

doio(t, pipein, pipeout)
	register struct command *t;
	int *pipein, *pipeout;
{
	register char *cp;
	register int flags = t->t_dflg;
	char *dp;

	if (didfds || (flags & FREDO))
		return;
	if (flags & FHERE)
		goto skipin;
	close(0);
	if (cp = t->t_dlef) {
		cp = globone(dp = Dfix1(cp));
		xfree(dp);
		xfree(cp);
		if (open(cp, 0) < 0)
			Perror(cp);
	} else if (flags & FPIN)
		dup(pipein[0]), close(pipein[0]), close(pipein[1]);
	else if (flags & FINT)
		close(0), open("/dev/null", 0);
	else
		dup(OLDSTD);

skipin:
	close(1);
	if (cp = t->t_drit) {
		cp = globone(dp = Dfix1(cp));
		xfree(dp);
		xfree(cp);
		if ((flags & FCAT) && open(cp, 1) >= 0)
			lseek(1, 0l, 2);
		else {
			if (!(flags & FANY) && adrof("noclobber")) {
				if (flags & FCAT)
					Perror(cp);
				chkclob(cp);
			}
#ifdef V6
			if (creat(cp, 0644) < 0)
				Perror(cp);
#else
			if (creat(cp, 0666) < 0)
				Perror(cp);
#endif
		}
	} else
		dup((flags & FPOU) ? pipeout[1] : SHOUT);

	close(2);
	dup((flags & FDIAG) ? 1 : SHDIAG);
	didfds = 1;
}

dofork(shudint, shudhup)
	bool shudint, shudhup;
{
	register int pid, (*savint)();

	savint = signal(SIGINT, SIG_IGN);
	pid = fork();
	if (pid < 0) {
		signal(SIGINT, savint);
		error("No more processes");
	}
	if (pid == 0) {
		child++;
		signal(SIGINT, shudint ? SIG_DFL : savint);
		if (!shudhup)
			signal(SIGHUP, SIG_IGN);
	} else
		signal(SIGINT, savint);
	return (pid);
}

mypipe(pv)
	register int *pv;
{

	if (pipe(pv) < 0)
		goto oops;
	pv[0] = dmove(pv[0], -1);
	pv[1] = dmove(pv[1], -1);
	if (pv[0] >= 0 && pv[1] >= 0)
		return;
oops:
	error("Can't make pipe");
}

chkclob(cp)
	register char *cp;
{
	struct stat stb;

	if (stat(cp, &stb) < 0)
		return;
	if ((stb.st_mode & S_IFMT) == S_IFCHR)
		return;
	error("%s: File exists", cp);
}
