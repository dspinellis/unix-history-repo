#include <signal.h>
#include <errno.h>
/*
 * signal system call interface package.
 */
extern	errno;

#ifdef PDP11
#define BYTESPERVEC	4		/* size of machine language vector */
extern	char	mvectors[NSIG+1][BYTESPERVEC];
#endif

static	int (*cactions[NSIG+1])();	/* saved callers signal actions */
static	char setflg[NSIG+1];		/* =1 means action perm ``sigset'' */
int	(*sigsys())();
int	_sigcatch();

/*
 * old signal protocol.  when signal occurs, further
 * instances of same signal are not blocked, so that
 * recursive signals are possible.  the action will
 * not be re-enabled by these routines when return
 * is made from the interrupt (compare sigset).
 */
int (*
signal(signum, action))()
register int signum;
register int (*action)();
{
	register int (*retval)();

	if (signum <= 0 || signum > NSIG) {
		errno = EINVAL;
		return (BADSIG);
	}
	retval = cactions[signum];
	cactions[signum] = action;
	if (action != SIG_IGN && action != SIG_DFL && action != SIG_HOLD)
		if (SIGISDEFER(action))
#ifdef PDP11
			action = DEFERSIG(mvectors[signum]);
		else
			action = (int (*)())(int)mvectors[signum];
#else
			action = DEFERSIG(_sigcatch);
		else
			action = _sigcatch;
#endif
	action = sigsys(signum, action);
	if (action == SIG_IGN || action == SIG_DFL || action == SIG_HOLD)
		retval = action;
	setflg[signum] = 0;	/* 'tis fleeting (madness may take its toll) */
	return (retval);
}

/*
 * set ``permanent'' action for this signal.  if a function,
 * it will be deferred when interupt occurs and enabled again
 * when return occurs.  after sigset, sighold and sigrelse can
 * be used to protect signum signal critical sections.
 */
int (*
sigset(signum, action))()
register int signum;
register int (*action)();
{
	register int (*retval)();

	if (signum <= 0 || signum > NSIG) {
		errno = EINVAL;
		return (BADSIG);
	}
	retval = cactions[signum];
	cactions[signum] = action;
	if (action != SIG_IGN && action != SIG_DFL && action != SIG_HOLD)
		action = DEFERSIG(_sigcatch);
	action = sigsys(signum, action);
	if (action == SIG_IGN || action == SIG_DFL || action == SIG_HOLD)
		retval = action;
	setflg[signum] = 1;		/* don't want to lose control! */
	return (retval);
}

/*
 * temporarily hold a signal until further notice
 * via sigpause or sigrelse
 */
sighold(signum)
register int signum;
{

	if (signum <= 0 || signum > NSIG) {
		errno = EINVAL;
		return;
	}
	sigsys(signum, SIG_HOLD);
}

/*
 * atomically release the signal and pause
 * if no signals pending.  signal will normally
 * be held on return (unless an routine called at
 * interrupt time resets it).
 */
sigpause(signum)
register signum;
{

	if (signum <= 0 || signum > NSIG || setflg[signum] == 0) {
		errno = EINVAL;
		return;
	}
#ifdef PDP11
	sigsys(signum|SIGDOPAUSE, DEFERSIG(mvectors[signum]));
#else
	sigsys(signum|SIGDOPAUSE, DEFERSIG(_sigcatch));
#endif
}

/*
 * re-enable signals after sighold or possibly after sigpause
 */
sigrelse(signum)
register signum;
{
	if (signum <= 0 || signum > NSIG || setflg[signum] == 0) {
		errno = EINVAL;
		return (-1);
	}
#ifdef PDP11
	sigsys(signum, DEFERSIG(mvectors[signum]));
#else
	sigsys(signum, DEFERSIG(_sigcatch));
#endif
	return (0);
}

/*
 * ignore signal
 */
sigignore(signum)
int signum;
{

	sigsys(signum, SIG_IGN);
}

#ifdef PDP11
/*
 * called at interrupt time.  on pdp11 assembly language
 * routine resets signal catch to returned action, if
 * returned action is not SIG_DFL.
 */
int (*
_sigcatch(signum))()
register signum;
{

	(*cactions[signum])(signum);		/* call the C routine */
	if (setflg[signum])
		return (DEFERSIG(mvectors[signum]);
	else
		return (SIG_DFL);		/* old protocol */
}
#else
/*
 * called at interrupt time.  on vax, sigpeel will peel
 * off the interrupt frames and reenable the signal with
 * the argument action (cleanly).  if the action is
 * SIG_DFL sigpeel will do nothing, but we can as well
 * return ourselves without its help.
 */
_sigcatch(signum, code)
register signum;
int code;
{

	(*cactions[signum])(signum);
	if (setflg[signum]) {
		sigpeel(signum, DEFERSIG(_sigcatch));
		/*NOTREACHED*/
	}
	/* old protocol, just return */
}
#endif
