/*

 *      Copyright (c) 1984, 1985, 1986 AT&T
 *      All Rights Reserved

 *      THIS IS UNPUBLISHED PROPRIETARY SOURCE 
 *      CODE OF AT&T.
 *      The copyright notice above does not 
 *      evidence any actual or intended
 *      publication of such source code.

 */
/* @(#)error.c	1.1 */
/*
 * UNIX shell
 *
 * S. R. Bourne
 * Rewritten by David Korn
 * AT&T Bell Laboratories
 *
 */

#include	"flags.h"
#include	"defs.h"
#include	"io.h"
#include	"brkincr.h"
#include	"jobs.h"
#include	"sym.h"

/* These routines are defined by this module */
void	exitsh();
void	done();
void	failed();
void	rmtemp();

/* These routines are used by this module but defined elsewhere */
extern void	arg_clear();
extern void	chktrap();
extern void	free();
extern void	hist_flush();
extern char	*movstr();
extern void	name_unscope();
extern void	p_flush();
extern void	p_prp();
extern void	p_setout();
extern void	p_str();
extern void	restore();
extern void	rm_files();
extern void	setcooked();
#ifdef VFORK
extern void	vfork_restore();
#endif	/* VFORK */

/* ========	error handling	======== */

	/* Find out if it is time to go away.
	 * `trapnote' is set to SIGSET when fault is seen and
	 * no trap has been set.
	 */

/*
 *  This routine is called when fatal errors are encountered
 *  A message is printed out and the shell tries to exit
 */

void failed(s1,s2)
register char *s1,*s2;
{
	p_setout(stderr);
	p_prp(s1,s2?':':NL);
	if(s2)
	{
		putc(SP,output);
		p_str(s2,NL);
	}
	exitsh(ERROR);
}

/* Arrive here from `FATAL' errors
 *  a) exit command,
 *  b) default trap,
 *  c) fault with no trap set.
 *
 * Action is to return to command level or exit.
 */

void exitsh(xno)
int xno;
{
	register unsigned state=(states&~(ERRFLG|MONITOR));
	exitval=xno;
	if(state&BUILTIN)
		longjmp(*freturn,1);
	state |= is_option(ERRFLG|MONITOR);
	if((state&(ERRFLG|FORKED|TTYFLG)) != TTYFLG)
	{
		states = state;
		done(0);
	}
	else
	{
		if((state&FUNCTION)==0)
		{
			p_flush();
			/* flush out input buffer */
			setbuf(input,input->_base);
			name_unscope();
			arg_clear();
			restore(0);
		}
#ifdef VFORK
		vfork_restore();
#endif	/* VFORK */
		execbrk = breakcnt = 0;
		aliflg = 0;
		exec_flag = 0;
		hist_flush();
#ifdef JOBS
		state &= ~(FUNCTION|FIXFLG|NONSTOP|READC|RWAIT|PROMPT|READPR|MONITOR|BUILTIN|VFORKED);
		state |= is_option(INTFLG|READPR|MONITOR);
		jobstat.j_flag = 0;
#else
		state &= ~(FUNCTION|FIXFLG|RWAIT|PROMPT|READPR|BUILTIN);
		state |= is_option(INTFLG|READPR);
#endif	/* JOBS */
		states = state;
		longjmp(*freturn,1);
	}
}

/*
 * This is the exit routine for the shell
 */

void done(sig)
register int sig;
{
	register char *t;
	register int savxit = exitval;
	if(t=trapcom[0])
	{
		trapcom[0]=0; /*should free but not long */
		execexp(t,(FILE*)0);
	}
	else
	{
		/* avoid recursive call for set -e */
		states &= ~ERRFLG;
		chktrap();
	}
	rmtemp((IOPTR)0);
#ifdef ACCT
	doacct();
#endif	/* ACCT */
#if VSH || ESH
	if(is_option(EMACS|EDITVI|GMACS) && standin->fstak==0)
		setcooked(fileno(input));
#endif
	if(states&RM_TMP)
	/* clean up all temp files */
		rm_files(tmpout);
	p_flush();
#ifdef JOBS
	if(sig==SIGHUP || (is_option(INTFLG)&&(getppid()==1)))
		kill_all();
#endif	/* JOBS */
	if(sig)
	{
		/* generate fault termination code */
		signal(sig,SIG_DFL);
#ifdef BSD_4_2
		sigrelse(sig);
#endif	/* BSD_4_2 */
		kill(getpid(),sig);
		pause();
	}
	exit(savxit);
}

/*
 * remove temporary files
 */

void	rmtemp(base)
IOPTR 	base;
{
	register IOPTR iop = iotemp;
	while(iop>base)
	{
		unlink(iop->ioname);
		free(iop->iolink);
		iop=iop->iolst;
	}
	iotemp = iop;
}
