/*

 *      Copyright (c) 1984, 1985, 1986 AT&T
 *      All Rights Reserved

 *      THIS IS UNPUBLISHED PROPRIETARY SOURCE 
 *      CODE OF AT&T.
 *      The copyright notice above does not 
 *      evidence any actual or intended
 *      publication of such source code.

 */
/* @(#)fault.c	1.1 */
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
#include	"brkincr.h"
#include	"stak.h"
#include	"sym.h"
#include	"jobs.h"
#include	"timeout.h"


/* until the bug is fixed */
#define VOID	int

VOID	fault();
void	chktrap();
void	stdsig();
int	ignsig();
void	getsig();
void	oldsig();
void	clrsig();

extern VOID	done();
extern void	exitsh();
extern void	failed();
extern void	free();
extern void	p_str();
extern void	p_flush();
extern void	setbrk();

#ifdef VFORK
char trapflg[MAXTRAP+1];
#else
static char trapflg[MAXTRAP+1];
#endif	/* VFORK */

/* ========	fault handling routines	   ======== */


VOID	fault(sig)
register int 	sig;
{
	register int 	flag;
#ifdef JOBS
#ifndef BSD
	if(sig==SIGCLD)
	{
		trapnote |= SIGJOBS;
		return;
	}
#endif	/* BSD */
#endif	/* JOBS */
	signal(sig, fault);
	if(sig==SIGSEGV)
		setbrk(BRKINCR);
	if(sig==SIGALRM)
	{
		if((states&WAITING) && timeout>0)
		{
			if(states&RWAIT)
			{
				/* force exit */
					states |= FORKED;
					error(timed_out);
			}
			else
			{
				states |= RWAIT;
				alarm(TGRACE);
				p_str(time_warn,NL);
				p_flush();
			}
		}
	}
#ifdef JOBS
#ifdef BSD
	else if(sig==SIGCHLD || sig==SIGTSTP || sig==SIGTTIN || sig==SIGTTOU)
		trapnote |= SIGJOBS;
#endif	/* BSD */
#endif	/* JOBS */
	else
	{
		flag = (trapcom[sig] ? TRAPSET : SIGSET);
		trapnote |= flag;
		trapflg[sig] |= flag;
		if(sig <= SIGQUIT)
			trapnote |= SIGSLOW;
	}
#ifdef JOBS
#ifdef BSD
	/* This is needed because broken reads automatically restart */
	if(states&READC)
		interrupt();
#endif	/* BSD */
#endif	/* JOBS */
}

void stdsigs()
{
	register int i;
	register int n;
	register SYSPTR	syscan = signal_names;
	while(*syscan->sysnam)
	{
		n = syscan->sysval;
		i = n&((1<<SIGBITS)-1);
		n >>= SIGBITS;
		trapflg[--i] = n;
		if((n&(SIGIGNORE|SIGNOSET))==0 && ignsig(i)==0)
			signal(i,(n&SIGCAUGHT?fault:done));
		else if(i==SIGQUIT)
			ignsig(SIGQUIT);
		syscan++;
	}
	syscan = sig_messages;
	while(n=syscan->sysval)
	{
		if(*syscan->sysnam)
			sysmsg[n-1] = syscan->sysnam;
		syscan++;
	}
}

/*
 * set signal n to ignore
 * returns 1 if signal was already ignored, 0 otherwise
 */
int	ignsig(n)
register int n;
{
	register int 	s;
	if((s=(signal(n,SIG_IGN)==SIG_IGN)) == 0)
		trapflg[n] |= SIGMOD;
	return(s);
}

void	getsig(n)
register int n;
{
	if(trapflg[n]&SIGMOD || ignsig(n)==0)
		signal(n,fault);
}

void	oldsigs()
{
	register int 	i;
	register char *t;
	i=MAXTRAP+1;
	while(i--)
	{
		t=trapcom[i];
		if(t==0 || *t)
		{
#ifdef VFORK
			/* don't free the trap string */
			if(states&VFORKED);
				trapcom[i] = 0;
#endif /* VFORK */
			 clrsig(i);
		}
		trapflg[i]=0;
	}
	trapnote=0;
}

void	clrsig(n)
register int 	n;
{
	if(trapcom[n])
	{
		free(trapcom[n]);
		trapcom[n]=0;
	}
	if(trapflg[n]&SIGMOD)
	{
		if(trapflg[n]&SIGCAUGHT)
			signal(n, fault);
		else if(trapflg[n]&SIGIGNORE)
			 signal(n, SIG_DFL);
		else
			signal(n, done);
		trapflg[n] &= ~SIGMOD;
	}
}


/*
 * check for traps
 */

void	chktrap()
{
	register int 	i=MAXTRAP+1;
	register char *t;
	trapnote &= ~(TRAPSET|SIGSLOW);
	if(states&ERRFLG)
	{
		if(is_option(ONEFLG))
			exitsh(exitval);
		else if(exitval)
		{
			if(trapcom[MAXTRAP])
				trapflg[MAXTRAP] = TRAPSET;
			if(is_option(ERRFLG))
				exitsh(exitval);
		}
	}
	while(--i)
	{
		if(trapflg[i]&TRAPSET)
		{
			trapflg[i] &= ~TRAPSET;
			if(t=trapcom[i])
			{
				int savxit=exitval;
				execexp(t,(FILE*)0);
				exitval=savxit;
				exitset();
			}
		}
	}
}
