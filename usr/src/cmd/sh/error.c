#
/*
 * UNIX shell
 *
 * S. R. Bourne
 * Bell Telephone Laboratories
 *
 */

#include	"defs.h"


/* ========	error handling	======== */

exitset()
{
	assnum(&exitadr,exitval);
}

sigchk()
{
	/* Find out if it is time to go away.
	 * `trapnote' is set to SIGSET when fault is seen and
	 * no trap has been set.
	 */
	IF trapnote&SIGSET
	THEN	exitsh(SIGFAIL);
	FI
}

failed(s1,s2)
	STRING	s1, s2;
{
	prp(); prs(s1); 
	IF s2
	THEN	prs(colon); prs(s2);
	FI
	newline(); exitsh(ERROR);
}

error(s)
	STRING	s;
{
	failed(s,NIL);
}

exitsh(xno)
	INT	xno;
{
	/* Arrive here from `FATAL' errors
	 *  a) exit command,
	 *  b) default trap,
	 *  c) fault with no trap set.
	 *
	 * Action is to return to command level or exit.
	 */
	exitval=xno;
	IF (flags & (forked|errflg|ttyflg)) != ttyflg
	THEN	done();
	ELSE	clearup();
		longjmp(errshell,1);
	FI
}

done()
{
	REG STRING	t;
	IF t=trapcom[0]
	THEN	trapcom[0]=0; /*should free but not long */
		execexp(t,0);
	FI
	rmtemp(0);
	exit(exitval);
}

rmtemp(base)
	IOPTR		base;
{
	WHILE iotemp>base
	DO  unlink(iotemp->ioname);
	    iotemp=iotemp->iolst;
	OD
}
