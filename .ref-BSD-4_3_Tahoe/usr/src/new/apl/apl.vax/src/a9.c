static char Sccsid[] = "a9.c @(#)a9.c	1.4	10/1/82 Berkeley ";
#include "apl.h"
#include <math.h>

ex_dibm()
{
	register j, arg;
	register struct item *p;

	/* Dyadic i-beam functions.  I-beam 63 assumes that the
	 * "empty" system call (check whether pipe empty) has been
	 * implemented in the Unix kernel.
	 */

	arg = topfix();		/* Get left argument */

	switch(topfix()) {

	default:
		error("unknown i-beam");

	case 29: /* Set origin, return old one */
		datum = thread.iorg;
		thread.iorg = arg;
		break;

	case 30: /* Set width, return old one */
		datum = thread.width;
		thread.width = arg;
		break;

	case 31: /* Set number of digits, return old one */
		datum = thread.digits;
		thread.digits = arg;
		break;

	case 34: /* "Nice" system call */
		datum = nice(arg);
		break;

	case 35: /* "Sleep" system call */
		datum = sleep(arg);
		break;

	case 38: /* Sets random seed */
		datum = thread.rl;
		thread.rl = arg;
		srand((int) arg);
		break;
		

	case 63: /* "Empty" system call */
		datum = empty(arg);
		break;

	case 90: /* Enable/disable exit with ")off" only */
		datum = offexit;
		arg = !!arg;
		offexit = arg ? isatty(0) : arg;
		break;

	case 99: /* Buffer flush */
		/* Warning -- information is lost if an input pipe
		 * file descriptor is flushed.  No checking is
		 * made for this i-beam function!!
		 */
#ifdef NBUF
		datum = newbuf(-1, arg);
#else
		datum = zero;		/* No-op if unbuffered */
#endif
		break;

	}

	p = newdat(DA, 0, 1);
	p->datap[0] = datum;
	*sp++ = p;

}

int afnfree, afnused;


ex_mibm()
{
	struct tm *tp, *localtime();
	struct si *gp;
	register struct item *p;
	register struct nlist *np;
	register i;
	long tvec;
	struct {
		long proc_user_time;
		long proc_system_time;
		long child_user_time;
		long child_system_time;
	} t;

	switch(topfix()) {

	default:
		error("unknown i-beam");

	case 20: /* time of day */
		time(&tvec);
		goto tod;

	case 21: /* CPU time */
		times(&t);
		datum = t.proc_user_time+t.proc_system_time;
		break;

	case 22: /* ws bytes unused */
		datum = afnfree;
		break;

	case 24: /* starting time */
		tvec = stime;

	tod:
		tp = localtime(&tvec);
		datum = 60.*(tp->tm_sec+60.*(tp->tm_min+60.*tp->tm_hour));
		break;

	case 25: /* date */
		time(&tvec);
		goto dt;

	case 26:	/* current line */
		datum = (gsip ? gsip->funlc - 1 : 0);
		break;

	case 27: /* vector of line numbers of fn activations # */
		i = 0;
		gp = gsip;
		while(gp){
			if(gp->np)
				i++;
			gp = gp->sip;
		}
		p = newdat(DA, 1, i);
		gp = gsip;
		i = 0;
		while(gp){
			if(gp->np);
				p->datap[i++] = gp->funlc - 1;
			gp = gp->sip;
		}
		*sp++ = p;
		return;

	/*
	 * non standard I functions
	 */

	case 28: /* starting date */
		tvec = stime;

	dt:
		tp = localtime(&tvec);
		datum = tp->tm_year+100.*(tp->tm_mday+100.*(tp->tm_mon+1));
		break;

	case 29: /* iorg */
		datum = thread.iorg;
		break;

	case 30: /* width */
		datum = thread.width;
		break;

	case 31: /* digits */
		datum = thread.digits;
		break;

	case 32: /* ws bytes in use */
		datum = afnused;
		break;

	case 36: /* 2nd element of ib27 */
		datum = ((gsip && gsip->sip) ? gsip->sip->funlc - 1 : 0);
		break;

	case 40: /* Total accumulated child's time */
		times(&t);
		datum = t.child_user_time+t.child_system_time;
		break;

	case 41: /* Total accumulated user time -- including all kids */
		times(&t);
		datum = t.proc_user_time+t.child_user_time;
		break;

	case 42: /* Total system time -- including all kids */
		times(&t);
		datum = t.proc_system_time+t.child_system_time;
		break;

	case 43: /* User time -- parent only */
		times(&t);
		datum = t.proc_user_time;
		break;

	case 44: /* System time -- parent only */
		times(&t);
		datum = t.proc_system_time;
		break;

	case 95: /* dump namelist */
		for (np=nlist; np->namep; np++)
			printf("%s: use=%d, type=%d, itemp=%o, label=%d\n",
			    np->namep, np->use, np->type, np->itemp,
			    np->label);
		datum = 0;
		break;

	case 96:
		dstack();

	case 97:
		datum = (sp - stack) / 2;
		break;

	case 98: /* turn off alloc/free trace */
		datum = aftrace;
		aftrace = 0;
		break;

	case 99: /* turn on alloc/free trace */
		datum = aftrace;
		aftrace = 1;
		break;
	}
	p = newdat(DA, 0, 1);
	p->datap[0] = datum;
	*sp++ = p;
}
