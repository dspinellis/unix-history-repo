/*
char id_signal[] = "@(#)signal_.c	1.3";
 *
 * change the action for a specified signal
 *
 * calling sequence:
 *	integer cursig, signal, savsig
 *	external proc
 *	cursig = signal(signum, proc, flag)
 * where:
 *	'cursig' will receive the current value of signal(2)
 *	'signum' must be in the range 0 <= signum <= 16
 *
 *	If 'flag' is negative, 'proc' must be an external proceedure name.
 *	
 *	If 'flag' is 0 or positive, it will be passed to signal(2) as the
 *	signal action flag. 0 resets the default action; 1 sets 'ignore'.
 *	'flag' may be the value returned from a previous call to signal.
 *
 * This routine arranges to trap user specified signals so that it can
 * pass the signum fortran style - by address. (boo)
 */

#include	"../libI77/f_errno.h"

int (*dispatch[17])();
int (*signal())();
int sig_trap();

long signal_(sigp, procp, flag)
long *sigp, *flag;
int (*procp)();
{
	int (*oldsig)();
	int (*oldispatch)();

	oldispatch = dispatch[*sigp];

	if (*sigp < 0 || *sigp > 16)
		return(-((long)(errno=F_ERARG)));

	if (*flag < 0)	/* function address passed */
	{
		dispatch[*sigp] = procp;
		oldsig = signal((int)*sigp, sig_trap);
	}

	else		/* integer value passed */
		oldsig = signal((int)*sigp, (int)*flag);

	if (oldsig == sig_trap)
		return((long)oldispatch);
	return((long)oldsig);
}

sig_trap(sn)
int sn;
{
	long lsn = (long)sn;
	return((*dispatch[sn])(&lsn));
}
