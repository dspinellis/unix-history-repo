/*	vmmon.c	4.6	81/04/28	*/

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/vmmeter.h"
#include "../h/trace.h"
#include "../h/mtpr.h"

#ifdef PGINPROF

int pmonmin = PMONMIN;
int pres = PRES;
int rmonmin = RMONMIN;
int rres = RRES;

vmsizmon()
{
	register int i;

	i = (u.u_dsize / DRES) < NDMON ? (u.u_dsize / DRES):NDMON;
	dmon[i] += u.u_vm.vm_utime - u.u_outime;

	i = (u.u_ssize / SRES) < NSMON ? (u.u_ssize / SRES):NSMON;
	smon[i] += u.u_vm.vm_utime - u.u_outime;
	u.u_outime = u.u_vm.vm_utime;
}

vmfltmon(hist, atime, amin, res, nmax)
	register unsigned int *hist;
	register int atime, amin, res, nmax;
{
	register int i;

	i = (atime - amin) / res;
	if (i>=0 && i<nmax)
		hist[i+1]++;
	else 
		i<0 ? hist[0]++ : hist[nmax+1]++;
}
#endif

#ifdef TRACE
/*VARARGS*/
trace1(args)
	int args;
{
	register int nargs;
	register int x;
	register int *argp, *tracep;

	nargs = 4;
	x = tracex % TRCSIZ;
	if (x + nargs >= TRCSIZ) {
		tracex += (TRCSIZ - x);
		x = 0;
	}
	argp = &args;
	tracep = &tracebuf[x];
	tracex += nargs;
	*tracep++ = (time*60 + lbolt+1)*16667 + mfpr(ICR);
	nargs--;
	do
		*tracep++ = *argp++;
	while (--nargs > 0);
}
#endif
