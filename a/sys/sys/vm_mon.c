/*	vm_mon.c	4.9	82/10/31	*/

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/vmmeter.h"
#include "../h/trace.h"

#ifdef PGINPROF

int pmonmin = PMONMIN;
int pres = PRES;
int rmonmin = RMONMIN;
int rres = RRES;

vmsizmon()
{
	register int i;

	i = (u.u_dsize / DRES) < NDMON ? (u.u_dsize / DRES):NDMON;
	dmon[i] += u.u_ru.ru_utime.tv_sec - u.u_outime;

	i = (u.u_ssize / SRES) < NSMON ? (u.u_ssize / SRES):NSMON;
	smon[i] += u.u_ru.ru_utime.tv_sec - u.u_outime;
	u.u_outime = u.u_ru.ru_utime.tv_sec;
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
	*tracep++ = (time.tv_sec%1000)*1000 + (time.tv_usec/1000);
	nargs--;
	do
		*tracep++ = *argp++;
	while (--nargs > 0);
}
#endif
