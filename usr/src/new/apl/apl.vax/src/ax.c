static char Sccsid[] = "ax.c @(#)ax.c	1.1	10/1/82 Berkeley ";
#

/*
 *	Floating-point initialization and trap service
 *
 *	On the PDP-11's the code in "ax.pdp.s" is preferred
 *	since it enables the FPP for all floating exception
 *	traps and fishes out the cause of an exception from
 *	the FPP error register.
 */

#include <signal.h>

#ifdef vax
char *fpelist[] = {
	"floating exception",
	"integer overflow",
	"integer divide by zero",
	"floating overflow",
	"floating divide by zero",
	"floating underflow",
	"decimal overflow",
	"subscript range",
	"floating overflow",
	"floating divide by zero",
	"floating underflow"
};

fpe(signo, param)
unsigned param;
{
	signal(SIGFPE, fpe);
	if (param >= sizeof fpelist/sizeof fpelist[0])
		error("floating exception");
	else
		error(fpelist[param]);
}
#else
fpe()
{
	signal(SIGFPE, fpe);
	error("floating exception");
}
#endif

fppinit()
{
	signal(SIGFPE, fpe);
}
