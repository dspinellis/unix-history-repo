/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)except.c 1.6 6/10/81";

#include 	<signal.h>
#include	"whoami.h"
#include	"panics.h"
#include	"vars.h"

/*
 * Routine panic is called from the interpreter when a runtime panic occurs.
 * Its argument is the internal number of the panic which occurred.
 */
panic(errnum)
	int	errnum;
{
	signal(SIGINT,SIG_IGN);
	signal(SIGSEGV,SIG_DFL);
	signal(SIGFPE,SIG_DFL);
	signal(SIGTRAP,SIG_DFL);
	signal(SIGILL,SIG_DFL);
	signal(SIGBUS,SIG_DFL);
	signal(SIGSYS,SIG_DFL);
	PFLUSH();
	fputs("\n\n",stderr);
	switch (errnum) {
		case PINTR:
		case PLIBERR:
			break;
		case PNUMOVFLO:
			fputs("Overflow arithmetic operation\n",stderr);
			break;
		case PHALT:
			if (_nodump == TRUE)
				psexit(0);
			fputs("Call to procedure halt\n",stderr);
			backtrace(errnum);
			/* pfree(objprog); */
			psexit(0);
		case PSTKOVFLO:
			fputs("Run time stack overflow\n",stderr);
			break;
		case PSYSTEM:
			fputs("Panic: Computational error in interpreter\n",
				stderr);
			break;
		case PSTKNEMP:
			fputs("Panic: stack not empty between statements\n",
				stderr);
			break;
		case PBADOP:
			fputs("Panic: bad op code\n",stderr);
			break;
		default:
			fputs("Panic: unknown error\n",stderr);
	}
	backtrace(errnum);
	/* pfree(objprog); */
	psexit(errnum);
}

liberr()
{
	panic(PLIBERR);
}

intr()
{
	signal(SIGINT, intr);
	panic(PINTR);
}

memsize()
{
	signal(SIGSEGV, memsize);
	panic(PSTKOVFLO);
}

syserr(signum)
	int signum;
{
	signal(signum, syserr);
	panic(PSYSTEM);
}

except()
{
	signal(SIGFPE, except);
	panic(PNUMOVFLO);
}
