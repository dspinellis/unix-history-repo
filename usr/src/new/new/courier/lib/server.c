#ifndef lint
static char sccsid[] = "@(#)server.c	4.2 (Berkeley) 7/7/83";
#endif

#include <stdio.h>
#include <sys/time.h>
#include "courier.h"

#if DEBUG
int CourierServerDebuggingFlag = 0;
#endif

/*
 * Message stream handle.
 */
static int Connection = -1;

/*
 * Information about user-specified I/O dispatch function.
 */
static void (*selectfunc)() = 0;
static int readmask, writemask, exceptmask, blockflag;
static struct timeval timeout;

Unspecified *
ReceiveCallMessage(procp)
	Cardinal *procp;
{
	Cardinal proc, n, nwords;
	Unspecified *bp;

	if (ServerRead(&proc, 1) == 0)
		exit(0);				/* clean EOF */
	if (ServerRead(&nwords, 1) == 0)
		goto eof;
	UnpackCardinal(procp, &proc);
	UnpackCardinal(&n, &nwords);
#if DEBUG
	if (CourierServerDebuggingFlag)
		fprintf(stderr, "[ReceiveCallMessage %d %d]\n", *procp, n);
#endif
	bp = Allocate(n);
	if (ServerRead(bp, n) == 0)
		goto eof;
	return (bp);
eof:
	exit(1);
}

SendReturnMessage(n, results)
	Cardinal n;
	Unspecified *results;
{
	Cardinal nwords;

#if DEBUG
	if (CourierServerDebuggingFlag)
		fprintf(stderr, "[SendReturnMessage %d]\n", n);
#endif
	PackCardinal(&n, &nwords, 1);
	ServerWrite(&nwords, 1);
	ServerWrite(results, n);
}

NoSuchProcedureValue(prog_name, proc)
	String prog_name;
	Cardinal proc;
{
	fprintf(stderr, "Courier program %s: no such procedure value %d\n",
		prog_name, proc);
}

static
ServerRead(addr, nwords)
	char *addr;
	int nwords;
{
	register int nbytes, n;
	register char *p;
	int rbits, wbits, ebits;

	for (p = addr, nbytes = 2*nwords; nbytes > 0; nbytes -= n, p += n) {
		while (selectfunc != 0) {
			rbits = (1 << Connection) | readmask;
			wbits = writemask;
			ebits = exceptmask;
			if (blockflag)
				select(32, &rbits, &wbits, &ebits, 0);
			else
				select(32, &rbits, &wbits, &ebits, &timeout);
			if (rbits & (1 << Connection))
				break;
			rbits &= ~(1 << Connection);
			(*selectfunc)(rbits, wbits, ebits);
		}
		n = read(Connection, p, nbytes);
		if (n <= 0)
			return (0);
	}
	return (1);
}

static
ServerWrite(addr, nwords)
	char *addr;
	int nwords;
{
	int rbits, wbits, ebits;

	for (;;) {
		rbits = readmask;
		wbits = (1 << Connection) | writemask;
		ebits = exceptmask;
		if (blockflag)
			select(32, &rbits, &wbits, &ebits, 0);
		else
			select(32, &rbits, &wbits, &ebits, &timeout);
		if (wbits & (1 << Connection))
			break;
		wbits &= ~(1 << Connection);
		(*selectfunc)(rbits, wbits, ebits);
	}
	write(Connection, addr, 2*nwords);
	return (1);
}

CourierSelect(func, in, out, except, timelimit)
	void (*func)();
	int in, out, except;
	struct timeval *timelimit;
{
	if ((selectfunc = func) != 0) {
		readmask = in;
		writemask = out;
		exceptmask = except;
		if (!(blockflag = (timelimit == 0)))
			timeout = *timelimit;
	}
}

ServerInit()
{
	write(1, "\0", 1);
	Connection = 20;
	while (dup2(0, Connection) < 0)
		Connection--;
	close(0); close(1);
}

main()
{
	Server();
}
