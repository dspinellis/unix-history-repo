#include <arpa/telnet.h>
#include <sys/types.h>

#include "ring.h"

#include "externs.h"
#include "types.h"

Ring	ttyoring;
char	ttyobuf[2*BUFSIZ];

char
    termEofChar,
    termEraseChar,
    termFlushChar,
    termIntChar,
    termKillChar,
    termLiteralNextChar,
    termQuitChar;

/*
 * initialize the terminal data structures.
 */

init_terminal()
{
    ring_init(&ttyoring, ttyobuf, sizeof ttyobuf);
    autoflush = TerminalAutoFlush();
}


/*
 *		Send as much data as possible to the terminal.
 *
 *		The return value indicates whether we did any
 *	useful work.
 */


int
ttyflush(drop)
int drop;
{
    int n;

    if ((n = ring_full_consecutive(&ttyoring)) > 0) {
	if (drop) {
	    TerminalFlushOutput();
	    /* we leave 'n' alone! */
	} else {
	    n = TerminalWrite(tout, ttyoring.consume, n);
	}
    }
    if (n >= 0) {
	ring_consumed(&ttyoring, n);
    }
    return n > 0;
}


#if	defined(unix)
/*
 * Various signal handling routines.
 */

void
deadpeer()
{
	setcommandmode();
	longjmp(peerdied, -1);
}

void
intr()
{
    if (localchars) {
	intp();
	return;
    }
    setcommandmode();
    longjmp(toplevel, -1);
}

void
intr2()
{
    if (localchars) {
	sendbrk();
	return;
    }
}

void
doescape()
{
    command(0);
}
#endif	/* defined(unix) */

/*
 * These routines decides on what the mode should be (based on the values
 * of various global variables).
 */


int
getconnmode()
{
    static char newmode[16] =
			{ 4, 5, 3, 3, 2, 2, 1, 1, 6, 6, 6, 6, 6, 6, 6, 6 };
    int modeindex = 0;

    if (dontlecho && (clocks.echotoggle > clocks.modenegotiated)) {
	modeindex += 1;
    }
    if (hisopts[TELOPT_ECHO]) {
	modeindex += 2;
    }
    if (hisopts[TELOPT_SGA]) {
	modeindex += 4;
    }
    if (In3270) {
	modeindex += 8;
    }
    return newmode[modeindex];
}

void
setconnmode()
{
    TerminalNewMode(tin, tout, getconnmode());
}


void
setcommandmode()
{
    TerminalNewMode(tin, tout, 0);
}
