#include <stdio.h>
#include <setjmp.h>

#define	SUBBUFSIZE	100

extern int errno;		/* outside this world */

extern int
	flushout,		/* flush output */
	connected,		/* Are we connected to the other side? */
	globalmode,		/* Mode tty should be in */
	In3270,			/* Are we in 3270 mode? */
	telnetport,		/* Are we connected to the telnet port? */
	localchars,		/* we recognize interrupt/quit */
	donelclchars,		/* the user has set "localchars" */
	showoptions,
	net,
	tin,
	tout,
	crlf,		/* Should '\r' be mapped to <CR><LF> (or <CR><NUL>)? */
	autoflush,		/* flush output when interrupting? */
	autosynch,		/* send interrupt characters with SYNCH? */
	SYNCHing,		/* Is the stream in telnet SYNCH mode? */
	donebinarytoggle,	/* the user has put us in binary */
	dontlecho,		/* do we suppress local echoing right now? */
	crmod,
	netdata,		/* Print out network data flow */
	debug;			/* Debug level */

extern char
	echoc,			/* Toggle local echoing */
	escape,			/* Escape to command mode */
	doopt[],
	dont[],
	will[],
	wont[],
	hisopts[],
	myopts[],
	subbuffer[SUBBUFSIZE],
	*hostname,		/* Who are we connected to? */
	*prompt;		/* Prompt for command. */

extern FILE
	*NetTrace;		/* Where debugging output goes */

extern jmp_buf
	peerdied,
	toplevel;		/* For error conditions. */

extern void
	intr(),
	intr2(),
	deadpeer(),
	dosynch(),
	doflush(),
	setconnmode(),
	setcommandmode();

extern char
    termEofChar,
    termEraseChar,
    termFlushChar,
    termIntChar,
    termKillChar,
    termLiteralNextChar,
    termQuitChar;

/* Ring buffer structures which are shared */

extern Ring
	netoring,
	netiring,
	ttyoring,
	ttyiring;
