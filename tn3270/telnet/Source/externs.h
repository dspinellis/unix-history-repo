/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)externs.h	1.13 (Berkeley) 11/29/88
 */

#include <stdio.h>
#include <setjmp.h>

#define	SUBBUFSIZE	100

extern int errno;		/* outside this world */

extern char
    *strcat(),
    *strcpy();			/* outside this world */

extern int
    flushout,		/* flush output */
    connected,		/* Are we connected to the other side? */
    globalmode,		/* Mode tty should be in */
    In3270,			/* Are we in 3270 mode? */
    telnetport,		/* Are we connected to the telnet port? */
    localchars,		/* we recognize interrupt/quit */
    donelclchars,		/* the user has set "localchars" */
    showoptions,
    net,		/* Network file descriptor */
    tin,		/* Terminal input file descriptor */
    tout,		/* Terminal output file descriptor */
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
    *hostname,		/* Who are we connected to? */
    *prompt;		/* Prompt for command. */

extern FILE
    *NetTrace;		/* Where debugging output goes */

extern jmp_buf
    peerdied,
    toplevel;		/* For error conditions. */

extern void
    command(),
#if	!defined(NOT43)
    dosynch(),
#endif	/* !defined(NOT43) */
    Dump(),
    init_3270(),
    printoption(),
    printsub(),
    setconnmode(),
    setcommandmode(),
    setneturg(),
    sys_telnet_init(),
    telnet(),
    TerminalFlushOutput(),
    TerminalNewMode(),
    TerminalRestoreState(),
    TerminalSaveState(),
    tninit(),
    upcase(),
    willoption(),
    wontoption();

#if	defined(NOT43)
extern int
    dosynch();
#endif	/* defined(NOT43) */

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

/* Tn3270 section */
#if	defined(TN3270)

extern int
    HaveInput,		/* Whether an asynchronous I/O indication came in */
    noasynchtty,	/* Don't do signals on I/O (SIGURG, SIGIO) */
    noasynchnet,	/* Don't do signals on I/O (SIGURG, SIGIO) */
    sigiocount,		/* Count of SIGIO receptions */
    shell_active;	/* Subshell is active */

extern char
    *Ibackp,		/* Oldest byte of 3270 data */
    Ibuf[],		/* 3270 buffer */
    *Ifrontp,		/* Where next 3270 byte goes */
    tline[],
    *transcom;		/* Transparent command */

extern int
    settranscom();

extern void
    inputAvailable();
#endif	/* defined(TN3270) */
