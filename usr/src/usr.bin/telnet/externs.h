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
 *	@(#)externs.h	1.18 (Berkeley) %G%
 */

#ifdef	CRAY
#define	USE_TERMIO
#endif

#include <stdio.h>
#include <setjmp.h>
#include <sys/ioctl.h>
#ifdef	USE_TERMIO
#ifndef	VINTR
#include <sys/termio.h>
#endif
#endif

#define	SUBBUFSIZE	256

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
    localflow,		/* Flow control handled locally */
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
    prettydump,		/* Print "netdata" output in user readable format */
#if	defined(unix)
#if	defined(TN3270)
    cursesdata,		/* Print out curses data flow */
#endif	/* defined(TN3270) */
    termdata,		/* Print out terminal data flow */
#endif	/* defined(unix) */
    debug;			/* Debug level */

extern char
    echoc,			/* Toggle local echoing */
    escape,			/* Escape to command mode */
    doopt[],
    dont[],
    will[],
    wont[],
    options[],		/* All the little options */
    *hostname,		/* Who are we connected to? */
    *prompt;		/* Prompt for command. */

/*
 * We keep track of each side of the option negotiation.
 */

#define	MY_STATE_WILL		0x01
#define	MY_WANT_STATE_WILL	0x02
#define	MY_STATE_DO		0x04
#define	MY_WANT_STATE_DO	0x08

/*
 * Macros to check the current state of things
 */

#define	my_state_is_do(opt)		(options[opt]&MY_STATE_DO)
#define	my_state_is_will(opt)		(options[opt]&MY_STATE_WILL)
#define my_want_state_is_do(opt)	(options[opt]&MY_WANT_STATE_DO)
#define my_want_state_is_will(opt)	(options[opt]&MY_WANT_STATE_WILL)

#define	my_state_is_dont(opt)		(!my_state_is_do(opt))
#define	my_state_is_wont(opt)		(!my_state_is_will(opt))
#define my_want_state_is_dont(opt)	(!my_want_state_is_do(opt))
#define my_want_state_is_wont(opt)	(!my_want_state_is_will(opt))

#define	set_my_state_do(opt)		{options[opt] |= MY_STATE_DO;}
#define	set_my_state_will(opt)		{options[opt] |= MY_STATE_WILL;}
#define	set_my_want_state_do(opt)	{options[opt] |= MY_WANT_STATE_DO;}
#define	set_my_want_state_will(opt)	{options[opt] |= MY_WANT_STATE_WILL;}

#define	set_my_state_dont(opt)		{options[opt] &= ~MY_STATE_DO;}
#define	set_my_state_wont(opt)		{options[opt] &= ~MY_STATE_WILL;}
#define	set_my_want_state_dont(opt)	{options[opt] &= ~MY_WANT_STATE_DO;}
#define	set_my_want_state_wont(opt)	{options[opt] &= ~MY_WANT_STATE_WILL;}

/*
 * Make everything symetrical
 */

#define	HIS_STATE_WILL			MY_STATE_DO
#define	HIS_WANT_STATE_WILL		MY_WANT_STATE_DO
#define HIS_STATE_DO			MY_STATE_WILL
#define HIS_WANT_STATE_DO		MY_WANT_STATE_WILL

#define	his_state_is_do			my_state_is_will
#define	his_state_is_will		my_state_is_do
#define his_want_state_is_do		my_want_state_is_will
#define his_want_state_is_will		my_want_state_is_do

#define	his_state_is_dont		my_state_is_wont
#define	his_state_is_wont		my_state_is_dont
#define his_want_state_is_dont		my_want_state_is_wont
#define his_want_state_is_wont		my_want_state_is_dont

#define	set_his_state_do		set_my_state_will
#define	set_his_state_will		set_my_state_do
#define	set_his_want_state_do		set_my_want_state_will
#define	set_his_want_state_will		set_my_want_state_do

#define	set_his_state_dont		set_my_state_wont
#define	set_his_state_wont		set_my_state_dont
#define	set_his_want_state_dont		set_my_want_state_wont
#define	set_his_want_state_wont		set_my_want_state_dont


extern FILE
    *NetTrace;		/* Where debugging output goes */
extern char
    NetTraceFile[];	/* Name of file where debugging output goes */
extern void
    SetNetTrace();	/* Function to change where debugging goes */

extern jmp_buf
    peerdied,
    toplevel;		/* For error conditions. */

extern void
    command(),
#if	!defined(NOT43)
    dosynch(),
#endif	/* !defined(NOT43) */
    get_status(),
    Dump(),
    init_3270(),
    printoption(),
    printsub(),
    sendnaws(),
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

#if	!defined(MSDOS)
# ifndef	USE_TERMIO

extern struct	tchars ntc;
extern struct	ltchars nltc;
extern struct	sgttyb nttyb;

#  define termEofChar		ntc.t_eofc
#  define termEraseChar		nttyb.sg_erase
#  define termFlushChar		nltc.t_flushc
#  define termIntChar		ntc.t_intrc
#  define termKillChar		nttyb.sg_kill
#  define termLiteralNextChar	nltc.t_lnextc
#  define termQuitChar		ntc.t_quitc
#  define termSuspChar		nltc.t_suspc
#  define termRprntChar		nltc.t_rprntc
#  define termWerasChar		nltc.t_werasc
#  define termStartChar		ntc.t_startc
#  define termStopChar		ntc.t_stopc

#  define termEofCharp		&ntc.t_eofc
#  define termEraseCharp	&nttyb.sg_erase
#  define termFlushCharp	&nltc.t_flushc
#  define termIntCharp		&ntc.t_intrc
#  define termKillCharp		&nttyb.sg_kill
#  define termLiteralNextCharp	&nltc.t_lnextc
#  define termQuitCharp		&ntc.t_quitc
#  define termSuspCharp		&nltc.t_suspc
#  define termRprntCharp	&nltc.t_rprntc
#  define termWerasCharp	&nltc.t_werasc
#  define termStartCharp	&ntc.t_startc
#  define termStopCharp		&ntc.t_stopc

# else

extern struct	termio new_tc;

#  define termEofChar		new_tc.c_cc[VEOF]
#  define termEraseChar		new_tc.c_cc[VERASE]
#  define termIntChar		new_tc.c_cc[VINTR]
#  define termKillChar		new_tc.c_cc[VKILL]
#  define termQuitChar		new_tc.c_cc[VQUIT]

extern char
    termSuspChar,
    termFlushChar,
    termWerasChar,
    termRprntChar,
    termLiteralNextChar,
    termStartChar,
    termStopChar;

# ifndef CRAY
#  define termEofCharp		&new_tc.c_cc[VEOF]
#  define termEraseCharp	&new_tc.c_cc[VERASE]
#  define termIntCharp		&new_tc.c_cc[VINTR]
#  define termKillCharp		&new_tc.c_cc[VKILL]
#  define termQuitCharp		&new_tc.c_cc[VQUIT]
# else
	/* Work around a compiler bug */
#  define termEofCharp		0
#  define termEraseCharp	0
#  define termIntCharp		0
#  define termKillCharp		0
#  define termQuitCharp		0
# endif
#  define termSuspCharp		&termSuspChar
#  define termFlushCharp	&termFlushChar
#  define termWerasCharp	&termWerasChar
#  define termRprntCharp	&termRprntChar
#  define termLiteralNextCharp	&termLiteralNextChar
#  define termStartCharp	&termStartChar
#  define termStopCharp		&termStopChar
# endif

#else	/* MSDOS */

extern char
    termEofChar,
    termEraseChar,
    termIntChar,
    termKillChar,
    termQuitChar,
    termSuspChar,
    termFlushChar,
    termWerasChar,
    termRprntChar,
    termLiteralNextChar,
    termStartChar,
    termStopChar;

#endif


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
