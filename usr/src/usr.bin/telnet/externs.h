/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)externs.h	1.23 (Berkeley) %G%
 */

#ifndef	BSD
# define BSD 43
#endif

#if (BSD > 43 || defined(SYSV_TERMIO)) && !defined(USE_TERMIO)
# define USE_TERMIO
#endif

#include <stdio.h>
#include <setjmp.h>
#include <sys/ioctl.h>
#ifdef	USE_TERMIO
# ifndef	VINTR
#  ifdef SYSV_TERMIO
#   include <sys/termio.h>
#  else
#   include <sys/termios.h>
#   define termio termios
#  endif
# endif
#else
typedef char cc_t;
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
    apitrace,		/* Trace API transactions */
#endif	/* defined(TN3270) */
    termdata,		/* Print out terminal data flow */
#endif	/* defined(unix) */
    debug;			/* Debug level */

extern cc_t
    echoc,		/* Toggle local echoing */
    escape;		/* Escape to command mode */

extern unsigned char
    *prompt;		/* Prompt for command. */

extern char
    doopt[],
    dont[],
    will[],
    wont[],
    options[],		/* All the little options */
    *hostname;		/* Who are we connected to? */

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
extern unsigned char
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

#ifndef	USE_TERMIO

extern struct	tchars ntc;
extern struct	ltchars nltc;
extern struct	sgttyb nttyb;

# define termEofChar		ntc.t_eofc
# define termEraseChar		nttyb.sg_erase
# define termFlushChar		nltc.t_flushc
# define termIntChar		ntc.t_intrc
# define termKillChar		nttyb.sg_kill
# define termLiteralNextChar	nltc.t_lnextc
# define termQuitChar		ntc.t_quitc
# define termSuspChar		nltc.t_suspc
# define termRprntChar		nltc.t_rprntc
# define termWerasChar		nltc.t_werasc
# define termStartChar		ntc.t_startc
# define termStopChar		ntc.t_stopc
# define termForw1Char		ntc.t_brkc
extern char termForw2Char;

# define termEofCharp		(cc_t *)&ntc.t_eofc
# define termEraseCharp		(cc_t *)&nttyb.sg_erase
# define termFlushCharp		(cc_t *)&nltc.t_flushc
# define termIntCharp		(cc_t *)&ntc.t_intrc
# define termKillCharp		(cc_t *)&nttyb.sg_kill
# define termLiteralNextCharp	(cc_t *)&nltc.t_lnextc
# define termQuitCharp		(cc_t *)&ntc.t_quitc
# define termSuspCharp		(cc_t *)&nltc.t_suspc
# define termRprntCharp		(cc_t *)&nltc.t_rprntc
# define termWerasCharp		(cc_t *)&nltc.t_werasc
# define termStartCharp		(cc_t *)&ntc.t_startc
# define termStopCharp		(cc_t *)&ntc.t_stopc
# define termForw1Charp		(cc_t *)&ntc.t_brkc
# define termForw2Charp		(cc_t *)&termForw2Char

# else

extern struct	termio new_tc;

# define termEofChar		new_tc.c_cc[VEOF]
# define termEraseChar		new_tc.c_cc[VERASE]
# define termIntChar		new_tc.c_cc[VINTR]
# define termKillChar		new_tc.c_cc[VKILL]
# define termQuitChar		new_tc.c_cc[VQUIT]

# ifndef	VSUSP
extern char termSuspChar;
# else
#  define termSuspChar		new_tc.c_cc[VSUSP]
# endif
# ifndef	VDISCARD
extern char termFlushChar;
# else
#  define termFlushChar		new_tc.c_cc[VDISCARD]
# endif
# ifndef VWERASE
extern char termWerasChar;
# else
#  define termWerasChar		new_tc.c_cc[VWERASE]
# endif
# ifndef	VREPRINT
extern char termRprntChar;
# else
#  define termRprntChar		new_tc.c_cc[VREPRINT]
# endif
# ifndef	VLNEXT
extern char termLiteralNextChar;
# else
#  define termLiteralNextChar	new_tc.c_cc[VLNEXT]
# endif
# ifndef	VSTART
extern char termStartChar;
# else
#  define termStartChar		new_tc.c_cc[VSTART]
# endif
# ifndef	VSTOP
extern char termStopChar;
# else
#  define termStopChar		new_tc.c_cc[VSTOP]
# endif
# ifndef	VEOL
extern char termForw1Char;
# else
#  define termForw1Char		new_tc.c_cc[VEOL]
# endif
# ifndef	VEOL2
extern char termForw2Char;
# else
#  define termForw2Char		new_tc.c_cc[VEOL]
# endif

# ifndef CRAY
#  define termEofCharp		&termEofChar
#  define termEraseCharp	&termEraseChar
#  define termIntCharp		&termIntChar
#  define termKillCharp		&termKillChar
#  define termQuitCharp		&termQuitChar
#  define termSuspCharp		&termSuspChar
#  define termFlushCharp	&termFlushChar
#  define termWerasCharp	&termWerasChar
#  define termRprntCharp	&termRprntChar
#  define termLiteralNextCharp	&termLiteralNextChar
#  define termStartCharp	&termStartChar
#  define termStopCharp		&termStopChar
#  define termForw1Charp	&termForw1Char
#  define termForw2Charp	&termForw2Char
# else
	/* Work around a compiler bug */
#  define termEofCharp		0
#  define termEraseCharp	0
#  define termIntCharp		0
#  define termKillCharp		0
#  define termQuitCharp		0
#  define termSuspCharp		0
#  define termFlushCharp	0
#  define termWerasCharp	0
#  define termRprntCharp	0
#  define termLiteralNextCharp	0
#  define termStartCharp	0
#  define termStopCharp		0
#  define termForw1Charp	0
#  define termForw2Charp	0
# endif
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
