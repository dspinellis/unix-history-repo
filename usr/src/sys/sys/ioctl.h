/*	ioctl.h	4.6	81/02/25	*/
/*
 * ioctl definitions, and special character and local tty definitions
 */
#ifndef	_IOCTL_
#define	_IOCTL_
struct tchars {
	char	t_intrc;	/* interrupt */
	char	t_quitc;	/* quit */
	char	t_startc;	/* start output */
	char	t_stopc;	/* stop output */
	char	t_eofc;		/* end-of-file */
	char	t_brkc;		/* input delimiter (like nl) */
};
struct ltchars {
	char	t_suspc;	/* stop process signal */
	char	t_dsuspc;	/* delayed stop process signal */
	char	t_rprntc;	/* reprint line */
	char	t_flushc;	/* flush output (toggles) */
	char	t_werasc;	/* word erase */
	char	t_lnextc;	/* literal next character */
};

/*
 * local mode settings
 */
#define	LCRTBS	01		/* correct backspacing for crt */
#define	LPRTERA 02		/* printing terminal \ ... / erase */
#define	LCRTERA	04		/* do " \b " to wipe out character */
#define	LTILDE	010		/* IIASA - hazeltine tilde kludge */
#define	LMDMBUF	020		/* IIASA - start/stop output on carrier intr */
#define	LLITOUT	040		/* IIASA - suppress any output translations */
#define	LTOSTOP	0100		/* send stop for any background tty output */
#define	LFLUSHO	0200		/* flush output sent to terminal */
#define	LNOHANG 0400		/* IIASA - don't send hangup on carrier drop */
#define	LETXACK 01000		/* IIASA - diablo style buffer hacking */
#define	LCRTKIL	02000		/* erase whole line on kill with " \b " */
#define	LINTRUP 04000		/* interrupt on every input char - SIGTINT */
#define	LCTLECH	010000		/* echo control characters as ^X */
#define	LPENDIN	020000		/* tp->t_rawq is waiting to be reread */

/* local state */
#define	LSBKSL	01		/* state bit for lowercase backslash work */
#define	LSQUOT	02		/* last character input was \ */
#define	LSERASE	04		/* within a \.../ for LPRTRUB */
#define	LSLNCH	010		/* next character is literal */
#define	LSTYPEN	020		/* retyping suspended input (LPENDIN) */
#define	LSCNTTB	040		/* counting width of tab; leave LFLUSHO alone */

/*
 * tty ioctl commands
 */
#define	TIOCGETD	(('t'<<8)|0)	/* get line discipline */
#define	TIOCSETD	(('t'<<8)|1)	/* set line discipline */
#define	TIOCHPCL	(('t'<<8)|2)	/* set hangup line on close bit */
#define	TIOCMODG	(('t'<<8)|3)	/* modem bits get (???) */
#define	TIOCMODS	(('t'<<8)|4)	/* modem bits set (???) */
#define	TIOCGETP	(('t'<<8)|8)	/* get parameters - like old gtty */
#define	TIOCSETP	(('t'<<8)|9)	/* set parameters - like old stty */
#define	TIOCSETN	(('t'<<8)|10)	/* set params w/o flushing buffers */
#define	TIOCEXCL	(('t'<<8)|13)	/* set exclusive use of tty */
#define	TIOCNXCL	(('t'<<8)|14)	/* reset exclusive use of tty */
#define	TIOCFLUSH	(('t'<<8)|16)	/* flush buffers */
#define	TIOCSETC	(('t'<<8)|17)	/* set special characters */
#define	TIOCGETC	(('t'<<8)|18)	/* get special characters */
#define	TIOCIOANS	(('t'<<8)|20)
#define	TIOCSIGNAL	(('t'<<8)|21)
#define	TIOCUTTY	(('t'<<8)|22)
/* locals, from 127 down */
#define	TIOCLBIS	(('t'<<8)|127)	/* bis local mode bits */
#define	TIOCLBIC	(('t'<<8)|126)	/* bic local mode bits */
#define	TIOCLSET	(('t'<<8)|125)	/* set entire local mode word */
#define	TIOCLGET	(('t'<<8)|124)	/* get local modes */
#define	TIOCSBRK	(('t'<<8)|123)	/* set break bit */
#define	TIOCCBRK	(('t'<<8)|122)	/* clear break bit */
#define	TIOCSDTR	(('t'<<8)|121)	/* set data terminal ready */
#define	TIOCCDTR	(('t'<<8)|120)	/* clear data terminal ready */
#define	TIOCGPGRP	(('t'<<8)|119)	/* get pgrp of tty */
#define	TIOCSPGRP	(('t'<<8)|118)	/* set pgrp of tty */
#define	TIOCSLTC	(('t'<<8)|117)	/* set local special characters */
#define	TIOCGLTC	(('t'<<8)|116)	/* get local special characters */
#define	TIOCOUTQ	(('t'<<8)|115)	/* number of chars in output queue */
#define	TIOCSTI		(('t'<<8)|114)	/* simulate a terminal in character */

#define	OTTYDISC	0		/* old, v7 std tty driver */
#define	NETLDISC	1		/* line discip for berk net */
#define	NTTYDISC	2		/* new tty discipline */

#define	FIOCLEX		(('f'<<8)|1)
#define	FIONCLEX	(('f'<<8)|2)
/* another local */
#define	FIONREAD	(('f'<<8)|127)	/* get # bytes to read */
#endif
