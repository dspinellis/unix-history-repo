/*	ioctl.h	3.2	%H%	*/
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
	char	t_lerase;	/* local erase */
	char	t_lkill;	/* local kill */
	char	t_lintr;	/* local interrupt */
};

/*
 * local mode settings
 */
#define	LCRTBS	01		/* correct backspacing for crt */
#define LPRTERA 02		/* printing terminal \ ... / erase */
#define	LCRTERA	04		/* do " \b " to wipe out character */
#define LTILDE	010		/* IIASA - hazeltine tilde kludge */
#define LMDMBUF	020		/* IIASA - start/stop output on carrier intr */
#define LLITOUT	040		/* IIASA - suppress any output translations */
#define LTOSTOP	0100		/* send stop for any background tty output */
#define LFLUSHO	0200		/* flush output sent to terminal */
#define LNOHANG 0400		/* IIASA - don't send hangup on carrier drop */
#define LETXACK 01000		/* IIASA - diablo style buffer hacking */
#define	LCRTKIL	02000		/* erase whole line on kill with " \b " */
#define LINTRUP 04000		/* interrupt on every input char - SIGTINT */
#define	LCTLECH	010000		/* echo control characters as ^X */
#define	LPENDIN	020000		/* tp->t_rawq is waiting to be reread */

/* local state */
#define LSBKSL	01		/* state bit for lowercase backslash work */
#define	LSQUOT	02		/* last character input was \ */
#define LSERASE	04		/* within a \.../ for LPRTRUB */
#define	LSLNCH	010		/* next character is literal */
#define	LSTYPEN	020		/* retyping suspended input (LPENDIN) */

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
/* locals, from 127 down */
#define TIOCLBIS	(('t'<<8)|127)	/* bis local mode bits */
#define TIOCLBIC	(('t'<<8)|126)	/* bic local mode bits */
#define TIOCLSET	(('t'<<8)|125)	/* set entire local mode word */
#define TIOCLGET	(('t'<<8)|124)	/* get local modes */
#define	TIOCSBRK	(('t'<<8)|123)	/* set break bit */
#define TIOCCBRK	(('t'<<8)|122)	/* clear break bit */
#define TIOCSDTR	(('t'<<8)|121)	/* set data terminal ready */
#define TIOCCDTR	(('t'<<8)|120)	/* clear data terminal ready */
#define TIOCGPGRP	(('t'<<8)|119)	/* get pgrp of tty */
#define TIOCSPGRP	(('t'<<8)|118)	/* set pgrp of tty */
#define	TIOCSLTC	(('t'<<8)|117)	/* set local special characters */
#define	TIOCGLTC	(('t'<<8)|116)	/* get local special characters */

#define	NETLDISC	1		/* line discip for berk net */
#define	NTTYDISC	2

#define	DIOCLSTN	(('d'<<8)|1)
#define	DIOCNTRL	(('d'<<8)|2)
#define	DIOCMPX		(('d'<<8)|3)
#define	DIOCNMPX	(('d'<<8)|4)
#define	DIOCSCALL	(('d'<<8)|5)
#define	DIOCRCALL	(('d'<<8)|6)
#define	DIOCPGRP	(('d'<<8)|7)
#define	DIOCGETP	(('d'<<8)|8)
#define	DIOCSETP	(('d'<<8)|9)
#define	DIOCLOSE	(('d'<<8)|10)
#define	DIOCTIME	(('d'<<8)|11)
#define	DIOCRESET	(('d'<<8)|12)
#define	DIOCSMETA	(('d'<<8)|13)
#define	DIOCMERGE	(('d'<<8)|14)
#define	DIOCICHAN	(('d'<<8)|15)
#define	DIOCPAD		(('d'<<8)|16)
#define	DIOCRMETA	(('d'<<8)|17)
#define	DIOCXOUT	(('d'<<8)|18)
#define	DIOCBMETA	(('d'<<8)|19)
#define	DIOCAMETA	(('d'<<8)|20)
#define	DIOCSBMETA	(('d'<<8)|21)

#define	FIOCLEX		(('f'<<8)|1)
#define	FIONCLEX	(('f'<<8)|2)
/* another local */
#define	FIONREAD	(('f'<<8)|127)	/* get # bytes to read */
/* FIONREAD is not implemented on mpx channels yet */

#define	MXLSTN		(('x'<<8)|1)
#define	MXNBLK		(('x'<<8)|2)

/*
 * These are defined in sys/vcmd.h
 *
#define	VGETSTATE	(('v'<<8)|0)
#define	VSETSTATE	(('v'<<8)|1)
 */
#endif
