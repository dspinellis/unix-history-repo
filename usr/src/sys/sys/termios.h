/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)termios.h	7.1 (Berkeley) %G%
 */

/*
 *  posix termios structure
 */
#ifndef _TERMIOS_
#define _TERMIOS_

#ifdef KERNEL
#include "ioctl.h"
#else
#include <sys/ioctl.h>
#endif


/* 
 * Special Control Characters 
 *
 * Index into c_cc[] character array.
 *
 *	Name	     Subscript	Enabled by 
 */
#define	VEOF		0	/* ICANON */
#define	VEOL		1	/* ICANON */
#define	VEOL2		2	/* ICANON */
#define	VERASE		3	/* ICANON */
#define VWERASE 	4	/* ICANON */
#define VKILL		5	/* ICANON */
#define	VREPRINT 	6	/* ICANON */
#define VQUOTE		7	/* ICANON */
#define VINTR		8	/* ISIG */
#define VQUIT		9	/* ISIG */
#define VSUSP		10	/* ISIG */
#define VDSUSP		11	/* ISIG */
#define VSTART		12	/* IXON, IXOFF */
#define VSTOP		13	/* IXON, IXOFF */
#define	VLNEXT		14	/* IEXTEN */
#define	VFLUSHO		15	/* IEXTEN */
#define	VFLUSH		VFLUSHO	/* for sun */
#define VMIN		16	/* !ICANON */
#define VTIME		17	/* !ICANON */

#define	NCC		20	/* two spares */

#define POSIX_V_DISABLE	((unsigned char)'\377')
#define _POSIX_V_DISABLE POSIX_V_DISABLE

/*
 * Input flags - software input processing
 */
#define	IGNBRK		0x00000001	/* ignore BREAK condition */
#define	BRKINT		0x00000002	/* map BREAK to SIGINTR */
#define	IGNPAR		0x00000004	/* ignore (throw out) parity errors */
#define	PARMRK		0x00000008	/* mark parity and framing errors */
#define	INPCK		0x00000010	/* disable checking of parity errors */
#define	ISTRIP		0x00000020	/* strip 8th bit off chars */
#define	INLCR		0x00000040	/* map NL into CR */
#define	IGNCR		0x00000080	/* ignore CR */
#define	ICRNL		0x00000100	/* map CR to NL (ala CRMOD) */
#define	IXON		0x00000200	/* enable output flow control */
#define	IFLOW		IXON		/* "" */
#define	IXOFF		0x00000400	/* enable input flow control */
#define	ITANDEM		IXOFF		/* "" */
#define	IXANY		0x00000800	/* any char will restart after stop */
#define	IEXTEN		0x00001000	/* enable FLUSHO and LNEXT */
#define IMAXBEL		0x00002000	/* ring bell on input queue full */

/*
 * Output flags - software output processing
 */
#define	OPOST		0x00000001	/* enable output processing */
#define ONLCR		0x00000002	/* map NL to CR-NL (ala CRMOD) */
#define OXTABS		0x00000004	/* expand tabs to spaces */

/*
 * Control flags - hardware control of terminal
 */
#define CSIZE		0x00000300	/* character size mask */
#define     CS5		    0x00000000	    /* 5 bits - pseudo */
#define     CS6		    0x00000100	    /* 6 bits */
#define     CS7		    0x00000200	    /* 7 bits */
#define     CS8		    0x00000300	    /* 8 bits */
#define CSTOPB		0x00000400	/* send 2 stop bits */
#define CREAD		0x00000800	/* enable receiver */
#define PARENB		0x00001000	/* parity enable */
#define PARODD		0x00002000	/* odd parity, else even */
#define HUPCL		0x00004000	/* hang up on last close */
#define CLOCAL		0x00008000	/* ignore mode status lines */
#define CRTSCTS		0x00010000	/* RTS/CTS flow control */


/* 
 * "Local" flags - dumping ground for other state
 *
 *  Note presence of ISIG and ICANON.  Its not *our* fault.
 */

#define ECHO		0x00000001	/* enable echoing */
#define	ECHOE		0x00000002	/* visually erase chars */
#define	ECHOK		0x00000004	/* echo NL after line kill */
#define	ECHOKE		0x00000008	/* visual erase for line kill */
#define	ECHONL		0x00000010	/* echo NL even if ECHO is off */
#define	ECHOPRT		0x00000020	/* visual erase mode for hardcopy */
#define ECHOCTL  	0x00000040	/* echo control chars as ^(Char) */
#define	ISIG		0x00000080	/* enable signals INTR, QUIT, [D]SUSP */
#define	ICANON		0x00000100	/* canonicalize input lines */
#define	NOFLSH		0x00000200	/* don't flush after interrupt */
#define TOSTOP		0x00000400	/* stop background jobs from output */
#define	MDMBUF		0x00000800	/* flow control output via Carrier */
#define	NOHANG		0x00001000	/* XXX this should go away */
#define FLUSHO		0x00002000	/* output being flushed (state) */
#define PENDIN		0x00004000	/* retype pending input (state) */

struct termios {
	unsigned long	c_iflag;	/* input flags */
	unsigned long	c_oflag;	/* output flags */
	unsigned long	c_cflag;	/* control flags */
	unsigned long	c_lflag;	/* local flags */
	unsigned char	c_cc[NCC];	/* control chars */
	long		c_ispeed;	/* input speed */
	long		c_ospeed;	/* output speed */
};

/* 
 * Flags to tcsetattr(), for setting the termios structure.
 * 
 * If TCSASOFT is or'ed in with one of the first three, then
 * only the software processing flags in the termios structure
 * are set.  That is, the settings of the cflag and speeds
 * are ignored.
 */
#define	TCSANOW		0		/* make change immediate */
#define	TCSADRAIN	1		/* drain output, then change */
#define	TCSADFLUSH	2		/* drain output, flush input */
#define TCSASOFT	0x80000000	/* but ignore hardware settings */

/*
 * Is c equal to control character val?  XXX - should reverse val and c
 */
#define CCEQ(val, c)	(c == val ? val != POSIX_V_DISABLE : 0)

#endif _TERMIOS_
