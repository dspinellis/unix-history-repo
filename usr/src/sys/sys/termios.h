/*
 * Copyright (c) 1988 The Regents of the University of California.
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
 *	@(#)termios.h	7.3 (Berkeley) %G%
 */

/*
 *  termios structure
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
#define	VERASE2		18	/* ICANON */
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

#define _POSIX_VDISABLE	((unsigned char)'\377')

/*
 * Input flags - software input processing
 */
#define	IGNBRK		0x00000001	/* ignore BREAK condition */
#define	BRKINT		0x00000002	/* map BREAK to SIGINTR */
#define	IGNPAR		0x00000004	/* ignore (discard) parity errors */
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
#define	OPOST		0x00000001	/* enable following output processing */
#define ONLCR		0x00000002	/* map NL to CR-NL (ala CRMOD) */
#define ONLCRNL		ONLCR
#define OXTABS		0x00000004	/* expand tabs to spaces */
#define ONOEOT		0x00000008	/* discard EOT's (^D) on output) */

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
#define CLOCAL		0x00008000	/* ignore modem status lines */
#define CRTSCTS		0x00010000	/* RTS/CTS flow control */


/* 
 * "Local" flags - dumping ground for other state
 *
 * Warning: some flags in this structure begin with
 * the letter "I" and look like they belong in the
 * input flag.  Isn't history fun.
 */

#define	ECHOKE		0x00000001	/* visual erase for line kill */
#define	ECHOE		0x00000002	/* visually erase chars */
#define	ECHOK		0x00000004	/* echo NL after line kill */
#define	ECHONL		0x00000010	/* echo NL even if ECHO is off */
#define	ECHOPRT		0x00000020	/* visual erase mode for hardcopy */
#define ECHOCTL  	0x00000040	/* echo control chars as ^(Char) */
#define	ISIG		0x00000080	/* enable signals INTR, QUIT, [D]SUSP */
#define	ICANON		0x00000100	/* canonicalize input lines */
#define ALTWERASE	0x00000200	/* use alternate WERASE algorithm */
#ifdef notdef	/* XXX already defined in ioctl.h */
#define ECHO		0x00000008	/* enable echoing */
#define	MDMBUF		0x00100000	/* flow control output via Carrier */
#define TOSTOP		0x00400000	/* stop background jobs from output */
#define FLUSHO		0x00800000	/* output being flushed (state) */
#define	NOHANG		0x01000000	/* XXX this should go away */
#define PENDIN		0x20000000	/* retype pending input (state) */
#define	NOFLSH		0x80000000	/* don't flush after interrupt */
#endif /*notdef*/

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
 * Commands passed to tcsetattr() for setting the termios structure.
 */
#define	TCSANOW		0		/* make change immediate */
#define	TCSADRAIN	1		/* drain output, then change */
#define	TCSADFLUSH	2		/* drain output, flush input */
/*
 * TCSASOFT is a flag which can be or'ed in with a command.
 * If set, only the software processing flags in the termios 
 * structure are altered.  That is, the settings of the cflag and 
 * speeds are ignored.
 */
#define TCSASOFT	0x80000000	/* but ignore hardware settings */

/*
 * Is c equal to control character val?
 */
#define CCEQ(val, c)	(c == val ? val != _POSIX_VDISABLE : 0)

#endif _TERMIOS_
