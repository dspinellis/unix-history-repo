/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * %sccs.include.redist.c%
 *
 * from: Utah $Hdr: hpux.h 1.15 89/09/25$
 *
 *	@(#)hpux_termio.h	7.1 (Berkeley) %G%
 */

/* HP-UX termio stuff */

#define	HPUXNCC	8

/* control characters */
#define	HPUXVINTR	0
#define	HPUXVQUIT	1
#define	HPUXVERASE	2
#define	HPUXVKILL	3
#define	HPUXVEOF	4
#define	HPUXVEOL	5
#define	HPUXVMIN	4
#define	HPUXVTIME	5

/* input modes */
#define	TIO_IGNBRK	0000001
#define	TIO_BRKINT	0000002
#define	TIO_IGNPAR	0000004
#define	TIO_PARMRK	0000010
#define	TIO_INPCK	0000020
#define	TIO_ISTRIP	0000040
#define	TIO_INLCR	0000100
#define	TIO_IGNCR	0000200
#define	TIO_ICRNL	0000400
#define	TIO_IUCLC	0001000
#define	TIO_IXON	0002000
#define	TIO_IXANY	0004000
#define	TIO_IXOFF	0010000
#define	TIO_IENQAK	0020000

/* output modes */
#define	TIO_OPOST	0000001
#define	TIO_OLCUC	0000002
#define	TIO_ONLCR	0000004
#define	TIO_OCRNL	0000010
#define	TIO_ONOCR	0000020
#define	TIO_ONLRET	0000040
#define	TIO_OFILL	0000100
#define	TIO_OFDEL	0000200
#define	TIO_NLDLY	0000400
#define	TIO_NL0		0
#define	TIO_NL1		0000400
#define	TIO_CRDLY	0003000
#define	TIO_CR0		0
#define	TIO_CR1		0001000
#define	TIO_CR2		0002000
#define	TIO_CR3		0003000
#define	TIO_TABDLY	0014000
#define	TIO_TAB0	0
#define	TIO_TAB1	0004000
#define	TIO_TAB2	0010000
#define	TIO_TAB3	0014000
#define	TIO_BSDLY	0020000
#define	TIO_BS0		0
#define	TIO_BS1		0020000
#define	TIO_VTDLY	0040000
#define	TIO_VT0		0
#define	TIO_VT1		0040000
#define	TIO_FFDLY	0100000
#define	TIO_FF0		0
#define	TIO_FF1		0100000

/* control modes */
#define	TIO_CBAUD	0000037
#define	TIO_B0		0
#define	TIO_B50		0000001
#define	TIO_B75		0000002
#define	TIO_B110	0000003
#define	TIO_B134	0000004
#define	TIO_B150	0000005
#define	TIO_B200	0000006
#define	TIO_B300	0000007
#define	TIO_B600	0000010
#define	TIO_B900	0000011
#define	TIO_B1200	0000012
#define	TIO_B1800	0000013
#define	TIO_B2400	0000014
#define	TIO_B3600	0000015
#define	TIO_B4800	0000016
#define	TIO_B7200	0000017
#define	TIO_B9600	0000020
#define	TIO_B19200	0000021
#define	TIO_B38400	0000022
#define	TIO_EXTA	0000036
#define	TIO_EXTB	0000037
#define	TIO_CSIZE	0000140
#define	TIO_CS5		0
#define	TIO_CS6		0000040
#define	TIO_CS7		0000100
#define	TIO_CS8		0000140
#define	TIO_CSTOPB	0000200
#define	TIO_CREAD	0000400
#define	TIO_PARENB	0001000
#define	TIO_PARODD	0002000
#define	TIO_HUPCL	0004000
#define	TIO_CLOCAL	0010000
#define TIO_CRTS   	0020000 /* Obsolete */

/* line discipline 0 modes */
#define	TIO_ISIG	0000001
#define	TIO_ICANON	0000002
#define	TIO_XCASE	0000004
#define	TIO_ECHO	0000010
#define	TIO_ECHOE	0000020
#define	TIO_ECHOK	0000040
#define	TIO_ECHONL	0000100
#define	TIO_NOFLSH	0000200

struct hpuxtermio {
	u_short	c_iflag;	/* input modes */
	u_short	c_oflag;	/* output modes */
	u_short	c_cflag;	/* control modes */
	u_short	c_lflag;	/* line discipline modes */
	char    c_line;		/* line discipline */
	u_char	c_cc[HPUXNCC];	/* control chars */
};

#define	HPUXTCGETA	_IOR('T', 1, struct hpuxtermio)
#define	HPUXTCSETA	_IOW('T', 2, struct hpuxtermio)
#define	HPUXTCSETAW	_IOW('T', 3, struct hpuxtermio)
#define	HPUXTCSETAF	_IOW('T', 4, struct hpuxtermio)
