/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Chris Torek.
 *
 * Redistribution is only permitted until one year after the first shipment
 * of 4.4BSD by the Regents.  Otherwise, redistribution and use in source and
 * binary forms are permitted provided that: (1) source distributions retain
 * this entire copyright notice and comment, and (2) distributions including
 * binaries display the following acknowledgement:  This product includes
 * software developed by the University of California, Berkeley and its
 * contributors'' in the documentation or other materials provided with the
 * distribution and in all advertising materials mentioning features or use
 * of this software.  Neither the name of the University nor the names of
 * its contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)rx50reg.h	7.2 (Berkeley) 6/28/90
 */

/*
 * RX50 registers.
 */

/*
 * The names below do not quite match the DEC documentation simply because
 * the names in the documentation are so bad.
 */
struct rx50device {
	u_short	rxid;		/* identification */
	u_short	reserved;
	u_short	rxcmd;		/* command function reg */
	u_short	rxtrk;		/* track */
	u_short	rxsec;		/* sector */
	u_short	rxcsc;		/* current sector */
	u_short	rxict;		/* incorrect track (???) */
	u_short	rxext;		/* extend command register */
	u_short	rxedb;		/* empty data buffer (read) */
	u_short	rxrda;		/* reset data address */
	u_short	rxgo;		/* read to start current cmd */
	u_short	rxfdb;		/* fill data buffer (write) */
};

#define	RX50SEC		10	/* sectors per track */
#define	RX50MAXSEC	800	/* 10 sectors times 80 tracks */

/*
 * Do the sector skew given the sector and track
 * number (it depends on both!).
 */
/*			(((((s) / 5) + 2 * ((s) + (t))) % 10) + 1) */
#define	RX50SKEW(s, t)	(((s) / 5) + "\1\3\5\7\11\1\3\5\7"[((s) + (t)) % 5])

/*
 * Values in the command function register.
 */
#define	RXCMD_ERROR	0x80	/* error bit (composite?) */
#define	RXCMD_READ	0x40	/* read command */
#define	RXCMD_WRITE	0x70	/* write command */
#define	RXCMD_RESET	0x20	/* reset command */
#define	RXCMD_DONE	0x08	/* operation done (status) */
#define	RXCMD_DRIVE0	0x00	/* select drive 0 (csa1) */
#define	RXCMD_DRIVE1	0x02	/* select drive 1 (csa2) */
