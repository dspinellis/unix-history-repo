/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
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
 * from: Utah $Hdr: hilvar.h 1.1 89/08/22$
 *
 *	@(#)hilvar.h	7.1 (Berkeley) 5/8/90
 */

#ifndef TRUE
#define TRUE	1
#define FALSE	0
#endif

#define NHILD		8		/* 7 actual + loop pseudo (dev 0) */
#define NHILQ		8		/* must be <= sizeof(int) */

#define HILBUFSIZE	40		/* size of interrupt poll buffer */
#define HILMAXCLIST	1024		/* max chars in clists for HPUX io */

#define HILLOOPDEV	0		/* loop device index */

/*
 * XXX: HPUX minor numbers are of the form "D0" where D is the device number
 * BSD uses "0D".  For compatibility we accept either.  Maybe we should just
 * use the HPUX numbering.
 */
#define HILUNIT(d)	(((((d)>>4)&7)==0)?((d)&7):(((d)>>4)&7))

#define	hildevmask(d)	(1 << (d))
#define	hilqmask(q)	(1 << (q))

struct hiliqueue {
	HILQ	*hq_eventqueue;		/* input queue shared with user */
	struct	proc *hq_procp;		/* process this queue belongs to */
	char	hq_devmask;		/* devices mapped to this queue */
};

struct hilloopdev {
	int	hd_flags;		/* device state */
	int	hd_qmask;		/* queues this device is mapped to */
	struct	clist hd_queue;		/* event queue for HPUX-style input */
	struct	proc *hd_selr;		/* process read selecting */
	uid_t	hd_uid;			/* uid of mapping process */
};

/* hd_flags */
#define	HIL_ALIVE	0x01	/* device is present */
#define HIL_PSEUDO	0x02	/* device is virtual */
#define HIL_READIN	0x04	/* device using read() input interface */
#define HIL_QUEUEIN	0x08	/* device using shared Q input interface */
#define HIL_SELCOLL	0x10	/* select collision on device */
#define HIL_NOBLOCK	0x20	/* device is in non-blocking read mode */
#define HIL_ASLEEP	0x40	/* process awaiting input on device */
#define HIL_DERROR	0x80	/* loop has reconfigured, reality altered */

struct hilloop {
	struct	hil_dev	*hl_addr;	/* base of hardware registers */
	u_char 	hl_cmddone;		/* */
	u_char 	hl_cmdending;		/* */
	u_char	hl_actdev;		/* current input device */
	u_char	hl_cmddev;		/* device to perform command on */
	u_char	hl_pollbuf[HILBUFSIZE];	/* interrupt time input buffer */
	u_char	hl_cmdbuf[HILBUFSIZE];	/* */
	u_char 	*hl_pollbp;		/* pointer into hl_pollbuf */
	u_char	*hl_cmdbp;		/* pointer into hl_cmdbuf */
	struct	hiliqueue hl_queue[NHILQ];	/* input queues */
	struct  hilloopdev hl_device[NHILD];	/* device data */
	u_char  hl_maxdev;		/* number of devices on loop */
	u_char	hl_kbddev;		/* keyboard device on loop */
	u_char	hl_kbdlang;		/* keyboard language */
	u_char	hl_kbdflags;		/* keyboard state */
};

/* hl_kbdflags */
#define KBD_RAW		0x01		/* keyboard is raw */
#define KBD_AR1		0x02		/* keyboard auto-repeat rate 1 */
#define KBD_AR2		0x04		/* keyboard auto-repeat rate 2 */
