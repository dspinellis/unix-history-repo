/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 * from: Utah $Hdr: hilvar.h 1.3 92/01/21$
 *
 *	@(#)hilvar.h	8.1 (Berkeley) 6/10/93
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
 * Minor device numbers.
 * HP-UX uses 12 bits of the form:
 *	LLLLDDDD0000
 * where L is 4 bits of loop number, D 4 bits of device and 4 bits of 0.
 * BSD uses 8 bits:
 *	LLLLDDDD
 * Device files are in BSD format, we map device numbers to HP-UX format
 * on stat calls.
 */
#define HILUNIT(d)	((d) & 0xF)
#define HILLOOP(d)	(((d)>>4) & 0xF)

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
	struct	selinfo hd_selr;	/* process read selecting */
	uid_t	hd_uid;			/* uid of mapping process */
};

/* hd_flags */
#define	HIL_ALIVE	0x01	/* device is present */
#define HIL_PSEUDO	0x02	/* device is virtual */
#define HIL_READIN	0x04	/* device using read() input interface */
#define HIL_QUEUEIN	0x08	/* device using shared Q input interface */
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
