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
 * from: Utah $Hdr: hilioctl.h 1.10 92/01/21$
 *
 *	@(#)hilioctl.h	7.6 (Berkeley) %G%
 */

struct _hilbell {
	u_char	duration;
	u_char	frequency;
};

struct _hilbuf16 {
	u_char	string[16];
};

struct _hilbuf11 {
	u_char	string[11];
};

struct _hilbuf5 {
  	u_char  string[5];
};

struct _hilbuf4 {
  	u_char  string[4];
};

struct _hilbuf2 {
	u_char	string[2];
};

struct hilqinfo {
	int	qid;
	char	*addr;
};

/*
 * HPUX ioctls (here for the benefit of the HIL driver).
 * Named as they are under HPUX.
 * The first set are loop device ioctls.
 * The second set are ioctls for the 8042.
 * Note that some are not defined as in HPUX
 * due to the difference in the definitions of IOC_VOID.
 */
#ifdef hp800
#define _IOHpux(x,y)	_IO(x,y)
#else
#define _IOHpux(x,y)	(IOC_IN|((x)<<8)|y)	/* IOC_IN is IOC_VOID */
#endif

/*
 * The HP compiler (at least as of HP-UX 7.X) pads odd sized structures
 * to a short boundary.  To avoid issues of whether our compiler pads
 * and, if so to what boundary, we explicitly state the values for
 * troublesome ioctls:
 *
 *	HILID (HILIOCID)	_IOR('h',0x03, struct _hilbuf11),
 *	EFTRRT (HILIOCRRT)	_IOR('H',0x31, struct _hilbuf5).
 */
#define HILID	0x400C6803			/* Identify & describe */
#define HILSC	_IOR('h',0x33, struct _hilbuf16) /* Security code */
#define HILRN	_IOR('h',0x30, struct _hilbuf16) /* Report name */
#define HILRS	_IOR('h',0x31, struct _hilbuf16) /* Report status */
#define HILED	_IOR('h',0x32, struct _hilbuf16) /* Extended describe*/
#define HILDKR  _IOHpux('h',0x3D)		/* Disable autorepeat */
#define HILER1  _IOHpux('h',0x3E)		/* Autorepeat 1/30 */
#define HILER2  _IOHpux('h',0x3F)		/* Autorepeat 1/60 */
#define HILP1	_IOHpux('h',0x40)		/* Prompt 1 */
#define HILP2	_IOHpux('h',0x41)		/* Prompt 2 */
#define HILP3	_IOHpux('h',0x42)		/* Prompt 3 */
#define HILP4	_IOHpux('h',0x43)		/* Prompt 4 */
#define HILP5	_IOHpux('h',0x44)		/* Prompt 5 */
#define HILP6	_IOHpux('h',0x45)		/* Prompt 6 */
#define HILP7	_IOHpux('h',0x46)		/* Prompt 7 */
#define HILP	_IOHpux('h',0x47)		/* Prompt */
#define HILA1	_IOHpux('h',0x48)		/* Acknowledge 1 */
#define HILA2	_IOHpux('h',0x49)		/* Acknowledge 2 */
#define HILA3	_IOHpux('h',0x4A)		/* Acknowledge 3 */
#define HILA4	_IOHpux('h',0x4B)		/* Acknowledge 4 */
#define HILA5	_IOHpux('h',0x4C)		/* Acknowledge 5 */
#define HILA6	_IOHpux('h',0x4D)		/* Acknowledge 6 */
#define HILA7	_IOHpux('h',0x4E)		/* Acknowledge 7 */
#define HILA	_IOHpux('h',0x4F)		/* Acknowledge */

#define EFTSRD  _IOW('H',0xa0,char)		/* Set the repeat delay. */
#define EFTSRR  _IOW('H',0xa2,char)		/* Set the repeat rate. */
#define EFTSRPG _IOW('H',0xa6,char)		/* Set RPG interrupt rate. */
#define EFTSBP  _IOW('H',0xc4,struct _hilbuf4)	/* Send data to the beeper. */
#define EFTRLC  _IOR('H',0x12,char)		/* Read the language code. */
#define EFTRCC  _IOR('H',0x11,char)		/* Read configuration code. */
#define EFTRRT  0x40064831			/* Read the real time. */
#define EFTRT   _IOR('H',0xf4,struct _hilbuf4)	/* Read the timers for the
	                                              four voices. */
#ifdef hp800
#define EFTSBI  _IOW('H',0xa3,char)		/* Do the beep thing. */
#else
#define EFTSBI  _IOW('H',0xa3,struct _hilbuf2)	/* Set the bell information. */
#endif

/*
 * BSD ioctls.
 * Mostly the same as the HPUX versions except for shared-queue ioctls.
 */
#define OHILIOCID	0x400B6803		/* XXX compat */
#define HILIOCID	HILID
#define HILIOCSC	_IOR('h',0x33, struct _hilbuf16)
#define HILIOCRN	_IOR('h',0x30, struct _hilbuf16)
#define HILIOCRS	_IOR('h',0x31, struct _hilbuf16)
#define HILIOCED	_IOR('h',0x32, struct _hilbuf16)
#define HILIOCAROFF	_IO('h',0x3D)
#define HILIOCAR1	_IO('h',0x3E)
#define HILIOCAR2	_IO('h',0x3F)
#define HILIOCSBP	_IOW('H',0xc4,struct _hilbuf4)
#define OHILIOCRRT	0x40054831		/* XXX compat */
#define HILIOCRRT	EFTRRT
#define HILIOCRT	_IOR('H',0xf4,struct _hilbuf4)
#define HILIOCBEEP	_IOW('H',0xA3,struct _hilbell)
#	define	BELLDUR		80	/* tone duration in msec (10 - 2560) */
#	define	BELLFREQ	8	/* tone frequency (0 - 63) */

#define HILIOCALLOCQ	_IOWR('H',0x72, struct hilqinfo)	/* allocate queue */
#define HILIOCFREEQ	_IOW('H',0x73, struct hilqinfo)	/* deallocate queue */
#define HILIOCMAPQ	_IOW('H',0x74, int)	/* map device to queue */
#define HILIOCUNMAPQ	_IOW('H',0x75, int)	/* unmap device from dev */
#define HILIOCTEST      _IOW('H',0x76, int)	/* Toggle debugging mode */
#define HILIOCHPUX	_IO('H',0x77)		/* use HPUX (read) semantics */
#define HILIOCRESET	_IO('H',0x78)		/* Reset the HIL loop. */

/*
 * HIL input queue.
 * This is the circular queue (allocated by HILIOCALLOC) shared by kernel
 * and user.  It consists of a sixteen byte header followed by space for
 * 255 input data packets (a total of 4096 bytes).  The kernel adds packets
 * at tail.  The user is expected to remove packets from head.  This is the
 * only field in the header that the user should modify.
 */
typedef struct hil_packet {
	u_char	size;		/* total packet size */
	u_char	dev;		/* loop device packet was generated by */
	long	tstamp;		/* time stamp */
	u_char	data[10];	/* device data */
} hil_packet;

typedef struct hil_eventqueue {
	int	size;
	int	head;
	int	tail;
	int	pad;
} hil_eventqueue;

typedef union hilqueue {
	char	hqu_size[0x1000];
	struct	q_data {
		hil_eventqueue	h_eventqueue;
		hil_packet	h_event[1];
	} q_data;
#define hil_evqueue	q_data.h_eventqueue
#define hil_event	q_data.h_event
} HILQ;

#define HEVQSIZE	\
	((sizeof(HILQ) - sizeof(struct q_data)) / sizeof(hil_packet) + 1)
