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
 * from: Utah $Hdr: hil.c 1.33 89/12/22$
 *
 *	@(#)hil.c	7.4 (Berkeley) 6/22/90
 */

#include "param.h"
#include "conf.h"
#include "user.h"
#include "proc.h"
#include "ioctl.h"
#include "file.h"
#include "tty.h"
#include "systm.h"
#include "uio.h"
#include "kernel.h"
#include "mapmem.h"

#include "hilreg.h"
#include "hilioctl.h"
#include "hilvar.h"
#include "kbdmap.h"

#include "machine/cpu.h"

struct	hilloop	hil0;
struct	_hilbell default_bell = { BELLDUR, BELLFREQ };

#ifdef MAPMEM
int	hilqfork(), hilqvfork(), hilqexit();
struct	mapmemops hilqops = { hilqfork, hilqvfork, hilqexit, hilqexit };
#endif

#ifdef DEBUG
int 	hildebug = 0;
#define HDB_FOLLOW	0x01
#define HDB_MMAP	0x02
#define HDB_MASK	0x04
#define HDB_CONFIG	0x08
#define HDB_KEYBOARD	0x10
#define HDB_IDMODULE	0x20
#define HDB_EVENTS	0x80
#endif

/* symbolic sleep message strings */
char hilin[] = "hilin";

hilinit()
{
  	register struct hilloop *hilp = &hil0;	/* XXX */
	register int i;

	/*
	 * Initialize loop information
	 */
	hilp->hl_addr = HILADDR;
	hilp->hl_cmdending = FALSE;
	hilp->hl_actdev = hilp->hl_cmddev = 0;
	hilp->hl_cmddone = FALSE;
	hilp->hl_cmdbp = hilp->hl_cmdbuf;
	hilp->hl_pollbp = hilp->hl_pollbuf;
	hilp->hl_kbddev = 0;
	hilp->hl_kbdlang = KBD_DEFAULT;
	hilp->hl_kbdflags = 0;
	/*
	 * Clear all queues and device associations with queues
	 */
	for (i = 0; i < NHILQ; i++) {
		hilp->hl_queue[i].hq_eventqueue = NULL;
		hilp->hl_queue[i].hq_procp = NULL;
		hilp->hl_queue[i].hq_devmask = 0;
	}
	for (i = 0; i < NHILD; i++)
		hilp->hl_device[i].hd_qmask = 0;
	hilp->hl_device[HILLOOPDEV].hd_flags = (HIL_ALIVE|HIL_PSEUDO);
	/*
	 * Reset the loop hardware, and collect keyboard/id info
	 */
	hilreset(hilp);
	hilinfo(hilp);
	kbdenable();
}

hilopen(dev, flags)
	dev_t dev;
{
	struct proc *p = u.u_procp;		/* XXX */
  	register struct hilloop *hilp = &hil0;	/* XXX */
	register struct hilloopdev *dptr;
	u_char device = HILUNIT(dev);

#ifdef DEBUG
	if (hildebug & HDB_FOLLOW)
		printf("hilopen(%d): device %x\n", p->p_pid, device);
#endif
	
	if ((hilp->hl_device[HILLOOPDEV].hd_flags & HIL_ALIVE) == 0)
		return(ENXIO);

	dptr = &hilp->hl_device[device];
	if ((dptr->hd_flags & HIL_ALIVE) == 0)
		return(ENODEV);

	/*
	 * Pseudo-devices cannot be read, nothing more to do.
	 */
	if (dptr->hd_flags & HIL_PSEUDO)
		return(0);

	/*
	 * Open semantics:
	 * 1.	Open devices have only one of HIL_READIN/HIL_QUEUEIN.
	 * 2.	HPUX processes always get read syscall interface and
	 *	must have exclusive use of the device.
	 * 3.	BSD processes default to shared queue interface.
	 *	Multiple processes can open the device.
	 */
	if (p->p_flag & SHPUX) {
		if (dptr->hd_flags & (HIL_READIN|HIL_QUEUEIN))
			return(EBUSY);
		dptr->hd_flags |= HIL_READIN;
	} else {
		if (dptr->hd_flags & HIL_READIN)
			return(EBUSY);
		dptr->hd_flags |= HIL_QUEUEIN;
	}
	if (flags & FNDELAY)
		dptr->hd_flags |= HIL_NOBLOCK;
	/*
	 * It is safe to flush the read buffer as we are guarenteed
	 * that no one else is using it.
	 */
	ndflush(&dptr->hd_queue, dptr->hd_queue.c_cc);

	send_hil_cmd(hilp->hl_addr, HIL_INTON, NULL, 0, NULL);
	/*
	 * Opened the keyboard, put in raw mode.
	 */
	(void) splhil();
	if (device == hilp->hl_kbddev) {
		u_char mask = 0;
		send_hil_cmd(hilp->hl_addr, HIL_WRITEKBDSADR, &mask, 1, NULL);
		hilp->hl_kbdflags |= KBD_RAW;
#ifdef DEBUG
		if (hildebug & HDB_KEYBOARD)
			printf("hilopen: keyboard %d raw\n", hilp->hl_kbddev);
#endif
	}
	(void) spl0();
	return (0);
}

/* ARGSUSED */
hilclose(dev, flags)
	dev_t dev;
{
	struct proc *p = u.u_procp;		/* XXX */
  	register struct hilloop *hilp = &hil0;	/* XXX */
	register struct hilloopdev *dptr;
	register int i;
	u_char device = HILUNIT(dev);
	char mask, lpctrl;

#ifdef DEBUG
	if (hildebug & HDB_FOLLOW)
		printf("hilclose(%d): device %x\n", p->p_pid, device);
#endif

	dptr = &hilp->hl_device[device];
	if (device && (dptr->hd_flags & HIL_PSEUDO))
		return (0);

	if ((p->p_flag & SHPUX) == 0) {
		/*
		 * If this is the loop device,
		 * free up all queues belonging to this process.
		 */
		if (device == 0) {
			for (i = 0; i < NHILQ; i++)
				if (hilp->hl_queue[i].hq_procp == p)
					(void) hilqfree(i);
		} else {
			mask = ~hildevmask(device);
			(void) splhil();
			for (i = 0; i < NHILQ; i++)
				if (hilp->hl_queue[i].hq_procp == p) {
					dptr->hd_qmask &= ~hilqmask(i);
					hilp->hl_queue[i].hq_devmask &= mask;
				}
			(void) spl0();
		}
	}
	/*
	 * Always flush the read buffer
	 */
	dptr->hd_flags &= ~(HIL_QUEUEIN|HIL_READIN|HIL_NOBLOCK);
	ndflush(&dptr->hd_queue, dptr->hd_queue.c_cc);
	/*
	 * Set keyboard back to cooked mode when closed.
	 */
	(void) splhil();
	if (device && device == hilp->hl_kbddev) {
		mask = 1 << (hilp->hl_kbddev - 1);
		send_hil_cmd(hilp->hl_addr, HIL_WRITEKBDSADR, &mask, 1, NULL);
		hilp->hl_kbdflags &= ~(KBD_RAW|KBD_AR1|KBD_AR2);
		/*
		 * XXX: We have had trouble with keyboards remaining raw
		 * after close due to the LPC_KBDCOOK bit getting cleared
		 * somewhere along the line.  Hence we check and reset
		 * LPCTRL if necessary.
		 */
		send_hil_cmd(hilp->hl_addr, HIL_READLPCTRL, NULL, 0, &lpctrl);
		if ((lpctrl & LPC_KBDCOOK) == 0) {
			printf("hilclose: bad LPCTRL %x, reset to %x\n",
			       lpctrl, lpctrl|LPC_KBDCOOK);
			lpctrl |= LPC_KBDCOOK;
			send_hil_cmd(hilp->hl_addr, HIL_WRITELPCTRL,
					&lpctrl, 1, NULL);
		}
#ifdef DEBUG
		if (hildebug & HDB_KEYBOARD)
			printf("hilclose: keyboard %d cooked\n",
			       hilp->hl_kbddev);
#endif
		kbdenable();
	}
	(void) spl0();
	return (0);
}

/*
 * Read interface to HIL device.
 */
hilread(dev, uio)
	dev_t dev;
	register struct uio *uio;
{
	struct hilloop *hilp = &hil0;		/* XXX */
	register struct hilloopdev *dptr;
	register int cc;
	u_char device = HILUNIT(dev);
	char buf[HILBUFSIZE];
	int error;

#if 0
	/*
	 * XXX: Don't do this since HP-UX doesn't.
	 *
	 * Check device number.
	 * This check is necessary since loop can reconfigure.
	 */
	if (device > hilp->hl_maxdev)
		return(ENODEV);
#endif

	dptr = &hilp->hl_device[device];
	if ((dptr->hd_flags & HIL_READIN) == 0)
		return(ENODEV);

	(void) splhil();
	while (dptr->hd_queue.c_cc == 0) {
		if (dptr->hd_flags & HIL_NOBLOCK) {
			spl0();
			return(EWOULDBLOCK);
		}
		dptr->hd_flags |= HIL_ASLEEP;
		if (error = tsleep((caddr_t)dptr, TTIPRI | PCATCH, hilin, 0)) {
			(void) spl0();
			return (error);
		}
	}
	(void) spl0();

	error = 0;
	while (uio->uio_resid > 0 && error == 0) {
		cc = hilq_to_b(&dptr->hd_queue, buf,
			       MIN(uio->uio_resid, HILBUFSIZE));
		if (cc <= 0)
			break;
		error = uiomove(buf, cc, uio);
	}
	return(error);
}

hilioctl(dev, cmd, data, flag)
	dev_t dev;
	caddr_t data;
{
	struct proc *p = u.u_procp;		/* XXX */
	register struct hilloop *hilp = &hil0;	/* XXX */
	char device = HILUNIT(dev);
	struct hilloopdev *dptr;
	register int i;
	u_char hold;
	int error;

#ifdef DEBUG
	if (hildebug & HDB_FOLLOW)
		printf("hilioctl(%d): dev %x cmd %x\n",
		       p->p_pid, device, cmd);
#endif

	dptr = &hilp->hl_device[device];
	if ((dptr->hd_flags & HIL_ALIVE) == 0)
		return (ENODEV);

	/*
	 * Don't allow hardware ioctls on virtual devices.
	 * Note that though these are the BSD names, they have the same
	 * values as the HP-UX equivalents so we catch them as well.
	 */
	if (dptr->hd_flags & HIL_PSEUDO) {
		switch (cmd) {
		case HILIOCSC:
		case HILIOCID:
		case HILIOCRN:
		case HILIOCRS:
		case HILIOCED:
			return(ENODEV);

		/*
		 * XXX: should also return ENODEV but HP-UX compat
		 * breaks if we do.  They work ok right now because
		 * we only recognize one keyboard on the loop.  This
		 * will have to change if we remove that restriction.
		 */
		case HILIOCAROFF:
		case HILIOCAR1:
		case HILIOCAR2:
			break;

		default:
			break;
		}
	}

#ifdef HPUXCOMPAT
	if (p->p_flag & SHPUX)
		return(hpuxhilioctl(dev, cmd, data, flag));
#endif

	hilp->hl_cmdbp = hilp->hl_cmdbuf;
	bzero((caddr_t)hilp->hl_cmdbuf, HILBUFSIZE);
	hilp->hl_cmddev = device;
	error = 0;
	switch (cmd) {

	case HILIOCSBP:
		/* Send four data bytes to the tone gererator. */
		send_hil_cmd(hilp->hl_addr, HIL_STARTCMD, data, 4, NULL);
		/* Send the trigger beeper command to the 8042. */
		send_hil_cmd(hilp->hl_addr, (cmd & 0xFF), NULL, 0, NULL);
		break;

	case HILIOCRRT:
		/* Transfer the real time to the 8042 data buffer */
		send_hil_cmd(hilp->hl_addr, (cmd & 0xFF), NULL, 0, NULL);
		/* Read each byte of the real time */
		for (i = 0; i < 5; i++) {
			send_hil_cmd(hilp->hl_addr, HIL_READTIME + i, NULL,
					0, &hold);
			data[4-i] = hold;
		}
		break;
		
	case HILIOCRT:
		for (i = 0; i < 4; i++) {
			send_hil_cmd(hilp->hl_addr, (cmd & 0xFF) + i,
					NULL, 0, &hold);
			data[i] = hold;
		}
		break;

	case HILIOCID:
	case HILIOCSC:
	case HILIOCRN:
	case HILIOCRS:
	case HILIOCED:
	  	send_hildev_cmd(hilp, device, (cmd & 0xFF));
		bcopy(hilp->hl_cmdbuf, data, hilp->hl_cmdbp-hilp->hl_cmdbuf);
	  	break;

        case HILIOCAROFF:
        case HILIOCAR1:
        case HILIOCAR2:
		if (hilp->hl_kbddev) {
			hilp->hl_cmddev = hilp->hl_kbddev;
			send_hildev_cmd(hilp, hilp->hl_kbddev, (cmd & 0xFF));
			hilp->hl_kbdflags &= ~(KBD_AR1|KBD_AR2);
			if (cmd == HILIOCAR1)
				hilp->hl_kbdflags |= KBD_AR1;
			else if (cmd == HILIOCAR2)
				hilp->hl_kbdflags |= KBD_AR2;
		}
		break;

	case HILIOCBEEP:
		hilbeep(hilp, (struct _hilbell *)data);
		break;

	case FIONBIO:
		dptr = &hilp->hl_device[device];
		if (*(int *)data)
			dptr->hd_flags |= HIL_NOBLOCK;
		else
			dptr->hd_flags &= ~HIL_NOBLOCK;
		break;

	/*
	 * FIOASYNC must be present for FIONBIO above to work!
	 * (See fcntl in kern_descrip.c).
	 */
	case FIOASYNC:
		break;

        case HILIOCALLOCQ:
		error = hilqalloc((struct hilqinfo *)data);
		break;

        case HILIOCFREEQ:
		error = hilqfree(((struct hilqinfo *)data)->qid);
		break;

        case HILIOCMAPQ:
		error = hilqmap(*(int *)data, device);
		break;

        case HILIOCUNMAPQ:
		error = hilqunmap(*(int *)data, device);
		break;

	case HILIOCHPUX:
		dptr = &hilp->hl_device[device];
		dptr->hd_flags |= HIL_READIN;
		dptr->hd_flags &= ~HIL_QUEUEIN;
		break;

        case HILIOCRESET:
	        hilreset(hilp);
		break;
		
#ifdef DEBUG
        case HILIOCTEST:
		hildebug = *(int *) data;
		break;
#endif

        default:
		error = EINVAL;
		break;

	}
	hilp->hl_cmddev = 0;
	return(error);
}

#ifdef HPUXCOMPAT
/* ARGSUSED */
hpuxhilioctl(dev, cmd, data, flag)
	dev_t dev;
	caddr_t data;
{
	register struct hilloop *hilp = &hil0;	/* XXX */
	char device = HILUNIT(dev);
	struct hilloopdev *dptr;
	register int i;
	u_char hold;

	hilp->hl_cmdbp = hilp->hl_cmdbuf;
	bzero((caddr_t)hilp->hl_cmdbuf, HILBUFSIZE);
	hilp->hl_cmddev = device;
	switch (cmd) {

	case HILSC:
	case HILID:
	case HILRN:
	case HILRS:
	case HILED:
	case HILP1:
	case HILP2:
	case HILP3:
	case HILP4:
	case HILP5:
	case HILP6:
	case HILP7:
	case HILP:
	case HILA1:
	case HILA2:
	case HILA3:
	case HILA4:
	case HILA5:
	case HILA6:
	case HILA7:
	case HILA:
		send_hildev_cmd(hilp, device, (cmd & 0xFF));
		bcopy(hilp->hl_cmdbuf, data, hilp->hl_cmdbp-hilp->hl_cmdbuf);
	  	break;

        case HILDKR:
        case HILER1:
        case HILER2:
		if (hilp->hl_kbddev) {
			hilp->hl_cmddev = hilp->hl_kbddev;
			send_hildev_cmd(hilp, hilp->hl_kbddev, (cmd & 0xFF));
			hilp->hl_kbdflags &= ~(KBD_AR1|KBD_AR2);
			if (cmd == HILIOCAR1)
				hilp->hl_kbdflags |= KBD_AR1;
			else if (cmd == HILIOCAR2)
				hilp->hl_kbdflags |= KBD_AR2;
		}
		break;

	case EFTSBP:
		/* Send four data bytes to the tone gererator. */
		send_hil_cmd(hilp->hl_addr, HIL_STARTCMD, data, 4, NULL);
		/* Send the trigger beeper command to the 8042. */
		send_hil_cmd(hilp->hl_addr, (cmd & 0xFF), NULL, 0, NULL);
		break;

	case EFTRRT:
		/* Transfer the real time to the 8042 data buffer */
		send_hil_cmd(hilp->hl_addr, (cmd & 0xFF), NULL, 0, NULL);
		/* Read each byte of the real time */
		for (i = 0; i < 5; i++) {
			send_hil_cmd(hilp->hl_addr, HIL_READTIME + i, NULL,
					0, &hold);
			data[4-i] = hold;
		}
		break;
		
	case EFTRT:
		for (i = 0; i < 4; i++) {
			send_hil_cmd(hilp->hl_addr, (cmd & 0xFF) + i,
					NULL, 0, &hold);
			data[i] = hold;
		}
		break;

        case EFTRLC:
        case EFTRCC:
		send_hil_cmd(hilp->hl_addr, (cmd & 0xFF), NULL, 0, &hold);
		*data = hold;
		break;
		
        case EFTSRPG:
        case EFTSRD:
        case EFTSRR:
		send_hil_cmd(hilp->hl_addr, (cmd & 0xFF), data, 1, NULL);
		break;
		
	case EFTSBI:
		hilbeep(hilp, (struct _hilbell *)data);
		break;

	case FIONBIO:
		dptr = &hilp->hl_device[device];
		if (*(int *)data)
			dptr->hd_flags |= HIL_NOBLOCK;
		else
			dptr->hd_flags &= ~HIL_NOBLOCK;
		break;

	case FIOASYNC:
		break;

        default:
		hilp->hl_cmddev = 0;
		return(EINVAL);
	}
	hilp->hl_cmddev = 0;
	return(0);
}
#endif

/*
 * XXX: the mmap inteface for HIL devices should be rethought.
 * We used it only briefly in conjuntion with shared queues
 * (instead of HILIOCMAPQ ioctl).  Perhaps mmap()ing a device
 * should give a single queue per process.
 */
/* ARGSUSED */
hilmap(dev, off, prot)
	dev_t dev;
	register int off;
{
#ifdef MMAP
	struct proc *p = u.u_procp;		/* XXX */
	register struct hilloop *hilp = &hil0;	/* XXX */
	register struct hiliqueue *qp;
	register int qnum;

	/*
	 * Only allow mmap() on loop device
	 */
	if (HILUNIT(dev) != 0 || off >= NHILQ*sizeof(HILQ))
		return(-1);
	/*
	 * Determine which queue we want based on the offset.
	 * Queue must belong to calling process.
	 */
	qp = &hilp->hl_queue[off / sizeof(HILQ)];
	if (qp->hq_procp != p)
		return(-1);

	off %= sizeof(HILQ);
	return(kvtop((u_int)qp->hq_eventqueue + off) >> PGSHIFT);
#endif
}

/*ARGSUSED*/
hilselect(dev, rw)
	dev_t dev;
{
	struct proc *p = u.u_procp;		/* XXX */
	register struct hilloop *hilp = &hil0;	/* XXX */
	register struct hilloopdev *dptr;
	register struct hiliqueue *qp;
	register int mask;
	int s, device;
	
	if (rw == FWRITE)
		return (1);
	device = HILUNIT(dev);

	/*
	 * Read interface.
	 * Return 1 if there is something in the queue, 0 ow.
	 */
	dptr = &hilp->hl_device[device];
	if (dptr->hd_flags & HIL_READIN) {
		s = splhil();
		if (dptr->hd_queue.c_cc) {
			splx(s);
			return (1);
		}
		if (dptr->hd_selr &&
		    dptr->hd_selr->p_wchan == (caddr_t)&selwait)
			dptr->hd_flags |= HIL_SELCOLL;
		else
			dptr->hd_selr = p;
		splx(s);
		return (0);
	}

	/*
	 * Make sure device is alive and real (or the loop device).
	 * Note that we do not do this for the read interface.
	 * This is primarily to be consistant with HP-UX.
	 */
	if (device && (dptr->hd_flags & (HIL_ALIVE|HIL_PSEUDO)) != HIL_ALIVE)
		return (1);

	/*
	 * Select on loop device is special.
	 * Check to see if there are any data for any loop device
	 * provided it is associated with a queue belonging to this user.
	 */
	if (device == 0)
		mask = -1;
	else
		mask = hildevmask(device);
	/*
	 * Must check everybody with interrupts blocked to prevent races.
	 */
	s = splhil();
	for (qp = hilp->hl_queue; qp < &hilp->hl_queue[NHILQ]; qp++)
		if (qp->hq_procp == p && (mask & qp->hq_devmask) &&
		    qp->hq_eventqueue->hil_evqueue.head !=
		    qp->hq_eventqueue->hil_evqueue.tail) {
			splx(s);
			return (1);
		}

	if (dptr->hd_selr && dptr->hd_selr->p_wchan == (caddr_t)&selwait)
		dptr->hd_flags |= HIL_SELCOLL;
	else
		dptr->hd_selr = p;
	splx(s);
	return (0);
}

hilint()
{
	struct hilloop *hilp = &hil0;		/* XXX */
	register struct hil_dev *hildevice = hilp->hl_addr;
	u_char c, stat;

	stat = hildevice->hil_stat;
	c = hildevice->hil_data;		/* clears interrupt */
	hil_process_int(stat, c);
}

#include "ite.h"

hil_process_int(stat, c)
	register u_char stat, c;
{
  	register struct hilloop *hilp;

#ifdef DEBUG
	if (hildebug & HDB_EVENTS)
		printf("hilint: %x %x\n", stat, c);
#endif

	/* the shift enables the compiler to generate a jump table */
	switch ((stat>>HIL_SSHIFT) & HIL_SMASK) {

#if NITE > 0
	case HIL_KEY:
	case HIL_SHIFT:
	case HIL_CTRL:
	case HIL_CTRLSHIFT:
		itefilter(stat, c);
		return;
#endif
		
	case HIL_STATUS:			/* The status info. */
		hilp = &hil0;			/* XXX */
		if (c & HIL_ERROR) {
		  	hilp->hl_cmddone = TRUE;
			if (c == HIL_RECONFIG)
				hilconfig(hilp);
			break;
		}
		if (c & HIL_COMMAND) {
		  	if (c & HIL_POLLDATA)	/* End of data */
				hilevent(hilp);
			else			/* End of command */
			  	hilp->hl_cmdending = TRUE;
			hilp->hl_actdev = 0;
		} else {
		  	if (c & HIL_POLLDATA) {	/* Start of polled data */
			  	if (hilp->hl_actdev != 0)
					hilevent(hilp);
				hilp->hl_actdev = (c & HIL_DEVMASK);
				hilp->hl_pollbp = hilp->hl_pollbuf;
			} else {		/* Start of command */
				if (hilp->hl_cmddev == (c & HIL_DEVMASK)) {
					hilp->hl_cmdbp = hilp->hl_cmdbuf;
					hilp->hl_actdev = 0;
				}
			}
		}
	        return;

	case HIL_DATA:
		hilp = &hil0;			/* XXX */
		if (hilp->hl_actdev != 0)	/* Collecting poll data */
			*hilp->hl_pollbp++ = c;
		else if (hilp->hl_cmddev != 0)  /* Collecting cmd data */
			if (hilp->hl_cmdending) {
				hilp->hl_cmddone = TRUE;
				hilp->hl_cmdending = FALSE;
			} else  
				*hilp->hl_cmdbp++ = c;
		return;
		
	case 0:		/* force full jump table */
	default:
		return;
	}
}

#if defined(DEBUG) && !defined(PANICBUTTON)
#define PANICBUTTON
#endif

/*
 * Optimized macro to compute:
 *	eq->head == (eq->tail + 1) % eq->size
 * i.e. has tail caught up with head.  We do this because 32 bit long
 * remaidering is expensive (a function call with our compiler).
 */
#define HQFULL(eq)	(((eq)->head?(eq)->head:(eq)->size) == (eq)->tail+1)
#define HQVALID(eq) \
	((eq)->size == HEVQSIZE && (eq)->tail >= 0 && (eq)->tail < HEVQSIZE)

hilevent(hilp)
	struct hilloop *hilp;
{
	register struct hilloopdev *dptr = &hilp->hl_device[hilp->hl_actdev];
	register int len, mask, qnum;
	register u_char *cp, *pp;
	register HILQ *hq;
	struct timeval ourtime;
	hil_packet *proto;
	int s, len0;
	long tenths;

#ifdef PANICBUTTON
	static int first;
	extern int panicbutton;

	cp = hilp->hl_pollbuf;
	if (panicbutton && (*cp & HIL_KBDDATA)) {
		if (*++cp == 0x4E)
			first = 1;
		else if (first && *cp == 0x46 && !panicstr)
			panic("are we having fun yet?");
		else
			first = 0;
	}
#endif
#ifdef DEBUG
	if (hildebug & HDB_EVENTS) {
		printf("hilevent: dev %d pollbuf: ", hilp->hl_actdev);
		printhilpollbuf(hilp);
		printf("\n");
	}
#endif

	/*
	 * Note that HIL_READIN effectively "shuts off" any queues
	 * that may have been in use at the time of an HILIOCHPUX call.
	 */
	if (dptr->hd_flags & HIL_READIN) {
		hpuxhilevent(hilp, dptr);
		return;
	}

	/*
	 * If this device isn't on any queue or there are no data
	 * in the packet (can this happen?) do nothing.
	 */
	if (dptr->hd_qmask == 0 ||
	    (len0 = hilp->hl_pollbp - hilp->hl_pollbuf) <= 0)
		return;

	/*
	 * Everybody gets the same time stamp
	 */
	s = splclock();
	ourtime = time;
	splx(s);
	tenths = (ourtime.tv_sec * 100) + (ourtime.tv_usec / 10000);

	proto = NULL;
	mask = dptr->hd_qmask;
	for (qnum = 0; mask; qnum++) {
		if ((mask & hilqmask(qnum)) == 0)
			continue;
		mask &= ~hilqmask(qnum);
		hq = hilp->hl_queue[qnum].hq_eventqueue;
		
		/*
		 * Ensure that queue fields that we rely on are valid
		 * and that there is space in the queue.  If either
		 * test fails, we just skip this queue.
		 */
		if (!HQVALID(&hq->hil_evqueue) || HQFULL(&hq->hil_evqueue))
			continue;

		/*
		 * Copy data to queue.
		 * If this is the first queue we construct the packet
		 * with length, timestamp and poll buffer data.
		 * For second and sucessive packets we just duplicate
		 * the first packet.
		 */
		pp = (u_char *) &hq->hil_event[hq->hil_evqueue.tail];
		if (proto == NULL) {
			proto = (hil_packet *)pp;
			cp = hilp->hl_pollbuf;
			len = len0;
			*pp++ = len + 6;
			*pp++ = hilp->hl_actdev;
			*(long *)pp = tenths;
			pp += sizeof(long);
			do *pp++ = *cp++; while (--len);
		} else
			*(hil_packet *)pp = *proto;

		if (++hq->hil_evqueue.tail == hq->hil_evqueue.size)
			hq->hil_evqueue.tail = 0;
	}

	/*
	 * Wake up anyone selecting on this device or the loop itself
	 */
	if (dptr->hd_selr) {
		selwakeup(dptr->hd_selr, dptr->hd_flags & HIL_SELCOLL);
		dptr->hd_selr = NULL;
		dptr->hd_flags &= ~HIL_SELCOLL;
	}
	dptr = &hilp->hl_device[HILLOOPDEV];
	if (dptr->hd_selr) {
		selwakeup(dptr->hd_selr, dptr->hd_flags & HIL_SELCOLL);
		dptr->hd_selr = NULL;
		dptr->hd_flags &= ~HIL_SELCOLL;
	}
}

#undef HQFULL

hpuxhilevent(hilp, dptr)
	register struct hilloop *hilp;
	register struct hilloopdev *dptr;
{
	register int len;
	struct timeval ourtime;
	long tstamp;
	int s;

	/*
	 * Everybody gets the same time stamp
	 */
	s = splclock();
	ourtime = time;
	splx(s);
	tstamp = (ourtime.tv_sec * 100) + (ourtime.tv_usec / 10000);

	/*
	 * Each packet that goes into the buffer must be preceded by the
	 * number of bytes in the packet, and the timestamp of the packet.
	 * This adds 5 bytes to the packet size. Make sure there is enough
	 * room in the buffer for it, and if not, toss the packet.
	 */
	len = hilp->hl_pollbp - hilp->hl_pollbuf;
	if (dptr->hd_queue.c_cc <= (HILMAXCLIST - (len+5))) {
		putc(len+5, &dptr->hd_queue);
		(void) b_to_q((char *)&tstamp, sizeof tstamp, &dptr->hd_queue);
		(void) b_to_q((char *)hilp->hl_pollbuf, len, &dptr->hd_queue);
	}

	/*
	 * Wake up any one blocked on a read or select
	 */
	if (dptr->hd_flags & HIL_ASLEEP) {
		dptr->hd_flags &= ~HIL_ASLEEP;
		wakeup((caddr_t)dptr);
	}
	if (dptr->hd_selr) {
		selwakeup(dptr->hd_selr, dptr->hd_flags & HIL_SELCOLL);
		dptr->hd_selr = NULL;
		dptr->hd_flags &= ~HIL_SELCOLL;
	}
}

/*
 * Shared queue manipulation routines
 */

hilqalloc(qip)
	struct hilqinfo *qip;
{
#ifdef MAPMEM
	struct proc *p = u.u_procp;		/* XXX */
	register struct hilloop *hilp = &hil0;	/* XXX */
	register HILQ *hq;
	register int qnum;
	struct mapmem *mp;
	int error, hilqmapin();

#ifdef DEBUG
	if (hildebug & HDB_FOLLOW)
		printf("hilqalloc(%d): addr %x\n",
		       p->p_pid, qip->addr);
#endif
	/*
	 * Find a free queue
	 */
	for (qnum = 0; qnum < NHILQ; qnum++)
		if (hilp->hl_queue[qnum].hq_procp == NULL)
			break;
	if (qnum == NHILQ)
		return(EMFILE);

	/*
	 * Allocate and clear memory for the queue
	 */
	if (hilp->hl_queue[qnum].hq_eventqueue)
		panic("hilqalloc");
	hq = (HILQ *) cialloc(sizeof(HILQ));
	if (hq == NULL)
		return(ENOMEM);
	bzero((caddr_t)hq, sizeof(HILQ));
	hilp->hl_queue[qnum].hq_eventqueue = hq;
	hq->hil_evqueue.size = HEVQSIZE;

	/*
	 * Map queue into user address space as instructed
	 */
	error = mmalloc(p, qnum, &qip->addr, sizeof(HILQ), MM_RW|MM_CI,
			&hilqops, &mp);
	if (error) {
		cifree((caddr_t)hq, sizeof(HILQ));
		hilp->hl_queue[qnum].hq_eventqueue = NULL;
		return(error);
	}
	qip->qid = qnum;
	if (error = mmmapin(p, mp, hilqmapin)) {
		(void) mmfree(p, mp);
		cifree((caddr_t)hq, sizeof(HILQ));
		hilp->hl_queue[qnum].hq_eventqueue = NULL;
		return(error);
	}
	hilp->hl_queue[qnum].hq_procp = p;
	hilp->hl_queue[qnum].hq_devmask = 0;
	return(0);
#else
	return(EINVAL);
#endif
}

hilqfree(qnum)
	register int qnum;
{
#ifdef MAPMEM
	struct proc *p = u.u_procp;		/* XXX */
	register struct hilloop *hilp = &hil0;	/* XXX */
	register struct mapmem *mp;

#ifdef DEBUG
	if (hildebug & HDB_FOLLOW)
		printf("hilqfree(%d): qnum %d\n",
		       p->p_pid, qnum);
#endif
	if (qnum >= NHILQ || hilp->hl_queue[qnum].hq_procp != p)
		return(EINVAL);
	for (mp = u.u_mmap; mp; mp = mp->mm_next)
		if (qnum == mp->mm_id && mp->mm_ops == &hilqops) {
			(void) hilqexit(mp);
			return(0);
		}
	panic("hilqfree");
	/* NOTREACHED */
#else
	return(EINVAL);
#endif
}

hilqmap(qnum, device)
	register int qnum, device;
{
	struct proc *p = u.u_procp;		/* XXX */
	register struct hilloop *hilp = &hil0;	/* XXX */
	register struct hilloopdev *dptr = &hilp->hl_device[device];
	int s;

#ifdef DEBUG
	if (hildebug & HDB_FOLLOW)
		printf("hilqmap(%d): qnum %d device %x\n",
		       p->p_pid, qnum, device);
#endif
	if (qnum >= NHILQ || hilp->hl_queue[qnum].hq_procp != p)
		return(EINVAL);
	if ((dptr->hd_flags & HIL_QUEUEIN) == 0)
		return(EINVAL);
	if (dptr->hd_qmask && u.u_uid && u.u_uid != dptr->hd_uid)
		return(EPERM);

	hilp->hl_queue[qnum].hq_devmask |= hildevmask(device);
	if (dptr->hd_qmask == 0)
		dptr->hd_uid = u.u_uid;
	s = splhil();
	dptr->hd_qmask |= hilqmask(qnum);
	splx(s);
#ifdef DEBUG
	if (hildebug & HDB_MASK)
		printf("hilqmap(%d): devmask %x qmask %x\n",
		       p->p_pid, hilp->hl_queue[qnum].hq_devmask,
		       dptr->hd_qmask);
#endif
	return(0);
}

hilqunmap(qnum, device)
	register int qnum, device;
{
	struct proc *p = u.u_procp;		/* XXX */
	register struct hilloop *hilp = &hil0;	/* XXX */
	int s;

#ifdef DEBUG
	if (hildebug & HDB_FOLLOW)
		printf("hilqunmap(%d): qnum %d device %x\n",
		       p->p_pid, qnum, device);
#endif

	if (qnum >= NHILQ || hilp->hl_queue[qnum].hq_procp != p)
		return(EINVAL);

	hilp->hl_queue[qnum].hq_devmask &= ~hildevmask(device);
	s = splhil();
	hilp->hl_device[device].hd_qmask &= ~hilqmask(qnum);
	splx(s);
#ifdef DEBUG
	if (hildebug & HDB_MASK)
		printf("hilqunmap(%d): devmask %x qmask %x\n",
		       p->p_pid, hilp->hl_queue[qnum].hq_devmask,
		       hilp->hl_device[device].hd_qmask);
#endif
	return(0);
}

#ifdef MAPMEM
hilqmapin(mp, off)
	struct mapmem *mp;
{
	struct hilloop *hilp = &hil0;		/* XXX */
	register HILQ *hq = hilp->hl_queue[mp->mm_id].hq_eventqueue;

	if (hq == NULL || off >= sizeof(HILQ))
		return(-1);
	return(kvtop((u_int)hq + off) >> PGSHIFT);
}

/*
 * Fork hook.
 * Unmap queue from child's address space
 */
hilqfork(mp, ischild)
	struct mapmem *mp;
{
	struct proc *p = u.u_procp;		/* XXX */
#ifdef DEBUG
	if (hildebug & HDB_MMAP)
		printf("hilqfork(%d): %s qnum %d\n", p->p_pid,
		       ischild ? "child" : "parent", mp->mm_id);
#endif
	if (ischild) {
		mmmapout(p, mp);
		(void) mmfree(p, mp);
	}
}

/*
 * Vfork hook.
 * Associate queue with child when VM resources are passed.
 */
hilqvfork(mp, fup, tup)
	struct mapmem *mp;
	struct user *fup, *tup;
{
	struct hilloop *hilp = &hil0;		/* XXX */
	register struct hiliqueue *qp = &hilp->hl_queue[mp->mm_id];

#ifdef DEBUG
	if (hildebug & HDB_MMAP)
		printf("hilqvfork(%d): from %x to %x qnum %d, qprocp %x\n",
		       u.u_procp->p_pid, fup->u_procp, tup->u_procp,
		       mp->mm_id, qp->hq_procp);
#endif
	if (qp->hq_procp == fup->u_procp)
		qp->hq_procp = tup->u_procp;
}

/*
 * Exit hook.
 * Unmap all devices and free all queues.
 */
hilqexit(mp)
	struct mapmem *mp;
{
	struct proc *p = u.u_procp;		/* XXX */
	register struct hilloop *hilp = &hil0;	/* XXX */
	register int mask, i;
	int s;

#ifdef DEBUG
	if (hildebug & HDB_MMAP)
		printf("hilqexit(%d): qnum %d\n", p->p_pid, mp->mm_id);
#endif
	/*
	 * Atomically take all devices off the queue
	 */
	mask = ~hilqmask(mp->mm_id);
	s = splhil();
	for (i = 0; i < NHILD; i++)
		hilp->hl_device[i].hd_qmask &= mask;
	splx(s);
	/*
	 * Now unmap from user address space and free queue
	 */
	i = mp->mm_id;
	cifree((caddr_t)hilp->hl_queue[i].hq_eventqueue, sizeof(HILQ));
	hilp->hl_queue[i].hq_eventqueue = NULL;
	hilp->hl_queue[i].hq_procp = NULL;
	return(mmfree(p, mp));
}
#endif

#include "clist.h"

/*
 * This is just a copy of the virgin q_to_b routine with minor
 * optimizations for HIL use.  It is used for two reasons:
 * 1. If we have PAGE mode defined, the normal q_to_b processes
 *    chars one at a time and breaks on newlines.
 * 2. We don't have to raise the priority to spltty() for most
 *    of the clist manipulations.
 */
hilq_to_b(q, cp, cc)
	register struct clist *q;
	register char *cp;
{
	register struct cblock *bp;
	register int nc;
	char *acp;
	int s;
	extern char cwaiting;

	if (cc <= 0)
		return (0);
	s = splhil();
	if (q->c_cc <= 0) {
		q->c_cc = 0;
		q->c_cf = q->c_cl = NULL;
		splx(s);
		return (0);
	}
	acp = cp;

	while (cc) {
		nc = sizeof (struct cblock) - ((int)q->c_cf & CROUND);
		nc = MIN(nc, cc);
		nc = MIN(nc, q->c_cc);
		(void) bcopy(q->c_cf, cp, (unsigned)nc);
		q->c_cf += nc;
		q->c_cc -= nc;
		cc -= nc;
		cp += nc;
		if (q->c_cc <= 0) {
			bp = (struct cblock *)(q->c_cf - 1);
			bp = (struct cblock *)((int)bp & ~CROUND);
			q->c_cf = q->c_cl = NULL;
			spltty();
			bp->c_next = cfreelist;
			cfreelist = bp;
			cfreecount += CBSIZE;
			if (cwaiting) {
				wakeup(&cwaiting);
				cwaiting = 0;
			}
			break;
		}
		if (((int)q->c_cf & CROUND) == 0) {
			bp = (struct cblock *)(q->c_cf);
			bp--;
			q->c_cf = bp->c_next->c_info;
			spltty();
			bp->c_next = cfreelist;
			cfreelist = bp;
			cfreecount += CBSIZE;
			if (cwaiting) {
				wakeup(&cwaiting);
				cwaiting = 0;
			}
			splhil();
		}
	}
	splx(s);
	return (cp-acp);
}

/*
 * Cooked keyboard functions for ite driver.
 * There is only one "cooked" ITE keyboard (the first keyboard found)
 * per loop.  There may be other keyboards, but they will always be "raw".
 */

kbdbell()
{
	struct hilloop *hilp = &hil0;		/* XXX */

	hilbeep(hilp, &default_bell);
}

kbdenable()
{
	struct hilloop *hilp = &hil0;	/* XXX */
	register struct hil_dev *hildevice = hilp->hl_addr;
	char db;

	/* Set the autorepeat rate register */
	db = ar_format(KBD_ARR);
	send_hil_cmd(hildevice, HIL_SETARR, &db, 1, NULL);

	/* Set the autorepeat delay register */
	db = ar_format(KBD_ARD);
	send_hil_cmd(hildevice, HIL_SETARD, &db, 1, NULL);

	/* Enable interrupts */
	send_hil_cmd(hildevice, HIL_INTON, NULL, 0, NULL);
}

kbddisable()
{
}

/*
 * XXX: read keyboard directly and return code.
 * Used by console getchar routine.  Could really screw up anybody
 * reading from the keyboard in the normal, interrupt driven fashion.
 */
kbdgetc(statp)
	int *statp;
{
	struct hilloop *hilp = &hil0;		/* XXX */
	register struct hil_dev *hildevice = hilp->hl_addr;
	register int c, stat;
	int s;

	s = splhil();
	while (((stat = hildevice->hil_stat) & HIL_DATA_RDY) == 0)
		;
	c = hildevice->hil_data;
	splx(s);
	*statp = stat;
	return(c);
}

/*
 * Recoginize and clear keyboard generated NMIs.
 * Returns 1 if it was ours, 0 otherwise.  Note that we cannot use
 * send_hil_cmd() to issue the clear NMI command as that would actually
 * lower the priority to splimp() and it doesn't wait for the completion
 * of the command.  Either of these conditions could result in the
 * interrupt reoccuring.  Note that we issue the CNMT command twice.
 * This seems to be needed, once is not always enough!?!
 */
kbdnmi()
{
	register struct hilloop *hilp = &hil0;		/* XXX */

	if ((*KBDNMISTAT & KBDNMI) == 0)
		return(0);
	HILWAIT(hilp->hl_addr);
	hilp->hl_addr->hil_cmd = HIL_CNMT;
	HILWAIT(hilp->hl_addr);
	hilp->hl_addr->hil_cmd = HIL_CNMT;
	HILWAIT(hilp->hl_addr);
	return(1);
}

#define HILSECURITY	0x33
#define HILIDENTIFY	0x03
#define HILSCBIT	0x04

/*
 * Called at boot time to print out info about interesting devices
 */
hilinfo(hilp)
	register struct hilloop *hilp;
{
	register int id, len;
	register struct kbdmap *km;

	/*
	 * Keyboard info.
	 */
	if (hilp->hl_kbddev) {
		printf("hil%d: ", hilp->hl_kbddev);
		for (km = kbd_map; km->kbd_code; km++)
			if (km->kbd_code == hilp->hl_kbdlang) {
				printf("%s ", km->kbd_desc);
				break;
			}
		printf("keyboard\n");
	}
	/*
	 * ID module.
	 * Attempt to locate the first ID module and print out its
	 * security code.  Is this a good idea??
	 */
	id = hiliddev(hilp);
	if (id) {
		hilp->hl_cmdbp = hilp->hl_cmdbuf;
		hilp->hl_cmddev = id;
		send_hildev_cmd(hilp, id, HILSECURITY);
		len = hilp->hl_cmdbp - hilp->hl_cmdbuf;
		hilp->hl_cmdbp = hilp->hl_cmdbuf;
		hilp->hl_cmddev = 0;
		printf("hil%d: security code", id);
		for (id = 0; id < len; id++)
			printf(" %x", hilp->hl_cmdbuf[id]);
		while (id++ < 16)
			printf(" 0");
		printf("\n");
	}
}

#define HILAR1	0x3E
#define HILAR2	0x3F

/*
 * Called after the loop has reconfigured.  Here we need to:
 *	- determine how many devices are on the loop
 *	  (some may have been added or removed)
 *	- locate the ITE keyboard (if any) and ensure
 *	  that it is in the proper state (raw or cooked)
 *	  and is set to use the proper language mapping table
 *	- ensure all other keyboards are raw
 * Note that our device state is now potentially invalid as
 * devices may no longer be where they were.  What we should
 * do here is either track where the devices went and move
 * state around accordingly or, more simply, just mark all
 * devices as HIL_DERROR and don't allow any further use until
 * they are closed.  This is a little too brutal for my tastes,
 * we prefer to just assume people won't move things around.
 */
hilconfig(hilp)
	register struct hilloop *hilp;
{
	u_char db;
	int s;

	s = splhil();
#ifdef DEBUG
	if (hildebug & HDB_CONFIG) {
		printf("hilconfig: reconfigured: ");
		send_hil_cmd(hilp->hl_addr, HIL_READLPSTAT, NULL, 0, &db);
		printf("LPSTAT %x, ", db);
		send_hil_cmd(hilp->hl_addr, HIL_READLPCTRL, NULL, 0, &db);
		printf("LPCTRL %x, ", db);
		send_hil_cmd(hilp->hl_addr, HIL_READKBDSADR, NULL, 0, &db);
		printf("KBDSADR %x\n", db);
		hilreport(hilp);
	}
#endif
	/*
	 * Determine how many devices are on the loop.
	 * Mark those as alive and real, all others as dead.
	 */
	db = 0;
	send_hil_cmd(hilp->hl_addr, HIL_READLPSTAT, NULL, 0, &db);
	hilp->hl_maxdev = db & LPS_DEVMASK;
	for (db = 1; db < NHILD; db++) {
		if (db <= hilp->hl_maxdev)
			hilp->hl_device[db].hd_flags |= HIL_ALIVE;
		else
			hilp->hl_device[db].hd_flags &= ~HIL_ALIVE;
		hilp->hl_device[db].hd_flags &= ~HIL_PSEUDO;
	}
#ifdef DEBUG
	if (hildebug & (HDB_CONFIG|HDB_KEYBOARD))
		printf("hilconfig: max device %d\n", hilp->hl_maxdev);
#endif
	if (hilp->hl_maxdev == 0) {
		hilp->hl_kbddev = 0;
		splx(s);
		return;
	}
	/*
	 * Find out where the keyboards are and record the ITE keyboard
	 * (first one found).  If no keyboards found, we are all done.
	 */
	db = 0;
	send_hil_cmd(hilp->hl_addr, HIL_READKBDSADR, NULL, 0, &db);
#ifdef DEBUG
	if (hildebug & HDB_KEYBOARD)
		printf("hilconfig: keyboard: KBDSADR %x, old %d, new %d\n",
		       db, hilp->hl_kbddev, ffs((int)db));
#endif
	hilp->hl_kbddev = ffs((int)db);
	if (hilp->hl_kbddev == 0) {
		splx(s);
		return;
	}
	/*
	 * Determine if the keyboard should be cooked or raw and configure it.
	 */
	db = (hilp->hl_kbdflags & KBD_RAW) ? 0 : 1 << (hilp->hl_kbddev - 1);
	send_hil_cmd(hilp->hl_addr, HIL_WRITEKBDSADR, &db, 1, NULL);
	/*
	 * Re-enable autorepeat in raw mode, cooked mode AR is not affected.
	 */
	if (hilp->hl_kbdflags & (KBD_AR1|KBD_AR2)) {
		db = (hilp->hl_kbdflags & KBD_AR1) ? HILAR1 : HILAR2;
		hilp->hl_cmddev = hilp->hl_kbddev;
		send_hildev_cmd(hilp, hilp->hl_kbddev, db);
		hilp->hl_cmddev = 0;
	}
	/*
	 * Determine the keyboard language configuration, but don't
	 * override a user-specified setting.
	 */
	db = 0;
	send_hil_cmd(hilp->hl_addr, HIL_READKBDLANG, NULL, 0, &db);
#ifdef DEBUG
	if (hildebug & HDB_KEYBOARD)
		printf("hilconfig: language: old %x new %x\n",
		       hilp->hl_kbdlang, db);
#endif
	if (hilp->hl_kbdlang != KBD_SPECIAL) {
		struct kbdmap *km;

		for (km = kbd_map; km->kbd_code; km++)
			if (km->kbd_code == db) {
				hilp->hl_kbdlang = db;
				/* XXX */
				kbd_keymap = km->kbd_keymap;
				kbd_shiftmap = km->kbd_shiftmap;
				kbd_ctrlmap = km->kbd_ctrlmap;
				kbd_ctrlshiftmap = km->kbd_ctrlshiftmap;
				kbd_stringmap = km->kbd_stringmap;
			}
	}
	splx(s);
}

hilreset(hilp)
	struct hilloop *hilp;
{
	register struct hil_dev *hildevice = hilp->hl_addr;
	u_char db;

	/*
	 * Initialize the loop: reconfigure, don't report errors,
	 * cook keyboards, and enable autopolling.
	 */
	db = LPC_RECONF | LPC_KBDCOOK | LPC_NOERROR | LPC_AUTOPOLL;
	send_hil_cmd(hildevice, HIL_WRITELPCTRL, &db, 1, NULL);
	/*
	 * Delay one second for reconfiguration and then read the the
	 * data register to clear the interrupt (if the loop reconfigured).
	 */
	DELAY(1000000);
	if (hildevice->hil_stat & HIL_DATA_RDY)
		db = hildevice->hil_data;
	/*
	 * The HIL loop may have reconfigured.  If so we proceed on,
	 * if not we loop until a successful reconfiguration is reported
	 * back to us.  The HIL loop will continue to attempt forever.
	 * Probably not very smart.
	 */
	do {
		send_hil_cmd(hildevice, HIL_READLPSTAT, NULL, 0, &db);
        } while ((db & (LPS_CONFFAIL|LPS_CONFGOOD)) == 0);
	/*
	 * At this point, the loop should have reconfigured.
	 * The reconfiguration interrupt has already called hilconfig()
	 * so the keyboard has been determined.
	 */
	send_hil_cmd(hildevice, HIL_INTON, NULL, 0, NULL);
}

hilbeep(hilp, bp)
	struct hilloop *hilp;
	register struct _hilbell *bp;
{
	u_char buf[2];

	buf[0] = ~((bp->duration - 10) / 10);
	buf[1] = bp->frequency;
	send_hil_cmd(hilp->hl_addr, HIL_SETTONE, buf, 2, NULL);
}

/*
 * Locate and return the address of the first ID module, 0 if none present.
 */
hiliddev(hilp)
	register struct hilloop *hilp;
{
	register int i, len;

#ifdef DEBUG
	if (hildebug & HDB_IDMODULE)
		printf("hiliddev(%x): looking for idmodule...", hilp);
#endif
	for (i = 1; i <= hilp->hl_maxdev; i++) {
		hilp->hl_cmdbp = hilp->hl_cmdbuf;
		hilp->hl_cmddev = i;
		send_hildev_cmd(hilp, i, HILIDENTIFY);
		/*
		 * XXX: the final condition checks to ensure that the
		 * device ID byte is in the range of the ID module (0x30-0x3F)
		 */
		len = hilp->hl_cmdbp - hilp->hl_cmdbuf;
		if (len > 1 && (hilp->hl_cmdbuf[1] & HILSCBIT) &&
		    (hilp->hl_cmdbuf[0] & 0xF0) == 0x30) {
			hilp->hl_cmdbp = hilp->hl_cmdbuf;
			hilp->hl_cmddev = i;
			send_hildev_cmd(hilp, i, HILSECURITY);
			break;
		}
	}		
	hilp->hl_cmdbp = hilp->hl_cmdbuf;
	hilp->hl_cmddev = 0;
#ifdef DEBUG
	if (hildebug & HDB_IDMODULE)
		if (i <= hilp->hl_maxdev)
			printf("found at %d\n", i);
		else
			printf("not found\n");
#endif
	return(i <= hilp->hl_maxdev ? i : 0);
}

/*
 * Low level routines which actually talk to the 8042 chip.
 */

/*
 * Send a command to the 8042 with zero or more bytes of data.
 * If rdata is non-null, wait for and return a byte of data.
 * We run at splimp() to make the transaction as atomic as
 * possible without blocking the clock (is this necessary?)
 */
send_hil_cmd(hildevice, cmd, data, dlen, rdata)
	register struct hil_dev *hildevice;
	u_char cmd, *data, dlen;
	u_char *rdata;
{
	u_char status;
	int s = splimp();

	HILWAIT(hildevice);
	hildevice->hil_cmd = cmd;
	while (dlen--) {
	  	HILWAIT(hildevice);
		hildevice->hil_data = *data++;
	}
	if (rdata) {
		do {
			HILDATAWAIT(hildevice);
			status = hildevice->hil_stat;
			*rdata = hildevice->hil_data;
		} while (((status >> HIL_SSHIFT) & HIL_SMASK) != HIL_68K);
	}
	splx(s);
}

/*
 * Send a command to a device on the loop.
 * Since only one command can be active on the loop at any time,
 * we must ensure that we are not interrupted during this process.
 * Hence we mask interrupts to prevent potential access from most
 * interrupt routines and turn off auto-polling to disable the
 * internally generated poll commands.
 *
 * splhigh is extremely conservative but insures atomic operation,
 * splimp (clock only interrupts) seems to be good enough in practice.
 */
send_hildev_cmd(hilp, device, cmd)
	register struct hilloop *hilp;
	char device, cmd;
{
	register struct hil_dev *hildevice = hilp->hl_addr;
	u_char status, c;
	int s = splimp();

	polloff(hildevice);

	/*
	 * Transfer the command and device info to the chip
	 */
	HILWAIT(hildevice);
	hildevice->hil_cmd = HIL_STARTCMD;
  	HILWAIT(hildevice);
	hildevice->hil_data = 8 + device;
  	HILWAIT(hildevice);
	hildevice->hil_data = cmd;
  	HILWAIT(hildevice);
	hildevice->hil_data = HIL_TIMEOUT;
	/*
	 * Trigger the command and wait for completion
	 */
	HILWAIT(hildevice);
	hildevice->hil_cmd = HIL_TRIGGER;
	hilp->hl_cmddone = FALSE;
	do {
		HILDATAWAIT(hildevice);
		status = hildevice->hil_stat;
		c = hildevice->hil_data;
		hil_process_int(status, c);
	} while (!hilp->hl_cmddone);

	pollon(hildevice);
	splx(s);
}

/*
 * Turn auto-polling off and on.
 * Also disables and enable auto-repeat.  Why?
 */
polloff(hildevice)
	register struct hil_dev *hildevice;
{
	register char db;

	/*
	 * Turn off auto repeat
	 */
	HILWAIT(hildevice);
	hildevice->hil_cmd = HIL_SETARR;
	HILWAIT(hildevice);
	hildevice->hil_data = 0;
	/*
	 * Turn off auto-polling
	 */
	HILWAIT(hildevice);
	hildevice->hil_cmd = HIL_READLPCTRL;
	HILDATAWAIT(hildevice);
	db = hildevice->hil_data;
	db &= ~LPC_AUTOPOLL;
	HILWAIT(hildevice);
	hildevice->hil_cmd = HIL_WRITELPCTRL;
	HILWAIT(hildevice);
	hildevice->hil_data = db;
	/*
	 * Must wait til polling is really stopped
	 */
	do {	
		HILWAIT(hildevice);
		hildevice->hil_cmd = HIL_READBUSY;
		HILDATAWAIT(hildevice);
		db = hildevice->hil_data;
	} while (db & BSY_LOOPBUSY);
}

pollon(hildevice)
	register struct hil_dev *hildevice;
{
	register char db;

	/*
	 * Turn on auto polling
	 */
	HILWAIT(hildevice);
	hildevice->hil_cmd = HIL_READLPCTRL;
	HILDATAWAIT(hildevice);
	db = hildevice->hil_data;
	db |= LPC_AUTOPOLL;
	HILWAIT(hildevice);
	hildevice->hil_cmd = HIL_WRITELPCTRL;
	HILWAIT(hildevice);
	hildevice->hil_data = db;
	/*
	 * Turn on auto repeat
	 */
	HILWAIT(hildevice);
	hildevice->hil_cmd = HIL_SETARR;
	HILWAIT(hildevice);
	hildevice->hil_data = ar_format(KBD_ARR);
}

#ifdef DEBUG
printhilpollbuf(hilp)
	register struct hilloop *hilp;
{
  	register u_char *cp;
	register int i, len;

	cp = hilp->hl_pollbuf;
	len = hilp->hl_pollbp - cp;
	for (i = 0; i < len; i++)
		printf("%x ", hilp->hl_pollbuf[i]);
	printf("\n");
}

printhilcmdbuf(hilp)
	register struct hilloop *hilp;
{
  	register u_char *cp;
	register int i, len;

	cp = hilp->hl_cmdbuf;
	len = hilp->hl_cmdbp - cp;
	for (i = 0; i < len; i++)
		printf("%x ", hilp->hl_cmdbuf[i]);
	printf("\n");
}

hilreport(hilp)
	register struct hilloop *hilp;
{
	register int i, len;
	int s = splhil();

	for (i = 1; i <= hilp->hl_maxdev; i++) {
		hilp->hl_cmdbp = hilp->hl_cmdbuf;
		hilp->hl_cmddev = i;
		send_hildev_cmd(hilp, i, HILIDENTIFY);
		printf("hil%d: id: ", i);
		printhilcmdbuf(hilp);
		len = hilp->hl_cmdbp - hilp->hl_cmdbuf;
		if (len > 1 && (hilp->hl_cmdbuf[1] & HILSCBIT)) {
			hilp->hl_cmdbp = hilp->hl_cmdbuf;
			hilp->hl_cmddev = i;
			send_hildev_cmd(hilp, i, HILSECURITY);
			printf("hil%d: sc: ", i);
			printhilcmdbuf(hilp);
		}
	}		
	hilp->hl_cmdbp = hilp->hl_cmdbuf;
	hilp->hl_cmddev = 0;
	splx(s);
}
#endif
