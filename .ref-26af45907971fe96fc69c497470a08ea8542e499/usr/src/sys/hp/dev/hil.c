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
 * from: Utah $Hdr: hil.c 1.38 92/01/21$
 *
 *	@(#)hil.c	7.15 (Berkeley) %G%
 */

#include <sys/param.h>
#include <sys/conf.h>
#include <sys/proc.h>
#include <sys/user.h>
#include <sys/ioctl.h>
#include <sys/file.h>
#include <sys/tty.h>
#include <sys/systm.h>
#include <sys/uio.h>
#include <sys/kernel.h>

#include <hp/dev/hilreg.h>
#include <hp/dev/hilioctl.h>
#include <hp/dev/hilvar.h>
#include <hp/dev/kbdmap.h>

#include <machine/cpu.h>

#include <vm/vm_param.h>
#include <vm/vm_map.h>
#include <vm/vm_kern.h>
#include <vm/vm_page.h>
#include <vm/vm_pager.h>

#ifdef hp300
#define NHIL	1	/* XXX */
#else
#include "hil.h"
#endif

struct  hilloop hilloop[NHIL];
struct	_hilbell default_bell = { BELLDUR, BELLFREQ };
#ifdef hp800
int	hilspl;
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

hilsoftinit(unit, hilbase)
	int unit;
	struct hil_dev *hilbase;
{
  	register struct hilloop *hilp = &hilloop[unit];
	register int i;

#ifdef DEBUG
	if (hildebug & HDB_FOLLOW)
		printf("hilsoftinit(%d, %x)\n", unit, hilbase);
#endif
	/*
	 * Initialize loop information
	 */
	hilp->hl_addr = hilbase;
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
}

hilinit(unit, hilbase)
	int unit;
	struct hil_dev *hilbase;
{
  	register struct hilloop *hilp = &hilloop[unit];
#ifdef DEBUG
	if (hildebug & HDB_FOLLOW)
		printf("hilinit(%d, %x)\n", unit, hilbase);
#endif
	/*
	 * Initialize software (if not already done).
	 */
	if ((hilp->hl_device[HILLOOPDEV].hd_flags & HIL_ALIVE) == 0)
		hilsoftinit(unit, hilbase);
	/*
	 * Initialize hardware.
	 * Reset the loop hardware, and collect keyboard/id info
	 */
	hilreset(hilp);
	hilinfo(unit);
	kbdenable(unit);
}

/* ARGSUSED */
hilopen(dev, flags, mode, p)
	dev_t dev;
	int flags, mode;
	struct proc *p;
{
  	register struct hilloop *hilp = &hilloop[HILLOOP(dev)];
	register struct hilloopdev *dptr;
	u_char device = HILUNIT(dev);

#ifdef DEBUG
	if (hildebug & HDB_FOLLOW)
		printf("hilopen(%d): loop %x device %x\n",
		       p->p_pid, HILLOOP(dev), device);
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
	if (p->p_md.md_flags & MDP_HPUX) {
		if (dptr->hd_flags & (HIL_READIN|HIL_QUEUEIN))
			return(EBUSY);
		dptr->hd_flags |= HIL_READIN;
	} else {
		if (dptr->hd_flags & HIL_READIN)
			return(EBUSY);
		dptr->hd_flags |= HIL_QUEUEIN;
	}
	if (flags & FNONBLOCK)
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
hilclose(dev, flags, mode, p)
	dev_t dev;
	struct proc *p;
{
  	register struct hilloop *hilp = &hilloop[HILLOOP(dev)];
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

	if (p && (p->p_md.md_flags & MDP_HPUX) == 0) {
		/*
		 * If this is the loop device,
		 * free up all queues belonging to this process.
		 */
		if (device == 0) {
			for (i = 0; i < NHILQ; i++)
				if (hilp->hl_queue[i].hq_procp == p)
					(void) hilqfree(hilp, i);
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
		kbdenable(HILLOOP(dev));
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
	struct hilloop *hilp = &hilloop[HILLOOP(dev)];
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
			       min(uio->uio_resid, HILBUFSIZE));
		if (cc <= 0)
			break;
		error = uiomove(buf, cc, uio);
	}
	return(error);
}

hilioctl(dev, cmd, data, flag, p)
	dev_t dev;
	caddr_t data;
	struct proc *p;
{
	register struct hilloop *hilp = &hilloop[HILLOOP(dev)];
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
		case OHILIOCID:
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
	if (p->p_md.md_flags & MDP_HPUX)
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

	case OHILIOCRRT:
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
	case OHILIOCID:
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
		error = hilqalloc(hilp, (struct hilqinfo *)data);
		break;

        case HILIOCFREEQ:
		error = hilqfree(hilp, ((struct hilqinfo *)data)->qid);
		break;

        case HILIOCMAPQ:
		error = hilqmap(hilp, *(int *)data, device);
		break;

        case HILIOCUNMAPQ:
		error = hilqunmap(hilp, *(int *)data, device);
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
	register struct hilloop *hilp = &hilloop[HILLOOP(dev)];
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
#ifdef hp800
		/* XXX big magic */
		hold = 7 - (*(u_char *)data >> 5);
		*(int *)data = 0x84069008 | (hold << 8);
		send_hil_cmd(hilp->hl_addr, HIL_STARTCMD, data, 4, NULL);
		send_hil_cmd(hilp->hl_addr, 0xC4, NULL, 0, NULL);
		break;
#else
		hilbeep(hilp, (struct _hilbell *)data);
#endif
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

/* ARGSUSED */
hilmap(dev, off, prot)
	dev_t dev;
	register int off;
{
}

/*ARGSUSED*/
hilselect(dev, rw, p)
	dev_t dev;
	struct proc *p;
{
	register struct hilloop *hilp = &hilloop[HILLOOP(dev)];
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
		selrecord(p, &dptr->hd_selr);
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

	selrecord(p, &dptr->hd_selr);
	splx(s);
	return (0);
}

/*ARGSUSED*/
hilint(unit)
{
#ifdef hp300
	struct hilloop *hilp = &hilloop[0]; /* XXX how do we know on 300? */
#else
	struct hilloop *hilp = &hilloop[unit];
#endif
	register struct hil_dev *hildevice = hilp->hl_addr;
	u_char c, stat;

	stat = READHILSTAT(hildevice);
	c = READHILDATA(hildevice);		/* clears interrupt */
	hil_process_int(hilp, stat, c);
}

#include "ite.h"

hil_process_int(hilp, stat, c)
	register struct hilloop *hilp;
	register u_char stat, c;
{
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
	selwakeup(&dptr->hd_selr);
	dptr = &hilp->hl_device[HILLOOPDEV];
	selwakeup(&dptr->hd_selr);
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
	selwakeup(&dptr->hd_selr);
}

/*
 * Shared queue manipulation routines
 */

hilqalloc(hilp, qip)
	register struct hilloop *hilp;
	struct hilqinfo *qip;
{
	struct proc *p = curproc;		/* XXX */

#ifdef DEBUG
	if (hildebug & HDB_FOLLOW)
		printf("hilqalloc(%d): addr %x\n", p->p_pid, qip->addr);
#endif
	return(EINVAL);
}

hilqfree(hilp, qnum)
	register int qnum;
{
	struct proc *p = curproc;		/* XXX */

#ifdef DEBUG
	if (hildebug & HDB_FOLLOW)
		printf("hilqfree(%d): qnum %d\n", p->p_pid, qnum);
#endif
	return(EINVAL);
}

hilqmap(hilp, qnum, device)
	register struct hilloop *hilp;
	register int qnum, device;
{
	struct proc *p = curproc;		/* XXX */
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
	if (dptr->hd_qmask && p->p_ucred->cr_uid &&
	    p->p_ucred->cr_uid != dptr->hd_uid)
		return(EPERM);

	hilp->hl_queue[qnum].hq_devmask |= hildevmask(device);
	if (dptr->hd_qmask == 0)
		dptr->hd_uid = p->p_ucred->cr_uid;
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

hilqunmap(hilp, qnum, device)
	register struct hilloop *hilp;
	register int qnum, device;
{
	struct proc *p = curproc;		/* XXX */
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

/*
 * Cooked keyboard functions for ite driver.
 * There is only one "cooked" ITE keyboard (the first keyboard found)
 * per loop.  There may be other keyboards, but they will always be "raw".
 */

kbdbell(unit)
	int unit;
{
	struct hilloop *hilp = &hilloop[unit];

	hilbeep(hilp, &default_bell);
}

kbdenable(unit)
	int unit;
{
	struct hilloop *hilp = &hilloop[unit];
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

kbddisable(unit)
	int unit;
{
}

/*
 * XXX: read keyboard directly and return code.
 * Used by console getchar routine.  Could really screw up anybody
 * reading from the keyboard in the normal, interrupt driven fashion.
 */
kbdgetc(unit, statp)
	int unit, *statp;
{
	struct hilloop *hilp = &hilloop[unit];
	register struct hil_dev *hildevice = hilp->hl_addr;
	register int c, stat;
	int s;

	s = splhil();
	while (((stat = READHILSTAT(hildevice)) & HIL_DATA_RDY) == 0)
		;
	c = READHILDATA(hildevice);
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
kbdnmi(unit)
	int unit;
{
#ifdef hp300
	struct hilloop *hilp = &hilloop[0]; /* XXX how do we know on 300? */
#else
	struct hilloop *hilp = &hilloop[unit];
#endif
#ifdef hp300
	if ((*KBDNMISTAT & KBDNMI) == 0)
		return(0);
#endif
	HILWAIT(hilp->hl_addr);
	WRITEHILCMD(hilp->hl_addr, HIL_CNMT);
	HILWAIT(hilp->hl_addr);
	WRITEHILCMD(hilp->hl_addr, HIL_CNMT);
	HILWAIT(hilp->hl_addr);
	return(1);
}

#define HILSECURITY	0x33
#define HILIDENTIFY	0x03
#define HILSCBIT	0x04

/*
 * Called at boot time to print out info about interesting devices
 */
hilinfo(unit)
	int unit;
{
  	register struct hilloop *hilp = &hilloop[unit];
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
#ifdef DEBUG
	if (hildebug & HDB_CONFIG)
		printf("hilconfig: %d devices found\n", hilp->hl_maxdev);
#endif
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

#ifdef DEBUG
	if (hildebug & HDB_FOLLOW)
		printf("hilreset(%x)\n", hilp);
#endif
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
	if (READHILSTAT(hildevice) & HIL_DATA_RDY)
		db = READHILDATA(hildevice);
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
		printf("hiliddev(%x): max %d, looking for idmodule...",
		       hilp, hilp->hl_maxdev);
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

#ifdef HPUXCOMPAT
/*
 * XXX map devno as expected by HP-UX
 */
hildevno(dev)
	dev_t dev;
{
	int newdev;

	newdev = 24 << 24;
#ifdef HILCOMPAT
	/*
	 * XXX compat check
	 * Don't convert old style specfiles already in correct format
	 */
	if (minor(dev) && (dev & 0xF) == 0)
		newdev |= minor(dev);
	else
#endif
	newdev |= (HILLOOP(dev) << 8) | (HILUNIT(dev) << 4);
	return(newdev);
}
#endif

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
	WRITEHILCMD(hildevice, cmd);
	while (dlen--) {
	  	HILWAIT(hildevice);
		WRITEHILDATA(hildevice, *data++);
	}
	if (rdata) {
		do {
			HILDATAWAIT(hildevice);
			status = READHILSTAT(hildevice);
			*rdata = READHILDATA(hildevice);
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
	WRITEHILCMD(hildevice, HIL_STARTCMD);
  	HILWAIT(hildevice);
	WRITEHILDATA(hildevice, 8 + device);
  	HILWAIT(hildevice);
	WRITEHILDATA(hildevice, cmd);
  	HILWAIT(hildevice);
	WRITEHILDATA(hildevice, HIL_TIMEOUT);
	/*
	 * Trigger the command and wait for completion
	 */
	HILWAIT(hildevice);
	WRITEHILCMD(hildevice, HIL_TRIGGER);
	hilp->hl_cmddone = FALSE;
	do {
		HILDATAWAIT(hildevice);
		status = READHILSTAT(hildevice);
		c = READHILDATA(hildevice);
		hil_process_int(hilp, status, c);
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
	WRITEHILCMD(hildevice, HIL_SETARR);
	HILWAIT(hildevice);
	WRITEHILDATA(hildevice, 0);
	/*
	 * Turn off auto-polling
	 */
	HILWAIT(hildevice);
	WRITEHILCMD(hildevice, HIL_READLPCTRL);
	HILDATAWAIT(hildevice);
	db = READHILDATA(hildevice);
	db &= ~LPC_AUTOPOLL;
	HILWAIT(hildevice);
	WRITEHILCMD(hildevice, HIL_WRITELPCTRL);
	HILWAIT(hildevice);
	WRITEHILDATA(hildevice, db);
	/*
	 * Must wait til polling is really stopped
	 */
	do {	
		HILWAIT(hildevice);
		WRITEHILCMD(hildevice, HIL_READBUSY);
		HILDATAWAIT(hildevice);
		db = READHILDATA(hildevice);
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
	WRITEHILCMD(hildevice, HIL_READLPCTRL);
	HILDATAWAIT(hildevice);
	db = READHILDATA(hildevice);
	db |= LPC_AUTOPOLL;
	HILWAIT(hildevice);
	WRITEHILCMD(hildevice, HIL_WRITELPCTRL);
	HILWAIT(hildevice);
	WRITEHILDATA(hildevice, db);
	/*
	 * Turn on auto repeat
	 */
	HILWAIT(hildevice);
	WRITEHILCMD(hildevice, HIL_SETARR);
	HILWAIT(hildevice);
	WRITEHILDATA(hildevice, ar_format(KBD_ARR));
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
