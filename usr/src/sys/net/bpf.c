/*
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that: (1) source code distributions
 * retain the above copyright notice and this paragraph in its entirety, (2)
 * distributions including binary code include the above copyright notice and
 * this paragraph in its entirety in the documentation or other materials
 * provided with the distribution, and (3) all advertising materials mentioning
 * features or use of this software display the following acknowledgement:
 * ``This product includes software developed by the University of California,
 * Lawrence Berkeley Laboratory and its contributors.'' Neither the name of
 * the University nor the names of its contributors may be used to endorse
 * or promote products derived from this software without specific prior
 * written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 * This code is derived from the Stanford/CMU enet packet filter,
 * (net/enet.c) distributed in 4.3BSD Unix.
 */
#ifndef lint
static char rcsid[] =
    "$Header: bpf.c,v 1.23 91/01/30 18:22:13 mccanne Exp $";
#endif

#include "bpfilter.h"

#if (NBPFILTER > 0)

#ifndef __GNUC__
#define inline
#endif

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/mbuf.h>
#include <sys/buf.h>
#include <sys/dir.h>
#include <sys/user.h>
#include <sys/ioctl.h>
#include <sys/map.h>
#include <sys/proc.h>

#include <sys/file.h>
#ifdef sparc
#include <sys/stream.h>
#endif
#include <sys/tty.h>
#include <sys/uio.h>

#include <sys/protosw.h>
#include <sys/socket.h>
#include <net/if.h>

#include <net/bpf.h>
#include <net/bpfdesc.h>

#include <sys/errno.h>

#include <netinet/in.h>
#include <netinet/if_ether.h>
#include <sys/kernel.h>

#define PRINET  26			/* interruptible */

/*
 *  'bpf_iftab' is the driver state table per logical unit number
 *  'bpf_dtab' holds the descriptors, indexed by minor device #
 *  'bpf_units' is the number of attached units
 *
 * We really don't need NBPFILTER bpf_if entries, but this eliminates
 * the need to account for all possible drivers here.
 * This problem will go away when these structures are allocated dynamically.
 */
static struct bpf_if 	bpf_iftab[NBPFILTER];
static struct bpf_d	bpf_dtab[NBPFILTER];
static u_int		bpf_units = 0;

static int	bpf_timeout();
static void	bpf_ifname();
static void	catchpacket();
static int	bpf_setif();
static int	bpf_initd();

/*
 * The default filter accepts the maximum number of bytes from each packet.
 */
struct bpf_insn bpf_default_filter[] = {
	BPF_STMT(RetOp, MCLBYTES),
};

/*
 * This routine was inspired by/stolen from ../sys/uipc_socket.c
 * Move data from 'm' to user's read buffer.  
 * We assume 'm' is not chained.
 * Returns error code (or 0 if success).
 */
static inline int
bpf_moveout(m, uio)
	register struct mbuf *m;
	register struct uio *uio;
{
	register int len;
	
	len = m->m_len;
	if (uio->uio_resid < len)
		len = uio->uio_resid;

	if (len > 0)
		return uiomove(mtod(m, caddr_t), len, UIO_READ, uio);

	return 0;
}

static int
bpf_movein(uio, linktype, mp, sockp)
	register struct uio *uio;
	int linktype;
	register struct mbuf **mp;
	register struct sockaddr *sockp;
{
	struct mbuf *m;
	int error;
	int len;
	int hlen;

	/*
	 * Build a sockaddr based on the data link layer type.
	 * We do this at this level because the ethernet header
	 * is copied directly into the data field of the sockaddr.
	 * In the case of SLIP, there is no header and the packet
	 * is forwarded as is.
	 * Also, we are careful to leave room at the front of the mbuf
	 * for the link level header.
	 */
	switch (linktype) {
	case DLT_SLIP:
		sockp->sa_family = AF_INET;
		hlen = 0;
		break;

	case DLT_EN10MB:
		sockp->sa_family = AF_UNSPEC;
		/* XXX Would MAXLINKHDR be better? */
		hlen = sizeof(struct ether_header);
		break;

       case DLT_FDDI:
		sockp->sa_family = AF_UNSPEC;
		/* XXX 4(FORMAC)+6(dst)+6(src)+3(LLC)+5(SNAP) */
		hlen = 24;
		break;

	default:
		return EIO;
	}

	len = uio->uio_resid;
	if ((unsigned)len > MCLBYTES)
		return EIO;

	MGET(m, M_WAIT, MT_DATA);
	if (m == 0)
		return ENOBUFS;
	if (len > MLEN) {
		MCLGET(m);
		if (m->m_len != MCLBYTES) {
			error = ENOBUFS;
			goto bad;
		}
	}
	m->m_len = len;
	*mp = m;
	/*
	 * Make room for link header.
	 */
	if (hlen) {
		m->m_len -= hlen;
		m->m_off += hlen;

		error = uiomove((caddr_t)sockp->sa_data, hlen, UIO_WRITE, uio);
		if (error)
			goto bad;
	}
	error = uiomove(mtod(m, caddr_t), len - hlen, UIO_WRITE, uio);
	if (!error) 
		return 0;
 bad:
	m_freem(m);
	return error;
}

/*
 * Attach 'd' to the bpf interface 'bp', i.e. make 'd' listen on 'bp'.
 * Must be called at splimp.
 */
static void
bpf_attachd(d, bp)
	struct bpf_d *d;
	struct bpf_if *bp;
{
	/* Point 'd' at 'bp'. */
	d->bd_bif = bp;

	/* Add 'd' to 'bp's list of listeners. */
	d->bd_next = bp->bif_dlist;
	bp->bif_dlist = d;

	/*
	 * Let the driver know we're here (if it doesn't already).
	 */
	*bp->bif_driverp = bp;
}

static void
bpf_detachd(d)
	struct bpf_d *d;
{
	struct bpf_d **p;
	struct bpf_if *bp;

	bp = d->bd_bif;
	/*
	 * Check if this descriptor had requested promiscuous mode.
	 * If so, turn it off.
	 */
	if (d->bd_promisc) {
		d->bd_promisc = 0;
		if (ifpromisc(bp->bif_ifp, 0))
			/*
			 * Something is really wrong if we were able to put
			 * the driver into promiscuous mode, but can't
			 * take it out.
			 */
			panic("bpf_detachd: exit promisc unsucessful");
	}
	/* Remove 'd' from the interface's descriptor list. */
	p = &bp->bif_dlist;
	while (*p != d) {
		p = &(*p)->bd_next;
		if (*p == 0)
			panic("bpf_detachd: descriptor not in list");
	}
	*p = (*p)->bd_next;
	if (bp->bif_dlist == 0)
		/*
		 * Let the driver know that there are no more listeners.
		 */
		*d->bd_bif->bif_driverp = 0;
	d->bd_bif = 0;
}


/*
 * Mark a descriptor free by making it point to itself. 
 * This is probably cheaper than marking with a constant since
 * the address should be in a register anyway.
 */
#define D_ISFREE(d) ((d) == (d)->bd_next)
#define D_MARKFREE(d) ((d)->bd_next = (d))
#define D_MARKUSED(d) ((d)->bd_next = 0)

/*
 *  bpfopen - open ethernet device
 *
 *  Errors:	ENXIO	- illegal minor device number
 *		EBUSY	- too many files open
 */
/* ARGSUSED */
int
bpfopen(dev, flag)
	dev_t dev;
	int flag;
{
	int error, s;
	register struct bpf_d *d;
	
	if (minor(dev) >= NBPFILTER)
		return ENXIO;

	/*
	 * Each minor can be opened by only one process.  If the requested
	 * minor is in use, return EBUSY.
	 */
	s = splimp();
	d = &bpf_dtab[minor(dev)];
	if (!D_ISFREE(d)) {
		splx(s);
		return EBUSY;
	} else
		/* Mark "free" and do most initialization. */
		bzero((char *)d, sizeof(*d));
	d->bd_filter = bpf_default_filter;
	splx(s);

	error = bpf_initd(d);
	if (error) {
		D_MARKFREE(d);
		return error;
	}
	return 0;
}

/*
 * Close the descriptor by detaching it from its interface,
 * deallocating its buffers, and marking it free.
 */
/* ARGSUSED */
bpfclose(dev, flag)
	dev_t dev;
	int flag;
{
	register struct bpf_d *d = &bpf_dtab[minor(dev)];
	int s;

	s = splimp();
	if (d->bd_bif)
		bpf_detachd(d);
	splx(s);

	/* Let the buffers go. */
	m_freem(d->bd_hbuf);
	m_freem(d->bd_sbuf);
	m_freem(d->bd_fbuf);
	m_freem(d->bd_filterm);
	
	D_MARKFREE(d);
}

#define RS_IDLE 0
#define RS_WAIT 1
#define RS_TIMEDOUT 2

/*
 *  bpfread - read next chunk of packets from buffers
 */
int
bpfread(dev, uio)
	dev_t dev;
	register struct uio *uio;
{
	register struct bpf_d *d = &bpf_dtab[minor(dev)];
	struct mbuf *m;
	int error;
	int s;

	/*
	 * Restrict application to use a buffer the same size as 
	 * as kernel buffers.
	 */
	if (uio->uio_resid != MCLBYTES)
		return EIO;

	s = splimp();
	/*
	 * If the hold buffer is empty, then set a timer and sleep
	 * until either the timeout has occurred or enough packets have
	 * arrived to fill the store buffer.
	 */
	while (d->bd_hbuf == 0) {
		if (d->bd_immediate && d->bd_sbuf->m_len) {
			/*
			 * A packet(s) either arrived since the previous
			 * read or arrived while we were asleep.
			 * Rotate the buffers and return what's here.
			 */
			d->bd_hbuf = d->bd_sbuf;
			d->bd_sbuf = d->bd_fbuf;
			d->bd_sbuf->m_len = 0;
			d->bd_fbuf = 0;
			break;
		}
		if (d->bd_rtout) {
			/*
			 * If there was a previous timeout pending for this 
			 * file, cancel it before setting another.  This is
			 * necessary since a cancel after the sleep might 
			 * never happen if the read is interrupted by a signal.
			 */
			if (d->bd_state == RS_WAIT)
				untimeout(bpf_timeout, (caddr_t)d);
			timeout(bpf_timeout, (caddr_t)d, (int)d->bd_rtout);
			d->bd_state = RS_WAIT;
		}
		else
			d->bd_state = RS_IDLE;
		
		sleep((caddr_t)d, PRINET);
		
		if (d->bd_state == RS_WAIT) {
			untimeout(bpf_timeout, (caddr_t)d);
			d->bd_state = RS_IDLE;
		}
		else if (d->bd_state == RS_TIMEDOUT) {
			/*
			 * On a timeout, return what's in the buffer,
			 * which may be nothing.  We do this by moving
			 * the store buffer into the hold slot.
			 */
			if (d->bd_hbuf)
				/*
				 * We filled up the buffer in between 
				 * getting the timeout and arriving
				 * here, so we don't need to rotate.
				 */
				break;

			if (d->bd_sbuf->m_len == 0) {
				splx(s);
				return(0);
			}
			d->bd_hbuf = d->bd_sbuf;
			d->bd_sbuf = d->bd_fbuf;
			d->bd_sbuf->m_len = 0;
			d->bd_fbuf = 0;
			break;
		}
	}
	/*
	 * At this point, we know we have something in the hold slot.
	 */
	m = d->bd_hbuf;

	splx(s);
	
	/*  
	 * Move data from hold buffer into user space.
	 * We know the entire buffer is transferred since
	 * we checked above that the read buffer is MCLBYTES.
	 */
	error = bpf_moveout(m, uio);

	s = splimp();
	if (d->bd_fbuf != 0)
		panic("bpfread: free mbuf slot occupied");
	d->bd_fbuf = m;
	d->bd_hbuf = (struct mbuf *)0;
	splx(s);
	
	return error;
}


/*
 * If there are processes select sleeping on this descriptor,
 * wake them up.  
 */
static inline void
bpf_wakeup(d)
	register struct bpf_d *d;
{
	if (d->bd_SelProc) {
		selwakeup(d->bd_SelProc, (int)d->bd_SelColl);
		d->bd_SelColl = 0;
		d->bd_SelProc = 0;
	}
}

/*
 *  bpf_timeout - process ethernet read timeout
 */
static int
bpf_timeout(d)
	register struct bpf_d * d;
{
	register int s = splimp();

	d->bd_state = RS_TIMEDOUT;
	wakeup((caddr_t)d);
	bpf_wakeup(d);

	splx(s);
}

int
bpfwrite(dev, uio)
	dev_t dev;
	struct uio *uio;
{
	register struct bpf_d *d = &bpf_dtab[minor(dev)];
	struct ifnet *ifp;
	struct mbuf *m;
	int error, s;
	static struct sockaddr dst;

	if (d->bd_bif == 0)
		return ENXIO;

	ifp = d->bd_bif->bif_ifp;

	if (uio->uio_resid == 0)
		return 0;
	if (uio->uio_resid > ifp->if_mtu)
		return EMSGSIZE;

	error = bpf_movein(uio, (int)d->bd_bif->bif_devp.bdev_type, &m, &dst);
	if (error)
		return error;

	s = splnet();
	error = (*ifp->if_output)(ifp, m, &dst);
	splx(s);
	/*
	 * The driver frees the mbuf. 
	 */
	return error;
}

/*
 * Reset a descriptor by flushing its packet before
 * and clearing the receive and drop counts.  Should
 * be called at splimp.
 */
static void
reset_d(d)
	struct bpf_d *d;
{
	if (d->bd_hbuf) {
		/* Free the hold buffer. */
		d->bd_fbuf = d->bd_hbuf;
		d->bd_hbuf = 0;
	}
	d->bd_sbuf->m_len = 0;
	d->bd_rcount = 0;
	d->bd_dcount = 0;
}

/*
 *  bpfioctl - packet filter control
 *
 *  FIONREAD		Check for read packet available.
 *  SIOCGIFADDR		Get interface address - convenient hook to driver.
 *  BIOCGFLEN		Get max filter len.
 *  BIOCGBLEN		Get buffer len [for read()].
 *  BIOCSETF		Set ethernet read filter.
 *  BIOCFLUSH		Flush read packet buffer.
 *  BIOCPROMISC		Put interface into promiscuous mode.
 *  BIOCDEVP		Get device parameters.
 *  BIOCGETIF		Get interface name.
 *  BIOCSETIF		Set interface.
 *  BIOCSRTIMEOUT	Set read timeout.
 *  BIOCGRTIMEOUT	Get read timeout.
 *  BIOCGSTATS		Get packet stats.
 *  BIOCIMMEDIATE	Set immediate mode.
 */
/* ARGSUSED */
int
bpfioctl(dev, cmd, addr, flag)
	dev_t dev;
	int cmd;
	caddr_t addr;
	int flag;
{
	register struct bpf_d *d = &bpf_dtab[minor(dev)];
	int s, error = 0;

	switch (cmd) {

	default:
		error = EINVAL;
		break;

	/*
	 * Check for read packet available.
	 */
	case FIONREAD:
		{
			int n;
		
			s = splimp();
			n = d->bd_sbuf->m_len;
			if (d->bd_hbuf) 
				n += d->bd_hbuf->m_len;
			splx(s);

			*(int *)addr = n;
			break;
		}

	case SIOCGIFADDR:
		{
			struct ifnet *ifp;

			if (d->bd_bif == 0)
				error = EINVAL;
			else {
				ifp = d->bd_bif->bif_ifp;
				error =  (*ifp->if_ioctl)(ifp, cmd, addr);
			}
			break;
		}

	/*
	 * Get max filter len.
	 */
	case BIOCGFLEN:
		*(u_int *)addr = MCLBYTES / sizeof(struct bpf_insn);
		break;
	/*
	 * Get buffer len [for read()].
	 */
	case BIOCGBLEN:
		*(u_int *)addr = MCLBYTES;
		break;

	/*
	 * Set ethernet read filter.
	 */
        case BIOCSETF:
		error = bpf_setf(d, (struct bpf_program *)addr);
		break;

	/*
	 * Flush read packet buffer.
	 */
	case BIOCFLUSH:
		s = splimp();
		reset_d(d);
		splx(s);
		break;

	/*
	 * Put interface into promiscuous mode.
	 */
	case BIOCPROMISC:
		if (d->bd_bif == 0) {
			/*
			 * No interface attached yet.
			 */
			error = EINVAL;
			break;
		}
		s = splimp();
		if (d->bd_promisc == 0) {
			d->bd_promisc = 1;
			error = ifpromisc(d->bd_bif->bif_ifp, 1);
		}
		splx(s);
		break;

	/*
	 * Get device parameters.
	 */
	case BIOCDEVP:
		if (d->bd_bif == 0)
			error = EINVAL;
		else
			*(struct bpf_devp *)addr = d->bd_bif->bif_devp;
		break;

	/*
	 * Set interface name.
	 */
	case BIOCGETIF:
		if (d->bd_bif == 0)
			error = EINVAL;
		else
			bpf_ifname(d->bd_bif->bif_ifp, (struct ifreq *)addr);
		break;

	/*
	 * Set interface.
	 */
	case BIOCSETIF:
		error = bpf_setif(d, (struct ifreq *)addr);
		break;

	/*
	 * Set read timeout.
	 */
 	case BIOCSRTIMEOUT:
		{
			struct timeval *tv = (struct timeval *)addr;
			u_long msec;

			/* Compute number of milliseconds. */
			msec = tv->tv_sec * 1000 + tv->tv_usec / 1000;
			/* Scale milliseconds to ticks.  Assume hard
			   clock has millisecond or greater resolution
			   (i.e. tick >= 1000).  For 10ms hardclock,
			   tick/1000 = 10, so rtout<-msec/10. */
			d->bd_rtout = msec / (tick / 1000);
			break;
		}

	/*
	 * Get read timeout.
	 */
 	case BIOCGRTIMEOUT:
		{
			struct timeval *tv = (struct timeval *)addr;
			u_long msec = d->bd_rtout;

			msec *= tick / 1000;
			tv->tv_sec = msec / 1000;
			tv->tv_usec = msec % 1000;
			break;
		}

	/*
	 * Get packet stats.
	 */
	case BIOCGSTATS:
		{
			struct bpf_stat *bs = (struct bpf_stat *)addr;

			bs->bs_recv = d->bd_rcount;
			bs->bs_drop = d->bd_dcount;
			break;
		}

	/*
	 * Set immediate mode.
	 */
	case BIOCIMMEDIATE:
		d->bd_immediate = *(u_int *)addr;
		break;
	}
	return error;
}

/* 
 * Set d's packet filter program to 'fp'.  If 'd' already has a filter,
 * free it and replace it.  Returns an appropriate ioctl error code.
 */
int
bpf_setf(d, fp)
	struct bpf_d *d;
	struct bpf_program *fp;
{
	struct bpf_insn *fcode;
	struct mbuf *m;
	u_int flen, size;
	int s;

	if (fp->bf_insns == 0) {
		s = splimp();
		if (fp->bf_len != 0)
			return EINVAL;
		if (d->bd_filterm)
			m_freem(d->bd_filterm);
		d->bd_filterm = 0;
		d->bd_filter = bpf_default_filter;
		reset_d(d);
		splx(s);
		return 0;
	}
	flen = fp->bf_len;
	size = flen * sizeof(*fp->bf_insns);
	
	if (size > MCLBYTES)
		return EINVAL;
	
	MGET(m, M_DONTWAIT, MT_DATA);
	if (m == 0)
		return ENOBUFS;
	
	if (size > MLEN) {
		MCLGET(m);
		if (m->m_len != MCLBYTES) {
			m_freem(m);
			return ENOBUFS;
		}
	}
	fcode = mtod(m, struct bpf_insn *);
	if (copyin((caddr_t)(fp->bf_insns), (caddr_t)fcode, size))
		return EINVAL;
	
	if (bpf_validate(fcode, (int)flen)) {
		s = splimp();
		if (d->bd_filterm)
			m_freem(d->bd_filterm);
		d->bd_filterm = m;
		d->bd_filter = fcode;
		reset_d(d);
		splx(s);

		return 0;
	}
	m_freem(m);
	return EINVAL;
}

/*
 * Detach 'd' from its current interface (if attached at all) and attach to 
 * the interface named 'name'.  Return ioctl error code or 0.
 */
static int
bpf_setif(d, ifr)
	struct bpf_d *d;
	struct ifreq *ifr;
{
	struct bpf_if *bp;
	char *cp;
	int unit, i, s;

	/*
	 * Separate string into name part and unit number.  Put a null
	 * byte at the end of the name part, and compute the number. 
	 * If the a unit number is unspecified, the default is 0,
	 * as initialized above.
	 */
	unit = 0;
	cp = ifr->ifr_name;
	cp[sizeof(ifr->ifr_name) - 1] = '\0';
	while (*cp++) {
		if (*cp >= '0' && *cp <= '9') {
			unit = *cp - '0';
			*cp++ = '\0';
			while (*cp)
				unit = 10 * unit + *cp++ - '0';
			break;
		}
	}
	/*
	 * Look through attached interfaces for the named one.
	 */
	bp = bpf_iftab;
	for (i = 0; i < NBPFILTER; ++bp, ++i) {
		struct ifnet *ifp = bp->bif_ifp;

		if (ifp == 0 || unit != ifp->if_unit 
		    || strcmp(ifp->if_name, ifr->ifr_name) != 0)
			continue;
		/*
		 * We found the requested interface.  If we're
		 * already attached to it, just flush the buffer.
		 * If it's not up, return an error.
		 */
		if ((ifp->if_flags & IFF_UP) == 0)
			return ENETDOWN;
		s = splimp();
		if (bp != d->bd_bif) {
			if (d->bd_bif)
				/* 
				 * Detach if attached to  something else.
				 */
				bpf_detachd(d);

			bpf_attachd(d, bp);
		}
		reset_d(d);
		splx(s);
		return 0;
	}
	/* Not found. */
	return ENXIO;
}

/*
 * Lookup the name of the 'ifp' interface and return it in 'ifr->ifr_name'.
 * We augment the ifp's base name with its unit number.
 */
static void
bpf_ifname(ifp, ifr)
	struct ifnet *ifp;
	struct ifreq *ifr;
{
	char *s = ifp->if_name;
	char *d = ifr->ifr_name;

	while (*d++ = *s++)
		;
	/* Assume that unit number is less than 10. */
	*d++ = ifp->if_unit + '0';
	*d = '\0';
}

/*
 * Support for select() system call
 * Inspired by the code in tty.c for the same purpose.
 *
 * bpfselect - returns true iff the specific operation
 *	will not block indefinitely.  Otherwise, return
 *	false but make a note that a selwakeup() must be done.
 */
int
bpfselect(dev, rw)
	register dev_t dev;
	int rw;
{
	register struct bpf_d *d;
	register int s;
	
	if (rw != FREAD)
		return 0;
	/*
	 * An imitation of the FIONREAD ioctl code.
	 */
	d = &bpf_dtab[minor(dev)];
	
	s = splimp();
	if (d->bd_sbuf->m_len ||
	    d->bd_hbuf && d->bd_hbuf->m_len) {
		/*
		 * There is data waiting.
		 */
		splx(s);
		return 1;
	}
	/*
	 * No data ready.  If there's already a select() waiting on this
	 * minor device then this is a collision.  This shouldn't happen 
	 * because minors really should not be shared, but if a process
	 * forks while one of these is open, it is possible that both
	 * processes could select on the same descriptor.
	 */
	if (d->bd_SelProc && d->bd_SelProc->p_wchan == (caddr_t)&selwait)
		d->bd_SelColl = 1;
	else
		d->bd_SelProc = u.u_procp;		
	splx(s);	
	return 0;
}

/*
 * bpf_tap - incoming linkage from device drivers
 */
void
bpf_tap(arg, pbuf, plen)
	caddr_t arg;
	register u_char *pbuf;
	register u_int plen;
{
	struct bpf_if *bp;
	register struct bpf_d *d;
	register u_int slen;
	extern bcopy();
	/*
	 * Note that the ipl does not have to be raised at this point.
	 * The only problem that could arise here is that if two different
	 * interfaces shared any data.  This is not the case.
	 */
	bp = (struct bpf_if *)arg;
	for (d = bp->bif_dlist; d != 0; d = d->bd_next) {
		++d->bd_rcount;
		slen = bpf_filter(d->bd_filter, pbuf, plen, plen);
		if (slen != 0)
			catchpacket(d, pbuf, plen, slen, (void (*)())bcopy);
	}
}

/*
 * Copy data from an mbuf chain into a buffer.  This code is derived
 * from m_copydata in sys/uipc_mbuf.c.
 */
static void
bpf_m_copydata(src, dst, len)
	u_char *src;
	u_char *dst;
	register int len;
{
	register struct mbuf *m = (struct mbuf *)src;
	register unsigned count;

	while (len > 0) {
		if (m == 0)
			panic("bpf_m_copydata");
		count = MIN(m->m_len, len);
		(void)bcopy(mtod(m, caddr_t), (caddr_t)dst, count);
		len -= count;
		dst += count;
		m = m->m_next;
	}
}

/*
 * Length of ethernet and TCP/IP header header with no IP options.
 */
#define BPF_MIN_SNAPLEN 50

/*
 * bpf_mtap - incoming linkage from device drivers, when packet
 *   is in an mbuf chain
 */
void
bpf_mtap(arg, m0)
	caddr_t arg;
	struct mbuf *m0;
{
	static u_char buf[BPF_MIN_SNAPLEN];

	struct bpf_if *bp = (struct bpf_if *)arg;
	struct bpf_d *d;
	u_char *cp;
	u_int slen, plen;
	int nbytes;
	struct mbuf *m;

	if (m0->m_len >= BPF_MIN_SNAPLEN) {
		slen = m0->m_len;
		cp = mtod(m0, u_char *);
	} 
	else {
		nbytes = BPF_MIN_SNAPLEN;
		cp = buf;
		m = m0;
		while (m && nbytes > 0) {		
			slen = MIN(m->m_len, nbytes);
			bcopy(mtod(m, char *), (char *)cp, slen);
			cp += slen;
			nbytes -= slen;
			m = m->m_next;
		}
		if (nbytes > 0)
			/* Packet too small? */
			return;

		slen = BPF_MIN_SNAPLEN;
		cp = buf;
	}
	plen = 0;
	m = m0;
	while (m) {
		plen += m->m_len;
		m = m->m_next;
	}
	for (d = bp->bif_dlist; d != 0; d = d->bd_next) {
		++d->bd_rcount;
		slen = bpf_filter(d->bd_filter, cp, plen, slen);
		if (slen != 0)
			catchpacket(d, (u_char *)m0, plen, slen,
				    bpf_m_copydata);
	}
}

/*
 * Move the packet data from interface memory ('pbuf') into the
 * store buffer.  Return 1 if it's time to wakeup a listener (buffer full),
 * otherwise 0.  'copy' is the routine called to do the actual data 
 * transfer.  'bcopy' is passed in to copy contiguous chunks, while
 * 'bpf_m_copydata' is passed in to copy mbuf chains.  In the latter
 * case, 'pbuf' is really an mbuf.
 */
static void
catchpacket(d, pbuf, plen, snaplen, cpfn)
	struct bpf_d *d;
	u_char *pbuf;
	u_int plen, snaplen;
	void (*cpfn)();
{
	struct mbuf *m;
	struct bpf_hdr *hp;
	int totlen, curlen;
	int hdrlen = d->bd_bif->bif_hdrlen;
	/*
	 * Figure out how many bytes to move.  If the packet is
	 * greater or equal to the snapshot length, transfer that
	 * much.  Otherwise, transfer the whole packet (unless
	 * we hit the cluster limit).
	 */
	if (snaplen <= plen)
		totlen = snaplen + hdrlen;
	else {
		totlen = plen + hdrlen;
		if (totlen > MCLBYTES)
			totlen = MCLBYTES;
	}

	m = d->bd_sbuf;

	/*
	 * Round up the end of the previous packet to the next longword.
	 */
	curlen = BPF_WORDALIGN(m->m_len);

	if (curlen + totlen > MCLBYTES) {
		/*
		 * This packet will overflow the storage buffer.
		 * Move the current cluster buffer to the hold slot,
		 * and grab the free one.
		 */
		if (d->bd_fbuf == 0) {
			/* 
			 * We haven't completed the previous read yet?
			 * Drop the packet.
			 */
			++d->bd_dcount;
			return;
		}
		/*
		 * Rotate the buffers.  Move the 'store' buffer
		 * into the 'hold' slot, and the 'free' buffer
		 * into the 'store' slot.  Zero out the length of
		 * the new 'store' buffer.
		 */
		d->bd_hbuf = d->bd_sbuf;
		m = d->bd_sbuf = d->bd_fbuf;
		d->bd_fbuf = 0;
		curlen = m->m_len = 0;

		/*
		 * Wake up anyone sleeping on this descriptor. 
		 */
		wakeup((caddr_t)d);
		bpf_wakeup(d);
	}
	else if (d->bd_immediate) {
		/*
		 * Immediate mode is set.  A packet arrived so any
		 * reads should be woken up.
		 */
		wakeup((caddr_t)d);
		bpf_wakeup(d);
	}
	/*
	 * Append the bpf header.
	 */
	hp = (struct bpf_hdr *)(mtod(m, u_char *) + curlen);
#ifdef sun
	uniqtime(&hp->bh_tstamp);
#else
#ifdef hp300
	microtime(&hp->bh_tstamp);
#else
	hp->bh_tstamp = time;
#endif
#endif
	hp->bh_datalen = plen;
	hp->bh_hdrlen = hdrlen;
	/*
	 * Copy the packet data into the 'store' buffer and
	 * update the cluster length.
	 */
	(*cpfn)(pbuf, (u_char *)hp + hdrlen, hp->bh_caplen = totlen - hdrlen);

	m->m_len = curlen + totlen;
}

/*
 * Allocate an mbuf cluster and clear its length field.
 * If resources unavaiable, return 0.
 * We can wait in MGET since we assume that we are called
 * at a low priority.
 */
static struct mbuf *
bpf_mcluster()
{
	struct mbuf *m;

	MGET(m, M_WAIT, MT_DATA);
	if (m == 0)
		return 0;
	MCLGET(m);
	if (m->m_len == MCLBYTES) {
		m->m_len = 0;
		return m;
	}
	m_freem(m);
	return 0;
}

/* 
 * Initialize all nonzero fields of a descriptor.
 */
static int
bpf_initd(d)
	register struct bpf_d *d;
{
	struct mbuf *m;

	/* Get the buffer space. */
	m = bpf_mcluster();
	if (m == 0)
		return ENOBUFS;
	d->bd_fbuf = m;
	m = bpf_mcluster();
	if (m == 0) {
		m_freem(d->bd_fbuf);
		return ENOBUFS;
	}
	d->bd_sbuf = m;

	return 0;
}

/*
 * Register 'ifp' with bpf.  'devp' is the link-level device descriptor
 * and 'driverp' is a pointer to the 'struct bpf_if *' in the driver's softc.
 */
void
bpfattach(driverp, ifp, devp)
	caddr_t *driverp;
	struct ifnet *ifp;
	struct bpf_devp *devp;
{
	struct bpf_if *bp;
	int i;

	if (bpf_units >= NBPFILTER) {
		printf("bpf: too many interfaces: %s%d not attached\n",
		       ifp->if_name, ifp->if_unit);
		return;
	}
	bp = &bpf_iftab[bpf_units++];

	bp->bif_dlist = 0;
	bp->bif_driverp = (struct bpf_if **)driverp;
	bp->bif_ifp = ifp;
	bp->bif_devp = *devp;

	/*
	 * Compute the length of the bpf header.  This is not necessarily
	 * equal to SIZEOF_BPF_HDR because we want to insert spacing such 
	 * that the network layer header begins on a longword boundary (for 
	 * performance reasons and to alleviate alignment restrictions).
	 */
	i = devp->bdev_hdrlen;
	bp->bif_hdrlen = BPF_WORDALIGN(i + SIZEOF_BPF_HDR) - i;

	/*
	 * Mark all the descriptors free if this hasn't been done.
	 */
	if (!D_ISFREE(&bpf_dtab[0]))
		for (i = 0; i < NBPFILTER; ++i)
			D_MARKFREE(&bpf_dtab[i]);

	printf("bpf: %s%d attached\n", ifp->if_name, ifp->if_unit);
}

#endif (NBPFILTER > 0)
