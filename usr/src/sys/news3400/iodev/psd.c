/*
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Sony Corp. and Kazumasa Utashiro of Software Research Associates, Inc.
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
 * from: $Hdr: psd.c,v 4.300 91/06/09 06:38:07 root Rel41 $ SONY
 *
 *	@(#)psd.c	8.1 (Berkeley) 6/11/93
 */

/*
 * Copyright (c) 1987, 1988 by SONY Corporation.
 */

/*
 * psd.c	ver 1.0
 *			Fri Mar 31 16:01:42 JST 1989
 *
 * Probe SCSI device routine
 *
 */

#include <sys/param.h>
#include <sys/buf.h>
#include <sys/proc.h>
#include <sys/user.h>
#include <sys/dkstat.h>
#include <sys/uio.h>
#include <sys/kernel.h>
#include <sys/ioctl.h>
#include <sys/syslog.h>

#include <vm/vm.h>

#ifdef mips
#include <machine/cpu.h>
#endif

#ifdef IPC_MRX
#include "../iop/iopvar.h"
#include "../ipc/newsipc.h"
#endif

#ifdef CPU_SINGLE
#include <news3400/hbdev/hbvar.h>
#include <news3400/hbdev/scsic.h>
#endif

#include <news3400/iodev/scsireg.h>
#include <news3400/iodev/scu.h>

#include <news3400/iodev/ioptohb.h>


#define	PROBE_MAXRETRY	100
#define	NRETRY		10
#define	MAXHRDERR	100
#define	NSCSICHAN	7

#define	NOSUCHDEV	0x7f

#ifdef news3400
#define	MAXCTLR		8
#endif

#ifdef news3800
#define	MAXCTLR		16
#endif

#define	MAXSLAVE	8

struct scsi scsi[MAXCTLR];
struct sc_map scmap[MAXCTLR];
struct sc_inq scinq[MAXCTLR];

#ifdef IPC_MRX
extern int port_scsi;		/* UNIX port of SCSI */
extern int iop_scsi[];		/* IOP port of SCSI process */
#endif

#ifdef CPU_DOUBLE
extern struct scintsw scintsw[];
#endif

/*
 * Universal SCSI probe routine
 *	for mass storage controller.
 */
psdprobe(im)
	register struct iop/**/_ctlr *im;
{
	register int intr = im->im_intr;
	struct scintsw *sci;
	int ctlrintr;
	int j;
	char name[16];
	register struct sc_inq *inq = &scinq[intr];

#ifdef IPC_MRX
	int scintr();

	if (port_scsi == 0) {
		port_scsi = port_create("@scsi", scintr, -1);
		for(j = 0; j < MAXCTLR; j++) {
			if(j == 7 || j == 15)
				continue;
			iop_scsi[j] = object_query(make_name(name, "scsiportXX", j));
		}
	}
#endif /* IPC_MRX */
	sci = &scintsw[intr];
	if (sci->sci_inthandler) {
		/*
		 * already probed.
		 */
		return (-1);
	}

	/*
	 * probe device product ID
	 *	& set driver interrupt handler
	 */
	if (probe_inq(im, inq) == 0) {
		/*
		 * inquiry command terminate with bad status
		 *	set 0x7f(Specified device is nonexistent) to devtype
		 */
		inq->sci_devtype = NOSUCHDEV;
	}
	return (inq->sci_devtype);
}

set_inthandler(im, handler)
	register struct iop/**/_ctlr *im;
	int (*handler)();
{
	struct scintsw *sci;

	sci = &scintsw[im->im_intr];
	if (sci->sci_inthandler)
		return (0);	/* already probed. */

	sci->sci_inthandler = handler;
	sci->sci_ctlr = im->im_ctlr;
	return (1);
}

probe_inq(im, inq)
	register struct iop/**/_ctlr *im;
	register struct sc_inq *inq;
{
	register int intr = im->im_intr;
	register struct scsi *sc = &scsi[intr];
	register int retry = 0;
	
	bzero(inq, sizeof(struct sc_inq));

psdprobe_loop:

	scop_inquiry(intr, sc, 0, SCSI_INTDIS, sizeof(struct sc_inq), inq);

	if (sc->sc_istatus != INST_EP) {
		/*
		 * probe fault.
		 */
		if (sc->sc_istatus == (INST_EP|INST_HE)) {
			/*
			 * bus reset, retry
			 */
			goto psdprobe_loop;
		}
		return (0);
	}
#ifndef NO_STATUS_BYTE_MASK
	sc->sc_tstatus &= TGSTMASK;
#endif
	if (sc->sc_tstatus != TGST_GOOD) {
		if (sc->sc_tstatus == TGST_CC)
			scop_rsense(intr, sc, 0, SCSI_INTDIS, 18, 0);
		if (retry++ < PROBE_MAXRETRY) {
			DELAY(600000);		/* XXX  1 sec */
			goto psdprobe_loop;
		}
		/*
		 * probe fault.
		 */
		return (0);
	}
	return (1);
}

struct scsi *
get_scsi(intr)
	int	intr;
{
	return (&scsi[intr]);
}

struct sc_map *
get_sc_map(intr)
	int	intr;
{
	return (&scmap[intr]);
}

struct sc_inq *
get_sc_inq(intr)
	int	intr;
{
	return (&scinq[intr]);
}
