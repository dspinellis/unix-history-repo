/*
 * Copyright (c) University of British Columbia, 1984
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Laboratory for Computation Vision and the Computer Science Department
 * of the University of British Columbia.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)hd_debug.c	7.3 (Berkeley) %G%
 */

#include "param.h"
#include "systm.h"
#include "mbuf.h"
#include "domain.h"
#include "socket.h"
#include "protosw.h"
#include "errno.h"
#include "time.h"
#include "kernel.h"

#include "../net/if.h"

#include "hdlc.h"
#include "hd_var.h"
#include "x25.h"

#ifdef HDLCDEBUG
#define NTRACE		32

struct	hdlctrace {
	struct	hdcb *ht_hdp;
	short	ht_dir;
	struct	mbuf *ht_frame;
	struct	timeval ht_time;
} hdtrace[NTRACE];

int	lasttracelogged, freezetrace;
#endif

hd_trace (hdp, direction, frame)
struct hdcb *hdp;
register struct Hdlc_frame *frame;
{
	register char *s;
	register int nr, pf, ns, i;
	struct Hdlc_iframe *iframe = (struct Hdlc_iframe *) frame;

#ifdef HDLCDEBUG
	hd_savetrace (hdp, direction, frame);
#endif
	if (hdp -> hd_xcp -> xc_ltrace) {
		if (direction == RX)
			printf ("F-In:  ");
		else if (direction == 2)
				printf ("F-Xmt: ");
			else
				printf ("F-Out:   ");

		nr = iframe -> nr;
		pf = iframe -> pf;
		ns = iframe -> ns;

		switch (hd_decode (hdp, frame)) {
		case SABM: 
			printf ("SABM   : PF=%d\n", pf);
			break;

		case DISC: 
			printf ("DISC   : PF=%d\n", pf);
			break;

		case DM: 
			printf ("DM     : PF=%d\n", pf);
			break;

		case FRMR: 
			{
			register struct Frmr_frame *f = (struct Frmr_frame *)frame;

			printf ("FRMR   : PF=%d, TEXT=", pf);
			for (s = (char *) frame, i = 0; i < 5; ++i, ++s)
				printf ("%x ", (int) * s & 0xff);
			printf ("\n");
			printf ("control=%x v(s)=%d v(r)=%d w%d x%d y%d z%d\n",
				f->frmr_control, f->frmr_ns, f->frmr_nr,
				f->frmr_w, f->frmr_x, f->frmr_y, f->frmr_z);
			break;
			}

		case UA: 
			printf ("UA     : PF=%d\n", pf);
			break;

		case RR: 
			printf ("RR     : N(R)=%d, PF=%d\n", nr, pf);
			break;

		case RNR: 
			printf ("RNR    : N(R)=%d, PF=%d\n", nr, pf);
			break;

		case REJ: 
			printf ("REJ    : N(R)=%d, PF=%d\n", nr, pf);
			break;

		case IFRAME: 
			{
			register struct mbuf *m;
			register int len = 0;

			for(m = dtom (frame); m; m = m -> m_next)
				len += m -> m_len;
			len -= HDHEADERLN;
			printf ("IFRAME : N(R)=%d, PF=%d, N(S)=%d, DATA(%d)=",
				nr, pf, ns, len);
			for (s = (char *)iframe->i_field, i = 0; i < 3; ++i, ++s)
				printf ("%x ", (int) *s & 0xff);
			printf ("\n");
			break;
			}

		default: 
			printf ("ILLEGAL: ");
			for (s = (char *) frame, i = 0; i < 5; ++i, ++s)
				printf ("%x ", (int) *s & 0xff);
			printf ("\n");
		}

	}
}

#ifdef HDLCDEBUG
static
hd_savetrace (hdp, dir, frame)
struct hdcb *hdp;
struct Hdlc_frame *frame;
{
	register struct hdlctrace *htp;
	register struct mbuf *m;

	if (freezetrace)
		return;
	htp = &hdtrace[lasttracelogged];
	lasttracelogged = (lasttracelogged + 1) % NTRACE;
	if (m = htp->ht_frame)
		m_freem (m);
	m = dtom (frame);
	htp->ht_frame = m_copy (m, 0, m->m_len);
	htp->ht_hdp = hdp;
	htp->ht_dir = dir;
	htp->ht_time = time;
}

hd_dumptrace (hdp)
struct hdcb *hdp;
{
	register int i, ltrace;
	register struct hdlctrace *htp;

	freezetrace = 1;
	hd_status (hdp);
	printf ("retransmit queue:");
	for (i = 0; i < 8; i++)
		printf (" %X", hdp -> hd_retxq[i]);
	printf ("\n");
	ltrace = hdp -> hd_xcp -> xc_ltrace;
	hdp -> hd_xcp -> xc_ltrace = 1;
	for (i = 0; i < NTRACE; i++) {
		htp = &hdtrace[(lasttracelogged + i) % NTRACE];
		if (htp->ht_hdp != hdp || htp->ht_frame == 0)
			continue;
		printf ("%d/%d	", htp->ht_time.tv_sec & 0xff,
			htp->ht_time.tv_usec / 10000);
		hd_trace (htp->ht_hdp, htp->ht_dir,
			mtod (htp->ht_frame, struct Hdlc_frame *));
		m_freem (htp->ht_frame);
		htp->ht_frame = 0;
	}
	hdp -> hd_xcp -> xc_ltrace = ltrace;
	freezetrace = 0;
}
#endif
