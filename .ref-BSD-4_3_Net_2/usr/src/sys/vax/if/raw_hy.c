/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Tektronix Inc.
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
 *	@(#)raw_hy.c	7.4 (Berkeley) 12/16/90
 */

/*
 * 4.3 BSD Unix kernel - NSC HYPERchannel support
 *
 * $Header: raw_hy.c,v 3.1 84/02/15 04:27:44 steveg Exp $
 * $Locker:  $
 *
 * Copyright (c) 1984, Tektronix Inc.
 * All Rights Reserved
 *
 */

#include "hy.h"
#if NHY > 0

#include "sys/param.h"
#include "sys/mbuf.h"
#include "sys/socket.h"
#include "sys/protosw.h"
#include "sys/socketvar.h"
#include "sys/errno.h"

#include "net/if.h"
#include "net/route.h"
#include "net/raw_cb.h"

#include "netinet/in.h"
#include "netinet/in_systm.h"
#include "netinet/in_var.h"
#include "if_hy.h"

/*
 * Raw interface to HYPERchannel.
 */

/*
 * Generate HYPERchannel leader and pass packet to hyoutput.
 * The user must create a skeletal leader in order to
 * communicate message type, message subtype, etc.
 * We don't really check the header supplied by the user.
 */
rhy_output(m, so)
	register struct mbuf *m;
	struct socket *so;
{
	int error = 0;
	register struct sockaddr_in *sin;
	register struct rawcb *rp = sotorawcb(so);
	struct in_ifaddr *ia;

	/*
	 * Verify user has supplied necessary space
	 * for the header.
	 */
	if ((m->m_off > MMAXOFF || m->m_len < sizeof(struct hym_hdr)) &&
	    (m = m_pullup(m, sizeof(struct hym_hdr))) == 0) {
		error = EMSGSIZE;	/* XXX */
		goto bad;
	}

	sin = (struct sockaddr_in *)&rp->rcb_faddr;
	/* no routing here */
	ia = in_iaonnetof(in_netof(sin->sin_addr));
	if (ia)
		return (hyoutput(ia->ia_ifp, m, (struct sockaddr *)sin));
	error = ENETUNREACH;
bad:
	m_freem(m);
	return (error);
}
#endif
