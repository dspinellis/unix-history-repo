/*
 * Copyright (c) 1982, 1986, 1990 Regents of the University of California.
 * All rights reserved.
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
 *	from: @(#)kern_physio.c	7.20 (Berkeley) 5/11/91
 */

#include "param.h"
#include "systm.h"
#include "buf.h"
#include "conf.h"
#include "proc.h"
#include "seg.h"
#include "trace.h"
#include "map.h"
#include "vnode.h"
#include "specdev.h"

#ifdef HPUXCOMPAT
#include "user.h"
#endif

/*
 * This routine does raw device I/O for a user process.
 *
 * If the user has the proper access privileges, the process is
 * marked 'delayed unlock' and the pages involved in the I/O are
 * faulted and locked. After the completion of the I/O, the pages
 * are unlocked.
 */
physio(strat, bp, dev, rw, mincnt, uio)
	int (*strat)(); 
	register struct buf *bp;
	dev_t dev;
	int rw;
	u_int (*mincnt)();
	struct uio *uio;
{

	/*
	 * Body deleted.
	 */
	return (EIO);
}

/*
 * Calculate the maximum size of I/O request that can be requested
 * in a single operation. This limit is necessary to prevent a single
 * process from being able to lock more than a fixed amount of memory
 * in the kernel.
 */
u_int
minphys(bp)
	struct buf *bp;
{

	/*
	 * Body deleted.
	 */
	return;
}

/*
 * Do a read on a device for a user process.
 */
rawread(dev, uio)
	dev_t dev;
	struct uio *uio;
{
	return (physio(cdevsw[major(dev)].d_strategy, (struct buf *)NULL,
	    dev, B_READ, minphys, uio));
}

/*
 * Do a write on a device for a user process.
 */
rawwrite(dev, uio)
	dev_t dev;
	struct uio *uio;
{
	return (physio(cdevsw[major(dev)].d_strategy, (struct buf *)NULL,
	    dev, B_WRITE, minphys, uio));
}
