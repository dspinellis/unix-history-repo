/*
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
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
 *	@(#)ppiioctl.h	8.1 (Berkeley) 6/10/93
 */

#ifndef _IOCTL_
#include <sys/ioctl.h>
#endif

struct ppiparam {
	int	burst;	/* chars to send/recv in one call */
	int	timo;	/* timeout: -1 blocking, 0 non-blocking, >0 msec */
	int	delay;	/* delay between polls (msec) */
};

#define PPI_BLOCK	-1
#define PPI_NOBLOCK	0

/* default values */
#define	PPI_BURST	1024
#define PPI_TIMO	PPI_BLOCK
#define PPI_DELAY	10

/* limits */
#define	PPI_BURST_MIN	1
#define	PPI_BURST_MAX	1024
#define PPI_DELAY_MIN	0
#define PPI_DELAY_MAX	30000

#define PPIIOCSPARAM	_IOW('P', 0x1, struct ppiparam)
#define PPIIOCGPARAM	_IOR('P', 0x2, struct ppiparam)
#define PPIIOCSSEC	_IOW('P', 0x3, int)
