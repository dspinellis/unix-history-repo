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
 * from: $Hdr: ioptohb.h,v 4.300 91/06/09 06:38:04 root Rel41 $ SONY
 *
 *	@(#)ioptohb.h	8.1 (Berkeley) 6/11/93
 */

#ifndef __IOPTOHB__
#define __IOPTOHB__ 1

#ifdef CPU_SINGLE
# define	iop		hb

# define	im_driver	hm_driver
# define	im_ctlr		hm_ctlr
# define	im_alive	hm_alive
# define	im_addr		hm_addr
# define	im_intr		hm_intr
# define	im_scnum	hm_scnum
# define	im_hd		hm_hd
# define	im_tab		hm_tab

# define	ii_driver	hi_driver
# define	ii_unit		hi_unit
# define	ii_ctlr		hi_ctlr
# define	ii_slave	hi_slave
# define	ii_addr		hi_addr
# define	ii_intr		hi_intr
# define	ii_dk		hi_dk
# define	ii_flags	hi_flags
# define	ii_alive	hi_alive
# define	ii_type		hi_type
# define	ii_forw		hi_forw
# define	ii_mi		hi_mi
# define	ii_hd		hi_hd
#endif /* CPU_SINGLE */

#endif /* !__IOPTOHB__ */
