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
 * from: $Hdr: if_en.h,v 4.300 91/06/09 06:25:56 root Rel41 $ SONY
 *
 *	@(#)if_en.h	8.1 (Berkeley) 6/11/93
 */

/*
 * Structure of an Ethernet header -- receive format
 */
struct en_rheader {
	u_char	enr_dhost[6];		/* Destination Host */
	u_char	enr_shost[6];		/* Source Host */
	u_short	enr_type;		/* Type of packet */
};

#ifdef KERNEL
/*
 * Ethernet software status per interface.
 *
 * Each interface is referenced by a network interface structure,
 * es_if, which the routing code uses to locate the interface.
 * This structure contains the output queue for the interface, its address, ...
 * We also have, for each interface, a IOP interface structure, which
 * contains information about the IOP resources held by the interface:
 * map registers, buffered data paths, etc.  Information is cached in this
 * structure for use by the if_iop.c routines in running the interface
 * efficiently.
 */
struct	en_softc {
	struct	arpcom es_ac;		/* Ethernet common part */
	struct	ifnews es_ifnews;	/* IOP resources */
	int	es_flags;
#ifdef NOTDEF /* KU:XXX */
	int	es_oactive;		/* # of active output buffers */
#endif
	int	es_interval;		/* interval of watchdog */
	caddr_t	es_bpf;
};

#define	es_if	es_ac.ac_if		/* network-visible interface */
#define	es_addr	es_ac.ac_enaddr		/* hardware Ethernet address */

#define	ENF_RUNNING	0x01		/* board is running */
#define	ENF_SETADDR	0x02		/* physical address is changed */

#define	ENWATCHINTERVAL	60		/* once every 60 seconds */

#endif /* KERNEL */
