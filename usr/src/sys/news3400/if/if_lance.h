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
 * from: $Hdr: if_lance.h,v 4.300 91/06/09 06:25:59 root Rel41 $ SONY
 *
 *	@(#)if_lance.h	8.1 (Berkeley) 6/11/93
 */

#undef VOLATILE
#ifdef mips
#define VOLATILE volatile
#else
#define VOLATILE
#endif

typedef VOLATILE struct lance Lance_reg;
typedef VOLATILE struct recv_msg_desc recv_msg_desc;
typedef VOLATILE struct xmit_msg_desc xmit_msg_desc;

/*
 * Structure of statistics record
 */
struct en_stats {
	u_char	ens_addr[8];		/* Ethernet Address */
	int	ens_frames;		/* Number of Frames Received */
	int	ens_xmit;		/* Number of Frames Transmitted */
	int	ens_xcollis;		/* Number of Excess Collisions */
	int	ens_frag;		/* Number of Fragments Received */
	int	ens_lost;		/* Number of Times Frames Lost */
	int	ens_crc;		/* Number of CRC Errors */
	int	ens_align;		/* Number of Alignment Errors */
	int	ens_collis;		/* Number of Collisions */
	int	ens_owcollis;		/* Number of Out-of-window Collisions */
};

/*
 *	LANCE control block
 */
typedef	VOLATILE struct lance_chan {
	Lance_reg	*lance_addr;	/* LANCE port address		*/
	caddr_t 	lance_memory;	/* LANCE memory address		*/
	caddr_t		lance_rom;	/* Ethernet address ROM		*/
	struct init_block *lance_ib;	/* initialization block address	*/
	int		lance_flags;	/* LANCE active flag		*/
	recv_msg_desc	*lance_rmd;	/* recv. message desc. address	*/
	xmit_msg_desc	*lance_tmd;	/* xmit. message desc. address	*/
	recv_msg_desc	*lance_last_rmd;	/* last rmd		*/
	xmit_msg_desc	*lance_last_tmd;	/* last tmd		*/
	struct en_stats	lance_stats;	/* LANCE statistics		*/
} Lance_chan;

#undef VOLATILE

#define	LANCE_ACTIVE	1
#define	LANCE_PROM	2
#define	LANCE_IDON	8
