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
 * from: $Hdr: scc.h,v 4.300 91/06/09 06:44:56 root Rel41 $ SONY
 *
 *	@(#)scc.h	8.1 (Berkeley) 6/11/93
 */

#ifdef news3400
#define	splscc		spl4
#endif

#define	SCCWAIT		DELAY(2)

#define SCC_REMOTE0	0
#define SCC_REMOTE1	1
#define SCC_REMOTE2	2
#define SCC_REMOTE3	3
#define SCC_REMOTE4	4
#define SCC_REMOTE5	5
#define SCC_REMOTE6	6
#define SCC_REMOTE7	7
#define SCC_REMOTE8	8
#define SCC_REMOTE9	9

#define	SCCVEC0		64
#define	SCCVEC1		(SCCVEC0+16)
#define SCCVEC2		(SCCVEC0+32)
#define SCCVEC3		(SCCVEC0+48)
#define SCCVEC4		(SCCVEC0+64)

/*
 *	SCC channel control block
 */
typedef struct scc_dma {
	char	*dma_addr;
	int	dma_count;
} Scc_dma;

typedef struct scc_channel {
	int	scc_status;		/* channel status		*/
	int	scc_param;		/* channel parameter		*/
	struct	scc_reg	*scc_port;	/* port address			*/
	char	*scc_init;		/* initialize data		*/
	int	scc_vec;		/* interrupt vector		*/
	Scc_dma	x_dma;
	Scc_dma	r_dma;
} Scc_channel;

/*
 *	SCC channel status
 */
#define	OACTIVE		0x00000001	/* transmit in progress	*/
#define	OSTOP		0x00000002	/* output stop request	*/
#define	OFLUSH		0x00000004	/* output flush request	*/
#define	OBUSY		0x00000008	/* output in use	*/
#define LINE_BREAK	0x00000010	/* line break interrupt	*/
#define	ENABLE		0x00000020	/* receiver enable	*/
#define	CHAN_ACTIVE	0x80000000	/* channel active	*/

/*
 *	SCC channel usage
 */
#define	SCC_MOUSE	0
#define	SCC_KEYBOARD	1
