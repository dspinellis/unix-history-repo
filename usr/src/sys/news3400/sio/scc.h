/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Sony Corp. and Kazumasa Utashiro of Software Research Associates, Inc.
 *
 * %sccs.include.redist.c%
 *
 * from: $Hdr: scc.h,v 4.300 91/06/09 06:44:56 root Rel41 $ SONY
 *
 *	@(#)scc.h	7.2 (Berkeley) %G%
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
