/*	if_dmc.h	4.1	82/02/15	*/

/*
 * DMC-11 Interface
 */

struct dmcdevice {
	union {
		char	b[8];
		short	w[4];
	} un;
};

#define	bsel0	un.b[0]
#define	bsel1	un.b[1]
#define	bsel2	un.b[2]
#define	bsel3	un.b[3]
#define	bsel4	un.b[4]
#define	bsel5	un.b[5]
#define	bsel6	un.b[6]
#define	bsel7	un.b[7]
#define	sel0	un.w[0]
#define	sel2	un.w[1]
#define	sel4	un.w[2]
#define	sel6	un.w[3]

#define	DMCMTU	(2048)

#define RDYSCAN	16	/* loop delay for RDYI after RQI */

/* defines for bsel0 */
#define	DMC_BACCI	0
#define	DMC_CNTLI	1
#define	DMC_PERR	2
#define	DMC_BASEI	3
#define	DMC_WRITE	0		/* transmit block */
#define	DMC_READ	4		/* read block */
#define	DMC_RQI		0040		/* port request bit */
#define	DMC_IEI		0100		/* enable input interrupts */
#define	DMC_RDYI	0200		/* port ready */

/* defines for bsel1 */
#define	DMC_MCLR	0100		/* DMC11 Master Clear */
#define	DMC_RUN		0200		/* clock running */

/* defines for bsel2 */
#define	DMC_BACCO	0
#define	DMC_CNTLO	1
#define	DMC_OUX		0		/* transmit block */
#define	DMC_OUR		4		/* read block */
#define	DMC_IEO		0100		/* enable output interrupts */
#define	DMC_RDYO	0200		/* port available */

/* defines for CNTLI mode */
#define	DMC_HDPLX	02000		/* half duplex DDCMP operation */
#define	DMC_SEC		04000		/* half duplex secondary station */
#define	DMC_MAINT	00400		/* enter maintenance mode */

/* defines for BACCI/O and BASEI mode */
#define	DMC_XMEM	0140000		/* xmem bit position */
#define	DMC_CCOUNT	0037777		/* character count mask */
#define	DMC_RESUME	0002000		/* resume (BASEI only) */

/* defines for CNTLO */
#define	DMC_CNTMASK	01777
#define	DMC_FATAL	01620
