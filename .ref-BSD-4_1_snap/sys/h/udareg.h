/*	udareg.h	81/10/26	1.1	*/
/*
 * UDA-50 registers and structures
 */

struct udadevice {
	short	udaip;		/* initialization and polling */
	short	udasa;		/* status and address */
};

#define	UDA_ERR		0100000	/* error bit */
#define	UDA_STEP4	0040000	/* step 4 has started */
#define	UDA_STEP3	0020000	/* step 3 has started */
#define	UDA_STEP2	0010000	/* step 2 has started */
#define	UDA_STEP1	0004000	/* step 1 has started */
#define	UDA_NV		0002000	/* no host settable interrupt vector */
#define	UDA_QB		0001000	/* controller supports Q22 bus */
#define	UDA_DI		0000400	/* controller implements diagnostics */
#define	UDA_IE		0000200	/* interrupt enable */
#define	UDA_PI		0000001	/* host requests adapter purge interrupts */
#define	UDA_GO		0000001	/* start operation, after init */


/*
 * UDA Communications Area
 */

struct udaca {
	short	ca_xxx1;	/* unused */
	char	ca_xxx2;	/* unused */
	char	ca_bdp;		/* BDP to purge */
	short	ca_cmdint;	/* command queue transition interrupt flag */
	short	ca_rspint;	/* response queue transition interrupt flag */
	long	ca_rspdsc[NRSP];/* response descriptors */
	long	ca_cmddsc[NCMD];/* command descriptors */
};

#define	ca_ringbase	ca_rspdsc[0]

#define	UDA_OWN	0x80000000	/* UDA owns this descriptor */
#define	UDA_INT	0x40000000	/* allow interrupt on ring transition */

/*
 * MSCP packet info
 */
struct mscp_header {
	short	uda_msglen;	/* length of MSCP packet */
	char	uda_credits;	/* low 4 bits: credits, high 4 bits: msgtype */
	char	uda_vcid;	/* virtual circuit id */
};
