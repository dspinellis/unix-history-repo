/*	cp.h	1.1	86/01/05	*/
/*	Console Processor Interface 	*/
/*	Tahoe version, Nov. 1982	*/

/****************************************/
/*					*/
/*	Reduced DCB layout for byte	*/
/*	communication.			*/
/*					*/
/****************************************/

#define	CPBUFLEN 200		/* Output buffer length */
#ifndef	LOCORE
struct	cphdr
{
	char	cp_unit;	/* Done bit & unit # */
	char	cp_comm;	/* Command */
	short	cp_count;	/* Counter (when relevant) */
};

struct	cpdcb_o			/* Output structure */
{
	struct	cphdr	cp_hdr;
	char	cp_buf[CPBUFLEN]; /* Buffer for output or 'stty' */
};

struct	cpdcb_i			/* Structure for input */
{
	struct	cphdr	cp_hdr;
	char	cpi_buf[4]; 	/* Buffer for input */
};
#endif

#define	CPDONE	0x80		/* 'Done' bit in cp_unit */
#define	CPTAKE	0x40		/* CP 'ack' to this cpdcb */

		/* Values for 'unit' */
#define	CPUNIT	0		/* The CP itself */
#define	CPCONS	1		/* Console line */
#define	CPREMOT	2		/* Remote line */

		/* Values for 'command' */
#define	CPRESET 0
#define	CPWRITE 1
#define	CPREAD	2
#define	CPSTTY	3
#define	CPBOOT	4
