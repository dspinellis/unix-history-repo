/*	if_accreg.h	4.1	82/02/04	*/

/*
 * ACC LH/DH-11 interface
 */

struct accdma {
	short	csr;	/* control and status */
	short	db;	/* data buffer */
	u_short	ba;	/* buss address */
	short	wc;	/* word count */
};

struct accdevice {
	struct	accdma input;
	struct	accdma output;
};

#define	icsr	input.csr
#define	iba	input.ba
#define	iwc	input.wc
#define	ocsr	output.csr
#define	oba	output.ba
#define	owc	output.wc

/*
 * Bits Common to both input and out CSR's
 */
#define	ACC_ERR		0x8000		/* error present */
#define	ACC_NXM		0x4000		/* non-existant memory */
#define	ACC_RDY		0x0080		/* ready */
#define ACC_IE		0x0040		/* interrupt enable */
#define	ACC_RESET	0x0002		/* reset interface */
#define	ACC_GO		0x0001		/* start operation */

/*
 * Input Control Status Register
 */
#define IN_EOM		0x2000		/* end-of-message recieved */
#define IN_HRDY		0x0800		/* host ready */
#define IN_IMPBSY	0x0400		/* IMP not ready */
#define IN_RMR		0x0200		/* receive master ready error */
#define IN_IBF		0x0100		/* input data buffer full */
#define IN_WEN		0x0008		/* write enable */
#define IN_MRDY		0x0004		/* master ready */

#define ACC_INBITS \
"\20\20ERR\17NXM\16EOM\14HRDY\13IMPBSY\12RMR\11IBF\10RDY\7IE\
\4WEN\3MRDY\2RESET\1GO"

/*
 * Output Control Status Register
 */
#define OUT_TMR		0x0200		/* transmit master ready error */
#define OUT_BBACK	0x0008		/* bus back */
#define OUT_ENLB 	0x0004		/* enable last bit */

#define ACC_OUTBITS \
"\20\20ERR\17NXM\12TMR\10RDY\7IE\4BBACK\3ENLB\2RESET\1GO"
