/*	udc.h	1.2	86/01/05	*/

struct UDPAC {		/* UDC command packet */
	char _stx  ;
	char _pkid ;
	char _pksiz ;	/* 21  bytes */
	char _pkdev ;	/* device # */	
	char _pkcmd ;	/* Command type */
	char _pkfnc ;	/* Function type */
	short _pkcnt ;	/* # of data blocks (1/4K each) */
	short _pbsiz ;	/* Block length size (0x100 = 1/4K) */
	short _pkmem[2] ;	/* Starting memory address.Should be 'long' but
				   we can't write it this way on Tahoe,
				   because of alignment problems - the C
				   will gap after the previous 'short' */
	short _pcksm ;	/* Checksum (unused) */
	long _psecno ;	/* Starting sector # */
	char _etx ;
} 
;

#define SECTSIZ 256		/* sector size */
#define L2SIZ	8		/* log2 of sector size */
#define L2BSIZ	10		/* log2 of block size */
#define NUDC	1		/* number of UDC controllers */
#define NUNIT	8		/* number of units per controller */
#define NUDDRV	5		/* number of drive types supported */
#define	DK_N	1		/* Monitoring device bit */

/*
**	Block devices sizes structure
*/

struct	size
{
	daddr_t	nblocks;
	int	block0;
};

#define udaddr ( (char *)(0xff0000+IOBASE) )
#define INT 0x00d		/* interrupt offset */
#define RST 0x00f		/* controller reset offset */
#define OB1 0x101
#define OB2 0x103
#define IB1 0x181
#define IB2 0x183
#define	PKID  0x187		/* packet id */
#define	PKLEN 0x189		/* packet length */
#define	PKDEV 0x18b		/* device # */
#define	PKCMD 0x18d		/* command type */
#define	PKSTT 0x18f		/* status type */
#define STAT1 0x191		/* i/o status result offset */
#define STAT2 0x193
#define DEVSTAT1 0x195
#define DEVSTAT2 0x197
#define DEVSIZE 0x199		/* # of sectors in device status message */

#define UDREAD 0x10
#define UDWTRY 0x01	/* read with retry */

#define UDWRITE 0x20
#define UDWCRC  0x01	/* write sectors and check CRC */
#define UDWSECT 0x02	/* write sectors */

#define UDTEST  0x30
#define UDSTAT  0x02	/* Test status */

#define DEVRDY 0x80		/* device ready bit in status byte */
#define	DEVTYPE 0x7		/* bits 0-2 in status byte define device */
#define FLP 0x1
#define HARDDISK 0x2
#define TAPE 	0x5

/*  Some I/O addresses used to generate pulses for scopes */
#define	OUT1	0xffffb034
#define	OUT2	0xffffb018
#define	OUT3	0xffffb020
#define	OUT4	0xffffb004
#define	OUT5	0xffffb024
#define	OUT6	0xffffb00c
#define	OUT7	0xffffb02c

#define	IN1	0xffffb030
#define	IN2	0xffffb03c
#define	IN3	0xffffb004
#define	IN4	0xffffb00c
#define	IN5	0xffffb02c

int dummy;
extern char vmem[][16*NBPG];

#define	IOaddr(off)	(caddr_t)((int)vmem + ((off) & 0x0fffff))

int	iospace_mapped;
#define	scope_out(x)	if(iospace_mapped) movob(IOaddr(OUT/**/x),0)
#define	scope_in(x)	if(iospace_mapped) dummy =  *IOaddr(IN/**/x)
