/*	udcformat.c	1.2	86/01/21	*/
/* format disk - cmd/smd type */

#include "../machine/mtpr.h"

#include "param.h"
#include "inode.h"
#include "fs.h"

#include "../tahoevba/vbaparam.h"
#include "../tahoevba/udc.h"

#include "saio.h"

char disk[50] = "xmd(00,000)";

main()
{
	int j, c, i, n;
	char buf[50];
	int output;

	do {
		printf("Device to format: ");
		gets(disk);
		output = open(disk, 1);
		if (output <= 0) printf("Cannot open disk\n", disk);
		/* dummy call just to get to the local strategy routine */
		else {
			printf("Type  <return> to start formatting ");
			gets(buf);
			write(output,buf,0); 
			printf("Formatting completed. \n");
		}
	} while (output <= 0);

}

/*
 *  Universal disk controller driver for the Motorola M68000/IPC.
 *	Stand-alone version (no interrupts, etc.)
 */


static struct UDPAC udpkt = {
	2, 0, 21, 0, 0, 0, 0, SECTSIZ, {0, 0}, 0, 0, 3
} ;

long udstd[] = { 	/* May be used some day to boot from any of
			 *  several UDC controllers */
	0xf0000 
};

/*****************************************************
/*
/*The next layout of major/minor number assignments are for the UDC
/*devices.
/*
/* 	  1
/*	  5		 8 7     4 3 2   0
/*	 +----------------+-----+-+-+-----+
/*	 | Major device # |     |D|R| FLS |
/*	 +----------------+-----+-+-+-----+
/*				 | |   |_____ File system # ( 0-7 )
/*				 | |_________ Fixed (0) or removable(1) media
/*				 |___________ Drive # (0-1)
/*
/* For the floppy drives, the major / minor assignment will be
/* 	  1
/*	  5		 8 7     4 3 2   0
/*	 +----------------+-----+---+-----+
/*	 |      4         |     | D | FLS |
/*	 +----------------+-----+---+-----+
/*				  |    |_____ File system # ( 0-7 )
/*				  |____________ Drive # (0-3)
/*	
/****************************************************/

#define	UDCUNIT(x)	((minor(x) & 0x18) >> 3)

udstrategy(io, func)
register struct iob *io;
long func;		/* Known to be 'read' */
{

	register unit = io->i_unit;
	register bn = io->i_bn;
	register char *cntaddr ;
	register char *addr ;
	register timeout , retries , i;

	cntaddr = (char *)(udstd[0] + VBIOBASE); /* Booting from cntrlr 0 */
	/*
	 * prepare a command packet for the controller.
	 */
	retries = 3;
loop:
	if (cntaddr[OB1]) {
		printf("UDC controller not ready, %x=%x\n",OB1+cntaddr,
			cntaddr[OB1] & 0xff);
		return(0);
	}
	udpkt._pksiz = 7 ;
	udpkt._pkid = 0xAA ;
 	udpkt._pkdev = UDCUNIT(unit);
	udpkt._pkcmd = 0x40; 	/* control command */
	if (io->i_ino.i_dev == 1) { 
		udpkt._pkdev += 4;  /* Floppy */
		udpkt._pkfnc = 2; /* format floppy */
	}
	else udpkt._pkfnc = 2;	/* format disk */
	udpkt._pkcnt = 3 << 8; 	/* EOT (sort of) */
	if (movep21(&udpkt,cntaddr+0x105,7)) {
		cntaddr[OB1] = (char)0x80 ;	/* signal packet transmitted */
		cntaddr[IB2] = (char)0 ;	/* clear ACK/NAK field */
		cntaddr[INT] = (char)0x0 ;	/* interrupt the controller */
	}
	else {
		printf ("Wrong command packet arrived at UDC\n");
		printf ("Original	UDC\n");
		for (i = 0; i < sizeof(udpkt); i++ ) 
			printf("   %0x\t%0x\n", ((char *)&udpkt)[i*2] & 0xff,
				cntaddr[0x105+i*2] & 0xff);
	}
/*
 *
 * Wait until done (no interrupts now).
 *
 */
wait:
	timeout  =  10000;
	while (cntaddr[IB2] != (char)0x06 && cntaddr[IB2] != (char)0x15) {
/**************
		DELAY(10000);		
		timeout--;
		if (timeout <= 0) {
			printf("UDC controller timeout\n");
			return(0);
		}
*****************/
	}
	udpkt._pksiz = 21;
	if (cntaddr[IB2] == (char)0x15) {
		if (retries-- < 0) {
			printf("Too many NAK from UDC - give up\n");
			return(0);
		} else goto loop;
	}

	while (cntaddr[IB1] != (char)DEVRDY)
/*		DELAY (10000);		/* Wait for his response */;


	/* Ignore unsolicited status messages */
	if (cntaddr[PKID] != (char)udpkt._pkid && cntaddr[PKSTT] == (char)0x80)
	{
		cntaddr[IB1] = (char)0;
		cntaddr[OB2] = (char)6;
		cntaddr[INT] = (char)0x80;
		goto loop;
	}
	if (cntaddr[PKID] != (char)udpkt._pkid ||
		cntaddr[PKDEV] != (char)udpkt._pkdev ||
		cntaddr[PKLEN] != (char)19 ||
		cntaddr[PKCMD] != (char)udpkt._pkcmd ||
		cntaddr[PKSTT] != (char)0x70 ||	/* Command completion */
		cntaddr[STAT1] != (char)0 ||
		cntaddr[STAT2] != (char)0 ) {
			printf ("Strange status from UDC:\n");
			printf("Packet id=%x,unit=%x,original command=%x,status type=%x,status=%x\n", 
			cntaddr[PKID] & 0xff,
			cntaddr[PKDEV] & 0xff,
			cntaddr[PKCMD] & 0xff, 
			cntaddr[PKSTT] & 0xff,
			(cntaddr[STAT1]*256+cntaddr[STAT2]) & 0xffff);
			if  (cntaddr[PKLEN] > 9) {
				printf("More response info : ");
				for (i=1; i<=cntaddr[PKLEN]-9; i++)
				      printf("%x ", cntaddr[STAT2+2*i] & 0xff);
				printf("\n");
			}
			cntaddr[IB1] = (char)0;
			cntaddr[OB2] = (char)6;
			cntaddr[INT] = (char)0x80;
			return(0);
	} else {
		cntaddr[IB1] = (char)0;
		cntaddr[OB2] = (char)6;
		cntaddr[INT] = (char)0x80;
		mtpr(PADC, 0);		/* So data will come in right */
		return(io->i_cc);
	  }
}

/*
 *	Transfer a 21 bytes packet to the controller.
 *  the message is written to odd addresses, starting from
 *  the given address.
 *	For reliability, read it back and see if it's the same. If not,
 *  return an error code.
 */
movep21(src, dest,cnt)

char	*src, *dest;
int cnt;
{
	register char *running_src,  *running_dest;
	register long running_cnt;

	running_src = src;
	running_dest = dest;
	running_cnt = cnt;

	for (; running_cnt>0; running_cnt--) {
		*running_dest++ = *running_src++;
		running_dest++;
	}
	running_src = src;
	running_dest = dest;
	running_cnt = cnt;
	for (; running_cnt>0; running_cnt--) {
		if (*running_dest++ != *running_src++) return(0);
		running_dest++;
	}
	return(1);
}

udopen(io)
struct iob *io;
{
	register char *cntaddr;
/*
 * Just clean up any junk in the controller's response buffers.
 */	
	cntaddr = (char *)(udstd[0] + VBIOBASE); /* Booting from cntrlr 0 */
	while (cntaddr[IB1] == (char)DEVRDY) {
		cntaddr[IB1] = (char)0;
		cntaddr[OB2] = (char)0x06;  /* ACK */
		cntaddr[INT] = (char)0;	/* Force him to listen and  respond */
		DELAY(50000);
	}
}
