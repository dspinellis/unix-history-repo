/*	xpformat.c	1.2	86/01/21	*/
/*
/* format disk on Xylogics controller - fsd/smd/fujitsu type */
/**/

#include "../machine/mtpr.h"
#include "param.h"
#include "inode.h"
#include "saio.h"

#include "../tahoevba/vbaparam.h"
#include "../tahoevba/xpreg.h"

char disk[10] ;			/* disk type (smd/fsd/fuj) */
char drive[10] ;		/* drive number */
char start[10];	
char buf[512];			/* format data buffer */

int vdebug = 1;

long	xpstand[] = {
		0x0fee40 };

struct	xp_iopb *iopb = &iopbx;
int dsktype;
int nsect,ncyl,ntrack;

#define XY_SHORT(x)	(short)((((x) >> 8) & 0xff) + (((x) << 8) & 0xff00))
#define	b_cylin b_resid
int xytimeout;
#define POLLTILLDONE(x) { xytimeout = 1000*(x); \
			while (xpaddr->xpcsr & XP_GBSY) { \
				DELAY(1000); \
				xytimeout--; \
				if (xytimeout <= 0) { \
					printf("XY timeout\n"); \
					return(0); \
				} \
			} \
		}

main()
{
	int j, c, i, n;

	printf("Drive type [fsd/smd/fuj]: ");
	gets(disk);
	printf("Drive number [0-3]: ");
	gets(drive);
	j = number(drive);
	if ((strcmp(disk,"fsd") || strcmp(disk,"smd") ||
	     strcmp(disk,"fuj")) && ( j <= 3))
	{
		if (xpstart(disk,j) == 0) {
			printf("Initialization failed (drive not ready?), giving up!\n");
			return;
		}
		printf("Type  <return> to start formatting ");
		gets(start);
		if (xpformat(disk,j)); 
			printf("Formatting completed. \n");
	}
	else if (j>3) printf("Illegal drive number\n");
	     else printf("Illegal drive type\n"); 
}

int number (response)
char *response;
{
	int	i, j;
	
	j = 0;	/* Total */
	while (*response == ' ' || *response == '\t') response++;
	while (*response >= '0' && *response <= '9') {
		j = j*10 + *response - '0';
		response++;
	}
	return (j);
}

xpstart(disk,unit)
char *disk;
int unit;
{
	struct xpdevice *xpaddr;
	int ret;

	/*
	 * Check if a drive is really there. (NOP selects the drive and
	 * returns DRDY status
	 */
	xpmkiopb(XP_NOP,unit,0,0,0,0,0,0);
	iopb->io_comm &= ~XP_IEN;		/* disable interrupts */

	xpaddr = (struct xpdevice *)(xpstand[0] + VBIOBASE); /* formatting on cntl 0  */
	ret = xpaddr->xpreset;			/* reset controller */
	DELAY(400);				/* wait 400 ns */
	xpdgo(xpaddr,iopb);	/* start the controller */
	DELAY(200);	/* wait 200 ns before checking CSR for completion */

	POLLTILLDONE(1)
	DELAY(200);
	uncache((char *)&iopb->io_status);
	if ((XY_SHORT(iopb->io_status) != 5) ||  /* 5 = no errors, xy450, DONE */
		!(xpaddr->xpcsr & XP_DRDY) ||	/* drive is not ready */
		(xpaddr->xpcsr & (XP_ERR | XP_DERR))) { /* errors? */
			printf("XY start error. Status = %x, xpcsr= %x\n",
				XY_SHORT(iopb->io_status),xpaddr->xpcsr);
		return(0);
	}
	/*
	 * now set the drive size parameters in the controller 
	*/
	if (strcmp(disk,"fsd")) {
		xpmkiopb(XP_DSIZE,unit,9,822,31,0,0,0); /* 160M fsd */
		dsktype = 0;
		nsect = 32;
		ncyl = 823;
		ntrack = 10;
	}
	else
	  if (strcmp(disk,"smd")) {
		xpmkiopb(XP_DSIZE,unit,18,822,31,0,0,0x40); /* 300M smd */
		dsktype = 0x40;
		nsect = 32;
		ncyl = 823;
		ntrack = 19;
	  }
	  else {
		xpmkiopb(XP_DSIZE,unit,19,841,45,0,0,0x80);  /* 474M Fujitsu */
		dsktype = 0x80;
		nsect = 46;
		ncyl = 842;
		ntrack = 20;
	       }
	iopb->io_comm &= ~XP_IEN;	/* disable interrupts */
	xpdgo(xpaddr,iopb);
	DELAY(200);

	POLLTILLDONE(1)
	DELAY(200);
	uncache((char *)&iopb->io_status);
	if ((XY_SHORT(iopb->io_status) != 5) || 		/* errors */
		!(xpaddr->xpcsr & XP_DRDY) ||
		(xpaddr->xpcsr & (XP_ERR | XP_DERR))) 
	{
		printf("XY set size error. status= %x, drive $d, type %s\n",
			XY_SHORT(iopb->io_status),unit,disk);
		return(0);
	}
	else return(1);
}

xpformat(disk,unit)
char disk[10];
int unit;
{
	struct xpdevice *xpaddr;
	int i,j,flag;

	xpaddr = (struct xpdevice *)(xpstand[0] + VBIOBASE); /* formatting on cntl 0  */
	xpmkiopb(XP_FORMAT,unit,0,0,0,nsect,0,dsktype); 
	for (i=0; i<ncyl; i++) {
		iopb->io_comm &= ~XP_IEN;	/* disable interrupts */
		iopb->io_status = 0;
		iopb->io_sect = 0;
		iopb->io_scnt = XY_SHORT(nsect*ntrack);
		iopb->io_cyl = XY_SHORT(i);
		iopb->io_head = 0;
		xpdgo(xpaddr,iopb);
		DELAY(200);

		POLLTILLDONE(1*60)
		DELAY(200);
		uncache((char *)&iopb->io_status);
		if ((XY_SHORT(iopb->io_status) != 5) ||
			(xpaddr->xpcsr & (XP_ERR | XP_DERR)) )
		{
			printf("XY format error %x, drive $d, type %s\n",
				XY_SHORT(iopb->io_status),unit,disk);
			return(0);
		}
		printf(".");
	}
	return(1);
}

strcmp(str1,str2)
char *str1;
char *str2;
{

	while (*str1++ && *str2++ ) 
		if (*str1 != *str2) return(0) ;
	return(1);
} 
			
/*
 * Now all ready to go, stuff the registers.
 */
xpdgo(xpaddr, iopb)
	register struct xpdevice *xpaddr;
	register struct xp_iopb *iopb;
{
	movob(&xpaddr->xpmrel, (u_char)((int)iopb >> 24));
	DELAY(5);
	movob(&xpaddr->xplrel, (u_char)((int)iopb >> 16));
	DELAY(5);
	movob(&xpaddr->xpmba, (u_char)((int)iopb >> 8));
	DELAY(5);
	movob(&xpaddr->xplba, (u_char)((int)iopb));
	DELAY(5);
	movob(&xpaddr->xpcsr, XP_GBSY) ;
}

/*
 * Fill the iopb with the appropriate data.
 */
xpmkiopb(cmd,unit,head,cylinder,sector,scount,baddr,xptype)
  unsigned int cmd, unit, head, cylinder, sector, scount, xptype;
  caddr_t baddr;
{
	iopb->io_comm = cmd | XP_RELO ;
	iopb->io_imode = XPM_ASR | XPM_EEF | XPM_ECC;
	iopb->io_throt = XPT_T128;
	iopb->io_drive = xptype | unit;
	iopb->io_head = head;
	iopb->io_sect = sector;
	iopb->io_cyl = XY_SHORT(cylinder);
	iopb->io_scnt = XY_SHORT(scount);
	iopb->io_mladdr = XY_SHORT((int)baddr & 0xffff);
	iopb->io_mhaddr = XY_SHORT(((int)baddr >> 16) & 0xffff);
	iopb->io_status = 0;
}
