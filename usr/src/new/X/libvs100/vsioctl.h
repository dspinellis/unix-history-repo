/* $Header: vsioctl.h,v 10.3 86/02/01 15:48:03 tony Rel $ */
/* $Header: vsioctl.h,v 10.3 86/02/01 15:48:03 tony Rel $ */

/* 
 * vsio.h - VS100 I/O command definitions
 * 
 * Author:	Christopher A. Kent
 *		Digital Equipment Corporation
 *		Western Research Lab
 * Date:	Tue Jun 21 1983
 */

/* $Log:	vsioctl.h,v $
 * Revision 10.3  86/02/01  15:48:03  tony
 * X Version 10.0 Release 3.0
 * 
 * Revision 10.2  85/11/24  15:12:05  jg
 * add RCS id and mark it released...
 * 
 * Revision 10.1  85/11/08  17:38:41  newman
 * X Version 10.0 release
 * 
 * Revision 9.1  85/09/04  13:27:15  tony
 * X Version 9.0 release.
 * 
 * Revision 1.1  85/06/30  12:01:32  jg
 * Initial revision
 *  */

/* 
 * Possible ioctl calls
 */

#define	VSIOINIT	_IO(V, 0)	/* init the device */
#define	VSIOSTART	_IOW(V, 1, int)	/* start microcode */
#define	VSIOABORT	_IO(V, 2)	/* abort a command chain */
#define	VSIOPWRUP	_IO(V, 3)	/* power-up reset */
#define	VSIOGETVER	_IOR(V, 4, int)	/* get rom version */
#define	VSIOSYNC	_IO(V, 6)	/* synch with device */
#define	VSIOBBACTL	_IOW(V, 8, int)	/* control the BBA */
#define	VSIOFIBCTL	_IOW(V, 9, int)	/* lamp on/off */
#define	VSIOFIBRETRY	_IOW(V,10, int)	/* fiber retries */
#define	VSIOGETSTATS	_IOR(V,11, vsStats)	/* get statistics */
#define	VSIOGETIOA	_IOR(V,13, vsIoAddrAddr)/* get ioreg address */
#define	VSIOUSERWAIT	_IO(V, 15)	/* wait for user I/O completion */
#define VSIOWAITGO	_IOW(V, 16, caddr_t)	/* wait then go */


#define	VSIO_OFF	0		/* option off */
#define	VSIO_ON		1		/* option on */

#define	VS_FIB_FINITE	1		/* finite retries */
#define	VS_FIB_INFINITE	2		/* infinite retries */

typedef struct _vsStats{
	int	errors;			/* count errors */
	int	unsolIntr;		/* count unsolicited interrupts */
	int	overruns;		/* event queue overruns */
	int	flashes;		/* flashes on fiber link */
	int	ignites;		/* times turned on */
	int	douses;			/* times turned off */
	int	linkErrors;		/* link errors */
}vsStats;

typedef struct _vsIoAddr {
	short	 *ioreg;
	short	 status;
	caddr_t  obuff;
	int	 obufflen;
	int	 reloc;
	vsEvent  *ibuff;
	int	 iqsize;		/* may assume power of 2 */
	int	 ihead;			/* atomic write */
	int	 itail;			/* atomic read */
	vsCursor mouse;			/* atomic read/write */
	vsBox	 mbox;			/* atomic read/write */
} vsIoAddr;
typedef vsIoAddr *vsIoAddrAddr;
