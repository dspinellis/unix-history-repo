/* packet.c	Routines to allocate, free, and use packets
 *
 *	OpenDisplay		Open it
 *	InitDisplay		Download it
 *	DisplayDead		Check if dead
 *	Allocate_space		Allocate some temporary storage
 *	Deallocate_space	Flush space of unsent packet
 *
 */


#include <stdio.h>
#include <fcntl.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include "ddxqvss.h"
#include <vaxuba/qvioctl.h>

vsIoAddr *VSAddr;
vsEvent *VSEvent;
int VSqMax;

BITMAP pbm;		/* the actual screen memory    */
BITMAP vbm;		/* the size of the root window */
caddr_t ltable[YSIZE];	/* line address table	       */

int vsdev = -1;
extern int errno;

/* Open the display */

/*ARGSUSED*/
OpenDisplay (vsNumber)
	char *vsNumber;
{
	char vsname[10];

	return(vsdev = open ("/dev/mouse", O_RDWR|O_NDELAY));
}

/* Do qvss specific initialization */

InitDisplay (info)
	register DEVICE *info;
{
	register int i;
	if (ioctl(vsdev,QIOCADDR,&VSAddr) == -1) return(-1);
	if (ioctl(vsdev,QIOCINIT,0) == -1) return(-1);
	VSEvent = VSAddr->ibuff;
	VSqMax = VSAddr->iqsize - 1;
	vbm.data = VSAddr->bitmap;
	vbm.width = VSAddr->max_x;
	vbm.height = VSAddr->max_y;
	pbm.data = VSAddr->bitmap;
	pbm.width = 1024;
	pbm.height = VSAddr->max_y;
	for(i = 0; i < YSIZE; i++)  ltable[i] = VSAddr->bitmap + (i * XSIZE);
	info->id = XDEV_QVSS;
	info->width = vbm.width;
	info->height = vbm.height;
	info->planes = 1;
	info->entries = 0;
	info->mouse = &VSAddr->mouse;
	info->mbox  = &VSAddr->mbox;
	info->queue = (vsEventQueue *) &VSAddr->ibuff;
	return (0);
}

/* Check if display is dead */

DisplayDead ()
{
	return(0);
}

/* the presumption here is that only one Allocate_space call is made/request */

#define ABUFSIZE 3072
static char ABuffer[3072];	/* random size buffer for allocate space */
caddr_t AllocateSpace (size)
	register int size;
{
	if (size < ABUFSIZE) return(ABuffer);
	errno = ENOMEM;
	return (NULL);
}


