/* $Header: packet.c,v 10.4 86/07/29 11:33:13 wesommer Rel $ */
/* packet.c	Routines to allocate, free, and use packets
 *
 *	OpenDisplay		Open it
 *	InitDisplay		Download it
 *	DisplayDead		Check if dead
 *	PacketInit		Set things up
 *	WritePacket		Buffers a packet for writing
 *	SynchWrites		Wait for completion of all pending writes
 *	AllocateSpace		Allocate some temporary storage
 *	AllocateCopy		Copy data to temporary storage
 *	DeallocateSpace		Flush space of unsent packet
 *
 */

/****************************************************************************
 *									    *
 *  Copyright (c) 1983, 1984 by						    *
 *  DIGITAL EQUIPMENT CORPORATION, Maynard, Massachusetts.		    *
 *  All rights reserved.						    *
 * 									    *
 *  This software is furnished on an as-is basis and may be used and copied *
 *  only with inclusion of the above copyright notice. This software or any *
 *  other copies thereof may be provided or otherwise made available to     *
 *  others only for non-commercial purposes.  No title to or ownership of   *
 *  the software is hereby transferred.					    *
 * 									    *
 *  The information in this software is  subject to change without notice   *
 *  and  should  not  be  construed as  a commitment by DIGITAL EQUIPMENT   *
 *  CORPORATION.							    *
 * 									    *
 *  DIGITAL assumes no responsibility for the use  or  reliability of its   *
 *  software on equipment which is not supplied by DIGITAL.		    *
 * 									    *
 *									    *
 ****************************************************************************/

#include "vs100.h"
#include <fcntl.h>
#include <errno.h>
#include <sys/ioctl.h>
#include "vsioctl.h"
#include <vaxuba/vsreg.h>
#include "reason.h"

extern int errno;
extern BitMap screen;

char *ErrorString(), *strcpy(), *strcat();

int vsdev;

int VSReloc;

vsIoAddr *VSAddr;
caddr_t VSBuf;
int VSBuflen;
caddr_t VSBufLim;
short *IOReg;
short *VSResponse;

int active;
int upper;
caddr_t limptr, curptr;

/* Open the display */

OpenDisplay (vsNumber)
	char *vsNumber;
{
	char vsname[10];
	strcpy (vsname, "/dev/vs");
	strcat (vsname, vsNumber);

	return (vsdev = open (vsname, O_RDWR|O_NDELAY));
}

/* Do vs100 specific initialization */

InitDisplay (info)
	register DEVICE *info;
{
	if (DownLoad ())
	    return (-1);
	info->id = XDEV_VS100;
	info->width = screen.bm_width;
	info->height = screen.bm_height;
	info->planes = 1;
	info->entries = 0;
	info->mouse = &VSAddr->mouse;
	info->mbox = &VSAddr->mbox;
	info->queue = (vsEventQueue *) &VSAddr->ibuff;
	return (0);
}

/* Check if display is dead */

DisplayDead ()
{
	int ver;

	return(ioctl(vsdev, (int) VSIOGETVER, (caddr_t) &ver));
}

PacketInit()
{
	if (ioctl (vsdev, (int) VSIOGETIOA, (caddr_t) &VSAddr) ||
	    ioctl (vsdev, (int) VSIOINIT, (caddr_t) NULL))
	    return (-1);
	VSBuf = VSAddr->obuff;
	VSBuflen = VSAddr->obufflen;
	VSBufLim = VSBuf + VSBuflen;
	IOReg = (short *) VSAddr->ioreg;
	VSReloc = VSAddr->reloc - (int) VSBuf;
	VSResponse = (short *) &VSAddr->status;
        *VSResponse = 0;
	IOReg[0] &= ~VS_FCN;
	active = 0;
	curptr = VSBufLim;
	limptr = VSBuf;
	upper = 1;
	return (0);
}   

WritePacket (pkt)
	caddr_t pkt;
{
	register int ret;
	register short *ioreg;

	pkt += VSReloc;
	limptr = curptr;
	if (upper) {
	    curptr = VSBuf;
	    upper = 0;
	} else {
	    curptr = VSBufLim;
	    upper = 1;
	}
	if (ret = active) {
	    if (!(*VSResponse)) {
		if (ret = ioctl(vsdev, (int) VSIOWAITGO, (caddr_t) &pkt))
		    VSError ();
		return (ret);
	    }
	    if ((ret = *VSResponse) & VS_ERROR) {
		errno = ret & VS_REASON + IR_ERROR;
		VSError ();
		ret = -1;
	    } else
		ret = 0;
	}
	ioreg = IOReg;	/* C sucks */
	ioreg[3] = ((short *)&pkt)[0];
	ioreg[4] = ((short *)&pkt)[1];
	ioreg[0] &= ~VS_FCN;
	*VSResponse = 0;
	ioreg[0] |= VS_IE | (VS_SEND << VS_FCSHIFT) | VS_GO;
	active = 1;
	return (ret);
}

SynchWrites ()
{
	register int ret;

	if (!active) return (0);
	if (!(*VSResponse) && ioctl(vsdev, (int) VSIOUSERWAIT, (caddr_t) NULL))
	    VSError ();
	if ((ret = *VSResponse) & VS_ERROR) {
	    errno = ret & VS_REASON + IR_ERROR;
	    VSError ();
	    ret = -1;
	} else
	    ret = 0;
	*VSResponse = 0;
	active = 0;
	if (upper)
	    limptr = VSBuf;
	else
	    limptr = VSBufLim;
	return (ret);
}

caddr_t AllocateSpace (size)
	register int size;
{
	register caddr_t ptr;

	if (size & 1) size++;

	if (upper) {
	    if ((curptr -= size) >= limptr)
		return (curptr);
	    SynchWrites ();
	    if (curptr >= limptr)
		return (curptr);
	    curptr = VSBufLim;
	} else {
	    ptr = curptr;
	    if ((curptr += size) <= limptr)
		return (ptr);
	    SynchWrites ();
	    if (curptr <= limptr)
		return (ptr);
	    curptr = VSBuf;
	}
	errno = ENOMEM;
	VSError ();
	return (NULL);
}

caddr_t AllocateCopy (buf, size)
	caddr_t buf;
	register int size;
{
	register caddr_t ptr;

	if (size & 1) size++;

	if (upper) {
	    if ((curptr -= size) < limptr) {
		SynchWrites ();
		if (curptr < limptr) {
		    curptr = VSBufLim;
		    errno = ENOMEM;
		    return (NULL);
		}
	    }
	    ptr = curptr;
	} else {
	    ptr = curptr;
	    if ((curptr += size) > limptr) {
		SynchWrites ();
		if (curptr > limptr) {
		    curptr = VSBuf;
		    errno = ENOMEM;
		    VSError ();
		    return (NULL);
		}
	    }
	}
	bcopy (buf, ptr, size);
	return (ptr);
}

DeallocateSpace ()
{
	if (upper)
	    curptr = VSBufLim;
	else
	    curptr = VSBuf;
}

VSError ()
{
	DeviceError (ErrorString (errno));
}
