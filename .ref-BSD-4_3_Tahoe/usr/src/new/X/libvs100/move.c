/* $Header: move.c,v 10.3 86/02/01 15:47:04 tony Rel $ */
/* move.c	Routines to move data into and out of the workstation
 *
 *	MoveObjectDownRom	Use rom version to move an object down
 *	MoveObjectDown		Move data into the workstation
 *	MoveBufferDown		Move buffer into the workstation
 *	MoveBufferUp		Move date out of the workstation
 *	SendToPeripheral	Send data to peripheral
 *
 * MoveObjectDownRom should be used to download the firmware into the
 * workstation; after that MoveObjectDown and MoveBufferDown should be used.
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

extern int VSReloc;

char *AllocateSpace();

/* Move size bytes from src to dst.  If rom = 1, use the rom-code version.
 * This only works for objects which are <= MAXSIZE bytes;
 */

MoveObjectDownRom (src, dst, size)
	caddr_t src, dst;
	int size;
{
	return (MoveObject(src + VSReloc, dst, size, 1));
}

MoveObjectDown (src, dst, size)
	caddr_t src, dst;
	int size;
{
	return (MoveObject(src + VSReloc, dst, size, 0));
}

MoveBufferDown (src, dst, size)
	caddr_t src, dst;
	int size;
{
	register int len;
	caddr_t buf;

	while (len = size) {
	    if (len > VBUFSIZE) len = VBUFSIZE;
	    if ((buf = (caddr_t) AllocateSpace (len)) == NULL)
		return (-1);
	    bcopy (src, buf, len);
	    if (MoveObject(buf + VSReloc, dst, len, 0))
		return (-1);
	    src += len;
	    dst += len;
	    size -= len;
	}
	return (0);
}

MoveBufferUp (src, dst, size)
	caddr_t src, dst;
	int size;
{
	register int len;
	caddr_t buf;

	while (len = size) {
	    if (len > VBUFSIZE) len = VBUFSIZE;
	    if ((buf = (caddr_t) AllocateSpace (len)) == NULL ||
		MoveObject(src, buf + VSReloc, len, 0) ||
		SynchWrites())
		return (-1);
	    bcopy (buf, dst, len);
	    src += len;
	    dst += len;
	    size -= len;
	}
	return (0);
}

MoveObject (src, dst, size, rom)
	caddr_t src, dst;
	int size, rom;
{
	register MoveObjectPacket *mop;
#define	h ((PacketHeader *) mop->mop_head)

	mop = (MoveObjectPacket *) AllocateSpace (sizeof (MoveObjectPacket));
	if (mop == NULL) return (-1);

	/* Make sure size is a multiple of 2 */

	if (size & 0x1) size++;

	/* Format the packet */

	h->ph_modifier.emptymod = 0;
	h->ph_opcode = (rom ? MOVE_OBJECT_ROM : MOVE_OBJECT);
	*(long *) h->ph_next = NULL;

	mop->mop_objectType = 1;
	*(caddr_t *) mop->mop_source = src;
	*(caddr_t *) mop->mop_dest = dst;
	*(long *) mop->mop_objectSize = size;

	return (WritePacket ((caddr_t) mop));
#undef h
}

SendToPeripheral (src, size, device)
	char *src;
	int size, device;
{
	register MoveObjectPacket *mop;
#define	h ((PacketHeader *) mop->mop_head)

	mop = (MoveObjectPacket *) AllocateSpace (sizeof (MoveObjectPacket));
	if (mop == NULL) return;

	/* Make sure size is a multiple of 2 */

	if (size & 0x1) size++;

	/* Format the packet */

	h->ph_modifier.emptymod = 0;
	h->ph_opcode = MOVE_OBJECT;
	*(long *) h->ph_next = NULL;

	mop->mop_objectType = 2;
	*(caddr_t *) mop->mop_source = src + VSReloc;
	*(int *) mop->mop_dest = device;
	*(long *) mop->mop_objectSize = size;

	WritePacket ((caddr_t) mop);
#undef h
}
