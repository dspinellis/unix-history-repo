/* $Header: report.c,v 10.3 86/02/01 15:47:36 tony Rel $ */
/* report.c	routine to do a ReportStatus call to the vs
 *
 *	ReportStatus	Get information about the workstation
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

char *AllocateSpace();

/* Issue a report status.  This returns the device type and version (usually
 * useless), the microcode version (also useless), a bitmap describing the
 * frame buffer (very useful), a description of framebuffer memory
 * that isn't mapped to the screen, a description of the program memory
 * not used by the firmware (useful for caching data in the workstation),
 * and a description of where to map the host Vax's memory (useful to the
 * driver but not to anyone else).  There are two versions; if rom is
 * 1 the rom version is used; this only works before the firmware is started
 * up.  Any of the pointers may be NULL if the appropriate information
 * isn't required.
 */

ReportStatus (devType, devVersion, ucodeVersion, framebuffer,
		freeFramebuffer, freeProgram, hostMem, rom)
	int *devType, rom;
	short *devVersion, *ucodeVersion;
	BitMap *framebuffer;
	MemArea *freeFramebuffer, *freeProgram, *hostMem;
{
	register ReportStatusPacket *rsp;
#define	h ((PacketHeader *) rsp->rsp_head)

	rsp = (ReportStatusPacket *) AllocateSpace (sizeof (ReportStatusPacket));
	if (rsp == NULL) return (-1);

	/* Format the packet */

	h->ph_modifier.emptymod = 0;
	h->ph_opcode = (rom ? REPORT_STATUS_ROM : REPORT_STATUS);
	*(long *) h->ph_next = NULL;

	/* And send it off */

	if (WritePacket ((caddr_t) rsp) || SynchWrites()) return (-1);

	if (devType) *devType = *(long *) rsp->rsp_deviceType;
	if (devVersion) *devVersion = rsp->rsp_deviceVersion;
	if (ucodeVersion) *ucodeVersion = rsp->rsp_ucodeVersion;
	if (framebuffer)
		*framebuffer = *(BitMap *) rsp->rsp_visibleFramebuffer;
	if (freeFramebuffer)
		*freeFramebuffer = *(MemArea *) rsp->rsp_freeFramebuffer;
	if (freeProgram)
		*freeProgram = *(MemArea *) rsp->rsp_freeProgramMemory;
	if (hostMem)
		*hostMem = *(MemArea *) rsp->rsp_hostMemory;

	return (0);
}
