/* $Header: dl.c,v 10.3 86/02/01 15:46:40 tony Rel $ */
/* dl.c		downloads code into the vs100.  Routines are
 *
 *	DownLoad	Determines device version and downloads firmware
 *
 *	Takes an int which is the device to download and
 *		returns 0 if everything went well and -1 if
 *		there is some problem.
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
#include <sys/ioctl.h>
#include "vsioctl.h"
#include "vssite.h"

#define CODE_START_ADDRESS 0x1000

#define BUFSIZE 512		/* Why not?  Everyone uses 512 */

int HexArray[128];
#define Hex2(p)		((HexArray[*(p)]<<4)+HexArray[*((p)+1)])

extern int vsdev;		/* File number of the workstation */
BitMap screen;			/* The actual screen memory */

/* Determine what version of the vs we have and download the appropriate
 * firmware.  Determining the version is not trivial and as versions change
 * this may also have to change.
 */

int DownLoad ()
{
	FILE *fopen(), *fd;	/* The file descriptor */
	char *AllocateSpace();
	char *filename;		/* Name of download file */
	char *buf;		/* The input buffer */
	char inline[BUFSIZE];	/* The line from the file */
	int bufpos;		/* The current position in the buffer */
	int linesize;		/* The number of bytes represented by a line */
	int lineaddr;		/* The intended address of the line */
	int nextaddr;		/* The expected next address */
	int destaddr;		/* The destination for the current buffer */
	caddr_t pMem;		/* Program memory pointer */
	caddr_t startAddr;	/* Address to start microcode */
	MemArea programMemory;	/* Information about program memory */
	char textVersion[5];	/* The version number reported back */
	int version;
	char errmessage[BUFSIZE];	/* error message buffer */

	/* Initialize the device,
	 * figure out just what version of the device we have, and
	 * get the address to load the code into
	 */

	if (PacketInit() ||
	    ioctl (vsdev, (int) VSIOINIT, (caddr_t) NULL) ||
	    ioctl (vsdev, (int) VSIOGETVER, (caddr_t) &version) ||
	    ReportStatus ((int *) textVersion, (short *) NULL, (short *) NULL,
			  &screen, (MemArea *) NULL, &programMemory,
			  (MemArea *) NULL, 1)) {
		VSError ();
		return(-1);
	}

	switch (version) {
	    case 8:

		/* Everyone in the world reports 8!  Fortunately I can
		   tell them apart by other means (Then why isn't that
		   the version???  You tell me!) */

		if (screen.bm_height != 800) filename = LOAD_FILE_3_8;
		else filename = LOAD_FILE_2B;
		break;

	    case 1:	/* This is an SBO, I think!  */
		filename = LOAD_FILE_SBO;
		break;

	    default:

		/* Assume we have a 3.10.  We may even be right */

		filename = LOAD_FILE_3_10;
		break;
	}

	pMem = *(caddr_t *) programMemory.m_base;
	startAddr = pMem + CODE_START_ADDRESS;

	/* Open the file in read mode */

	if ((fd = fopen (filename, "r")) == NULL) {
		sprintf(errmessage, "Xvs100: Can't open uCode file %s.\n",
			filename);
		DeviceError(errmessage);
		return (-1);
	}

	InitHexArray();

	/* Read in the s-line file and copy to the workstation */

	bufpos = 0;
	buf = AllocateSpace (BUFSIZE);
	destaddr = 0;

	while (fgets (inline, BUFSIZE, fd) != NULL) {
	    if (inline[0] == '\n') continue;
	    if (ParseLine (inline, &linesize, &lineaddr)) break;
	    if (destaddr == 0) destaddr = nextaddr = lineaddr;
	    if (bufpos > 0 &&
		    (lineaddr != nextaddr || linesize + bufpos > BUFSIZE)) {
		if (MoveObjectDownRom (buf, pMem + destaddr, bufpos))
		    return (-1);
		buf = AllocateSpace (BUFSIZE);
		bufpos = 0;
		destaddr = nextaddr = lineaddr;
	    }
	    CopyLine (inline, buf, linesize, bufpos);
	    bufpos += linesize;
	    nextaddr += linesize;
	}

	/* Copy the last packet */

	if (bufpos > 0) {
	    if (MoveObjectDownRom (buf, pMem + destaddr, bufpos))
		    return (-1);
	}

	fclose (fd);

	/* Sync the writes to make sure it's all been downloaded! */

	if (SynchWrites()) return (-1);

	/* Start microcode */

	if (ioctl (vsdev, (int) VSIOSTART, (caddr_t) &startAddr)) return (-1);

	return (VSMemInit());		/* Initialize memory allocator */
}

/* The following routines parse the so-called s-line format.  This was
 * lifted more or less verbatim from the vms software; I don't fully
 * understand it and you certainly don't want to even bother trying.
 */

ParseLine (line, linesize, lineaddr)
	char *line;
	int *linesize, *lineaddr;
{
	if (line[0] != 'S') DLError ("Invalid format", line);

	switch (line[1]) {
	    case '0':
		break;
	    case '1': case '2':
#ifdef notdef
		CheckLine (line);	/* Checks checksum */
#endif
		if (line[1] == '1') {
		    *linesize = Hex2(line+2) - 3;
		    *lineaddr = Hex (line+4, 4);
		} else {
		    *linesize = Hex2(line+2) - 4;
		    *lineaddr = Hex (line+4, 6);
		}
		break;
	    case '9':
		return (-1);
	    default:
		DLError ("Invalid format", line);
		break;
	}
	return (0);
}

CopyLine (line, buffer, count, pos)
	char *line, *buffer;
	register int count, pos;
{
	register char *start;

	if (line[1] == '1') start = line + 8;
	else start = line + 10;

	while (--count >= 0) {
	    buffer[pos^1] = Hex2(start);
	    pos++;
	    start += 2;
	}
}

CheckLine (line)
	register char *line;
{
	register char *lp;
	register int count;
	register char checksum;

	lp = line + 2;
	count = Hex2(lp);
	checksum = 0;

	do {
	    checksum += Hex2(lp);
	    lp += 2;
	} while (--count >= 0);

	if (checksum != 0xff) DLError ("Bad checksum", line);
}

InitHexArray ()
{
    register char c;
    for (c = 'A'; c <= 'F'; c++) HexArray[c] = c - 'A' + 10;
    for (c = 'a'; c <= 'f'; c++) HexArray[c] = c - 'a' + 10;
    for (c= '0'; c <= '9'; c++) HexArray[c] = c - '0';
}

int Hex (cp, n)
	register char *cp;
	register int n;
{
	register int i = 0;
	while (--n >= 0)
	    i = (i<<4) + HexArray[*cp++];
	return (i);
}

DLError (str1, str2)
	char *str1, *str2;
{
	fprintf (stderr, "Downloader: %s: %s\n", str1, str2);
	fflush (stderr);
	exit (1);
}
