/*
 * Contributed to 386bsd 0.1 and later versions
 *
 *	Copyright 1992 by Holger Veit
 *	May be freely used with Bill Jolitz's port of 
 *	386bsd and may be included in a 386bsd collection
 *	as long as binary and source are available and reproduce the above
 *	copyright.
 *
 *	You may freely modify this code and contribute improvements based
 *	on this code as long as you don't claim to be the original author.
 *	Commercial use of this source requires permittance of the copyright 
 *	holder. A general license for 386bsd will override this restriction.
 *
 *	Use at your own risk. The copyright holder or any person who makes
 *	this code available for the public (administrators of public archives
 *	for instance) are not responsible for any harm to hardware or software
 *	that might happen due to wrong application or program faults.
 *
 * You must have the codrv-0.1.1 or later driver in the same package 
 * generated into the 386bsd kernel, otherwise this program does not work.
 *
 *	@(#)whichcons.c	1.0 (386bsd contribution) 01/10/93
 */

#include "pathnames.h"
#include <sys/types.h>
#include <sys/ioctl.h>

#define COMPAT_CO011
#define COMPAT_PCCONS
#include <sys/ioctl_pc.h>

/*
 *  find out, which console driver is there
 *
 *  returns:
 *	-1:	error
 *	0:	unknown
 *	1:	pccons.c
 *	2:	pccons with X support
 *	3:	codrv-0.1.1
 *	4:	codrv-0.1.2 or later
 *	5:	Rich's version (hopefully)
 */
#define UNKNOWN		0
#define PCCONS		1
#define PCCONSX		2
#define CODRV011	3
#define CODRV01X	4
#define PCCONSR		5

int whichcons()
{
	int fd;
	struct consinfo	ci;
	struct oldconsinfo oci;
	unsigned long	*ip;
	
	fd = open(_PATH_KEYBOARD,0);
	if (fd < 0) {
		/* no /dev/kbd, can be pccons with or without X */
		fd = open(_PATH_CONSOLE,0);
		if (fd < 0) return UNKNOWN;
	}

	ip = 0;
	/* try to get console info structure */
	if (ioctl(fd,CONSGINFO,&ci) >= 0)
		ip = &ci.info1;
	else if (ioctl(fd,OLDCONSGINFO,&oci) >= 0)
		ip = &oci.info1;
	if (ip) {
		close(fd);
		
		/* good, this can be a new driver */
		if (*ip & CONS_ISPC)
			return (*ip & CONS_HASOX386) ? PCCONSX : PCCONSR;
		else if (*ip & CONS_ISCO)
			return (*ip & CONS_CODRV2) ? CODRV01X : CODRV011;
		else
			/* who dared to use these fields */
			return UNKNOWN;
	}

	/* check whether console X MODE exists */
	if (ioctl(fd,CONSOLE_X_MODE_OFF,0) < 0) {
		close(fd);
		return PCCONS;
	}

	close(fd);
	return PCCONSX;
}

