/*
 * Copyright (c) 1988, 1992 The University of Utah and the Center
 *	for Software Science (CSS).
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Center for Software Science of the University of Utah Computer
 * Science Department.  CSS requests users of this software to return
 * to css-dist@cs.utah.edu any improvements that they make and grant
 * CSS redistribution rights.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)conf.c	8.1 (Berkeley) %G%
 *
 * Utah $Hdr: conf.c 3.1 92/07/06$
 * Author: Jeff Forys, University of Utah CSS
 */

#ifndef lint
static char sccsid[] = "@(#)conf.c	8.1 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/time.h>

#include <stdio.h>
#include "defs.h"
#include "pathnames.h"

/*
**  Define (and possibly initialize) global variables here.
**
**  Caveat:
**	The maximum number of bootable files (`char *BootFiles[]') is
**	limited to C_MAXFILE (i.e. the maximum number of files that
**	can be spec'd in the configuration file).  This was done to
**	simplify the boot file search code.
*/

char	*ProgName;				/* path-stripped argv[0] */
char	MyHost[MAXHOSTNAMELEN+1];		/* host name */
int	MyPid;					/* process id */
int	DebugFlg = 0;				/* set true if debugging */
int	BootAny = 0;				/* set true if we boot anyone */

char	*ConfigFile = NULL;			/* configuration file */
char	*DfltConfig = _PATH_RBOOTDCONF;		/* default configuration file */
char	*PidFile = _PATH_RBOOTDPID;		/* file w/pid of server */
char	*BootDir = _PATH_RBOOTDLIB;		/* directory w/boot files */
char	*DbgFile = _PATH_RBOOTDDBG;		/* debug output file */

FILE	*DbgFp = NULL;				/* debug file pointer */
char	*IntfName = NULL;			/* intf we are attached to */

u_short	SessionID = 0;				/* generated session ID */

char	*BootFiles[C_MAXFILE];			/* list of boot files */

CLIENT	*Clients = NULL;			/* list of addrs we'll accept */
RMPCONN	*RmpConns = NULL;			/* list of active connections */

char	RmpMcastAddr[RMP_ADDRLEN] = RMP_ADDR;	/* RMP multicast address */
