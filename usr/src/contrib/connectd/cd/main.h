/*-
 * Copyright (c) 1993 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Bill Jolitz.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)main.h	5.2 (Berkeley) %G%
 */

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <sys/uio.h>
#include <sys/file.h>
#include <stdio.h>
#include <connect.h>

struct conversation {
	struct connectdomain co_cd;	/* where we are connecting to */
	char	co_methods[100];	/* how we go about making connection */
	char	co_optionsbuf[1024] ;	/* options requestor wants */
	int	co_constatus ;		/* current connection status */
	int	co_errfd ;		/* requestor's stderr if set */
	int	co_sock ;		/* requestor's socket if set */
	int	co_rqst ;		/* requestor's request if set */
	int	co_pid ;		/* connector pid if active */
};
