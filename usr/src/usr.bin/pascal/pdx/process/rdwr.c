/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)rdwr.c	5.3 (Berkeley) %G%";
#endif not lint

/*
 * This routine is used to access the debuggee process from
 * outside the "process" module.
 *
 * They invoke "pio" which eventually leads to a call to "ptrace".
 * The system generates an I/O error when a ptrace fails, we catch
 * that here and assume its due to a misguided address.
 */

#include "defs.h"
#include <errno.h>
#include "process.h"
#include "process.rep"

#	include "pxinfo.h"

typedef int INTFUNC();

extern INTFUNC *onsyserr();

LOCAL badaddr;
LOCAL PIO_OP rwflg;
LOCAL rwerr();

/*
 * Read/Write to the process' data area.
 */

drdwr(rw, buff, addr, nbytes)
PIO_OP rw;
char *buff;
ADDRESS addr;
int nbytes;
{
	INTFUNC *f;

	f = onsyserr(EIO, rwerr);
	badaddr = addr;
	rwflg = rw;
	pio(process, rw, DATASEG, buff, addr, nbytes);
	onsyserr(EIO, f);
}

/*
 * Error handler.
 */

LOCAL rwerr()
{
	error("bad %s process address 0x%x",
		rwflg == PREAD ? "read" : "write", badaddr);
}
