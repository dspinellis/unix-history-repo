/* Copyright (c) 1982 Regents of the University of California */

static char sccsid[] = "@(#)rdwr.c 1.1 %G%";

/*
 * These routines are used to access the debuggee process from
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

#	if (isvaxpx)
#		include "pxinfo.h"
#	endif

typedef int INTFUNC();

extern INTFUNC *onsyserr();

LOCAL badaddr;
LOCAL rwerr();

/*
 * Read from the process' instruction area.  For px, this is actually
 * the data area.
 */

iread(buff, addr, nbytes)
char *buff;
ADDRESS addr;
int nbytes;
{
	INTFUNC *f;

	f = onsyserr(EIO, &rwerr);
#	if (isvaxpx)
		badaddr = addr + ENDOFF;
		pio(process, PREAD, DATASEG, buff, addr + ENDOFF, nbytes);
#	else
		badaddr = addr;
		pio(process, PREAD, TEXTSEG, buff, addr, nbytes);
#	endif
	onsyserr(EIO, f);
}

/* 
 * Write to the process' instruction area, usually in order to set
 * or unset a breakpoint.
 */

iwrite(buff, addr, nbytes)
char *buff;
ADDRESS addr;
int nbytes;
{
	INTFUNC *f;

	f = onsyserr(EIO, &rwerr);
#	if (isvaxpx)
		badaddr = addr + ENDOFF;
		pio(process, PWRITE, DATASEG, buff, addr + ENDOFF, nbytes);
#	else
		badaddr = addr;
		pio(process, PWRITE, TEXTSEG, buff, addr, nbytes);
#	endif
	onsyserr(EIO, f);
}

/*
 * Read for the process' data area.
 */

dread(buff, addr, nbytes)
char *buff;
ADDRESS addr;
int nbytes;
{
	INTFUNC *f;

	f = onsyserr(EIO, &rwerr);
	badaddr = addr;
	pio(process, PREAD, DATASEG, buff, addr, nbytes);
	onsyserr(EIO, f);
}

/*
 * Write to the process' data area.
 */

dwrite(buff, addr, nbytes)
char *buff;
ADDRESS addr;
int nbytes;
{
	INTFUNC *f;

	f = onsyserr(EIO, &rwerr);
	badaddr = addr;
	pio(process, PWRITE, DATASEG, buff, addr, nbytes);
	onsyserr(EIO, f);
}

/*
 * Error handler.
 */

LOCAL rwerr()
{
	error("bad read/write process address 0x%x", badaddr);
}
