/* Copyright (C) 1991, 1992 Aladdin Enterprises.  All rights reserved.
   Distributed by Free Software Foundation, Inc.

This file is part of Ghostscript.

Ghostscript is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility
to anyone for the consequences of using it or for whether it serves any
particular purpose or works at all, unless he says so in writing.  Refer
to the Ghostscript General Public License for full details.

Everyone is granted permission to copy, modify and redistribute
Ghostscript, but only under the conditions described in the Ghostscript
General Public License.  A copy of this license is supposed to have been
given to you along with Ghostscript so you can know your rights and
responsibilities.  It should be in a file named COPYING.  Among other
things, the copyright notice and this notice must be preserved on all
copies.  */

/* gp_iwatc.c */
/* Intel processor, Watcom C-specific routines for Ghostscript */
#include "dos_.h"
#include <fcntl.h>
#include <signal.h>
#include <stdlib.h>
#include "stat_.h"
#include "string_.h"
#include "gx.h"
#include "gp.h"

/* Define the size of the C stack. */
unsigned _stklen = 8000;		/* default is 4096, we need more */

/* Define a substitute for stdprn (see below). */
private FILE *gs_stdprn;

/* Forward declarations */
private void handle_FPE(P1(int));

/* Do platform-dependent initialization. */
extern void gp_init_console(P0());
void
gp_init()
{	_fmode = O_BINARY;		/* Open files in 'binary' mode */
	gs_stdprn = 0;
	/* Set up the handler for numeric exceptions. */
	signal(SIGFPE, handle_FPE);
	gp_init_console();
}

/* Trap numeric exceptions.  Someday we will do something */
/* more appropriate with these. */
private void
handle_FPE(int sig)
{	eprintf("Numeric exception:\n");
	exit(1);
}

/* Do platform-dependent cleanup. */
void
gp_exit()
{
}

/* ------ Printer accessing ------ */

/* Open a connection to a printer.  A null file name means use the */
/* standard printer connected to the machine, if any. */
/* Return NULL if the connection could not be opened. */
extern void gp_set_printer_binary(P1(int));
FILE *
gp_open_printer(char *fname)
{	if ( strlen(fname) == 0 || !strcmp(fname, "PRN") )
	   {	if ( gs_stdprn == 0 )
		   {	/* We have to effectively reopen the printer, */
			/* because the Watcom library does \n -> \r\n */
			/* substitution on the stdprn stream. */
			int fno = dup(fileno(stdprn));
			setmode(fno, O_BINARY);
			gs_stdprn = fdopen(fno, "wb");
			gp_set_printer_binary(fileno(gs_stdprn));
		   }
		return gs_stdprn;
	   }
	else
		return fopen(fname, "wb");
}

/* Close the connection to the printer. */
void
gp_close_printer(FILE *pfile, const char *fname)
{	fclose(pfile);
	if ( pfile == gs_stdprn )
		gs_stdprn = 0;
}

/* ------ File names ------ */

/* Create and open a scratch file with a given name prefix. */
/* Write the actual file name at fname. */
FILE *
gp_open_scratch_file(const char *prefix, char *fname, const char *mode)
{	/* Unfortunately, Watcom C doesn't provide mktemp, */
	/* so we have to simulate it ourselves. */
	struct stat fst;
	char *end;
	strcpy(fname, prefix);
	strcat(fname, "AA.AAA");
	end = fname + strlen(fname) - 1;
	while ( stat(fname, &fst) == 0 )
	   {	char *inc = end;
		while ( *inc == 'Z' || *inc == '.' )
		   {	if ( *inc == 'Z' ) *inc = 'A';
			inc--;
			if ( end - inc == 6 ) return 0;
		   }
		++*inc;
	   }
	return fopen(fname, mode);
}

/* ------ File operations ------ */

/* If the file given by fname exists, fill in its status and return 1; */
/* otherwise return 0. */
int
gp_file_status(const char *fname, file_status *pstatus)
{	struct stat fst;
	if ( stat(fname, &fst) < 0 ) return 0;
	pstatus->size_pages = (fst.st_size + 1023) >> 10;
	pstatus->size_bytes = fst.st_size;
	/****** CONVERSION PROBABLY REQUIRED HERE ******/
	pstatus->time_referenced = fst.st_atime;
	pstatus->time_created = fst.st_mtime;
	return 1;
}
