/* ps_alloc.c - allocate a presentation stream */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/psap/RCS/ps_alloc.c,v 7.1 91/02/22 09:36:31 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/psap/RCS/ps_alloc.c,v 7.1 91/02/22 09:36:31 mrose Interim $
 *
 *
 * $Log:	ps_alloc.c,v $
 * Revision 7.1  91/02/22  09:36:31  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  22:13:19  mrose
 * Release 6.0
 * 
 */

/*
 *				  NOTICE
 *
 *    Acquisition, use, and distribution of this module and related
 *    materials are subject to the restrictions of a license agreement.
 *    Consult the Preface in the User's Manual for the full terms of
 *    this agreement.
 *
 */


/* LINTLIBRARY */

#include <stdio.h>
#include "psap.h"


/* A Presentatation Stream (or PStream) is the second generation of
   "generic" I/O stream-based handling.  (For the first attempt,
   take a look at the prototype implementation of the TTI Trusted Mail
   Agent.)  The idea is to present a common, simple I/O paradigm (i.e.,
   the UNIX v7 philosophy) to protocol-translation entities regardless of
   the underlying medium (files, pipes, sockets, or strings).

   New streams are created by a call to ps_alloc().  It allocates memory
   and calls an open routine.  This routine fills in the dispatch vectors
   for read/write and (optionally) close.  It can also fill in any other
   part of the stream's structure it likes.

   Once created, I/O is done using the macros ps_read/ps_write.  These
   return either NOTOK or OK; depending on how things went. The read/write
   routines are invoked as:

	int	iofunc (ps, data, n, in_line)
	PS	ps;
	PElementData data;
	PElementLen  n;
	int	in_line;

   They should read/write upto len bytes, starting at data, and return the
   number of bytes processed, or NOTOK on error.  The routine ps_io() will
   make successive calls to fill/flush the data.  If the read/write routine
   returns NOTOK, it should set ps_errno as well.

   Streams are removed by a call to ps_free ().  It calls the close
   routine, if any, which should de-commission any parts of the stream's
   structure that are in use.  ps_free() will then free the allocated
   memory.
 */

/*  */

PS	ps_alloc (io)
register IFP	io;
{
    register PS	    ps;

    if ((ps = (PS) calloc (1, sizeof *ps)) == NULLPS)
	return NULLPS;

    if ((*io) (ps) == NOTOK) {
	ps_free (ps);
	return NULLPS;
    }

    return ps;
}
