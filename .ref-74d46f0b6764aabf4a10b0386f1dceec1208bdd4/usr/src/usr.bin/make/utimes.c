/*-
 * utimes.c --
 *
 *	This routine maps the 4.3BSD utimes system call onto the
 *	System V utime call.
 *
 * Copyright (c) 1988 by the Regents of the University of California
 * Copyright (c) 1988 by Adam de Boor
 *
 * Permission to use, copy, modify, and distribute this
 * software and its documentation for any purpose and without
 * fee is hereby granted, provided that the above copyright
 * notice appears in all copies.  Neither the University of California nor
 * Adam de Boor makes any representations about the suitability of this
 * software for any purpose.  It is provided "as is" without
 * express or implied warranty.
 */

static char *rcsid = "$Id: utimes.c,v 1.3 88/11/17 20:41:13 adam Exp $ SPRITE (Berkeley)"

#include <sys/time.h>
#include <time.h>
#include <sys/types.h>
#include <unistd.h>

utimes(file, tvp)
    char *file;
    struct timeval tvp[2];
{
    struct utimbuf t;

    t.actime  = tvp[0].tv_sec;
    t.modtime = tvp[1].tv_sec;
    return(utime(file, &t));
}
