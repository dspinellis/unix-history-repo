#ifndef lint
static char *rcsid_XError_c = "$Header: XError.c,v 10.4 86/11/19 16:25:00 jg Rel $";
#endif	lint

/*
 *			COPYRIGHT 1985, 1986
 *		   DIGITAL EQUIPMENT CORPORATION
 *		       MAYNARD, MASSACHUSETTS
 *			ALL RIGHTS RESERVED.
 *
 * THE INFORMATION IN THIS SOFTWARE IS SUBJECT TO CHANGE WITHOUT NOTICE AND
 * SHOULD NOT BE CONSTRUED AS A COMMITMENT BY DIGITAL EQUIPMENT CORPORATION.
 * DIGITAL MAKES NO REPRESENTATIONS ABOUT THE SUITIBILITY OF THIS SOFTWARE FOR
 * ANY PURPOSE.  IT IS SUPPLIED "AS IS" WITHOUT EXPRESS OR IMPLIED WARRANTY.
 *
 * IF THE SOFTWARE IS MODIFIED IN A MANNER CREATING DERIVATIVE COPYRIGHT RIGHTS,
 * APPROPRIATE LEGENDS MAY BE PLACED ON THE DERIVATIVE WORK IN ADDITION TO THAT
 * SET FORTH ABOVE.
 *
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting documentation,
 * and that the name of Digital Equipment Corporation not be used in advertising
 * or publicity pertaining to distribution of the software without specific, 
 * written prior permission.
 *
 */


/*
 * MODIFICATION HISTORY
 *
 * 000 -- M. Gancarz, DEC Ultrix Engineering Group
 */

#ifndef lint
static char *sccsid = "@(#)XError.c	3.8	1/24/86";
#endif

#include "uwm.h"

XError(dpy, rep)
Display *dpy;
XErrorEvent *rep;
{
#ifdef DEBUG
    fprintf(stderr, "uwm: X error occurred during a uwm operation.\n");
    fprintf(stderr, "     Description: '%s'\n", XErrDescrip(rep->error_code));
    fprintf(stderr, "     Request code: %d\n", rep->request_code);
    fprintf(stderr, "     Request function: %d\n", rep->func);
    fprintf(stderr, "     Request window 0x%x\n", rep->window);
    fprintf(stderr, "     Error Serial #%d\n", rep->serial);
    fprintf(stderr, "     Current serial #%d\n", dpy->request);
#endif
}
