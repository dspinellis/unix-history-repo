/* ryopblock.c - manage operation blocks */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/rosy/RCS/ryopblock.c,v 7.3 91/02/22 09:42:03 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/rosy/RCS/ryopblock.c,v 7.3 91/02/22 09:42:03 mrose Interim $
 *
 *
 * $Log:	ryopblock.c,v $
 * Revision 7.3  91/02/22  09:42:03  mrose
 * Interim 6.8
 * 
 * Revision 7.2  90/12/23  18:42:48  mrose
 * update
 * 
 * Revision 7.1  90/07/01  21:06:36  mrose
 * pepsy
 * 
 * Revision 7.0  89/11/23  22:22:01  mrose
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
#include "rosy.h"

/*    DATA */

static int  once_only = 0;
static struct opsblk opsque;
static struct opsblk *OPHead = &opsque;

/*    OPERATION BLOCKS */

struct opsblk  *newopblk (sd, id)
int	sd,
	id;
{
    register struct opsblk *opb;

    opb = (struct opsblk   *) calloc (1, sizeof *opb);
    if (opb == NULL)
	return NULL;

    opb -> opb_flags |= OPB_INITIATOR;

    opb -> opb_fd = sd;
    opb -> opb_id = id;

    if (once_only == 0) {
	OPHead -> opb_forw = OPHead -> opb_back = OPHead;
	once_only++;
    }

    insque (opb, OPHead -> opb_back);

    return opb;
}

/*  */

freeopblk (opb)
register struct opsblk *opb;
{
    if (opb == NULL)
	return;

#ifdef PEPSY_DEFINITIONS
    if (opb -> opb_out && opb -> opb_free_mod)
	(void) fre_obj (opb -> opb_out,
			opb -> opb_free_mod -> md_dtab[opb -> opb_free_index],
			opb -> opb_free_mod, 1);
#else
    if (opb -> opb_out && opb -> opb_free)
	(void) (*opb -> opb_free) (opb -> opb_out);
#endif

    if (opb -> opb_pe)
	pe_free (opb -> opb_pe);

    remque (opb);

    free ((char *) opb);
}

/*  */

struct opsblk   *findopblk (sd, id, flags)
register int	sd,
		id,
		flags;
{
    register struct opsblk *opb;

    if (once_only == 0)
	return NULL;

    flags &= OPB_INITIATOR | OPB_RESPONDER;
    for (opb = OPHead -> opb_forw; opb != OPHead; opb = opb -> opb_forw)
	if (opb -> opb_fd == sd
		&& opb -> opb_id == id
		&& (opb -> opb_flags & flags))
	    return opb;

    return NULL;
}

/*  */

struct opsblk   *firstopblk (sd)
register int	sd;
{
    register struct opsblk *opb,
                           *op2;

    if (once_only == 0)
	return NULL;

    op2 = NULLOPB;
    for (opb = OPHead -> opb_forw; opb != OPHead; opb = opb -> opb_forw)
	if (opb -> opb_fd == sd && (opb -> opb_flags & OPB_INITIATOR)) {
	    if (opb -> opb_flags & OPB_EVENT)
		return opb;
	    if (op2 == NULLOPB)
		op2 = opb;
	}

    return op2;
}

/*  */

loseopblk (sd, reason)
register int	sd;
int	reason;
{
    register struct opsblk *opb,
                           *op2;
    struct RoSAPindication  rois;

    if (once_only == 0)
	return;

    for (opb = OPHead -> opb_forw; opb != OPHead; opb = op2) {
	op2 = opb -> opb_forw;

	if (opb -> opb_fd == sd) {
	    if (opb -> opb_errfnx)
		(*opb -> opb_errfnx) (sd, opb -> opb_id, RY_REJECT,
				      (caddr_t) reason, &rois);

	    freeopblk (opb);
	}
    }
}

/*  */

#ifdef	lint

/* VARARGS */

int	rosaplose (roi, reason, what, fmt)
struct RoSAPindication *roi;
int     reason;
char   *what,
       *fmt;
{
    return rosaplose (roi, reason, what, fmt);
}
#endif
