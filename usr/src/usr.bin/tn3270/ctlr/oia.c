/*
 *	Copyright (c) 1984-1987 by the Regents of the
 *	University of California and by Gregory Glenn Minshall.
 *
 *	Permission to use, copy, modify, and distribute these
 *	programs and their documentation for any purpose and
 *	without fee is hereby granted, provided that this
 *	copyright and permission appear on all copies and
 *	supporting documentation, the name of the Regents of
 *	the University of California not be used in advertising
 *	or publicity pertaining to distribution of the programs
 *	without specific prior permission, and notice be given in
 *	supporting documentation that copying and distribution is
 *	by permission of the Regents of the University of California
 *	and by Gregory Glenn Minshall.  Neither the Regents of the
 *	University of California nor Gregory Glenn Minshall make
 *	representations about the suitability of this software
 *	for any purpose.  It is provided "as is" without
 *	express or implied warranty.
 */

#ifndef lint
static char sccsid[] = "@(#)oia.c	3.1 (Berkeley) %G%";
#endif	/* not lint */

/*
 * Routines to maintain the Operator Information Area.
 */

#include "../general/general.h"

#include "oia.h"
#include "../general/globals.h"


init_oia()
{
    ClearElement(OperatorInformationArea);
}
