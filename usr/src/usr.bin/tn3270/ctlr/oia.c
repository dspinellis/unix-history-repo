/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 */

#ifndef lint
static char sccsid[] = "@(#)oia.c	3.2 (Berkeley) %G%";
#endif /* not lint */

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
