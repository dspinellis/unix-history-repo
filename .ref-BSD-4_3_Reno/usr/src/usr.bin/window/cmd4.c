/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Edward Wang at The University of California, Berkeley.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that: (1) source distributions retain this entire copyright notice and
 * comment, and (2) distributions including binaries display the following
 * acknowledgement:  ``This product includes software developed by the
 * University of California, Berkeley and its contributors'' in the
 * documentation or other materials provided with the distribution and in
 * all advertising materials mentioning features or use of this software.
 * Neither the name of the University nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)cmd4.c	3.18 (Berkeley) 6/6/90";
#endif /* not lint */

#include "defs.h"

c_colon()
{
	char oldterse = terse;
	char buf[512];

	setterse(0);
	wwputc(':', cmdwin);
	wwgets(buf, wwncol - 3, cmdwin);
	wwputc('\n', cmdwin);
	wwcurtowin(cmdwin);
	setterse(oldterse);
	if (dolongcmd(buf, (struct value *)0, 0) < 0)
		error("Out of memory.");
}
