/*
 * Copyright (c) 1988 Mark Nudleman
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by Mark Nudleman and the University of California, Berkeley.  The
 * name of Mark Nudleman or the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)help.c	5.4 (Berkeley) %G%";
#endif /* not lint */

#include  "less.h"

/*
 * Display some help.
 * Just invoke another "less" to display the help file.
 *
 * {{ This makes this function very simple, and makes changing the
 *    help file very easy, but it may present difficulties on
 *    (non-Unix) systems which do not supply the "system()" function. }}
 */

	public void
help()
{
	char cmd[MAXPATHLEN+100];

	(void)sprintf(cmd,
	 "-less -m '-PmHELP -- ?eEND -- Press g to see it again:Press RETURN for more., or q when done ' %s",
	 HELPFILE);
	lsystem(cmd);
}
