/*
 * Copyright (c) 1983 Regents of the University of California.
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
char copyright[] =
"@(#) Copyright (c) 1983 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)talk.c	5.3 (Berkeley) %G%";
#endif /* not lint */

#include "talk.h"

/*
 * talk:	A visual form of write. Using sockets, a two way 
 *		connection is set up between the two people talking. 
 *		With the aid of curses, the screen is split into two 
 *		windows, and each users text is added to the window,
 *		one character at a time...
 *
 *		Written by Kipp Hickman
 *		
 *		Modified to run under 4.1a by Clem Cole and Peter Moore
 *		Modified to run between hosts by Peter Moore, 8/19/82
 *		Modified to run under 4.1c by Peter Moore 3/17/83
 */

main(argc, argv)
	int argc;
	char *argv[];
{
	get_names(argc, argv);
	init_display();
	open_ctl();
	open_sockt();
	start_msgs();
	if (!check_local())
		invite_remote();
	end_msgs();
	set_edit_chars();
	talk();
}
