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
 *
 *	@(#)cat.c	7.2 (Berkeley) %G%
 */

main()
{
	register int c, fd;

	fd = getfile("File: ", 0);
	while ((c = getc(fd)) > 0)
		putchar(c);
	exit(0);
}
