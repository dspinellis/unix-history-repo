/*
 * Copyright (c) 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1986 Regents of the University of California.\n\
 All rights reserved.\n";
#endif not lint

#ifndef lint
static char sccsid[] = "@(#)20b.c	5.1 (Berkeley) %G%";
#endif not lint

main()
{
	register char *base, *current;
	register int cc, want;
	int bsize = 20 * 512;
	char *alloca();

	base = alloca(bsize);
	for(cc = bsize; cc > 0;) {
		current = base;
		for(want = bsize; want > 0 && cc > 0; want -= cc) {
			cc = read(0,current,want);
			current += cc;
		}
		if((want = bsize - want) > 0)
		    write(1,base,want);
	}
	return(0);
}
