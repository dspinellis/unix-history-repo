/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1983 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)battlestar.c	5.3 (Berkeley) 6/1/90";
#endif /* not lint */

/*
 * Battlestar - a stellar-tropical adventure game
 *
 * Originally written by His Lordship, Admiral David W. Horatio Riggle,
 * on the Cory PDP-11/70, University of California, Berkeley.
 */

#include "externs.h"

main(argc,argv)
int  argc;
char **argv;
{
	char mainbuf[LINELENGTH];
	char *next;

	initialize(argc < 2 || strcmp(argv[1], "-r"));
start:
	news();
	beenthere[position]++;
	if (notes[LAUNCHED])
		crash();		/* decrements fuel & crash */
	if (matchlight) {
		puts("Your match splutters out.");
		matchlight = 0;
	}
	if (!notes[CANTSEE] || testbit(inven,LAMPON) ||
	    testbit(location[position].objects, LAMPON)) {
		writedes();
		printobjs();
	} else
		puts("It's too dark to see anything in here!");
	whichway(location[position]);
run:
	next = getcom(mainbuf, sizeof mainbuf, ">-: ",
		"Please type in something.");
	for (wordcount = 0; next && wordcount < 20; wordcount++)
		next = getword(next, words[wordcount], -1);
	parse();
	switch (cypher()) {
		case -1:
			goto run;
		case 0:
			goto start;
		default:
			exit();
	}
}
