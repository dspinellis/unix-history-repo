#ifndef lint
static char sccsid[] = "@(#)battlestar.c	1.2 %G%";
#endif

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
