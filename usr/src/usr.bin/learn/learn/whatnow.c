#ifndef lint
static char sccsid[] = "@(#)whatnow.c	4.2	(Berkeley)	4/25/83";
#endif not lint

#include "stdio.h"
#include "lrnref.h"

extern	char	togo[];
extern	int	review;

whatnow()
{
	if (again) {
		if (!review)
			printf("\nOK.  That was lesson %s.\n\n", todo);
		fflush(stdout);
		strcpy(level, togo);
		return;
	}
	if (skip) {
		printf("\nOK.  That was lesson %s.\n", todo);
		printf("Skipping to next lesson.\n\n");
		fflush(stdout);
		strcpy(level, todo);
		skip = 0;
		return;
	}
	if (todo == 0) {
		more=0;
		return;
	}
	if (didok) {
		strcpy(level,todo);
		if (speed<=9) speed++;
	}
	else {
		speed -= 4;
		/* the 4 above means that 4 right, one wrong leave
		    you with the same speed. */
		if (speed <0) speed=0;
	}
	if (wrong) {
		speed -= 2;
		if (speed <0 ) speed = 0;
	}
	if (didok && more) {
		printf("\nGood.  That was lesson %s.\n\n",level);
		fflush(stdout);
	}
}
