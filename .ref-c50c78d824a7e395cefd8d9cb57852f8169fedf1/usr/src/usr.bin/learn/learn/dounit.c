#ifndef lint
static char sccsid[] = "@(#)dounit.c	4.3	(Berkeley)	%G%";
#endif not lint

#include "stdio.h"
#include "lrnref.h"

int	remind = 2;		/* to remind user of "again" and "bye" */
extern	int	noclobber;

dounit()
{
	char tbuff[100];

	if (todo == 0)
		return;
	wrong = 0;
retry:
	if (!noclobber)
		start(todo);		/* clean up play directory */
	sprintf(tbuff, "%s/%s/L%s", direct, sname, todo); /* script = lesson */
	scrin = fopen(tbuff, "r");
	if (scrin == NULL) {
		perror(tbuff);
		fprintf(stderr, "Dounit:  no lesson %s.\n", tbuff);
		wrapup(1);
	}

	copy(0, scrin);			/* print lesson, usually */
	if (more == 0)
		return;
	copy(1, stdin);			/* user takes over */
	if (skip)
		setdid(todo, sequence++);
	if (again || skip)		/* if "again" or "skip" */
		return;
	if (more == 0)
		return;
	copy(0, scrin);			/* evaluate user's response */

	if (comfile >= 0)
		close(comfile);
	wait(&didok);
	didok = (status == 0);
	if (!didok) {
		wrong++;
		printf("\nSorry, that's %snot right.  Do you want to try again?  ",
			wrong > 1 ? "still " : "");
		fflush(stdout);
		for(;;) {
			gets(tbuff);
			if (tbuff[0] == 'y') {
				printf("Try the problem again.\n");
				if (remind--) {
					printf("[ Whenever you want to re-read the lesson, type \"again\".\n");
					printf("  You can always leave learn by typing \"bye\". ]\n");
				}
				goto retry;
			} else if (strcmp(tbuff, "bye") == 0) {
				wrapup(0);
			} else if (tbuff[0] == 'n') {
				wrong = 0;
				printf("\nOK.  That was lesson %s.\n", todo);
				printf("Skipping to next lesson.\n\n");
				fflush(stdout);
				break;
			} else {
				printf("\nPlease type yes, no or bye:  ");
				clearerr(stdin);
				fflush(stdout);
			}
		}
	}
	setdid(todo, sequence++);
}

