#ifndef lint
static char sccsid[] = "@(#)bind.c	4.2 (Berkeley) 7/7/83";
#endif

/*
 * Courier connection management.
 */
#include <stdio.h>
#include "courier.h"

#if DEBUG
extern int CourierClientDebuggingFlag;
#endif

#define NBINDINGS	10

struct binding {
	char *program;		/* Courier program name or 0 */
	char *machine;		/* machine name */
	int connection;		/* open connection descriptor */
} btab[NBINDINGS];

CourierProgram(program, machine)
	char *program, *machine;
{
	register struct binding *bp, *x;
	extern char *malloc();

	x = 0;
	for (bp = &btab[0]; bp < &btab[NBINDINGS]; bp++)
		if (bp->program != 0) {
			if (strcmp(bp->program, program) == 0 &&
			    strcmp(bp->machine, machine) == 0)
				if (bp->connection >= 0)
					return (bp->connection);
				else
					goto connect;
		} else
			x = bp;
	if (x == 0) {
		fprintf(stderr, "Too many active Courier programs.\n");
		exit(1);
	}
	bp = x;
	bp->program = malloc(strlen(program)+1);
	strcpy(bp->program, program);
	bp->machine = malloc(strlen(machine)+1);
	strcpy(bp->machine, machine);
connect:
	bp->connection = CourierActivate(bp->program, bp->machine);
	if (bp->connection >= 0)
		return (bp->connection);
}

CourierDeactivate(program, machine)
	char *program, *machine;
{
	register struct binding *bp;

	for (bp = &btab[0]; bp < &btab[NBINDINGS]; bp++)
		if (bp->program != 0 &&
		    strcmp(bp->program, program) == 0 &&
		    strcmp(bp->machine, machine) == 0) {
			free(bp->program);
			free(bp->machine);
			close(bp->connection);
			bp->program = 0;
			bp->machine = 0;
			bp->connection = -1;
			return;
		}
}
