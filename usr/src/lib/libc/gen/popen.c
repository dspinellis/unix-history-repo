/*
 * Copyright (c) 1988 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software written by Ken Arnold and published
 * in UNIX Review, Vol. 6, No. 8.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)popen.c	5.6 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <stdio.h>

#define	MAXFILES	32

typedef struct {
	int pid;
	int status;
} PID_STRUCT;

PID_STRUCT pids[MAXFILES];

FILE *
popen(program, type)
char *program, *type;
{
	char **argv;
	int pdes[2], pid;
	FILE *iop;

	if (*type != 'r' && *type != 'w')
		return NULL;
	if (pipe(pdes) < 0)
		return NULL;
	switch(pid = fork()) {
	case -1:
		iop = NULL;
		break;
	case 0:			/* child */
		if (*type == 'r') {
			close(1);
			dup(pdes[1]);
		}
		else {
			close(0);
			dup(pdes[0]);
		}
		close(pdes[1]);
		close(pdes[0]);
		execl("/bin/sh", "sh", "-c", program, NULL);
		exit(-1);
		/* NOTREACHED */
	default:
		if (*type == 'r') {
			iop = fdopen(pdes[0], "r");
			close(pdes[1]);
		} else {
			iop = fdopen(pdes[0], "w");
			close(pdes[0]);
		}
		break;
	}
	if (iop != NULL)
		pids[fileno(iop)].pid = pid;
	else {
		close(pdes[0]);
		close(pdes[1]);
	}
	return iop;
}

pclose(iop)
FILE *iop;
{
	int status;
	int pid, fdes, i;

	fdes = fileno(iop);
	fclose(iop);
	if (pids[fdes].pid == 0)
		return pids[fdes].status;
	for (;;) {
		pid = wait(&status);
		if (pid < 0)
			return -1;
		if (pid == pids[fdes].pid) {
			pids[fdes].pid = 0;
			return status;
		}
		for (i = 0; i < MAXFILES; i++)
			if (pids[i].pid == pid) {
				pids[i].pid = 0;
				pids[i].status = status;
				break;
			}
	}
}
