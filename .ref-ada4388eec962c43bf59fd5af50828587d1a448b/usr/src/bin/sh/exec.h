/*-
 * Copyright (c) 1991, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Kenneth Almquist.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)exec.h	8.3 (Berkeley) %G%
 */

/* values of cmdtype */
#define CMDUNKNOWN -1		/* no entry in table for command */
#define CMDNORMAL 0		/* command is an executable program */
#define CMDBUILTIN 1		/* command is a shell builtin */
#define CMDFUNCTION 2		/* command is a shell function */


struct cmdentry {
	int cmdtype;
	union param {
		int index;
		union node *func;
	} u;
};


extern char *pathopt;		/* set by padvance */

void shellexec __P((char **, char **, char *, int));
char *padvance __P((char **, char *));
int hashcmd __P((int, char **));
void find_command __P((char *, struct cmdentry *, int, char *));
int find_builtin __P((char *));
void hashcd __P((void));
void changepath __P((char *));
void deletefuncs __P((void));
void getcmdentry __P((char *, struct cmdentry *));
void addcmdentry __P((char *, struct cmdentry *));
void defun __P((char *, union node *));
int unsetfunc __P((char *));
