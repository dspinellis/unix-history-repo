/*-
 * Copyright (c) 1984 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)machdep.c	1.4 (Berkeley) %G%";
#endif /* not lint */

#include <stdio.h>
#include <ctype.h>
#include "inline.h"

extern char *strcpy();
extern char *strcat();
extern char *index();

/*
 * The routines and tables in this file must be rewritten
 * for each new machine that this program is ported to.
 */

/*
 * Instruction stop table.
 * All instructions that implicitly modify any of the temporary
 * registers, change control flow, or implicitly loop must be
 * listed in this table. It is used to find the end of a basic
 * block when scanning backwards through the instruction stream
 * trying to merge the inline expansion.
 */
struct inststoptbl inststoptable[] = {
/* control */
	{ "bbssi" }, { "bcc" }, { "bcs" }, { "beql" }, { "beqlu" },
	{ "bgeq" }, { "bgequ" }, { "bgtr" }, { "bgtru" }, { "bleq" },
	{ "blequ" }, { "blss" }, { "blssu" }, { "bneq" }, { "bnequ" },
	{ "brb" }, { "brw" }, { "bvc" }, { "bvs" }, { "jmp" },
/* jump versions of control */
	{ "jbc" }, { "jbs" }, { "jeql" }, { "jeqlu" },
	{ "jgeq" }, { "jgequ" }, { "jgtr" }, { "jgtru" }, { "jleq" },
	{ "jlequ" }, { "jlss" }, { "jlssu" }, { "jneq" }, { "jnequ" },
	{ "jcc" }, { "jcs" }, { "jvc" }, { "jvs" }, { "jbr" },
/* multiple registers */
	{ "loadr" },
/* bit field */
	{ "bbc" }, { "bbs" },
/* character string and block move */
	{ "cmps2" }, { "cmps3" }, { "movblk" }, { "movs2" }, { "movs3" },
/* procedure call */
	{ "callf" }, { "calls" }, { "ret" },
/* loop control */
	{ "aobleq" }, { "aoblss" }, { "casel" },
/* privileged and miscellaneous */
	{ "bpt" }, { "halt" }, { "kcall" }, { "ldpctx" }, { "rei" },
	{ "svpctx" },
	{ "" }
};

/*
 * Check to see if a line is a candidate for replacement.
 * Return pointer to name to be looked up in pattern table.
 */
char *
doreplaceon(cp)
	char *cp;
{

	if (bcmp(cp, "callf\t", 6))
		return (0);
	if ((cp = index(cp + 6, ',')) == 0)
		return (0);
	return (++cp);
}

/*
 * Find out how many arguments the function is being called with.
 * A return value of -1 indicates that the count can't be determined.
 */
countargs(cp)
	char *cp;
{
	int i;

	if ((cp = index(cp, '$')) == 0)
		return (-1);
	if (!isdigit(*++cp) || (i = atoi(cp)) == -1)
		return (-1);
	return (i/4 - 1);
}

/*
 * Find the next argument to the function being expanded.
 */
nextarg(argc, argv)
	int argc;
	char *argv[];
{
	register char *lastarg = argv[2];

	if (argc == 3 &&
	    bcmp(argv[0], "mov", 3) == 0 &&
	    bcmp(argv[1], "(sp)+", 6) == 0 &&
	    lastarg[0] == 'r' && isdigit(lastarg[1]) && lastarg[2] == '\0')
		return (lastarg[1] - '0');
	return (-1);
}

/*
 * Determine whether the current line pushes an argument.
 */
ispusharg(argc, argv)
	int argc;
	char *argv[];
{

	if (argc < 2)
		return (0);
	if (argc == 2 && bcmp(argv[0], "push", 4) == 0)
		return (1);
	if (bcmp(argv[argc - 1], "-(sp)", 6) == 0)
		return (1);
	return (0);
}

/*
 * Determine which (if any) registers are modified
 * Return register number that is modified, -1 if none are modified.
 */
modifies(argc, argv)
	int argc;
	char *argv[];
{
	register char *lastarg = argv[argc - 1];

	/*
	 * For the tahoe all we care about are r0 to r5
	 */
	if (lastarg[0] == 'r' && isdigit(lastarg[1]) && lastarg[2] == '\0')
		return (lastarg[1] - '0');
	return (-1);
}

/*
 * Rewrite the instruction in (argc, argv) to store its
 * contents into arg instead of onto the stack. The new
 * instruction is placed in the buffer that is provided.
 */
rewrite(instbuf, argc, argv, target)
	char *instbuf;
	int argc;
	char *argv[];
	int target;
{

	switch (argc) {
	case 0:
		instbuf[0] = '\0';
		fprintf(stderr, "blank line to rewrite?\n");
		return;
	case 1:
		sprintf(instbuf, "\t%s\n", argv[0]);
		fprintf(stderr, "rewrite?-> %s", instbuf);
		return;
	case 2:
		if (bcmp(argv[0], "push", 4) == 0) {
			sprintf(instbuf, "\tmov%s\t%s,r%d\n",
				&argv[0][4], argv[1], target);
			return;
		}
		sprintf(instbuf, "\t%s\tr%d\n", argv[0], target);
		return;
	case 3:
		sprintf(instbuf, "\t%s\t%s,r%d\n", argv[0], argv[1], target);
		return;
	case 4:
		sprintf(instbuf, "\t%s\t%s,%s,r%d\n",
			argv[0], argv[1], argv[2], target);
		return;
	case 5:
		sprintf(instbuf, "\t%s\t%s,%s,%s,r%d\n",
			argv[0], argv[1], argv[2], argv[3], target);
		return;
	default:
		sprintf(instbuf, "\t%s\t%s", argv[0], argv[1]);
		argc -= 2, argv += 2;
		while (argc-- > 0) {
			strcat(instbuf, ",");
			strcat(instbuf, *argv++);
		}
		strcat(instbuf, "\n");
		fprintf(stderr, "rewrite?-> %s", instbuf);
		return;
	}
}

/*
 * Do any necessary post expansion cleanup.
 */
cleanup(numargs)
	int numargs;
{

}
