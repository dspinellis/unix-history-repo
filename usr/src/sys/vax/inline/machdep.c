/* Copyright (c) 1984 Regents of the University of California */

#ifndef lint
static char sccsid[] = "@(#)machdep.c	1.3	(Berkeley)	%G%";
#endif not lint

#include <stdio.h>
#include <ctype.h>
#include "inline.h"

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
	{ "jbc" }, { "jlbc" }, { "jbs" }, { "jlbs" }, { "jbcc" },
	{ "jbsc" }, { "jbcs" }, { "jbss" }, { "jbr" }, { "jcc" },
	{ "jcs" }, { "jvc" }, { "jvs" }, { "jlss" }, { "jlssu" },
	{ "jleq" }, { "jlequ" }, { "jeql" }, { "jeqlu" }, { "jneq" },
	{ "jnequ" }, { "jgeq" }, { "jgequ" }, { "jgtr" }, { "jgtru" },
	{ "chmk" }, { "chme" }, { "chms" }, { "chmu" }, { "rei" },
	{ "ldpctx" }, { "svpctx" }, { "xfc" }, { "bpt" },
	{ "bugw" }, { "bugl" }, { "halt" }, { "pushr" }, { "popr" },
	{ "polyf" }, { "polyd" }, { "polyg" }, { "polyh" },
	{ "bneq" }, { "bnequ" }, { "beql" }, { "beqlu" }, { "bgtr" },
	{ "bleq" }, { "bgeq" }, { "blss" }, { "bgtru" }, { "blequ" },
	{ "bvc" }, { "bvs" }, { "bgequ" }, { "bcc" }, { "blssu" },
	{ "bcs" }, { "brb" }, { "brw" }, { "jmp" },
	{ "bbs" }, { "bbc" }, { "bbss" }, { "bbcs" }, { "bbsc" },
	{ "bbcc" }, { "bbssi" }, { "bbcci" }, { "blbs" }, { "blbc" },
	{ "acbb" }, { "acbw" }, { "acbl" }, { "acbf" }, { "acbd" },
	{ "acbg" }, { "acbh" }, { "aoblss" }, { "aobleq" },
	{ "sobgeq" }, { "sobgtr" }, { "caseb" }, { "casew" }, { "casel" },
	{ "bsbb" }, { "bsbw" }, { "jsb" }, { "rsb" },
	{ "callg" }, { "calls" }, { "ret" },
	{ "movc3" }, { "movc5" }, { "movtc" }, { "movtuc" },
	{ "cmpc3" }, { "cmpc5" }, { "scanc" }, { "spanc" },
	{ "locc" }, { "skpc" }, { "matchc" }, { "crc" },
	{ "movp" }, { "cmpp3" }, { "cmpp4" }, { "addp4" }, { "addp6" },
	{ "subp4" }, { "subp6" }, { "mulp" }, { "divp" }, { "cvtlp" },
	{ "cvtpl" }, { "cvtpt" }, { "cvttp" }, { "cvtps" }, { "cvtsp" },
	{ "ashp" }, { "editpc" },
	{ "escd" }, { "esce" }, { "escf" },
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

	if (bcmp(cp, "calls\t$", 7) == 0)
		return (cp + 7);
	return (0);
}

/*
 * Find the next argument to the function being expanded.
 * MACHINE DEPENDENT
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
 * MACHINE DEPENDENT
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
 * MACHINE DEPENDENT
 */
modifies(argc, argv)
	int argc;
	char *argv[];
{
	/*
	 * For the VAX all we care about are r0 to r5
	 */
	register char *lastarg = argv[argc - 1];

	if (lastarg[0] == 'r' && isdigit(lastarg[1]) && lastarg[2] == '\0')
		return (lastarg[1] - '0');
	return (-1);
}

/*
 * Rewrite the instruction in (argc, argv) to store its
 * contents into arg instead of onto the stack. The new
 * instruction is placed in the buffer that is provided.
 * MACHINE DEPENDENT
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
		fprintf("blank line to rewrite?\n");
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
