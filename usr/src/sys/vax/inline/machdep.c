/*-
 * Copyright (c) 1984, 1986 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)machdep.c	7.2 (Berkeley) %G%";
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

#ifdef vax
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

	if (bcmp(cp, "calls\t", 6) != 0)
		return (0);
	if ((cp = index(cp + 6, ',')) == 0)
		return (0);
	return (++cp);
}

/*
 * Find out how many arguments the function is being called with.
 * A return value of -1 indicates that the count can't be determined.
 */
int
countargs(cp)
	char *cp;
{

	if ((cp = index(cp, '$')) == 0)
		return (-1);
	if (!isdigit(*++cp))
		return (-1);
	return (atoi(cp));
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
			(void) strcat(instbuf, ",");
			(void) strcat(instbuf, *argv++);
		}
		(void) strcat(instbuf, "\n");
		fprintf(stderr, "rewrite?-> %s", instbuf);
		return;
	}
}

/*
 * Do any necessary post expansion cleanup.
 */
/*ARGSUSED*/
cleanup(numargs)
	int numargs;
{

	return;
}
#endif vax

#ifdef mc68000
/*
 * Instruction stop table.
 * All instructions that implicitly modify any of the temporary
 * registers, change control flow, or implicitly loop must be
 * listed in this table. It is used to find the end of a basic
 * block when scanning backwards through the instruction stream
 * trying to merge the inline expansion.
 */
struct inststoptbl inststoptable[] = {
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

	if (bcmp(cp, "jbsr\t", 5) == 0)
		return (cp + 5);
	return (0);
}

/*
 * Find out how many arguments the function is being called with.
 * A return value of -1 indicates that the count can't be determined.
 */
/* ARGSUSED */
int
countargs(cp)
	char *cp;
{

	/*
	 * TODO
	 * Figure out what the count should be. 
	 * Probably have to read the next instruction here
	 * instead of in cleanup() below.
	 */
	return (-1);
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
	    bcmp(argv[0], "movl", 5) == 0 &&
	    bcmp(argv[1], "sp@+", 5) == 0 &&
	    (lastarg[1] == '0' || lastarg[1] == '1') &&
	    lastarg[2] == '\0') {
		if (lastarg[0] == 'd')
			return (lastarg[1] - '0');
		return (lastarg[1] - '0' + 8);
	}
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
	if (argc == 2 && bcmp(argv[0], "pea", 4) == 0)
		return (1);
	if (bcmp(argv[argc - 1], "sp@-", 5) == 0)
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
	/*
	 * For the MC68000 all we care about are d0, d1, a0, and a1.
	 */
	register char *lastarg = argv[argc - 1];

	if (lastarg[0] == 'd' && isdigit(lastarg[1]) && lastarg[2] == '\0')
		return (lastarg[1] - '0');
	if (lastarg[0] == 'a' && isdigit(lastarg[1]) && lastarg[2] == '\0')
		return (lastarg[1] - '0' + 8);
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
	int regno;
	char regtype;

	if (target < 8) {
		regtype = 'd';
		regno = target;
	} else {
		regtype = 'a';
		regno = target - 8;
	}
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
		if (bcmp(argv[0], "pea", 4) == 0) {
			if (regtype == 'a') {
				sprintf(instbuf, "\tlea\t%s,%c%d\n",
					argv[1], regtype, regno);
				return;
			}
			if (argv[1][0] == '_' || isdigit(argv[1][0])) {
				sprintf(instbuf, "\tmovl\t#%s,%c%d\n",
					argv[1], regtype, regno);
				return;
			}
			sprintf(instbuf,
				"\texg\ta0,d%d\n\tlea\t%s,a0\n\texg\ta0,d%d\n",
				regno, argv[1], regno);
			return;
		}
		sprintf(instbuf, "\t%s\t%c%d\n", argv[0], regtype, regno);
		return;
	case 3:
		sprintf(instbuf, "\t%s\t%s,%c%d\n",
			argv[0], argv[1], regtype, regno);
		return;
	default:
		sprintf(instbuf, "\t%s\t%s", argv[0], argv[1]);
		argc -= 2, argv += 2;
		while (argc-- > 0) {
			(void) strcat(instbuf, ",");
			(void) strcat(instbuf, *argv++);
		}
		(void) strcat(instbuf, "\n");
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
	extern int lineno;
	
	if (numargs == 0)
		return;
	/*
	 * delete instruction to pop arguments.
	 * TODO:
	 *	CHECK FOR LABEL
	 *	CHECK THAT INSTRUCTION IS A POP
	 */
	fgets(line[bufhead], MAXLINELEN, stdin);
	lineno++;
}
#endif mc68000
