/* Copyright (c) 1984 Regents of the University of California */

#ifndef lint
static char sccsid[] = "@(#)machdep.c	1.4	(Berkeley)	9/20/84";
#endif not lint

#include <stdio.h>
#include <ctype.h>
#include "inline.h"

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

	if (bcmp(cp, "calls\t$", 7) == 0)
		return (cp + 7);
	return (0);
}

/*
 * Find the next argument to the function being expanded.
 * If register ends with a '#' then source may be used directly.
 * If register ends with a '@' then source may be used if an indirect
 *    version exists.
 */

nextarg(argc, argv, flag)
	int argc;
	char *argv[];
	int *flag;
{
	register char *lastarg = argv[2];

	*flag = 0;

	if (argc == 3 &&
	    bcmp(argv[0], "mov", 3) == 0 &&
	    bcmp(argv[1], "(sp)+", 6) == 0 &&
	    lastarg[0] == 'r' && isdigit(lastarg[1])) {
		if (lastarg[2] == '\0') {
		    return (lastarg[1] - '0');
		} else if (lastarg[2] == '$') {
		    *flag = F_VALUE;
		    return (lastarg[1] - '0');
		} else if (lastarg[2] == '*') {
		    *flag = F_INDIRECT;
		    return (lastarg[1] - '0');
		}
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

checkvar(argc, argv, flag, source, mod)
int argc;
char *argv[];
int flag;
char *source;
{
    register char *cp1, *cp2;
    register int opind = 0;
    char *indirect();

    if (flag == 0) return(0);

    if (bcmp(argv[0], "push", 4) != 0 && bcmp(argv[0], "mov", 3) != 0)
	return(0);

    cp1 = argv[1];
    while (*cp1) if (*cp1++ == 'r' && isdigit(*cp1) && 
		     (mod & (1 << (*cp1++ - '0'))) &&
		     (*cp1 == '\0' || *cp1 == ')' || *cp1 == ']'))
	return(0);

    if ((argv[0][0] == 'p' && argv[0][4] == 'a') || 
	(argv[0][0] == 'm' && argv[0][3] == 'a'))
	opind++;

    if (flag & F_VALUE) {
	if (opind) return(0);
	cp1 = argv[1];
	cp2 = source;
	while (*cp2++ = *cp1++) ;
	return(1);
    }
    
    if (flag & F_INDIRECT) {
	cp2 = source;
	if (opind) {
	    cp1 = argv[1];
	} else {
	    cp1 = indirect(argv[1]);
	    if (cp1 == NULL) return(0);
	}
	while (*cp2++ = *cp1++) ;
	return(1);
    }

    return(0);
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

/* Return indirect version of variable:
 *    $Lnn      -> Lnn
 *    rn        -> (rn)
 *    (rn)      -> *(rn)
 *    a(rn)     -> *a(rn)
 *    (rn)[rm]  -> *(rn)[rm]
 *    a(rn)[rm] -> *a(rn)[rm]
 *    _foo      -> *_foo
 *    _foo(rn)  -> *_foo(rn)
 *    _foo(rn)[rm] -> *_foo(rn)[rm]
 *    (rn)+     -> NULL
 *    -(rn)     -> NULL
 *    *<any>    -> NULL
 */
char *
indirect(cp)
register char *cp;
{
    static char newvar[16];
    register char *c;
    int neg = 0;
    int offset = 0;

    /* *<any> | -(rn) */
    if (*cp == '*' || (*cp == '-' && *(cp+1) == '(')) return(NULL);

    /* (rn)+ | (rn)x */
    if (*cp == '(') {
	c = cp;
	while (*++c != ')') ;
	if (*++c == '+') return(NULL);
	c = newvar;
	*c++ = '*';
	while (*c++ = *cp++);
	return(newvar);
    }

    /* $Lnn */
    if (*cp == '$' && *(cp+1) == 'L') {
	c = newvar;
	cp++;
	while (*c++ = *cp++) ;
	return(newvar);
    }

    /* rn */
    if (*cp == 'r') {
	c = newvar;
	*c++ = '(';
	while (*c++ = *cp++) ;
	*(c-1) = ')';
	*c = 0;
	return(newvar);
    }

    /* everything else */
    c = newvar;
    *c++ = '*';
    while (*c++ = *cp++) ;
    return(newvar);
}
    
output_replace(replace, oparg, argno, f)
register char *replace;
struct oparg oparg[];
int argno;
FILE *f;
{
    char newline[BUFSIZ];
    register int i;
    register int argc;
    char *argv[MAXARGS];
    char parsebuf[BUFSIZ];

    do {
	replace = copyline(replace, newline);
	argc = parseline(newline, argv, parsebuf);
	for (i = 0; i < argno; i++)
	    replace_arg(argc, argv, oparg[i].reg, oparg[i].source);
	buildline(argc, argv, newline);
	fputs(newline, f);
    } while (*replace != '\0');
}

replace_arg(argc, argv, reg, source)
int argc;
char *argv[];
int reg;
char *source;
{
    register int i;
    register char *c;

    for (i = 1; i < argc; i++) {
	c = argv[i];
	if (*c == '(') c++;
	if (*c++ == 'r' && (*c++ - '0') == reg && !isdigit(*c)) {
	    argv[i] = source;
	}
    }
}

buildline(argc, argv, newline)
register int argc;
register char *argv[];
register char *newline;
{
    register char *cp1;

    if (argc == 0) {
	*newline++ = '\n';
	*newline = '\0';
    } else if (argc == 1) {
	sprintf(newline, "%s\n", argv[0]);
    } else {
	sprintf(newline, "\t%s\t%s", argv[0], argv[1]);
	argc -= 2;
	argv += 2;
	while (argc-- > 0) {
	    strcat(newline, ",");
	    cp1 = *argv;
	    cp1 += strlen(cp1) - 1;
	    if (*cp1 == '$' || *cp1 == '*') *cp1 = '\0';
	    strcat(newline, *argv++);
	}
	strcat(newline, "\n");
    }
}



/*
 * Do any necessary post expansion cleanup.
 */
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
		fprintf("blank line to rewrite?\n");
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
	
	if (numargs == 0)
		return;
	/*
	 * delete instruction to pop arguments.
	 * TODO:
	 *	CHECK FOR LABEL
	 *	CHECK THAT INSTRUCTION IS A POP
	 */
	fgets(line[bufhead], MAXLINELEN, stdin);
}
#endif mc68000
