/* Copyright (c) 1979 Regents of the University of California */
#
/*
 * pi - Pascal interpreter code translator
 *
 * Charles Haley, Bill Joy UCB
 * Version 1.2 January 1979
 *
 *
 * pxp - Pascal execution profiler
 *
 * Bill Joy UCB
 * Version 1.2 January 1979
 */

#include "0.h"
#include "yy.h"

#ifdef PXP
int	yytokcnt;
#endif

/*
 * Getchar returns the next
 * character from the current
 * input line or -1 on end-of-file.
 * It also maintains yycol for use in
 * printing error messages.
 */
getchar()
{
	register i, c;

	if (*bufp == '\n' && bufp >= charbuf) {
#ifdef PXP
		yytokcnt = 0;
#endif
		if (getline() < 0)
			return (-1);
	}
	c = *++bufp;
	if (c == '\t')
		yycol = ((yycol + 8) & ~7);
	else
		yycol++;
	return (c);
}

/*
 * Definitions of the structures used for the
 * include facility.  The variable "ibp" points
 * to the getc buffer of the current input file.
 * There are "inclev + 1" current include files,
 * and information in saved in the incs stack
 * whenever a new level of include nesting occurs.
 *
 * Ibp in the incs structure saves the pointer
 * to the previous levels input buffer;
 * filename saves the previous file name;
 * Printed saves whether the previous file name
 * had been printed before this nesting occurred;
 * and yyline is the line we were on on the previous file.
 */
int	*ibp ibuf;

#define	MAXINC	10

struct inc {
	int	*ibp;
	char	*filename;
	int	Printed;
	int	yyline;
	int	yyLinpt;
} incs[MAXINC];

extern	char printed;

int	inclev	-1;

#ifdef PXP
/*
 * These initializations survive only if
 * pxp is asked to pretty print one file.
 * Otherwise they are destroyed by the initial
 * call to getline.
 */
char	charbuf[CBSIZE]	" program x(output);\n";
int	yycol 8;
char	*bufp charbuf;

#endif
/*
 * YyLinpt is the seek pointer to the beginning of the
 * next line in the file.
 */
int	yyLinpt;

/*
 * Getline places the next line
 * from the input stream in the
 * line buffer, returning -1 at YEOF.
 */
getline()
{
	register char *cp;
	register CHAR c;
#ifdef PXP
	static char ateof;
#endif
	register int *ib;
	int i;

	if (opt('l') && yyprtd == 0)
		yyoutline();
	yyprtd = 0;
top:
	yylinpt = yyLinpt;
	yyline++;
	yyseqid++;
	cp = charbuf;
	ib = ibp;
	i = sizeof charbuf - 1;
	for (;;) {
		c = getc(ib);
		if (c == -1) {
			if (uninclud())
				goto top;
#ifdef PXP
			if (ateof == 0 && bracket) {
				strcpy(charbuf, "begin end.\n");
				ateof = 1;
				goto out;
			}
#endif
			bufp = "\n";
			yyline--;
			yyseqid--;
			yyprtd = 1;
			return (-1);
		}
		*cp++ = c;
		if (c == '\n')
			break;
		if (--i == 0) {
			line = yyline;
			error("Input line too long - QUIT");
			pexit(DIED);
		}
	}
	*cp = 0;
	yyLinpt = yylinpt + cp - charbuf;
	if (includ())
		goto top;
#ifdef PXP
	if (cp == &charbuf[1])
		commnl();
	else if (cp == &charbuf[2])
		switch (charbuf[0]) {
			case ' ':
				commnlbl();
				break;
			case '\f':
				commform();
		}
#endif
	if (opt('u'))
		setuflg();
out:
	bufp = charbuf - 1;
	yycol = 8;
	return (1);
}

/*
 * Check an input line to see if it is a "#include" pseudo-statement.
 * We allow arbitrary blanks in the line and the file name
 * may be delimited by either 's or "s.  A single semicolon
 * may be placed after the name, but nothing else is allowed
 */
includ()
{
	register char *cp, *dp;
	char ch;
	register struct inc *ip;

	cp = charbuf;
	if (*cp++ != '#')
		return (0);
	cp = skipbl(cp);
	for (dp = "include"; *dp; dp++)
		if (*dp != *cp++)
			return (0);
	line = yyline;
	cp = skipbl(cp);
	ch = *cp++;
	if (ch != '\'' && ch != '"') {
		/*
		 * This should be a yerror flagging the place
		 * but its not worth figuring out the column.
		 */
		line = yyline;
		error("Include syntax error - expected ' or \" not found - QUIT");
		pexit(DIED);
	}
	for (dp = cp; *dp != ch; dp++)
		if (*dp == 0) {
			line = yyline;
			error("Missing closing %c for include file name - QUIT", ch);
			pexit(DIED);
		}
	*dp++ = 0;
/*
	if (*dp == ';')
		dp++;
	dp = skipbl(dp);
	if (*dp != '\n') {
		line = yyline;
		error("Garbage after filename in include");
		pexit(DIED);
	}
*/
	if (!dotted(cp, 'i')) {
		line = yyline;
		error("Include filename must end in .i");
	}
#ifdef PXP
	commincl(cp, ch);
	if (noinclude)
		return (1);
#endif
	inclev++;
	if (inclev > MAXINC) {
		line = yyline;
		error("Absurdly deep include nesting - QUIT");
		pexit(DIED);
	}
	ip = &incs[inclev];
	ip->filename = filename;
	filename = savestr(cp);
	cp = alloc(518);
	if (cp == -1) {
		error("Ran out of memory (include)");
		pexit(DIED);
	}
	if (fopen(filename, cp) < 0) {
		perror(filename);
		pexit(DIED);
	}
	if (inpflist(filename)) {
#ifdef PI
		opush('l');
#endif
#ifdef PXP
		opush('z');
#endif
	}
	ip->Printed = printed;
	printed = 0;
	ip->yyline = yyline;
	yyline = 0;
	ip->yyLinpt = yyLinpt;
	yyLinpt = 0;
	ip->ibp = ibp;
	ibp = cp;
	return (1);
}

skipbl(ocp)
	char *ocp;
{
	register char *cp;

	cp = ocp;
	while (*cp == ' ' || *cp == '\t')
		cp++;
	return (cp);
}


/*
 * At the end of an include,
 * close the file, free the input buffer,
 * and restore the environment before
 * the "push", including the value of
 * the z option for pxp and the l option for pi.
 */
uninclud()
{
	register struct inc *ip;

	if (inclev < 0)
		return (0);
	close(ibp[0]);
	free(ibp);
	ip = &incs[inclev];
	ibp = ip->ibp;
	yyline = ip->yyline;
	if (inpflist(filename)) {
#ifdef PI
		opop('l');
#endif
#ifdef PXP
		opop('z');
#endif
	}
	filename = ip->filename;
	yyLinpt = ip->yyLinpt;
	/*
	 * If we printed out the nested name,
	 * then we should print all covered names again.
	 * If we didn't print out the nested name
	 * we print the uncovered name only if it
	 * has not been printed before (unstack).
	 */
	if (printed) {
		printed = 0;
		while (ip >= incs) {
			ip->Printed = 0;
			ip--;
		}
	} else
		printed = ip->Printed;
	inclev--;
	return (1);
}
