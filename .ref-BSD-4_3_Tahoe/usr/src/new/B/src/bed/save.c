/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1984. */
static char rcsid[] = "$Header: save.c,v 2.5 85/08/22 16:07:04 timo Exp $";

/*
 * B editor -- Save Parse tree on file.
 */

#include <ctype.h>

#include "b.h"
#include "feat.h"
#include "bobj.h"
#include "node.h"
#include "gram.h"
#include "queu.h"

#define Indent "    " /* Output for each indentation level */

Hidden int spaces = 0; /* Saved-up spaces; emitted when non-blank found */

#ifdef BTOP
#include <setjmp.h>

Hidden bool piping; /* Set if output goes to B interpreter */
Hidden jmp_buf alas; /* Where to go when no command prompt gotten */
#endif

/*
 * Write the representation of a node.  If it has children,
 * they are written by recursive calls.
 */

Hidden Procedure
savewalk(n, level, file)
	node n;
	int level;
	FILE *file;
{
	string *rp;
	string cp;
	int nch;
	int i;
	char c;

	if (Type(n) == Tex) {
		for (; spaces > 0; --spaces)
			putc(' ', file);
		fputs(Str((value)n), file);
		return;
	}
	nch = nchildren(n);
	rp = noderepr(n);
	for (i = 0; i <= nch; ++i) {
		if (i)
			savewalk(child(n, i), level, file);
		cp = rp[i];
		if (cp) {
			for (; c = *cp; ++cp) {
				switch (c) {

				case '\n':
				case '\r':
					putc('\n', file);
#ifdef BTOP
					if (piping) {
						if (!expect(">>> "))
							longjmp(alas, 1);
					}
#endif BTOP
					if (c == '\n')
						for (i = level; i > 0; --i)
							fputs(Indent, file);
					spaces = 0;
					break;

				case '\b':
					--level;
					break;

				case '\t':
					++level;
					break;

				case ' ':
					++spaces;
					break;

				default:
					for (; spaces > 0; --spaces)
						putc(' ', file);
					putc(c, file);
					break;

				}
			}
		}
	}
}


/*
 * Save the entire Parse tree.
 */

Visible bool
save(p, filename)
	path p;
	string filename;
{
	FILE *file = fopen(filename, "w");

	if (!file)
		return No;
#ifdef BTOP
	piping = No;
#endif BTOP
	sendsave(p, file);
	return fclose(file) != EOF;
}


Hidden Procedure
sendsave(p, file)
	path p;
	FILE *file;
{
	p = pathcopy(p);
	top(&p);
	spaces = 0;
	savewalk(tree(p), 0, file);
	putc('\n', file);
	pathrelease(p);
}

#ifdef BTOP

/*
 * Interface to top level.
 */

Visible bool
send(p, pdown)
	path p;
	FILE *pdown;
{
	piping = Yes;
	if (setjmp(alas)) {
		pathrelease(p);
		return No;
	}
	sendsave(p, pdown);
	if (expect(">>> "))
		putc('\n', pdown);
	return Yes;
}
#endif BTOP

/* ------------------------------------------------------------------ */

#ifdef SAVEBUF

/*
 * Write a node.
 */

Hidden Procedure
writenode(n, fp)
	node n;
	FILE *fp;
{
	int nch;
	int i;

	if (!n) {
		fputs("(0)", fp);
		return;
	}
	if (((value)n)->type == Tex) {
		writetext((value)n, fp);
		return;
	}
	nch = nchildren(n);
	fprintf(fp, "(%s", symname(symbol(n)));
	for (i = 1; i <= nch; ++i) {
		putc(',', fp);
		writenode(child(n, i), fp);
	}
	fputc(')', fp);
}


Hidden Procedure
writetext(v, fp)
	value v;
	FILE *fp;
{
	string str;
	int c;

	Assert(v && Type(v) == Tex);
	putc('\'', fp);
	str = Str(v);
	for (str = Str(v); *str; ++str) {
		c = *str;
		if (c == ' ' || isprint(c)) {
			putc(c, fp);
			if (c == '\'' || c == '`')
				putc(c, fp);
		}
		else if (isascii(c))
			fprintf(fp, "`$%d`", c);
	}
	putc('\'', fp);
}


Visible bool
savequeue(v, filename)
	value v;
	string filename;
{
	register FILE *fp;
	auto queue q = (queue)v;
	register node n;
	register bool ok;
	register int lines = 0;

	fp = fopen(filename, "w");
	if (!fp)
		return No;
	q = qcopy(q);
	while (!emptyqueue(q)) {
		n = queuebehead(&q);
		writenode(n, fp);
		putc('\n', fp);
		++lines;
		noderelease(n);
	}
	ok = fclose(fp) != EOF;
	if (!lines)
		/* Try to */ unlink(filename); /***** UNIX! *****/
	return ok;
}
#endif SAVEBUF
