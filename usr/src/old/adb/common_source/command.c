#ifndef lint
static char sccsid[] = "@(#)command.c	5.1 (Berkeley) %G%";
#endif

/*
 * adb - commands
 */

#include "defs.h"
#include <ctype.h>
#include <sys/wait.h>

extern char BADEQ[];		/* "unexpected `='" */
extern char NOMATCH[];		/* "cannot locate value" */
extern char BADVAR[];		/* "bad variable" */
extern char BADCOM[];		/* "bad command" */
extern char NOFORK[];		/* "try again" */

/*
 * executing is used in main() to see if it is necessary to
 * delete any breakpoints that might have been set; if an
 * error occurs while a subprocess command is running, executing
 * will be set.
 */
int	executing;

/* lastcom remembers the previous command */
static struct {
	int	c;		/* the command */
	int	star;		/* true iff it was in alternate space */
} lastcom;

/*
 * Execute the given command buffer.
 * If defcom is nonzero, it is used as the default command.
 */
command(buf, defcom)
	char *buf;
	int defcom;
{

	lp = buf;
	do {
		cmds(defcom);
		flushbuf();
	} while (rdc() == ';');
	unreadc();
}

static
cmds(defcom)
	int defcom;
{
	int c;
	struct reglist *reg;

	/*
	 * Pick up the optional first expression (`dot'),
	 * then, if the next character is a comma, pick up
	 * the second optional expression (`ecount').
	 */
	if (gavedot = oexpr())
		ditto = dot = edot = expv;
	else
		edot = dot;	/* probably equal, but possibly truncating */
	if (rdc() == ',') {
		if (!oexpr())
			error("count expected");
		gavecount = 1;
		ecount = expv;
	} else {
		gavecount = 0;
		ecount = 1;
		unreadc();
	}

	/*
	 * Pick up the command.  If there is no command, do the
	 * previous (or default) command, and if no dot was given,
	 * use the `next' dot.
	 */
	c = rdc();
	if (eol(c)) {
		if (defcom != 0) {
			lastcom.c = defcom;
			lastcom.star = 0;
		}
		if (!gavedot)
			dot = inkdot(dotinc);
		unreadc();
	} else {
		lastcom.c = c;
		lastcom.star = 0;
	}

	switch (lastcom.c) {

	case '=':
		fmtcom(SP_NONE, 1);
		break;

	case '/':
		fmtcom(SP_DATA, 0);
		break;

	case '?':
		fmtcom(SP_INSTR, 0);
		break;

	case '>':
		lastcom.c = 0;
		if ((reg = reglookup()) != NULL) {
			if (setreg(reg, edot))
				prints("register write failed");
			break;
		}
		if ((c = varlookup(rdc())) != -1)
			var[c] = edot;
		else
			error(BADVAR);
		break;

	case '!':
		lastcom.c = 0;
		shell();
		break;

	case '$':
		lastcom.c = 0;
		printtrace(nextchar());
		break;

	case ':':
		if (!executing) {
			executing = 1;
			subpcs(nextchar());
			executing = 0;
			lastcom.c = 0;
		}
		break;

	case 0:
		prints("adb\n");
		break;

	default:
		error(BADCOM);
		/* NOTREACHED */
	}
}

/*
 * Perform a format-based command (one in ? / or =).
 */
static
fmtcom(space, eqcom)
	int space, eqcom;
{
	/* special commands m, lL, wW do not operate in SP_NONE (`=') */
	void mcom(), lcom(), wcom();
	static struct fcmd {
		int	c;
		void	(*fn)();
	} fcmd[] = {
		{ 'm', mcom },
		{ 'l', lcom }, { 'L', lcom },
		{ 'w', wcom }, { 'W', wcom },
		0
	};
	register struct fcmd *f;
	register int c;
	int ptype = space;
	static char stformat[LINELEN] = "X\"= \"^i";
	static char eqformat[LINELEN] = "z";

	/*
	 * Are we operating in the alternate `star' space?
	 */
	if (!eqcom) {
		if (rdc() == '*')
			lastcom.star = 1;
		else
			unreadc();
		if (lastcom.star) {
			space |= SP_STAR;
			/* print as data for instr, and vice versa */
			ptype = (SP_DATA + SP_INSTR) - ptype;
		}
	}

	/*
	 * Check for the special commands first.
	 */
	c = rdc();
	for (f = fcmd; f->c; f++) {
		if (c == f->c) {
			if (eqcom)
				error(BADEQ);
			(*f->fn)(space, ptype, isupper(c));
			return;
		}
	}
	unreadc();
	getformat(eqcom ? eqformat : stformat, LINELEN);
	scanform(!eqcom, eqcom ? eqformat : stformat, space, ptype);
}

/*
 * Set a map (?m, /m commands).
 */
/* ARGSUSED */
static void
mcom(space, ptype, fullword)
	int space, ptype, fullword;
{
	register struct map *smap;
	register struct m1 *mm;
	char c;

	smap = space & SP_DATA ? &datmap : &txtmap;
	mm = space & SP_STAR ? &smap->m2 : &smap->m1;
	if (oexpr()) {
		mm->b = expv;
		if (oexpr()) {
			mm->e = expv;
			if (oexpr())
				mm->f = expv;
		}
	}
	if ((c = rdc()) == '?')
		smap->ufd = symfile.fd;
	else if (c == '/')
		smap->ufd = corefile.fd;
	else
		unreadc();
}

/*
 * Locate a value (l, L commands).
 */
static void
lcom(space, ptype, fullword)
	int space, ptype, fullword;
{
	register expr_t val, mask;
	addr_t savdot;

	/* search for exp */
	savdot = dot;
	val = rexpr();
	if (oexpr())
		mask = expv;
	else
		mask = ~0L;
	if (fullword) {
		expr_t w;

		dotinc = sizeof(w);
		for (;;) {
			(void) adbread(space, dot, &w, sizeof(w));
			if (iserr() || (w & mask) == val)
				break;
			dot = inkdot(sizeof(w));
		}
	} else {
		hword_t hw;

		dotinc = sizeof(hw);
		mask = (hword_t)mask;
		val = (hword_t)val;
		for (;;) {
			(void) adbread(space, dot, &hw, sizeof(hw));
			if (iserr() || (hw & mask) == val)
				break;
			dot = inkdot(sizeof(hw));
		}
	}
	if (iserr()) {
		dot = savdot;
		errflag = NOMATCH;
	}
	psymoff("%R", dot, ptype, maxoff, "");
}

/*
 * Write new values (w, W).
 */
static void
wcom(space, ptype, fullword)
	int space, ptype, fullword;
{
	addr_t savdot;
	hword_t hw;

	(void) rexpr();
	do {
		savdot = dot;
		pdot();
		showdot(fullword, space, ptype);	/* also advances */
		errflag = NULL;
		dot = savdot;
		if (fullword)
			(void) adbwrite(space, dot, &expv, sizeof(expv));
		else {
			hw = expv;
			(void) adbwrite(space, dot, &hw, sizeof(hw));
		}
		savdot = dot;
		adbprintf("=%8t");
		showdot(fullword, space, ptype);
		printc('\n');
	} while (oexpr() && !iserr());
	dot = savdot;
	checkerr();
}

/*
 * Do a shell escape.
 *
 * THE vfork CODE BELOW IS CURRENTLY BROKEN
 * MUST CHANGE signal TO sigvec BELOW
 */
static
shell()
{
	int rc, unixpid;
	union wait status;
	char *argp = lp;
	char *getenv(), *eshell = getenv("SHELL");

	if (eshell == 0)
		eshell = "/bin/sh";
	while (readchar() != '\n')
		/* void */;
#ifndef VFORK
#define vfork fork
#endif
	if ((unixpid = vfork()) == 0) {
		*lp = 0;
		(void) signal(SIGINT, sigint);
		(void) signal(SIGQUIT, sigquit);
		execl(eshell, "sh", "-c", argp, (char *)NULL);
		_exit(16);
		/* NOTREACHED */
	}
#ifdef VFORK
	*lp = '\n';
#endif
	if (unixpid == -1)
		error(NOFORK);
	(void) signal(SIGINT, SIG_IGN);
	while ((rc = wait(&status)) != unixpid && rc != -1)
		/* void */;
	(void) signal(SIGINT, intcatch);
	prints("!");
	unreadc();
}

/*
 * Read a format into the given buffer.  If nothing is
 * read, leave the buffer alone.
 */
static
getformat(buf, n)
	char *buf;
	register int n;
{
	register char *p = buf;
	register int c, quote = 0;

	while ((c = readchar()), quote ? c != '\n' : !eol(c)) {
		if (c == '"')
			quote = !quote;
		if (--n > 0)
			*p++ = c;
	}
	unreadc();
	if (p != buf)		/* nonempty */
		*p++ = 0;
}

/*
 * Convert a (one-character) variable name to an index, or -1 for
 * error.
 */
varlookup(name)
	register int name;
{

	if (isdigit(name))
		return (name - '0');
	if (isalpha(name))
		return (isupper(name) ? name - 'a' + 10 : name - 'A' + 10);
	return (-1);
}

/* 
 * If the text at the current input point matches a register name,
 * consume that text and return a pointer to the register; otherwise
 * leave it unconsumed and return NULL.
 */
struct reglist *
reglookup()
{
	register struct reglist *p;
	register char *a, *b, c0, c1;
	char *oldlp = lp;
	extern struct reglist reglist[];

	c0 = rdc();
	c1 = readchar();
	for (p = reglist; (a = p->r_name) != NULL; p++) {
		if (*a++ != c0 || *a++ != c1)
			continue;
		b = lp;
		do {
			if (*a == 0) {	/* name matched: stop short */
				lp = b;
				return (p);
			}
		} while (*a++ == *b++);
	}
	lp = oldlp;
	return (NULL);
}
