#include "sh.h"
/*
 * Shell:
 *	Lexical and argument processing routines
 *
 *	lex   - driver for lexical analysis
 *	word  - reads next word into line and pointer thereto via args
 *	getc  - gets a character from the logical input stream
 *	readc - reads a character from the 'input device'
 *	setargs - sets up the parameter variables initially
 *	rewind - backs up the shell arguments to their original values
 *	setnargs - resets nargs variable after changes to arg list
 *	shift - manipulates the shell parameters
 */
struct	shvar2 *word();

/*
 * lex is the driver routine for the lexical input of the shell.
 * Basic strategy is to read a logical line into linebuf
 * with successive words pointed to by successive elements of args.
 * Termination condition is a newline.
 * Returns a pointer to a linked list of the words.
 */
lex(hp)
	register struct shvar2 *hp;
{
	register struct shvar2 *wdp;

	wdp = hp;
	do {
		wdp->next = calloc(1, sizeof *wdp);
		wdp->next->prev = wdp;
		wdp = wdp->next;
		wdp->value = word();
	} while (wdp->value[0] != '\n');
	hp->prev = wdp;
	wdp->next = hp;
}

freelex(vp)
	register struct shvar *vp;
{
	register struct shvar *fp;

	while (vp->next != vp) {
		fp = vp->next;
		vp->next = fp->next;
		xfree(fp->value);
		cfree(fp);
	}
	vp->prev = vp;
}

/* static */ char peekc, peekx;
/*
 * word breaks the input character stream into words.
 * Blanks and tabs in the input are ignored, the characters
 * 	&;<>()|^ and \n
 * are considered to be separators.
 * Characters may be escaped here by surrounding them with
 * 's or "s.  This causes the QUOTE (high order) bit of the
 * corresponding character to be set so the character will
 * fail subsequent comparisons.  The quoting is eventually
 * stripped off.  More quoting by QUOTE is also done in readc.
 * Note importantly that quoted character strings do not undergo
 * parameter substitution!
 * Return value is a pointer to a structure containing the word.
 */
struct shvar2 *
word()
{
	register c;
	char c1;
	register char *lp;
	char lbuf[514];
	int i;

	lp = lbuf;
	i = 512;
	/*
	 * Loop to get something solid
	 */
	for (;;) {
		c = getc();
		switch (c) {
			case ' ':
			case '\t':
				continue;
			case '\'':
			case '"':
				c1 = c;
				while ((c = echo(readc())) != c1) {
					if (c == '\n') {
						seterr("Unmatched ' or \"");
						*lp++ = 0;
						peekc = c;
						goto ret;
					}
					if (--i == 0)
						goto toochars;
					*lp++ = c | QUOTE;
				}
				break;
			case '&':
			case ';':
			case '<':
			case '>':
			case '(':
			case ')':
			case '|':
			case '^':
			case '\n':
				*lp++ = c;
				*lp++ = '\0';
				goto ret;
			default:
				peekc = c;
				break;
		}
		/*
		 * We have discovered something solid (not a separator).
		 * We want to gather in as many characters
		 * as possible but don't want to grab a separator.
		 * If we find another quotation in this word we go back to
		 * the top to take it.
		 */
		for (;;) {
			c = getc();
			if (any(c, " '\"\t;&<>()|^\n")) {
				peekc = c;
				if (any(c, "\"'"))
					break;
				*lp++ = '\0';
				goto ret;
			}
			if (--i == 0)
				goto toochars;
			*lp++ = c;
		}
	}
toochars:
	seterr("Too many characters");
	lbuf[1] = 0;
ret:
	return (savestr(lbuf));
}

/* static */ char *dolp;
/* static */ struct shvar2 paramhd, *paramp, *dolnxt;
/* static */ int dolc;
/*
 * setargs sets up the initial argument linked list.
 * paramp is a working pointer to the front of the list (actually
 * one before the front), paramhd the actual origin which contains
 * the true value of $0.
 *
 * dolnxt is used in expanding $*.
 * dolc is maintained by setnargs who also maintains the nargs variable
 * dolp is the pointer into the expanding string in getc
 */
setargs(c, v)
	int c;
	char *v[];
{
	register struct shvar2 *vp, *lvp;

	vp = &paramhd;
	for (;;) {
		vp->value = *v++;
		c--;
		if (c == 0)
			break;
		lvp = vp;
		vp = calloc(1, sizeof *vp);
		lvp->next = vp;
		vp->prev = lvp;
	}
	rewind();
}

/*
 * rewind the shell arguments
 */
rewind()
{
	paramp = &paramhd;
	setnargs();
}

/*
 * set up nargs variable after a parameter list change
 */
setnargs()
{
	register struct shvar2 *vp;

	dolc = 0;
	for (vp = paramp; vp != 0; vp = vp->next)
		dolc++;
	set(n_args, putn(dolc - 1));
	if (dolc == 1)
		unsetv(n_args);
}

/*
 * shift the shell arguments
 */
shift(v)
	register char *v[];
{
	register int n;
	register struct shvar2 *vp;

	v++;
	n = *v == 0 ? 1 : getn(*v++);
	for (vp = paramp; vp && n;)
		if (n > 0) {
			n--;
			vp = vp->next;
		} else {
			n++;
			vp = vp->prev;
		}
	if (n || vp == 0) {
		bferr(": Count too large");
		return;
	}
	paramp = vp;
	setnargs();
}

/* static */ 	char dol2bra;
/*
 * getc gets a character from the logical input stream.
 * It handles parameter expansion via $[0-9], all parameters
 * via $*, shell variables via $[A-Za-z], and the process number via $$.
 * Also handled is the trimming of the sufficies from expanded
 * names via the . notation.  For example if $1 is "foo.p" then
 * $.1 will be "foo".
 *
 * The variable dol2bra's value has the following meaning:
 *
 *	2	echo characters to : or }, if : discard chars to }
 *	1	echo characters to :
 *     -1	discard characters to }
 *     -2	discard characters to : or }, if : echo to }
 *
 * This handles the constructs
 *
 *	${name?str1:str2}	name set -> str1 ; t -> str2
 *	${name:default}		name set -> $name ; t -> default
 *	${name?string}		name set -> strings ; t -> ""
 */
getc()
{
	register c;
	static char doldot;

	if (peekc) {
		c = peekc;
		peekc = 0;
		return (c);
	}
	for (;;) {
		if (dolp) {
			c = *dolp++;
			if (c && (c != '.' || !doldot || any('.', dolp)))
				return (echo(c));
			if (dolnxt && (dolnxt = dolnxt->next)) {
				dolp = dolnxt->value;
				return (echo(' '));
			}
			dolp = 0;
			echo(']');
			continue;
		}
		if (peekx) {
			c = peekx;
			peekx = 0;
		} else
			c = readc();
		if (c == '\\') {
			echo(c);
			c = readc();
			if (c == '\n')
				c = ' ';
			else
				c =| QUOTE;
		}
		if (dol2bra) {
			switch (c) {
				case '}':
					if (dol2bra > 0)
						echo(']');
					dol2bra = 0;
					echo('}');
					continue;
				case '\n':
					dol2bra = 0;
					seterr("Missing }");
					return (echo('\n'));
				case ':':
					switch (dol2bra) {
						case 2:
							dol2bra = -1;
							echo(']');
							echo(':');
							continue;
						case -2:
							dol2bra = 1;
							echo(':');
							echo('[');
							continue;
					}
				default:
					echo(c);
			}
			if (dol2bra < 0)
				continue;
			return (c);
		}
		if (c == '$') {
			echo(c);
			doldot = 0;
			c = readc();
			echo(c);
			if (c == '.') {
				doldot = 1;
				c = readc();
				echo(c);
			}
			switch (c) {
				default:
					if (digit(c)) {
						dolp = rgadrof(c);
						if (dolp == 0)
							continue;
						dolp = dolp->value;
						break;
					}
					if (c == '{' || letter(c)) {
						dolp = dolvbl(c);
						if (dolp || dol2bra > 0)
							break;
						continue;
					}
					return (c & 0177);
				case '$':
					dolp = value(pid);
					break;
				case '*':
					if (dolc <= 1)
						break;
					dolnxt = paramp->next;
					dolp = dolnxt->value;
					break;
			}
			echo('[');
			continue;
		}
		echo(c);
		return (c);
	}
}

dolvbl(sc)
	char sc;
{
	register char *np;
	register struct shvar *vp;
	char name[20], c;

	np = name;
	if (sc != '{')
		*np++ = sc;
	for (c = readc(); letter(c); c = readc())
		if (np < &name[sizeof name - 1]) {
			echo(c);
			*np++ = c;
		}
	*np++ = 0;
	vp = adrof(name);
	if (sc != '{')
		peekx = c;
	else {
		switch (c) {
			case ':':
				if (vp)
					dol2bra = -1;
				else
					dol2bra = -2;
				peekx = ':';
				break;
			case '}':
				peekx = c;
				dol2bra = -1;
				break;
			case '?':
				echo('?');
				if (vp)
					dol2bra = 2;
				else
					dol2bra = -2;
				return (0);
			default:
				echo(c);
				seterr("Variable syntax");
				return (0);
		}
	}
	if (vp == 0) {
		seterr("Undefined variable");
		return (0);
	}
	return (vp->value);
}

/*
 * read a character from the input device.
 * this may be an argument e.g. for sh -c.
 * also for sh -t stop after one line.
 */
readc()
{
	char cc;
	register c;
	register char *cp;

again:
	if (arginp) {
		if (arginp == 1)
			exit(0);
		else if ((c = *arginp++) == '\0') {
			arginp = 1;
			c = '\n';
		}
	} else if (onelflg == 1)
		exit(0);
	else if (read(0, &cc, 1) != 1) {
		doneinp = 1;
		reset();
	} else if ((c = cc) == '\n' && onelflg)
		onelflg--;
	if (c == 0)
		goto again;
	return (c);
}

rgadrof(r)
	register int r;
{
	register struct shvar *tp;

	if (!digit(r) || (r =- '0') > dolc)
		return (0);
	if (r == 0)
		return (&paramhd);
	for (tp = paramp; r > 0; tp = tp->next)
		r--;
	return (tp);
}

rgvalue(r)
	int r;
{
	register struct shvar *tp;

	tp = rgadrof(r);
	return (tp ? tp->value : "");
}
