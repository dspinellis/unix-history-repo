#
/*
 * Ex - a text editor
 * Bill Joy UCB June 1977
 */

#include "ex.h"
#include "ex_tty.h"

int	(*Outchar)(), (*Putchar)();
int	listchar(), normchar();

/* Put in assembly language to save space
setlist()
{
	register int (*P)();

	P = Putchar;
	Putchar = &listchar;
	return (P);
}

setnorm()
{
	register int (*P)();

	P = Putchar;
	Putchar = &normchar;
	return (P);
}
*/

listchar(c)
	register CHAR c;
{

	c =& 0377;
	switch (c) {
		case '\t':
			c = '>';
			goto escit;
		case '\b':
			c = '<';
escit:
			if (!OS)
				break;
			outchar('-');
			outchar('\b');
			break;
		case '\n':
			break;
		case '\n' | QUOTE:
			outchar('$');
			break;
		default:
			if (c & QUOTE)
				break;
			if (c < ' ' && c != '\n' || c == 0177) {
				outchar('\\');
				if (c == 0177) {
					outchar('1');
					c = 077;
				}
				outchar((c >> 3) + '0');
				c = (c & 07) + '0';
			}
			break;
	}
	normchar(c);
}

normchar(c)
	register CHAR c;
{
	register char *colp;

	c =& 0377;
	if (c & QUOTE)
		switch (c) {
			case ' ' | QUOTE:
			case '\b' | QUOTE:
				break;
			case QUOTE:
				return;
			default:
				c =& 0177;
		}
	if (UPPERCASE) {
		if (ucletter(c)) {
			outchar('\\');
			c = letter(c);
		} else {
			colp = "({)}!|^~'`";
			while (*colp++)
				if (c == *colp++) {
					outchar('\\');
					c = colp[-2];
					break;
				}
		}
	}
	outchar(c);
}

extern	int (*Pline)(), numbline();

numbline(i)
	int i;
{

	if (shudclob)
		slobber(' ');
	printf("%6d  ", i);
	normline();
}

normline()
{

	if (shudclob)
		slobber(linebuf[0]);
	if (Putchar == &listchar || value(PRINTALL))
		printf("%s", linebuf);
	else {
		if (value(INDICATEUL) && !inopen && !inconf)
			doulg();
		dographic();
		if (value(INDICATEUL) && !inopen && !inconf && genbuf[0]) {
			outchar('\n');
			if (Pline == &numbline)
				outchar('\t');
			printf("%s", genbuf);
		}
	}
	if (!inopen && !inconf)
		putchar('\n' | QUOTE);
}

slobber(c)
	char c;
{

	shudclob = 0;
	switch (c) {
		case '\t':
			if (Putchar == &listchar)
				return;
			break;
		default:
			return;
		case ' ':
		case 0:
			break;
	}
	if (OS)
		return;
	printf("\240\210");
}

int	(*Outchar)(), termchar();

/*
setoutt()
{

	Outchar = &termchar;
}
*/

lprintf(a1, a2, a3)
{
	register int (*P)();

	P = setlist();
	printf(a1, a2, a3);
	Putchar = P;
}
