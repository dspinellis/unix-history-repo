/* @(#)getttyent.c	4.1 (Berkeley) %G% */

#include <stdio.h>
#include <ttyent.h>

static char TTYFILE[] = "/etc/ttys";
static char EMPTY[] = "";
static FILE *tf = NULL;
static char line[BUFSIZ+1];
static struct ttyent tty;

setttyent()
{
	if (tf == NULL)
		tf = fopen(TTYFILE, "r");
	else
		rewind(tf);
}

endttyent()
{
	if (tf != NULL) {
		(void) fclose(tf);
		tf = NULL;
	}
}

static char *
skip(p)
register char *p;
{
	register int c;

	while ((c = *p) != '\0') {
		if (c == '#') {
			*p = '\0';
			break;
		}
		if (c == '\t' || c == ' ' || c == '\n') {
			*p++ = '\0';
			while ((c = *p) == '\t' || c == ' ' || c == '\n')
				p++;
			break;
		}
		p++;
	}
	return (p);
}

struct ttyent *
getttyent()
{
	register char *p, *cp;
	register int c;

	if (tf == NULL) {
		if ((tf = fopen(TTYFILE, "r")) == NULL)
			return (NULL);
	}
	do {
		p = fgets(line, BUFSIZ, tf);
		if (p == NULL)
			return (NULL);
		while ((c = *p) == '\t' || c == ' ' || c == '\n')
			p++;
	} while (c == '\0' || c == '#');
	tty.ty_name = p;
	p = skip(p);
	tty.ty_getty = p;
	p = skip(p);
	tty.ty_type = p;
	tty.ty_status = 0;
	for (p = skip(p); *p; p = cp) {
		cp = skip(p);
		if (strcmp(p, "on") == 0)
			tty.ty_status |= TTY_ON;
		else if (strcmp(p, "off") == 0)
			tty.ty_status &= ~TTY_ON;
		else if (strcmp(p, "secure") == 0)
			tty.ty_status |= TTY_SECURE;
	}
	tty.ty_comment = EMPTY;
	return (&tty);
}
