/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)getttyent.c	5.1 (Berkeley) %G%";
#endif not lint

#include <stdio.h>
#include <strings.h>
#include <ttyent.h>

static char TTYFILE[] = "/etc/ttys";
static char EMPTY[] = "";
static FILE *tf = NULL;
#define LINE 256
static char line[LINE];
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

#define QUOTED 1

/*
 * Skip over the current field and 
 * return a pointer to the next field.
 */
static char *
skip(p)
	register char *p;
{
	register int c;
	register int q = 0;

	for (; (c = *p) != '\0'; p++) {
		if (c == '"') {
			q ^= QUOTED;	/* obscure, but nice */
			continue;
		}
		if (q == QUOTED)
			continue;
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
	}
	return (p);
}

static char *
value(p)
	register char *p;
{
	if ((p = index(p,'=')) == 0)
		return(NULL);
	p++;			/* get past the = sign */
	return(p);
}

/* get rid of quotes. */

static
qremove(p)
	register char *p;
{
	register char *t;

	for (t = p; *p; p++)
		if (*p != '"')
			*t++ = *p;
	*t = '\0';
}

struct ttyent *
getttyent()
{
	register char *p;
	register int c;

	if (tf == NULL) {
		if ((tf = fopen(TTYFILE, "r")) == NULL)
			return (NULL);
	}
	do {
		p = fgets(line, LINE, tf);
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
	p = skip(p);
	tty.ty_status = 0;
	tty.ty_window = EMPTY;
	for (; *p; p = skip(p)) {
		if (strncmp(p, "on", 2) == 0)
			tty.ty_status |= TTY_ON;
		else if (strncmp(p, "off", 3) == 0)
			tty.ty_status &= ~TTY_ON;
		else if (strncmp(p, "secure", 6) == 0)
			tty.ty_status |= TTY_SECURE;
		else if (strncmp(p, "window", 6) == 0) {
			if ((tty.ty_window = value(p)) == NULL)
				tty.ty_window = EMPTY;
		} else
			break;
	}
	tty.ty_comment = p;
	if (p = index(p, '\n'))
		*p = '\0';
	qremove(tty.ty_getty);
	qremove(tty.ty_window);
	qremove(tty.ty_comment);
	return(&tty);
}
