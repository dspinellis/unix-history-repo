/*-
 * Copyright (c) 1993 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Bill Jolitz.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)getttyent.c	5.2 (Berkeley) 5/29/93";
#endif /* LIBC_SCCS and not lint */

#include <stdio.h>
#include <strings.h>
#include <ttyent.h>

static char TTYFILE[] = "/etc/ttys";
static char zapchar;
static FILE *tf = NULL;
#define LINE 256
static char line[LINE];
static struct ttyent tty;
#define NOPTS 32
static char *options[NOPTS];

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

#define QUOTED	1

/*
 * Skip over the current field, removing quotes,
 * and return a pointer to the next field.
 */
static char *
skip(p)
	register char *p;
{
	register char *t = p;
	register int c;
	register int q = 0;

	for (; (c = *p) != '\0'; p++) {
		if (c == '"') {
			q ^= QUOTED;	/* obscure, but nice */
			continue;
		}
		if (q == QUOTED && *p == '\\' && *(p+1) == '"')
			p++;
		*t++ = *p;
		if (q == QUOTED)
			continue;
		if (c == '#') {
			zapchar = c;
			*p = 0;
			break;
		}
		if (c == '\t' || c == ' ' || c == '\n') {
			zapchar = c;
			*p++ = 0;
			while ((c = *p) == '\t' || c == ' ' || c == '\n')
				p++;
			break;
		}
	}
	*--t = '\0';
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

struct ttyent *
getttyent()
{
	register char *p;
	register int c;
	register char **op;

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
	zapchar = 0;
	tty.ty_name = p;
	p = skip(p);
	tty.ty_getty = p;
	p = skip(p);
	tty.ty_type = p;
	p = skip(p);
	tty.ty_status = 0;
	tty.ty_window = NULL;
	tty.ty_flags = options ;
	op = options ;
	for (; *p; p = skip(p)) {
		*op++ = p ; /* bounds check ? */
#define space(x) ((c = p[x]) == ' ' || c == '\t' || c == '\n')
		if (strncmp(p, "on", 2) == 0 && space(2))
			tty.ty_status |= TTY_ON;
		else if (strncmp(p, "off", 3) == 0 && space(3))
			tty.ty_status &= ~TTY_ON;
		else if (strncmp(p, "secure", 6) == 0 && space(6))
			tty.ty_status |= TTY_SECURE;
		else if (strncmp(p, "window=", 7) == 0)
			tty.ty_window = value(p);
		else if (zapchar == '#' || *p == '#')
			break;
	}
	*op = 0 ;	/* end sentinel */
	if (zapchar == '#' || *p == '#')
		while ((c = *++p) == ' ' || c == '\t')
			;
	tty.ty_comment = p;
	if (*p == 0)
		tty.ty_comment = 0;
	if (p = index(p, '\n'))
		*p = '\0';
	return(&tty);
}
