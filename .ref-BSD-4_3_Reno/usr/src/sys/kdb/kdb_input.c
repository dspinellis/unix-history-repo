/*
 * Copyright (c) 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)kdb_input.c	7.5 (Berkeley) 5/3/90
 */

#include "../kdb/defs.h"

char	line[LINSIZ];
char	*kdblp;
char	kdbpeekc,kdblastc = EOR;

/* input routines */

kdbeol(c)
	char c;
{
	return (c==EOR || c==';');
}

kdbrdc()
{
	do
		(void) kdbreadchar();
	while (kdblastc==SP || kdblastc==TB);
	return (kdblastc);
}

kdbreadchar()
{
	static char *erase = "\b \b";

	if (kdblp==0) {
		kdblp=line;
		do {
			(void) kdbreadc(kdblp);
			if (kdbmkfault)
				kdberror((char *)0);
			switch (*kdblp) {
			case CTRL('h'): case 0177:
				if (kdblp > line)
					kdbwrite(erase, 3), kdblp--;
				break;
			case CTRL('u'):
				while (kdblp > line) 
					kdbwrite(erase, 3), kdblp--;
				break;
			case CTRL('r'):
				kdbwrite("^R\n", 3);
				if (kdblp > line)
					kdbwrite(line, kdblp-line);
				break;
			case CTRL('w'):
				if (kdblp <= line)
					break;
				do {
					if (!isspace(*kdblp))
						goto erasenb;
					kdbwrite(erase, 3);
				} while (--kdblp > line);
				break;
			erasenb:
				do
					kdbwrite(erase, 3);
				while (--kdblp > line && !isspace(*kdblp));
				break;
			default:
				kdbecho(*kdblp++);
				break;
			}
		} while (kdblp == line || kdblp[-1] != EOR);
		*kdblp=0; kdblp=line;
	}
	if (kdblastc = kdbpeekc)
		kdbpeekc=0;
	else  if (kdblastc = *kdblp)
		kdblp++;
	return (kdblastc);
}

static
kdbecho(c)
	char c;
{
	char buf[2];

	if (c==0177 || (c<SP && c != TB && c != EOR)) {
		buf[0] = '^';
		buf[1] = c ^ 0100;
		kdbwrite(buf, 2);
	} else
		kdbwrite(&c, 1);
}

kdbnextchar()
{

	if (kdbeol(kdbrdc())) {
		kdblp--;
		return (0);
	}
	return (kdblastc);
}

kdbquotchar()
{

	if (kdbreadchar()=='\\')
		return (kdbreadchar());
	if (kdblastc=='\'')
		return (0);
	return (kdblastc);
}

kdbgetformat(deformat)
	char *deformat;
{
	register char *fptr;
	register int quote;

	fptr=deformat; quote=0;
	while ((quote ? kdbreadchar()!=EOR : !kdbeol(kdbreadchar())))
		if ((*fptr++ = kdblastc)=='"')
			quote = ~quote;
	kdblp--;
	if (fptr!=deformat)
		*fptr++ = '\0';
}
