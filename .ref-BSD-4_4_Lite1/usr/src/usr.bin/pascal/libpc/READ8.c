/*-
 * Copyright (c) 1979, 1993
 *	The Regents of the University of California.  All rights reserved.
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

#ifndef lint
static char sccsid[] = "@(#)READ8.c	8.1 (Berkeley) 6/6/93";
#endif /* not lint */

#include "h00vars.h"
#include <errno.h>
extern int errno;

double
READ8(curfile)
	register struct iorec	*curfile;
{
	double			data;
	int			retval;

	if (curfile->funit & FWRITE) {
		ERROR("%s: Attempt to read, but open for writing\n",
			curfile->pfname);
	}
	UNSYNC(curfile);
	errno = 0;
	retval = readreal(curfile, &data);
	if (retval == EOF) {
		ERROR("%s: Tried to read past end of file\n", curfile->pfname);
	}
	if (retval == 0) {
		ERROR("%s: Bad data found on real read\n", curfile->pfname);
	}
	if (errno == ERANGE) {
		if (data == 0.0)
			ERROR("%s: Underflow on real read\n", curfile->pfname);
		else
			ERROR("%s: Overflow on real read\n", curfile->pfname);
	}
	if (errno != 0) {
		PERROR("Error encountered on real read ", curfile->pfname);
	}
	return (data);
}

/*
 *	given a file pointer, read a sequence of characters of the
 *	syntax of section 6.1.5 and form them into a double.
 *
 *	the syntax of a signed-real is:
 *	    [-|+] digit {digit} [ . digit {digit} ] [ e [+|-] digit {digit} ]
 *
 *	returns:
 *		1	for success (with value in *doublep)
 *		0	on error (with *doublep unchanged)
 *	       -1	on end-of-file during read (with *doublep unchanged)
 *	side effects:
 *	      errno	may be set to ERANGE if atof() sets it.
 */
readreal(curfile, doublep)
	struct iorec	*curfile;
	double		*doublep;
{
	FILE	*filep = curfile->fbuf;	/* current file variable */
	char	*sequencep;		/* a pointer into sequence */
	int	read;			/* return value from fscanf() */
	char	sequence[BUFSIZ];	/* the character sequence */
	double	atof();

#define PUSHBACK(curfile, sequencep) \
	if (ungetc(*--(sequencep), (curfile)->fbuf) != EOF) { \
		*(sequencep) = '\0'; \
	} else if ((curfile)->funit & SYNC) { \
		(curfile)->funit &= ~SYNC; \
		*(curfile)->fileptr = *(sequencep); \
		*(sequencep) = '\0'; \
	} else { \
		return (0); \
	}

#define	RETURN_ON_EOF(read) \
	if (read == EOF) \
		return (EOF); \
	else \
		/* void */;

#define	PUSH_TO_NULL(sequencep) \
	while (*sequencep) \
		sequencep++;

	/* general reader of the next character */
#define	NEXT_CHAR(read, filep, format, sequencep) \
	read = fscanf(filep, "%c", sequencep); \
	RETURN_ON_EOF(read); \
	*++sequencep = '\0';

	/* e.g. use %[0123456789] for {digit}, and check read */
#define	SOME(read, filep, format, sequencep) \
	read = fscanf(filep, format, sequencep); \
	RETURN_ON_EOF(read); \
	PUSH_TO_NULL(sequencep);

	/* e.g. use %[0123456789] for digit {digit} */
#define	AT_LEAST_ONE(read, filep, format, sequencep) \
	read = fscanf(filep, format, sequencep); \
	RETURN_ON_EOF(read); \
	if (strlen(sequencep) < 1) \
		return (0); \
	PUSH_TO_NULL(sequencep);

#define	ANY_ONE_OF(read, filep, format, sequencep) \
	read = fscanf(filep, format, sequencep); \
	RETURN_ON_EOF(read); \
	if (strlen(sequencep) != 1) \
		return (0); \
	PUSH_TO_NULL(sequencep);

#define	AT_MOST_ONE(read, filep, format, sequencep) \
	read = fscanf(filep, format, sequencep); \
	RETURN_ON_EOF(read); \
	if (strlen(sequencep) > 1) \
		return (0); \
	PUSH_TO_NULL(sequencep);
		
	sequencep = &sequence[0];
	*sequencep = '\0';
	/*
	 *	skip leading whitespace
	 */
	SOME(read, filep, "%*[ \t\n]", sequencep);
	/*
	 *	this much is required:
	 *	[ "+" | "-" ] digit {digits}
	 */
	AT_MOST_ONE(read, filep, "%[+-]", sequencep);
	AT_LEAST_ONE(read, filep, "%[0123456789]", sequencep);
	/*
	 *	any of this is optional:
	 *	[ `.' digit {digit} ] [ `e' [ `+' | `-' ] digit {digits} ]
	 */
	NEXT_CHAR(read, filep, "%c", sequencep);
	switch (sequencep[-1]) {
	default:
		PUSHBACK(curfile, sequencep);
		goto convert;
	case '.':
		SOME(read, filep, "%[0123456789]", sequencep);
		if (!read) {
			PUSHBACK(curfile, sequencep);
			goto convert;
		}
		NEXT_CHAR(read, filep, "%c", sequencep);
		if (sequencep[-1] != 'e') {
			PUSHBACK(curfile, sequencep);
			goto convert;
		}
		/* fall through */
	case 'e':
		NEXT_CHAR(read, filep, "%c", sequencep);
		if (sequencep[-1] != '+' && sequencep[-1] != '-') {
			PUSHBACK(curfile, sequencep);
			SOME(read, filep, "%[0123456789]", sequencep);
			if (!read)
				PUSHBACK(curfile, sequencep);
			goto convert;
		}
		SOME(read, filep, "%[0123456789]", sequencep);
		if (!read) {
			PUSHBACK(curfile, sequencep);
			PUSHBACK(curfile, sequencep);
		}
	}

convert:
	/*
	 * convert sequence to double
	 */
	*doublep = atof(&sequence[0]);
	return (1);
}
