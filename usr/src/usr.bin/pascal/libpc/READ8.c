/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)READ8.c 1.6 %G%";

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
		return;
	}
	UNSYNC(curfile);
	errno = 0;
	retval = readreal(curfile->fbuf, &data);
	if (retval == EOF) {
		ERROR("%s: Tried to read past end of file\n", curfile->pfname);
		return;
	}
	if (retval == 0) {
		ERROR("%s: Bad data found on real read\n", curfile->pfname);
		return;
	}
	if (errno == ERANGE) {
		if (data == 0.0)
			ERROR("%s: Underflow on real read\n", curfile->pfname);
		else
			ERROR("%s: Overflow on real read\n", curfile->pfname);
		return;
	}
	if (errno != 0) {
		PERROR("Error encountered on real read ", curfile->pfname);
		return;
	}
	curfile->funit &= ~EOLN;
	curfile->funit |= SYNC;
	return (data);
}

/*
 *	given a file pointer, read a sequence of characters of the
 *	syntax of section 6.1.5 and form them into a double.
 *
 *	the syntax of a signed-real is:
 *	    [-|+] digit {digit} e [+|-] digit {digit}
 *	or
 *	    [-|+] digit {digit} . digit {digit} [e [+|-] digit {digit}]
 *
 *	returns:
 *		1	for success (with value in *doublep)
 *		0	on error (with *doublep unchanged)
 *	       -1	on end-of-file during read (with *doublep unchanged)
 *	side effects:
 *	      errno	may be set to ERANGE if atof() sets it.
 */
readreal(filep, doublep)
	FILE	*filep;
	double	*doublep;
{
	char	sequence[BUFSIZ];	/* the character sequence */
	char	*sequencep;		/* a pointer into sequence */
	int	read;			/* return value from fscanf() */
	double	atof();

#define	RETURN_ON_EOF(read) \
	if (read == EOF) \
		return (EOF); \
	else \
		/* void */;

#define	PUSH_TO_NULL(sequencep) \
	while (*sequencep) \
		sequencep++;

#define	SOME(read, filep, format, sequencep) \
	read = fscanf(filep, format, sequencep); \
	RETURN_ON_EOF(read); \
	PUSH_TO_NULL(sequencep);

#define	AT_LEAST_ONE(read, filep, format, sequencep) \
	read = fscanf(filep, format, sequencep); \
	RETURN_ON_EOF(read); \
	if (strlen(sequencep) < 1) \
		return (0); \
	PUSH_TO_NULL(sequencep);

#define	EXACTLY_ONE(read, filep, format, sequencep) \
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
	 * skip leading whitespace
	 */
	SOME(read, filep, "%*[ \t\n]", sequencep);
	/*
	 *	[ "+" | "-" ] digit {digits}
	 */
	AT_MOST_ONE(read, filep, "%[+-]", sequencep);
	AT_LEAST_ONE(read, filep, "%[0123456789]", sequencep);
	/*
	 *	either
	 *		"." digit {digit} [ "e" [ "+" | "-" ] digit {digits} ]
	 *	or
	 *		"e" [ "+" | "-" ] digit {digits}
	 */
	AT_MOST_ONE(read, filep, "%[.]", sequencep);
	if (read) {
		AT_LEAST_ONE(read, filep, "%[0123456789]", sequencep);
		AT_MOST_ONE(read, filep, "%[e]", sequencep);
		if (read) {
			AT_MOST_ONE(read, filep, "%[+-]", sequencep);
			AT_LEAST_ONE(read, filep, "%[0123456789]", sequencep);
		}
	} else {
		EXACTLY_ONE(read, filep, "%[e]", sequencep);
		AT_MOST_ONE(read, filep, "%[+-]", sequencep);
		AT_LEAST_ONE(read, filep, "%[0123456789]", sequencep);
	}
	/*
	 * convert sequence to double
	 */
	*doublep = atof(&sequence[0]);
	return (1);
}
