/*
 * strtod.c
 *
 * Stupid version of System V strtod(3) library routine.
 * Does no overflow/underflow checking.
 *
 * A real number is defined to be
 *	optional leading white space
 *	optional sign
 *	string of digits with optional decimal point
 *	optional 'e' or 'E'
 *		followed by optional sign or space
 *		followed by an integer
 *
 * if ptr is not NULL a pointer to the character terminating the
 * scan is returned in *ptr.  If no number formed, *ptr is set to str
 * and 0 is returned.
 *
 * For speed, we don't do the conversion ourselves.  Instead, we find
 * the end of the number and then call atof() to do the dirty work.
 * This bought us a 10% speedup on a sample program at uunet.uu.net.
 */

#if 0
#include <ctype.h>
#endif

extern double atof();

double
strtod (s, ptr)
register char *s;
register char **ptr;
{
	double ret = 0.0;
	char *start = s;
	char *begin = NULL;
	int success = 0;

	/* optional white space */
	while (isspace(*s))
		s++;

	/* optional sign */
	if (*s == '+' || *s == '-') {
		s++;
		if (*(s-1) == '-')
			begin = s - 1;
		else
			begin = s;
	}

	/* string of digits with optional decimal point */
	if (isdigit(*s) && ! begin)
		begin = s;

	while (isdigit(*s)) {
		s++;
		success++;
	}

	if (*s == '.') {
		if (! begin)
			begin = s;
		s++;
		while (isdigit(*s))
			s++;
		success++;
	}

	if (s == start || success == 0)		/* nothing there */
		goto out;

	/*
 	 *	optional 'e' or 'E'
	 *		followed by optional sign or space
	 *		followed by an integer
	 */

	if (*s == 'e' || *s == 'E') {
		s++;

		/* XXX - atof probably doesn't allow spaces here */
		while (isspace(*s))
			s++;

		if (*s == '+' || *s == '-')
			s++;

		while (isdigit(*s))
			s++;
	}

	/* go for it */
	ret = atof(begin);

out:
	if (! success)
		s = start;	/* in case all we did was skip whitespace */

	if (ptr)
		*ptr = s;

	return ret;
}

#ifdef TEST
main (argc, argv)
int argc;
char **argv;
{
	double d;
	char *p;

	for (argc--, argv++; argc; argc--, argv++) {
		d = strtod (*argv, & p);
		printf ("%lf [%s]\n", d, p);
	}
}
#endif
