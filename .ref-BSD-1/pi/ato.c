#
/*
 * pi - Pascal interpreter code translator
 *
 * Charles Haley, Bill Joy UCB
 * Version 1.0 August 1977
 */

#include "whoami"
#include "0.h"

long a8tol(cp)
	char *cp;
{
	int err;
	long l;
	register CHAR c;

	l = 0;
	err = 0;
	while ((c = *cp++) != '\0') {
		if (c == '8' || c == '9')
			if (err == 0) {
				error("8 or 9 in octal number");
				err++;
			}
		c =- '0';
		if (((&l)->pint & 0160000) != 0)
			if (err == 0) {
				error("Number too large for this implementation");
				err++;
			}
		l = (l << 3) | c;
	}
	return (l);
}

/*
 * Note that the version of atof
 * used in this compiler does not
 * (sadly) complain when floating
 * point numbers are too large.
 */
