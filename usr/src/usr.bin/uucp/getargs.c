#ifndef lint
static char sccsid[] = "@(#)getargs.c	5.1 (Berkeley) 7/2/83";
#endif

#include <stdio.h>

/*******
 *	getargs(s, arps)
 *	char *s, *arps[];
 *
 *	getargs  -  this routine will generate a vector of
 *	pointers (arps) to the substrings in string "s".
 *	Each substring is separated by blanks and/or tabs.
 *
 *	If FANCYARGS is defined, you get the following:
 *	Strings containing blanks may be specified by quoting,
 *	in a manner similar to using the shell.
 *	Control characters are entered by ^X where X is any
 *	character; ^? gets you a rubout and ^^ is a real ^.
 *	Warning (rti!trt): I doubt FANCYARGS is wise, since getargs
 *	is used all over the place.  Its features may be useful
 *	but a separate fancy_getargs() should be called instead.
 *
 *	return - the number of subfields.
 */

getargs(s, arps)
register char *s;
char *arps[];
{
	register int i;
#ifdef	FANCYARGS
	register char *sp;
	register char qchar;
#endif

	i = 0;
#ifndef	FANCYARGS
	for (;;) {
		arps[i] = NULL;
		while (*s == ' ' || *s == '\t')
			*s++ = '\0';
		if (*s == '\n')
			*s = '\0';
		if (*s == '\0')
			break;
		arps[i++] = s++;
		while (*s != '\0' && *s != ' '
			&& *s != '\t' && *s != '\n')
				s++;
	}
#else
	for (;;) {
		while (*s == ' ' || *s == '\t')
			++s;
		if (*s == '\n' || *s == '\0')
			break;
		arps[i++] = sp = s;
		qchar = 0;
		while(*s != '\0'  &&  *s != '\n') {
			if (qchar == 0 && (*s == ' ' || *s == '\t')) {
				++s;
				break;
			}
			switch(*s) {
			default:
				*sp++ = *s++;
				break;
			case '^':
				if(*++s == '^')
					*sp++ = '^';
				else if(*s == '?')
					*sp++ = 0177;
				else
					*sp++ = *s & 037;
				s++;
				break;
			case '"':
			case '\'':
				if(qchar == *s) {
					qchar = 0;
					++s;
					break;
				}
				if(qchar)
					*sp++ = *s++;
				else
					qchar = *s++;
				break;
			}
		}
		*sp++ = 0;
	}
	arps[i] = NULL;
#endif
	return(i);
}
