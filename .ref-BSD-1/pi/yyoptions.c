#
/*
 * pi - Pascal interpreter code translator
 *
 * Charles Haley, Bill Joy UCB
 * Version 1.0 August 1977
 */

#include "whoami"
#include "0.h"
#include "yy.h"

/*
 * Options processes the option
 * strings which can appear in
 * comments and returns the next character.
 */
options()
{
	register c, ch;
	register char *optp;
	int ok, *optsp;

	c = getchar();
	if (c != '$')
		return (c);
	do {
		c = getchar();
		switch (c) {
			case 'b':
				optp = &opts['b'-'a'];
				goto optdig;
			case 'x':
				optp = &opts['x'-'a'];
				goto optdig;
			optdig:
				c = getchar();
				if (!digit(c))
					return (c);
				*optp = c - '0';
				c = getchar();
				break;
			default:
				if (c < 'a' || c > 'z')
					return (c);
				optp = &opts[c-'a'];
				optsp = &optstk[c-'a'];
				ch = getchar();
				ok = 0;
				if (ch == '<') {
					*optp = *optsp & 1;
					*optsp =>> 1;
					ok = 1;
					c = getchar();
				} else if (ch == '>') {
					*optsp =<< 1;
					*optsp =| *optp & 1;
					ok = 1;
					c = getchar();
				} else {
					ok = 0;
					c = ch;
				}
				if (c == '+') {
					*optp = 1;
					c = getchar();
				} else if (c == '-') {
					*optp = 0;
					c = getchar();
				} else if (!ok)
					return (c);
				break;
			}
	} while (c == ',');
	if (opts['u'-'a'])
		setuflg();
	return (c);
}
