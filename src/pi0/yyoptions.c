/* Copyright (c) 1979 Regents of the University of California */
#
/*
 * pi - Pascal interpreter code translator
 *
 * Charles Haley, Bill Joy UCB
 * Version 1.2 January 1979
 */

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
	int ok;

	c = getchar();
	if (c != '$')
		return (c);
	do {
		ch = c = getchar();
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
				c = getchar();
				if (c == '+') {
					*optp = 1;
					c = getchar();
				} else if (c == '-') {
					*optp = 0;
					c = getchar();
				} else
					return (c);
				break;
			}
#ifdef PI0
		send(ROSET, ch, *optp);
#endif
	} while (c == ',');
	if (opts['u'-'a'])
		setuflg();
	return (c);
}
