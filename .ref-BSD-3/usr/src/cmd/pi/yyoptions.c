/* Copyright (c) 1979 Regents of the University of California */
#
/*
 * pi - Pascal interpreter code translator
 *
 * Charles Haley, Bill Joy UCB
 * Version 1.1 February 1978
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
	int ok;

	c = readch();
	if (c != '$')
		return (c);
	do {
		ch = c = readch();
		switch (c) {
			case 'b':
				optp = &opts['b'-'a'];
				goto optdig;
			case 'x':
				optp = &opts['x'-'a'];
				goto optdig;
			optdig:
				c = readch();
				if (!digit(c))
					return (c);
				*optp = c - '0';
				c = readch();
				break;
			default:
				if (c < 'a' || c > 'z')
					return (c);
				optp = &opts[c-'a'];
				c = readch();
				if (c == '+') {
					*optp = 1;
					c = readch();
				} else if (c == '-') {
					*optp = 0;
					c = readch();
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
