/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)outchar.c	5.1 (Berkeley) 4/26/85";
#endif not lint

#include "2648.h"

outchar(c)
char c;
{
	extern int QUIET;
#ifdef TRACE
	if (trace)
		fprintf(trace, "%s", rdchar(c));
#endif
	if (QUIET)
		return;
	_outcount++;
	putchar(c);

	/* Do 2648 ^E/^F handshake */
	if (_outcount > TBLKSIZ && _on2648) {
#ifdef TRACE
		if (trace)
			fprintf(trace, "ENQ .. ");
#endif
		putchar(ENQ);
		fflush(stdout);
		c = getchar();
		while (c != ACK) {
			if (_pb_front == NULL) {
				_pb_front = _pushback;
				_pb_back = _pb_front - 1;
			}
			*++_pb_back = c;
#ifdef TRACE
			if (trace)
				fprintf(trace, "push back %s, front=%d, back=%d, ", rdchar(c), _pb_front-_pushback, _pb_front-_pushback);
#endif
			c = getchar();
		}
#ifdef TRACE
		if (trace)
			fprintf(trace, "ACK\n");
#endif
		_outcount = 0;
	}
}
