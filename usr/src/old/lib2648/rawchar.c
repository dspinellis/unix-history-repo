/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)rawchar.c	5.1 (Berkeley) 4/26/85";
#endif not lint

/*
 * get a character from the terminal, with no line buffering.
 */

#include "2648.h"

rawchar()
{
	char c;

	sync();
	escseq(NONE);
	fflush(stdout);
	if (_pb_front && _on2648) {
		c = *_pb_front++;
#ifdef TRACE
		if (trace)
			fprintf(trace, "%s from queue, front=%d, back=%d\n", rdchar(c), _pb_front-_pushback, _pb_back-_pushback);
#endif
		if (_pb_front > _pb_back) {
			_pb_front = _pb_back = NULL;
#ifdef TRACE
			if (trace)
				fprintf(trace, "reset pushback to null\n");
#endif
		}
		return (c);
	}
	_outcount = 0;
	c = getchar();
#ifdef TRACE
	if (trace)
		fprintf(trace, "rawchar '%s'\n", rdchar(c));
#endif
	return (c);
}
