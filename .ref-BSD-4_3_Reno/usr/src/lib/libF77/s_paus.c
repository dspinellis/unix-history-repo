/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)s_paus.c	5.1	6/7/85
 */

#include <stdio.h>
#define PAUSESIG 15


s_paus(s, n)
char *s;
long int n;
{
int i;
int waitpause();

fprintf(stderr, "PAUSE: ");
if(n > 0)
	{
	for(i = 0; i<n ; ++i)
		putc(*s++, stderr);
	putc('\n', stderr);
	}
if( isatty(fileno(stdin)) )
	{
	fprintf(stderr, "To resume execution, type:   go\nAny other input will terminate the program.\n");
	if( getchar()!='g' || getchar()!='o' || getchar()!='\n' )
		{
		fprintf(stderr, "STOP\n");
		f_exit();
		_cleanup();
		exit(0);
		}
	}
else
	{
	fprintf(stderr, "To resume execution, type:    kill -%d %d\n",
		PAUSESIG, getpid() );
	signal(PAUSESIG, waitpause);
	pause();
	}
fprintf(stderr, "Execution resumed after PAUSE.\n");
}





static waitpause()
{
return;
}
