/*

 *      Copyright (c) 1984, 1985, 1986 AT&T
 *      All Rights Reserved

 *      THIS IS UNPUBLISHED PROPRIETARY SOURCE 
 *      CODE OF AT&T.
 *      The copyright notice above does not 
 *      evidence any actual or intended
 *      publication of such source code.

 */
/* @(#)echo.c	1.1 */

#ifdef KSHELL
#include	"defs.h"
#include	"brkincr.h"
#else
#include	<stdio.h>
#endif	/* KSHELL */

/*
 * echo the argument list on stream fd
 * if raw is non-zero then \ is not a special character.
 * returns 0 for \c otherwise 1.
 */

#ifdef KSHELL
extern void	exitsh();
#endif	/* KSHELL */

int echo_list(raw,com,fd)
int raw;
char *com[];
register FILE *fd;
{
	register int outc;
	register char *cp;
	while(cp= *com++)
	{
		for(; *cp; cp++)
		{
			outc = *cp;
			if(*cp == '\\' && raw==0)
			{
				switch(*++cp)
				{
					case 'b':
						outc = '\b';
						break;
					case 'c':
						return(0);
					case 'f':
						outc = '\f';
						break;
					case 'n':
						outc = '\n';
						break;
					case 'r':
						outc = '\r';
						break;
					case 'v':
						outc = '\v';
						break;
					case 't':
						outc = '\t';
						break;
					case '\\':
						outc = '\\';
						break;
					case '0':
					{
						register char *cpmax;
						outc = 0;
						cpmax = cp + 4;
						while(++cp<cpmax && *cp>='0' && 
							*cp<='7')
						{
							outc <<= 3;
							outc |= (*cp-'0');
						}
						cp--;
						break;
					}
					default:
					cp--;
				}
			}
			putc (outc, fd);
		}
		if(*com)
			putc(' ',fd);
#ifdef KSHELL
		if(trapnote&SIGSET)
			exitsh(SIGFAIL);
#endif	/* KSHELL */
	}
	return(1);
}

