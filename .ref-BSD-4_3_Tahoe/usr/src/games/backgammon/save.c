/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)save.c	5.3 (Berkeley) 6/18/88";
#endif /* not lint */

#include "back.h"

extern int	errno;

static char	confirm[] = "Are you sure you want to leave now?";
static char	prompt[] = "Enter a file name:  ";
static char	exist1[] = "The file '";
static char	exist2[] =
	"' already exists.\nAre you sure you want to use this file?";
static char	cantuse[] = "\nCan't use ";
static char	saved[] = "This game has been saved on the file '";
static char	type[] = "'.\nType \"backgammon ";
static char	rec[] = "\" to recover your game.\n\n";
static char	cantrec[] = "Can't recover file:  ";

save (n)
register int	n;

{
	register int	fdesc;
	register char	*fs;
	char		fname[50];

	if (n)  {
		if (tflag)  {
			curmove (20,0);
			clend();
		} else
			writec ('\n');
		writel (confirm);
		if (! yorn(0))
			return;
	}
	cflag = 1;
	for (;;)  {
		writel (prompt);
		fs = fname;
		while ((*fs = readc()) != '\n')  {
			if (*fs == tty.sg_erase)  {
				if (fs > fname)  {
					fs--;
					if (tflag)
						curmove (curr,curc-1);
					else
						writec (*fs);
				} else
					writec ('\007');
				continue;
			}
			writec (*fs++);
		}
		*fs = '\0';
		if ((fdesc = open(fname,2)) == -1 && errno == 2)  {
			if ((fdesc = creat (fname,0700)) != -1)
			break;
		}
		if (fdesc != -1)  {
			if (tflag)  {
				curmove (18,0);
				clend();
			} else
				writec ('\n');
			writel (exist1);
			writel (fname);
			writel (exist2);
			cflag = 0;
			close (fdesc);
			if (yorn (0))  {
				unlink (fname);
				fdesc = creat (fname,0700);
				break;
			} else  {
				cflag = 1;
				continue;
			}
		}
		writel (cantuse);
		writel (fname);
		writel (".\n");
		close (fdesc);
		cflag = 1;
	}
	write (fdesc,board,sizeof board);
	write (fdesc,off,sizeof off);
	write (fdesc,in,sizeof in);
	write (fdesc,dice,sizeof dice);
	write (fdesc,&cturn,sizeof cturn);
	write (fdesc,&dlast,sizeof dlast);
	write (fdesc,&pnum,sizeof pnum);
	write (fdesc,&rscore,sizeof rscore);
	write (fdesc,&wscore,sizeof wscore);
	write (fdesc,&gvalue,sizeof gvalue);
	write (fdesc,&raflag,sizeof raflag);
	close (fdesc);
	if (tflag)
		curmove (18,0);
	writel (saved);
	writel (fname);
	writel (type);
	writel (fname);
	writel (rec);
	if (tflag)
		clend();
	getout ();
}

recover (s)
char	*s;

{
	register int	i;
	int		fdesc;

	if ((fdesc = open (s,0)) == -1)
		norec (s);
	read (fdesc,board,sizeof board);
	read (fdesc,off,sizeof off);
	read (fdesc,in,sizeof in);
	read (fdesc,dice,sizeof dice);
	read (fdesc,&cturn,sizeof cturn);
	read (fdesc,&dlast,sizeof dlast);
	read (fdesc,&pnum,sizeof pnum);
	read (fdesc,&rscore,sizeof rscore);
	read (fdesc,&wscore,sizeof wscore);
	read (fdesc,&gvalue,sizeof gvalue);
	read (fdesc,&raflag,sizeof raflag);
	close (fdesc);
	rflag = 1;
}

norec (s)
register char	*s;

{
	register char	*c;

	tflag = 0;
	writel (cantrec);
	c = s;
	while (*c != '\0')
		writec (*c++);
	getout ();
}
