/*

 *      Copyright (c) 1984, 1985, 1986 AT&T
 *      All Rights Reserved

 *      THIS IS UNPUBLISHED PROPRIETARY SOURCE 
 *      CODE OF AT&T.
 *      The copyright notice above does not 
 *      evidence any actual or intended
 *      publication of such source code.

 */
/* @(#)print.c	1.1 */
/*
 * UNIX shell
 *
 * S. R. Bourne
 * Rewritten by David Korn
 * AT&T Bell Laboratories
 *
 */

#include	"flags.h"
#include	"defs.h"
#include	"io.h"
#include	"shtype.h"
#ifndef BSD
# ifdef XENIX
# include	<sys/types.h>
# endif /* XENIX */
#include	<sys/param.h>
#endif /* BSD */
#include	"name.h"
#include	"builtins.h"

/* This module defines the following routines */
void	p_flush();
void	p_list();
void	p_nchr();
void	p_num();
void	p_prp();
void	p_setout();
void	p_str();
void	p_sub();
void	p_time();

/* This module references the following externals */
extern char	*itos();
extern char	*qvalup();
extern void	rjust();
extern void	setbuf();
extern char	*strcpy();

/* printing and io conversion */
#ifdef BSD
#define TIC_SEC		60	/* number of ticks per second */
#else
#define TIC_SEC		HZ	/* number of ticks per second */
#endif /* BSD */


/*
 *  flush the output queue and reset the output stream
 */

void	p_setout(fd)
register FILE *fd;
{
	if(output==fd)
		return;
	p_flush();
	output = fd;
	setbuf(fd,(char*)_sobuf);
}

/*
 * flush the output if necessary and null terminate the buffer
 */

void p_flush()
{
	register FILE *oldfd = output;
	register char *cp;
	if(oldfd)
	{
		if((cp=(char*)(oldfd->_ptr)) > (char*)(oldfd->_base))
		{
			fflush(oldfd);
			/* leave the previous buffer as a null terminated string */
		}
		if(cp)
			*cp = 0;
	}
}

/*
 * print a message preceded by the command name
 */

void p_prp(s1,ch)
char *s1;
{
	register unsigned char *cp;
	register int c;
	register FILE *fd = output;
	if(cp=(unsigned char *)cmdadr)
	{
		if(*cp=='-')
			cp++;
		c = ((cmdline>1&&ch!=SP)?0:':');
		p_str(cp,c);
		if(c==0)
			p_sub(cmdline,':');
		putc(SP,fd);
	}
	for(cp=(unsigned char *)s1;c= *cp;cp++)
	{
		if(!isprint(c))
		{
			putc(HAT,fd);
			c ^= TO_PRINT;
		}
		putc(c,fd);
	}
	if(ch)
		putc(ch,fd);
}

/*
 * print a time and a separator 
 */

void	p_time(t,c)
long int  t;
char c;
{
	register int  min, sec, frac;
	register int hr;
	frac = t%TIC_SEC;
	frac = (frac*100)/TIC_SEC;
	t /= TIC_SEC;
	sec=t%60; t /= 60;
	min=t%60;
	if(hr=t/60)
	{
		p_num(hr,'h');
	}
	p_num(min,'m');
	p_num(sec,'.');
	if(frac<10)
		putc('0',output);
	p_num(frac,'s');
	putc(c,output);
}

/*
 * print a number optionally followed by a character
 */

void	p_num(n,c)
int 	n;
char c;
{
	p_str(itos(n),c);
}

/*
 * print a string optionally followed by a character
 */

void	p_str(string,c)
register char *string;
register int c;
{
	register FILE *fd = output;
	fputs(string,fd);
	if(c)
		putc(c,fd);
}

/*
 * print a given character a given number of times
 */

void	p_nchr(c,n)
register int c,n;
{
	register FILE *fd = output;
	while(n-- > 0)
		putc(c,fd);
}

/* 
 * print a list of arguments in columns
 */
#define NROW	15	/* number of rows in output before going to multi-columns */
#define LBLSIZ	3	/* size of label field and interfield spacing */

void	p_list(argn,com)
char *com[];
{
	register int i,j;
	register char **arg;
	char a1[12];
	int nrow = NROW;
	int ncol = 1;
	int ndigits = 1;
	int fldsize;
#if ESH || VSH
	int wsize = e_window();
#else
	int wsize = 80;
#endif
	char *cp = qvalup(LINES);
	nrow = (cp?1+2*(atoi(cp)/3):NROW);
	for(i=argn;i >= 10;i /= 10)
		ndigits++;
	if(argn < nrow)
	{
		nrow = argn;
		goto skip;
	}
	i = 0;
	for(arg=com; *arg;arg++)
	{
		i = max(i,strlen(*arg));
	}
	i += (ndigits+LBLSIZ);
	if(i < wsize)
		ncol = wsize/i;
	if(argn > nrow*ncol)
	{
		nrow = 1 + (argn-1)/ncol;
	}
	else
	{
		ncol = 1 + (argn-1)/nrow;
		nrow = 1 + (argn-1)/ncol;
	}
skip:
	fldsize = (wsize/ncol)-(ndigits+LBLSIZ);
	for(i=0;i<nrow;i++)
	{
		j = i;
		while(1)
		{
			arg = com+j;
			strcpy(a1,itos(j+1));
			rjust(a1,ndigits,' ');
			p_str(a1,')');
			putc(SP,output);
			fputs(*arg,output);
			j += nrow;
			if(j >= argn)
				break;
			p_nchr(SP,fldsize-strlen(*arg));
		}
		newline();
	}
}

/*
 * Print a number enclosed in [] followed by a character
 */

void	p_sub(n,c)
register int n;
register int c;
{
	register FILE *fd=output;
	putc('[',fd);
	p_num(n,']');
	putc(c,fd);
}
