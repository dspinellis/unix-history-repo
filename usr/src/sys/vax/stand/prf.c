/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)prf.c	7.8 (Berkeley) %G%
 */

#include "sys/param.h"

#include "../include/mtpr.h"
#include "../vax/cons.h"

/*
 * Print a character on console.
 */
putchar(c)
	register c;
{
	register s, timo;
#if VAX630 || VAX650
	extern (*v_putc)();

	if (v_putc) {
		(*v_putc)(c);
		if (c == '\n')
			(*v_putc)('\r');
		return;
	}
#endif
	timo = 30000;
	/*
	 * Try waiting for the console tty to come ready,
	 * otherwise give up after a reasonable time.
	 */
	while((mfpr(TXCS)&TXCS_RDY) == 0)
		if(--timo == 0)
			break;
	if(c == 0)
		return;
	s = mfpr(TXCS);
	mtpr(TXCS,0);
	mtpr(TXDB, c&0xff);
	if(c == '\n')
		putchar('\r');
	putchar(0);
	mtpr(TXCS, s);
}

scankbd()
{}

getchar()
{
	register c;
#if VAX630 || VAX650
	extern (*v_getc)();

	if (v_getc) {
		c = (*v_getc)();
	} else {
#endif
	while((mfpr(RXCS)&RXCS_DONE) == 0)
		;
	c = mfpr(RXDB)&0177;
#if VAX630 || VAX650
	}
#endif
	if (c=='\r')
		c = '\n';
	if (c != '\b' && c != '\177')
		putchar(c);
	return(c);
}
