/* $Header: /home/hyperion/mu/christos/src/sys/tcsh-6.01/RCS/sh.print.c,v 3.2 1991/10/12 04:23:51 christos Exp $ */
/*
 * sh.print.c: Primitive Output routines.
 */
/*-
 * Copyright (c) 1980, 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */
#include "sh.h"

RCSID("$Id: sh.print.c,v 3.2 1991/10/12 04:23:51 christos Exp $")

#include "ed.h"

extern int Tty_eight_bit;
extern int Tty_raw_mode;
extern Char GettingInput;

int     lbuffed = 1;		/* true if line buffered */

static	void	p2dig	__P((int));

/*
 * C Shell
 */

#ifdef RLIMIT_CPU
void
psecs(l)
    long    l;
{
    register int i;

    i = l / 3600;
    if (i) {
	xprintf("%d:", i);
	i = l % 3600;
	p2dig(i / 60);
	goto minsec;
    }
    i = l;
    xprintf("%d", i / 60);
minsec:
    i %= 60;
    xprintf(":");
    p2dig(i);
}

#endif

void
pcsecs(l)			/* PWP: print mm:ss.dd, l is in sec*100 */
#ifdef BSDTIMES
    long    l;

#else				/* BSDTIMES */
#ifndef POSIX
    time_t  l;

#else				/* POSIX */
    clock_t l;

#endif				/* POSIX */
#endif				/* BSDTIMES */
{
    register int i;

    i = l / 360000;
    if (i) {
	xprintf("%d:", i);
	i = (l % 360000) / 100;
	p2dig(i / 60);
	goto minsec;
    }
    i = l / 100;
    xprintf("%d", i / 60);
minsec:
    i %= 60;
    xprintf(":");
    p2dig(i);
    xprintf(".");
    p2dig((int) (l % 100));
}

static void 
p2dig(i)
    register int i;
{

    xprintf("%d%d", i / 10, i % 10);
}

char    linbuf[2048];		/* was 128 */
char   *linp = linbuf;
bool    output_raw = 0;		/* PWP */

void
xputchar(c)
    register int c;
{
    int     atr = 0;

    atr |= c & ATTRIBUTES & TRIM;
    c &= CHAR | QUOTE;
    if (!output_raw && (c & QUOTE) == 0) {
	if (Iscntrl(c)) {
	    if (c != '\t' && c != '\n' && c != '\r') {
		xputchar('^' | atr);
		if (c == ASCII)
		    c = '?';
		else
		    c |= 0100;
	    }
	}
	else if (!Isprint(c)) {
	    xputchar('\\' | atr);
	    xputchar((((c >> 6) & 7) + '0') | atr);
	    xputchar((((c >> 3) & 7) + '0') | atr);
	    c = (c & 7) + '0';
	}
	(void) putraw(c | atr);
    }
    else {
	c &= TRIM;
	if (haderr ? (didfds ? is2atty : isdiagatty) :
	    (didfds ? is1atty : isoutatty))
	    SetAttributes(c | atr);
	(void) putpure(c);
    }
    if (lbuffed && (c & CHAR) == '\n')
	flush();
}

int
putraw(c)
    register int c;
{
    if (haderr ? (didfds ? is2atty : isdiagatty) :
	(didfds ? is1atty : isoutatty)) {
	if (Tty_eight_bit == -1)
	    ed_set_tty_eight_bit();
	if (!Tty_eight_bit && (c & META)) {
	    c = (c & ~META) | STANDOUT;
	}
	SetAttributes(c);
    }
    return putpure(c);
}

int
putpure(c)
    register int c;
{
    c &= CHAR;

    *linp++ = c;
    if (linp >= &linbuf[sizeof linbuf - 10])
	flush();
    return (1);
}

void
draino()
{
    linp = linbuf;
}

void
flush()
{
    register int unit;

    /* int lmode; */

    if (linp == linbuf)
	return;
    if (GettingInput && !Tty_raw_mode && linp < &linbuf[sizeof linbuf - 10])
	return;
    if (haderr)
	unit = didfds ? 2 : SHDIAG;
    else
	unit = didfds ? 1 : SHOUT;
#ifdef COMMENT
#ifdef TIOCLGET
    if (didfds == 0 && ioctl(unit, TIOCLGET, (ioctl_t) & lmode) == 0 &&
	lmode & LFLUSHO) {
	lmode = LFLUSHO;
	(void) ioctl(unit, TIOCLBIC, (ioclt_t) & lmode);
	(void) write(unit, "\n", 1);
    }
#endif
#endif
    (void) write(unit, linbuf, (size_t) (linp - linbuf));
    linp = linbuf;
}
