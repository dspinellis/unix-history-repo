/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1982, 1986, 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
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
 *
 * from: Utah $Hdr: machdep.c 1.10 92/06/18
 *
 *	@(#)machdep.c	8.1 (Berkeley) 6/10/93
 */

#include <sys/param.h>

/*
 * Copy bytes within kernel
 */
bcopy(from, to, count)
	register caddr_t from, to;
	register unsigned count;
{
	while (count--)
		*to++ = *from++;
}

bzero(to, count)
	register caddr_t to;
	register unsigned count;
{
	while (count--)
		*to++ = 0;
}

bcmp(s1, s2, len)
	register char *s1, *s2;
	register int len;
{
	while (len--)
		if (*s1++ != *s2++)
			return (1);
	return (0);
}

#ifdef ROMPRF
int userom;
#endif

struct trapframe {
	int dregs[8];
	int aregs[8];
	int whoknows;
	short sr;
	int pc;
	short frame;
};

trap(fp)
	struct trapframe *fp;
{
	static int intrap = 0;

	if (intrap)
		return(0);
	intrap = 1;
#ifdef ROMPRF
	userom = 1;
#endif
	printf("Got unexpected trap: format=%x vector=%x ps=%x pc=%x\n",
		  (fp->frame>>12)&0xF, fp->frame&0xFFF, fp->sr, fp->pc);
	printf("dregs: %x %x %x %x %x %x %x %x\n",
	       fp->dregs[0], fp->dregs[1], fp->dregs[2], fp->dregs[3], 
	       fp->dregs[4], fp->dregs[5], fp->dregs[6], fp->dregs[7]);
	printf("aregs: %x %x %x %x %x %x %x %x\n",
	       fp->aregs[0], fp->aregs[1], fp->aregs[2], fp->aregs[3], 
	       fp->aregs[4], fp->aregs[5], fp->aregs[6], fp->aregs[7]);
#ifdef ROMPRF
	userom = 0;
#endif
	intrap = 0;
	return(0);
}

#ifdef ROMPRF
#define ROWS	46
#define COLS	128

romputchar(c)
	register int c;
{
	static char buf[COLS];
	static int col = 0, row = 0;
	register int i;

	switch (c) {
	case '\0':
		break;
	case '\r':
		break;	/* ignore */
	case '\n':
		for (i = col; i < COLS-1; i++)
			buf[i] = ' ';
		buf[i] = '\0';
		romout(row, buf);
		col = 0;
		if (++row == ROWS)
			row = 0;
		break;

	case '\t':
		do {
			romputchar(' ');
		} while (col & 7);
		break;

	default:
		buf[col] = c;
		if (++col == COLS-1)
			romputchar('\n');
		break;
	}
}
#endif
