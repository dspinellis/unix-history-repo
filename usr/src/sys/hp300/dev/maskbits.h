/*-
 * Copyright (c) 1994
 *	The Regents of the University of California.  All rights reserved.
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
 *	@(#)maskbits.h	8.2 (Berkeley) 3/21/94
 */

/*
 * Derived from X11R4
 */

/* the following notes use the following conventions:
SCREEN LEFT				SCREEN RIGHT
in this file and maskbits.c, left and right refer to screen coordinates,
NOT bit numbering in registers.

starttab[n]
	bits[0,n-1] = 0	bits[n,31] = 1
endtab[n] =
	bits[0,n-1] = 1	bits[n,31] = 0

maskbits(x, w, startmask, endmask, nlw)
	for a span of width w starting at position x, returns
a mask for ragged bits at start, mask for ragged bits at end,
and the number of whole longwords between the ends.

*/

#define maskbits(x, w, startmask, endmask, nlw) \
    startmask = starttab[(x)&0x1f]; \
    endmask = endtab[((x)+(w)) & 0x1f]; \
    if (startmask) \
	nlw = (((w) - (32 - ((x)&0x1f))) >> 5); \
    else \
	nlw = (w) >> 5;

#define FASTGETBITS(psrc, x, w, dst) \
    asm ("bfextu %3{%1:%2},%0" \
    : "=d" (dst) : "di" (x), "di" (w), "o" (*(char *)(psrc)))

#define FASTPUTBITS(src, x, w, pdst) \
    asm ("bfins %3,%0{%1:%2}" \
	 : "=o" (*(char *)(pdst)) \
	 : "di" (x), "di" (w), "d" (src), "0" (*(char *) (pdst)))

#define getandputrop(psrc, srcbit, dstbit, width, pdst, rop) \
{ \
  register unsigned int _tmpsrc, _tmpdst; \
  FASTGETBITS(pdst, dstbit, width, _tmpdst); \
  FASTGETBITS(psrc, srcbit, width, _tmpsrc); \
  DoRop(_tmpdst, rop, _tmpsrc, _tmpdst); \
  FASTPUTBITS(_tmpdst, dstbit, width, pdst); \
}

#define getandputrop0(psrc, srcbit, width, pdst, rop) \
    	getandputrop(psrc, srcbit, 0, width, pdst, rop)

#define getunalignedword(psrc, x, dst) { \
        register int _tmp; \
        FASTGETBITS(psrc, x, 32, _tmp); \
        dst = _tmp; \
}

#define fnCLEAR(src, dst)       (0)
#define fnCOPY(src, dst)        (src)
#define fnXOR(src, dst)         (src ^ dst)
#define fnCOPYINVERTED(src, dst)(~src)

#define DoRop(result, alu, src, dst) \
{ \
    if (alu == RR_COPY) \
        result = fnCOPY (src, dst); \
    else \
        switch (alu) \
        { \
          case RR_CLEAR: \
            result = fnCLEAR (src, dst); \
            break; \
          case RR_XOR: \
            result = fnXOR (src, dst); \
            break; \
          case RR_COPYINVERTED: \
            result = fnCOPYINVERTED (src, dst); \
            break; \
        } \
}
