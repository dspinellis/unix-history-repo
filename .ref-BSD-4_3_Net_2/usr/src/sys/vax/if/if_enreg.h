/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
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
 *
 *	@(#)if_enreg.h	7.3 (Berkeley) 6/28/90
 */

/*
 * Xerox experimental ethernet registers.
 *
 * N.B.: status register and device address are read/write,
 * device address is read-only, rest are WRITE ONLY!
 */
struct endevice {
	short	en_owc;		/* output word count (10 bits) */
	short	en_oba;		/* output buffer address */
	short	en_ostat;	/* output control and status */
	short	en_odelay;	/* output start delay, 25usec units  */
	short	en_iwc;		/* input word count */
	short	en_iba;		/* input buffer address */
	short	en_istat;	/* input csr */
	short	en_addr;	/* ~device address (low 8 bits) */
};

/*
 * Control and status bits.
 */
#define EN_IERROR	0x8000		/* CRC error, buf ovflo or overrun */
#define	EN_OERROR	0x8000		/* collision or output underrun */
#define EN_OPDONE	0x0080		/* previous operation completed */
#define EN_IEN		0x0040		/* enable interrupt when DONE */
#define	EN_PROMISCUOUS	0x0002		/* promiscuous, input any packet */
#define EN_GO		0x0001		/* start op bit */

#define	EN_BITS	"\10\20ERR\10OPDONE\7IEN\2PROM\1GO"

#define	spl_enet()	spl5()
