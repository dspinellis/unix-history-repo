/*-
 * Copyright (c) 1991 The Regents of the University of California.
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
 *	from: @(#)ns16550.h	7.1 (Berkeley) 5/9/91
 *	$Id: ns16550.h,v 1.2 1993/10/16 13:48:52 rgrimes Exp $
 */

/*
 * NS16550 UART registers
 */
#ifdef PC98
#define	com_data	0x000	/* data register (R/W) */
#define	com_dlbl	0x000	/* divisor latch low (W) */
#define	com_dlbh	0x100	/* divisor latch high (W) */
#define	com_ier		0x100	/* interrupt enable (W) */
#define	com_iir		0x200	/* interrupt identification (R) */
#define	com_fifo	0x200	/* FIFO control (W) */
#define	com_lctl	0x300	/* line control register (R/W) */
#define	com_cfcr	0x300	/* line control register (R/W) */
#define	com_mcr		0x400	/* modem control register (R/W) */
#define	com_lsr		0x500	/* line status register (R/W) */
#define	com_msr		0x600	/* modem status register (R/W) */
#else /* IBM-PC */
#define	com_data	0	/* data register (R/W) */
#define	com_dlbl	0	/* divisor latch low (W) */
#define	com_dlbh	1	/* divisor latch high (W) */
#define	com_ier		1	/* interrupt enable (W) */
#define	com_iir		2	/* interrupt identification (R) */
#define	com_fifo	2	/* FIFO control (W) */
#define	com_lctl	3	/* line control register (R/W) */
#define	com_cfcr	3	/* line control register (R/W) */
#define	com_mcr		4	/* modem control register (R/W) */
#define	com_lsr		5	/* line status register (R/W) */
#define	com_msr		6	/* modem status register (R/W) */
#endif /* PC98 */
