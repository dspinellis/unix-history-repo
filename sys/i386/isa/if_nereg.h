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
 *	@(#)if_nereg.h	7.1 (Berkeley) 5/9/91
 */

/*
 * NE2000 Ethernet Card registers
 */

/* The NE1000/NE2000 uses a DS8390 Ethernet controller in at the beginning of
   its i/o space */
#include "ic/ds8390.h"

#define ne_data		0x10	/* Data Transfer port */
#define ne_reset	0x1f	/* Card Reset port */

#define        PKTSZ   0x600
#define        TBUF(board)     (0x2000 * (board))      /* Starting location of Transmit Buffer */
#define        RBUF(board)     (TBUF(board)+PKTSZ)     /* Starting location of Receive Buffer */
#define        RBUFEND(board)  (0x4000 * (board))      /* Ending location of Tr ansmit Buffer */

#define        TBUF1           TBUF(1)         /* Starting location of Transmit Buffer */
#define        RBUF1           RBUF(1)         /* Starting location of Receive Buffer */
#define        RBUFEND1        RBUFEND(1)      /* Ending location of Transmit Buffer */
#define        TBUF2           TBUF(2)         /* Starting location of Transmit Buffer */
#define        RBUF2           RBUF(2)         /* Starting location of Receive Buffer */
#define        RBUFEND2        RBUFEND(2)      /* Ending location of Transmit Buffer */

