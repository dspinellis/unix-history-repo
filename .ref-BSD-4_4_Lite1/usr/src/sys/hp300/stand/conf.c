/*
 * Copyright (c) 1982, 1986, 1990, 1993
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
 *	@(#)conf.c	8.1 (Berkeley) 6/10/93
 */

#include <sys/param.h>
#include <stand.att/saio.h>

extern int	nullsys(), nodev(), noioctl();

#ifdef BOOT
#define	ctstrategy	nullsys
#define	ctopen		nodev
#define	ctclose		nullsys
#else
int	ctstrategy(), ctopen(), ctclose();
#endif
#define	ctioctl	noioctl

int	rdstrategy(), rdopen();
#define	rdioctl	noioctl

int	sdstrategy(), sdopen();
#define	sdioctl	noioctl


struct devsw devsw[] = {
	{ "ct",	ctstrategy,	ctopen,	ctclose,	ctioctl }, /*0*/
	{ "??",	nullsys,	nodev,	nullsys,	noioctl }, /*1*/
	{ "rd",	rdstrategy,	rdopen,	nullsys,	rdioctl }, /*2*/
	{ "??",	nullsys,	nodev,	nullsys,	noioctl }, /*3*/
	{ "sd",	sdstrategy,	sdopen,	nullsys,	sdioctl }, /*4*/
};

int	ndevs = (sizeof(devsw)/sizeof(devsw[0]));

/*
 * Convert old style unit syntax into adaptor/controller/unit
 */
devconvert(io)
	register struct iob *io;
{
	if (io->i_unit == 0 || io->i_adapt || io->i_ctlr)
		return;
	io->i_adapt = io->i_unit / 8;
	io->i_ctlr = io->i_unit % 8;
	io->i_unit = 0;
}
