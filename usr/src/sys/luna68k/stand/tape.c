/*
 * Copyright (c) 1992 OMRON Corporation.
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * OMRON Corporation.
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
 *	@(#)tape.c	8.1 (Berkeley) 6/10/93
 */

/*
 * tape.c -- operation commands for TAPE unit.
 * by A.Fujita, APR-14-1992
 */

#include <sys/param.h>
#include <luna68k/stand/status.h>

dev_t  rst0 = 0x0000;
dev_t nrst0 = 0x0004;

u_char buff[512];

int
tape(argc, argv)
	int   argc;
	char *argv[];
{
	int size, count;
	u_long *p = (u_long *) buff;

	if (!strcmp(argv[1], "read")) {
		count = 0;
		while ((size = stread(rst0, buff, 512)) == 512)
			count++;
		printf("tape: size  = %d\n", size);
		printf("tape: count = %d\n", count);
	} else if (!strcmp(argv[1], "write")) {
		for (count = 0; count < 500; count++) {
			if ((size = stwrite(rst0, buff, 512)) != 512)
				break;
		}
		printf("tape: size  = %d\n", size);
		printf("tape: count = %d\n", count);
	} else if (!strcmp(argv[1], "rewind")) {
		st_rewind(rst0);
	} else if (!strcmp(argv[1], "weof")) {
		st_write_EOF(rst0);
	} else if (!strcmp(argv[1], "skip")) {
		st_skip(rst0);
	} else {
		return(ST_ERROR);
	}

	return(ST_NORMAL);
}
