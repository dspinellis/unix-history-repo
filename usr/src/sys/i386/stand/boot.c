/*-
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * William Jolitz.
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

#ifndef lint
static char copyright[] =
"@(#) Copyright (c) 1990, 1993\n\
	The Regents of the University of California.  All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)boot.c	8.1 (Berkeley) 6/11/93";
#endif /* not lint */

#include <sys/param.h>
#include <sys/reboot.h>

#include <a.out.h>
#include <setjmp.h>
#include <stand/saio.h>

/*
 * Boot program... arguments from lower-level bootstrap determine
 * whether boot stops to ask for system name and which device
 * boot comes from.
 */

char line[100] = UNIX;
extern	int opendev, bootdev, cyloffset;
int	retry = 0;
extern jmp_buf  exception;

main(howto, dev, off)
{
	int io;

	if((dev&B_MAGICMASK) == B_DEVMAGIC) {
		bootdev = dev;
		cyloffset = off;
	} else	goto again;

	if(_setjmp(exception)) {
		close(io);
		printf("- load aborted\n");
again:
		howto = RB_SINGLE|RB_ASKNAME;
		cyloffset = 0; 
	}
		
	for (;;) {
		if (howto & RB_ASKNAME) {
			char *cp;

			printf("Boot: ");
			gets(line);

			/* process additional flags if any */
			if(cp = (char *)index(line, ' ')) {
				howto = strtol (cp, 0, 0);
				*cp = '\0';
			}
			cyloffset = 0;
		} else
			printf("Boot: %s\n", line);

		if (line[0] == 0) {
			strcpy(line, UNIX);
			printf("Boot: %s\n", line);
		}

		io = open(line, 0);
		if (io >= 0) {
			copyunix(io, howto);
			goto again;
		} else if (++retry > 2)
			goto again;
	}
}

/*ARGSUSED*/
copyunix(io, howto)
	register io;
{
	struct exec x;
	int i;
	char *addr,c;

	i = read(io, (char *)&x, sizeof x);
	if (i != sizeof x ||
	    (x.a_magic != 0407 && x.a_magic != 0413 && x.a_magic != 0410)) {
		printf("Bad format\n");
		return;
	}

	printf("%d", x.a_text);
	if (x.a_magic == 0413 && lseek(io, 0x400, 0) == -1)
		goto shread;
	if (read(io, (char *)0, x.a_text) != x.a_text)
		goto shread;

	addr = (char *)x.a_text;
	if (x.a_magic == 0413 || x.a_magic == 0410)
		while ((int)addr & CLOFSET)
			*addr++ = 0;
	printf("+%d", x.a_data);
	if (read(io, addr, x.a_data) != x.a_data)
		goto shread;

	addr += x.a_data;
	printf("+%d", x.a_bss);
	x.a_bss += 128*512;	/* slop */
	for (i = 0; i < x.a_bss; i++)
		*addr++ = 0;

	/* mask high order bits corresponding to relocated system base */
	x.a_entry &= 0x000fffff;
	printf(" start 0x%x\n", x.a_entry);

	if(c=scankbd())
		_longjmp(&exception,1);

	i = (*((int (*)()) x.a_entry))(howto, opendev, 0, cyloffset);

	if (i) printf("exit %d\n", i) ; 
	return;
shread:
	printf("Short read\n");
	return;
}
