/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Rick Macklem at The University of Guelph.
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
char copyright[] =
"@(#) Copyright (c) 1989 Regents of the University of California.\n\
 All rights reserved.\n";
#endif not lint

#ifndef lint
static char sccsid[] = "@(#)diskless_offset.c	5.1 (Berkeley) 7/16/89";
#endif /* not lint */

#include <stdio.h>
#include <a.out.h>
#include <fcntl.h>
#include <sys/types.h>
#include "param.h"

/*
 * This program looks up the nfs_diskless symbol in a vmunix image and
 * prints out the byte offset in the file for it.
 * The trick is that it must be compiled so that it knows how to handle
 * the target architecture for that vmunix.
 * (Unfortunately, for the pmax, this also implies knowing about Coff.)
 *
 * Define COFF if the vmunix is in Coff format. This will also require
 * linking to an nlist() that knows Coff. (I just compile it on a RISC/Ultrix
 * system) nb: Other Coff machines may need different .h files.
 *
 * You must compile it with a -I argument that points to the target
 * architecture's sys/machine/include directory.
 * Because of the above hassles, it has been kept as a separate program instead
 * of integrating it into diskless_setup.
 */

/*
 * In the bsd a.out.h, there is a different def'n of struct nlist, so we
 * fiddle about.
 */
#ifdef COFF
#define	nl_name	n_name
#else
#define	nl_name	n_un.n_name
#endif

#ifndef CLBYTES
#define	CLBYTES		(NBPG * CLSIZE)
#endif

main(argc, argv)
	int argc;
	char *argv[];
{
	int fd;
	u_long offset, textsize, dataoff;
#ifdef COFF
	struct filehdr filehdr;
	struct aouthdr aout;
#else
	struct exec aout;
#endif
	struct nlist nl[2];

	if (argc != 2) {
		fprintf(stderr, "No Exec file!\n");
		exit(1);
	}
	nl[0].nl_name = "_nfs_diskless";
	nl[1].nl_name = "";
	if (nlist(argv[1], nl) != 0) {
		fprintf(stderr, "Can't find nfs_diskless symbol\n");
		exit(1);
	}
	if ((fd = open(argv[1], O_RDONLY, 0)) < 0) {
		fprintf(stderr, "Can't open %s\n", argv[1]);
		exit(1);
	}
#ifdef COFF
	if (read(fd, &filehdr, sizeof (filehdr)) != sizeof (filehdr)) {
		fprintf(stderr, "Can't read a.out header\n");
		exit(1);
	}
#endif
	if (read(fd, &aout, sizeof (aout)) != sizeof (aout)) {
		fprintf(stderr, "Can't read a.out header\n");
		exit(1);
	}
	close(fd);

	/*
	 * First, calculate the offset of nfs_diskless within the .data
	 * segment.
	 */
#ifdef COFF
	dataoff = ((u_long)nl[0].n_value) - aout.data_start;
#else
	/*
	 * Does this work for all bsd ports?? I have only tested vax.
	 */
	textsize = ((aout.a_text + CLBYTES - 1) / CLBYTES) * CLBYTES;
	dataoff = (nl[0].n_value & ~KERNBASE) - textsize;
#endif

	/*
	 * Now, calculate the offset within the object file for that
	 * data offset.
	 */
#ifdef COFF
	offset = N_TXTOFF(filehdr, aout) + aout.tsize + dataoff;
#else
	offset = N_TXTOFF(aout) + aout.a_text + dataoff;
#endif
	printf("%d\n", offset);
}
