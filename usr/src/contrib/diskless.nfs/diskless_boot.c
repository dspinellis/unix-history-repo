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
static char sccsid[] = "@(#)diskless_boot.c	5.1 (Berkeley) 7/16/89";
#endif /* not lint */

#include <stdio.h>
#include <fcntl.h>
#include <netdb.h>
#include <strings.h>
#include <sys/types.h>
#include <sys/param.h>
#include <sys/socket.h>
#include <netinet/in.h>
#ifdef BSD4_4
#include <sys/ucred.h>
#include <sys/mount.h>
#include <net/if.h>
#include <nfs/nfsv2.h>
#include <nfs/nfsdiskless.h>
#else
#include "ucred.h"
#include "mount.h"
#include "if.h"
#include "nfsv2.h"
#include "nfsdiskless.h"
#endif	/* BSD4_4 */
#include "pathnames.h"

/*
 * These two library functions are meant to be called by a net boot server,
 * such as a hacked tftpd, to open and read the boot file being gotten.
 * They expect files to be set up in the /var/diskless subtree for the
 * requesting client.
 * The file /var/diskless/boot.<client> is an ascii file with 1 or 2 lines.
 * line 1: the path name of the vmunix object file to be sent for that client
 * line 2: the path name and file byte offset of the nfs_diskless structure to
 *         be filled in at the given offset (this is optional and the file
 *         is normally created by diskless_setup(8) and the offset is
 *         calculated by diskless_offset(8), run on the vmunix object file.)
 */

/*
 * Static structure for keeping boot file data.
 */
struct diskless_bootdata {
	int	fd;		/* File descriptor for boot file */
	int	offset;		/* Byte offset in file */
	int	ndoff;		/* Byte offset of nfs_diskless */
	int	len;		/* Length of nfs_diskless in bytes */
	struct	nfs_diskless nd; /* nfs_diskless structure itself */
};
static struct diskless_bootdata netb;
#define	LINSIZ	1024

/*
 * Open a vmunix object file for net booting.
 * The one argument is the client's internet address.
 */
open_boot(claddr)
	struct sockaddr_in *claddr;
{
	register char *cp;
	struct hostent *hp;
	FILE *bootf;
	int fd, ret;
	char line[LINSIZ], fname[MAXPATHLEN];

	/*
	 * First, get the official name for this client.
	 */
	if ((hp = gethostbyaddr((caddr_t)&claddr->sin_addr.s_addr,
		sizeof (u_long), AF_INET)) == NULL)
		return (-1);

	/*
	 * Get the boot file for this client.
	 */
	strcpy(fname, _PATH_BOOTF);
	strcat(fname, ".");
	strncat(fname, hp->h_name, MAXPATHLEN - 1 - strlen(fname));
	fname[MAXPATHLEN - 1] = '\0';
	if ((bootf = fopen(fname, "r")) == NULL)
		return (-1);
	if (fgets(line, LINSIZ, bootf) == NULL) {
		fclose(bootf);
		return (-1);
	}
	cp = strtok(line, " \t\n");
	if (cp == (char *)0 || (netb.fd = open(cp, O_RDONLY, 0)) < 0) {
		fclose(bootf);
		return (-1);
	}
	netb.offset = 0;
	netb.ndoff = -1;
	netb.len = sizeof (struct nfs_diskless);
	if (fgets(line, LINSIZ, bootf)) {
		fclose(bootf);
		cp = strtok(line, " \t\n");
		if (cp == (char *)0 || (fd = open(cp, O_RDONLY, 0)) < 0) {
			close(netb.fd);
			return (-1);
		}
		ret = read(fd, &netb.nd, sizeof (struct nfs_diskless));
		close(fd);
		cp = strtok((char *)0, " \t\n");
		if (ret != sizeof (struct nfs_diskless) || cp == (char *)0) {
			close(netb.fd);
			return (-1);
		}
		netb.ndoff = atoi(cp);
	} else
		fclose(bootf);
	return (netb.fd);
}

/*
 * Read the next block of the boot file.
 * Just do a read, except overlay the nfs_diskless structure, as required.
 */
read_boot(fd, buf, siz)
	int fd;
	char *buf;
	int siz;
{
	register int ret, len;
	char *netcp, *bufcp;

	ret = read(fd, buf, siz);
	if (fd != netb.fd || ret <= 0 || netb.ndoff == -1)
		return (ret);
	if (netb.offset < netb.ndoff) {
		netcp = (caddr_t)&netb.nd;
		bufcp = buf + (netb.ndoff - netb.offset);
		len = netb.offset + ret - netb.ndoff;
	} else {
		bufcp = buf;
		netcp = ((caddr_t)&netb.nd) + (netb.offset - netb.ndoff);
		len = netb.ndoff + netb.len - netb.offset;
	}
	if (len > 0)
		bcopy(netcp, bufcp, len);
	netb.offset += ret;
	return (ret);
}
