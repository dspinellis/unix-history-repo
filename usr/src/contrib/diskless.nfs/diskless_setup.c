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
static char sccsid[] = "@(#)diskless_setup.c	5.1 (Berkeley) 7/16/89";
#endif /* not lint */

#include <stdio.h>
#include <fcntl.h>
#include <netdb.h>
#include <strings.h>
#include <sys/types.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <pwd.h>
#include <grp.h>
#ifdef BSD4_4
#include <sys/socket.h>
#include <sys/ucred.h>
#include <sys/mount.h>
#include <net/if.h>
#include <netinet/in.h>
#include <nfs/nfsv2.h>
#include <nfs/nfs.h>
#include <nfs/nqnfs.h>
#include <nfs/nfsdiskless.h>
#else
#include "socket.h"
#include "ucred.h"
#include "mount.h"
#include "if.h"
#include "in.h"
#include "nfsv2.h"
#include "nfs.h"
#include "nqnfs.h"
#include "nfsdiskless.h"
#endif	/* BSD4_4 */
#include "pathnames.h"

struct nfs_args def_args = {
	(struct sockaddr *)0,
	sizeof (struct sockaddr_in),
	SOCK_DGRAM,
	0,
	(nfsv2fh_t *)0,
	0,
	NFS_WSIZE,
	NFS_RSIZE,
	NFS_TIMEO,
	NFS_RETRANS,
	NFS_MAXGRPS,
	NFS_DEFRAHEAD,
	NQ_MAXLEASE,
	NQ_DEADTHRESH,
	(char *)0,
};
void nfsargs_hton(), fatal_err();

/*
 * Setup of server to handle a diskless/dataless client. Most of the work is
 * just filling in all the fields of the nfs_diskless data structure.
 * This structure is then either written to a file named "diskless.<host>"
 * or stuffed into the client's vmunix file at the specified offset.
 * For now it is a bare bones tool with command line options only.
 * NB: It will need to be compiled with the sys/<files>.h for a 4.4BSD
 *     kernel, although it should be runable on most nfs Unix servers.
 *     (Except <sys/types.h> and <sys/param.h> which should be native
 *      files for the server OS)
 */

main(argc, argv)
	int argc;
	char *argv[];
{
	int fd = -1, c, i, j, sblks, sfd;
	register struct hostent *hp;
	register struct sockaddr_in *claddr, *cmaddr, *cbaddr;
	struct nfs_diskless nd;
	struct stat sb;
	struct passwd *pwd;
	struct group *grp;
	struct timeval tv;
	char **cpp;
	short ngroups;
	u_long taddr;
	char fname[MAXPATHLEN], servernm[MAXHOSTNAMELEN], *cp, buf[10240];
	u_char fh[100];
	extern int optind;
	extern char *optarg;

	/*
	 * First initialize nd with all the defaults.
	 */
	if (gethostname(servernm, MAXHOSTNAMELEN) < 0)
		fatal_err("Can't get server host name\n");
	if ((hp = gethostbyname(servernm)) == NULL)
		fatal_err("Can't get host addr\n");
	nfsargs_hton(&def_args);
	nd.swap_args = def_args;
	nd.swap_saddr.sin_addr.s_addr = *((u_long *) *hp->h_addr_list);
	nd.swap_saddr.sin_len = sizeof (struct sockaddr_in);
	nd.swap_saddr.sin_family = AF_INET;
	nd.swap_saddr.sin_port = htons(NFS_PORT);
	strncpy(nd.swap_hostnam, hp->h_name, MNAMELEN - 1);
	nd.swap_hostnam[MNAMELEN - 1] = '\0';
	nd.swap_nblks = 0;
	nd.root_args = def_args;
	nd.root_saddr.sin_len = sizeof (struct sockaddr_in);
	nd.root_saddr.sin_family = AF_INET;
	nd.root_saddr.sin_addr.s_addr = *((u_long *) *hp->h_addr_list);
	nd.root_saddr.sin_port = htons(NFS_PORT);
	strncpy(nd.root_hostnam, hp->h_name, MNAMELEN - 1);
	nd.root_hostnam[MNAMELEN - 1] = '\0';

	/*
	 * Put the current time in the structure. This should be replaced
	 * by the bootstrap server, if possible.
	 */
	gettimeofday(&tv, (struct timezone *)0);
	nd.root_time = htonl(tv.tv_sec);
	if (*argv[argc - 1] == '-' ||
		(hp = gethostbyname(argv[argc - 1])) == NULL)
		fatal_err("Can't get client addr\n");

	if (hp->h_addrtype != AF_INET || *hp->h_addr_list == NULL)
		fatal_err("Bad client hostent\n");
	strncpy(nd.my_hostnam, hp->h_name, MAXHOSTNAMELEN);
	nd.my_hostnam[MAXHOSTNAMELEN - 1] = '\0';
	claddr = (struct sockaddr_in *)&nd.myif.ifra_addr;
	cmaddr = (struct sockaddr_in *)&nd.myif.ifra_mask;
	cbaddr = (struct sockaddr_in *)&nd.myif.ifra_broadaddr;
	claddr->sin_len = sizeof (struct sockaddr_in);
	claddr->sin_family = AF_INET;
	claddr->sin_port = 0;
	cmaddr->sin_len = sizeof (struct sockaddr_in);
	cmaddr->sin_family = AF_INET;
	cmaddr->sin_port = 0;
	cbaddr->sin_len = sizeof (struct sockaddr_in);
	cbaddr->sin_family = AF_INET;
	cbaddr->sin_port = 0;
	claddr->sin_addr.s_addr = *((u_long *) *hp->h_addr_list);
	taddr = ntohl(claddr->sin_addr.s_addr);

	/*
	 * Default net mask to internet address class. Can be overridden
	 * by using the -m option.
	 */
	if (IN_CLASSA(taddr))
		cmaddr->sin_addr.s_addr = inet_addr("255.0.0.0");
	else if (IN_CLASSB(taddr))
		cmaddr->sin_addr.s_addr = inet_addr("255.255.0.0");
	else
		cmaddr->sin_addr.s_addr = inet_addr("255.255.255.0");
	nd.myif.ifra_name[0] = '\0';
	nd.mygateway.sin_len = 0;

	/*
	 * Now, process all the command line options and override defaults.
	 */
	while ((c = getopt(argc, argv, "os:h:m:a:g:l:x:r:c:")) != EOF) {
		switch (c) {
		case 'o':
			fd = 1;
			break;
		case 's':
			sblks = atoi(optarg);
			if (sblks < 0 || sblks > 500000)
				fatal_err("Swap size out of range\n");
			strcpy(fname, _PATH_SWAP);
			strcat(fname, ".");
			strcat(fname, hp->h_name);
			if ((sfd = open(fname, O_WRONLY | O_CREAT | O_TRUNC,
				0600)) < 0)
				fatal_err("Can't create swapfile\n");
			if (fchmod(sfd, 0600) < 0)
				fatal_err("Can't chmod swap file\n");
			if (fchown(sfd, -2, -2) < 0)
				fatal_err("Can't chown swap file\n");
			for (i = 0; i < sblks; ) {
				j = MIN(sblks - i, 20);
				if (write(sfd, buf, j * 512) < 0)
					fatal_err("Swapfile out of space\n");
				i += j * 512;
			}
			close(sfd);
			nd.swap_nblks = htonl(sblks);
			bzero((caddr_t)fh, 100);
			if (getfh(fname, fh) < 0)
				fatal_err("Can't get swap fh\n");
			bcopy((caddr_t)fh, nd.swap_fh, NFS_FHSIZE);
			break;
		case 'h':
			if ((taddr = inet_addr(optarg)) == -1)
				fatal_err("Bad -h addr\n");
			claddr->sin_addr.s_addr = taddr;
			break;
		case 'm':
			if ((taddr = inet_addr(optarg)) == -1)
				fatal_err("Bad -m addr\n");
			cmaddr->sin_addr.s_addr = taddr;
			break;
		case 'a':
			if ((taddr = inet_addr(optarg)) == -1)
				fatal_err("Bad -a addr\n");
			nd.swap_saddr.sin_addr.s_addr = taddr;
			nd.root_saddr.sin_addr.s_addr = taddr;
			break;
		case 'g':
			if ((taddr = inet_addr(optarg)) == -1)
				fatal_err("Bad -g addr\n");
			nd.mygateway.sin_addr.s_addr = taddr;
			nd.mygateway.sin_len = sizeof (struct sockaddr_in);
			nd.mygateway.sin_family = AF_INET;
			nd.mygateway.sin_port = 0;
			break;
		case 'l':
			strncpy(nd.myif.ifra_name, optarg, IFNAMSIZ - 1);
			nd.myif.ifra_name[IFNAMSIZ - 1] = '\0';
			break;
		case 'x':
			if ((cp = index(optarg, ':')) == (char *)0)
				fatal_err("No : in -x arg\n");
			*cp++ = '\0';
			i = atoi(cp);
			if ((fd = open(optarg, O_RDWR, 0)) < 0)
				fatal_err("Can't open for -x\n");
			if (i < 0 || fstat(fd, &sb) < 0 ||
				i > (sb.st_size - sizeof (struct nfs_diskless)))
				fatal_err("Bad -x arg\n");
			lseek(fd, i, 0);
			break;
		case 'r':
			bzero((caddr_t)fh, 100);
			if (getfh(optarg, fh) < 0)
				fatal_err("Can't get root fh\n");
			bcopy((caddr_t)fh, nd.root_fh, NFS_FHSIZE);
			break;
		case 'c':
			if (pwd = getpwnam(optarg)) {
			    nd.swap_ucred.cr_uid = htonl(pwd->pw_uid);
			    nd.swap_ucred.cr_groups[0] = htonl(pwd->pw_gid);
			    ngroups = 1;
			    setgrent();
			    while (grp = getgrent()) {
				if (grp->gr_gid == pwd->pw_gid)
				    continue;
				cpp = grp->gr_mem;
				while (*cpp) {
				    if (!strcmp(*cpp, pwd->pw_name))
					break;
				    cpp++;
				}
				if (*cpp) {
				    nd.swap_ucred.cr_groups[ngroups++] =
					htonl(grp->gr_gid);
				    if (ngroups == NGROUPS)
					break;
				}
			    }
			    endgrent();
			    nd.swap_ucred.cr_ngroups = htons(ngroups);
			} else
			    fatal_err("Swap_username bad\n");
			break;
		default:
			fprintf(stderr, "Bad arg %c\n", c);
			exit(1);
		};
	}

	/*
	 * Finish up and write it out.
	 */
	cbaddr->sin_addr.s_addr =
		(claddr->sin_addr.s_addr & cmaddr->sin_addr.s_addr) |
		 ~(cmaddr->sin_addr.s_addr);
	if (fd == -1) {
		strcpy(fname, _PATH_DISKLESS);
		strcat(fname, ".");
		strcat(fname, hp->h_name);
		if ((fd = open(fname, O_CREAT | O_TRUNC | O_WRONLY, 0600)) < 0) 
			fatal_err("Can't create diskless file\n");
	}
	if (write(fd, &nd, sizeof (nd)) != sizeof (nd))
		fatal_err("Can't write diskless file\n");
}

/*
 * Convert the integer fields of the nfs_args structure from host byte order
 * to net byte order.
 */
void
nfsargs_hton(nfsp)
	register struct nfs_args *nfsp;
{

	nfsp->sotype = htonl(nfsp->sotype);
	nfsp->proto = htonl(nfsp->proto);
	nfsp->flags = htonl(nfsp->flags);
	nfsp->wsize = htonl(nfsp->wsize);
	nfsp->rsize = htonl(nfsp->rsize);
	nfsp->timeo = htonl(nfsp->timeo);
	nfsp->retrans = htonl(nfsp->retrans);
	nfsp->maxgrouplist = htonl(nfsp->maxgrouplist);
	nfsp->readahead = htonl(nfsp->readahead);
	nfsp->leaseterm = htonl(nfsp->leaseterm);
	nfsp->deadthresh = htonl(nfsp->deadthresh);
}

/*
 * Just print out an error message and exit.
 */
void
fatal_err(str)
	char *str;
{

	fprintf(stderr, str);
	exit(1);
}
