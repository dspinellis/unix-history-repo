/*
 * Copyright (c) 1982, 1986, 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution is only permitted until one year after the first shipment
 * of 4.4BSD by the Regents.  Otherwise, redistribution and use in source and
 * binary forms are permitted provided that: (1) source distributions retain
 * this entire copyright notice and comment, and (2) distributions including
 * binaries display the following acknowledgement:  This product includes
 * software developed by the University of California, Berkeley and its
 * contributors'' in the documentation or other materials provided with the
 * distribution and in all advertising materials mentioning features or use
 * of this software.  Neither the name of the University nor the names of
 * its contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)ls.c	7.9 (Berkeley) 6/28/90
 */

#include "sys/param.h"
#include "ufs/dir.h"
#include "saio.h"
#include "sys/ttychars.h"

main()
{
	struct dinode *ip;
	int fd;

	for (;;) {
		if ((fd = getfile("ls", 0)) == -1)
			exit();
		ip = &iob[fd - 3].i_ino;
		if ((ip->di_mode & IFMT) != IFDIR) {
			printf("ls: not a directory\n");
			continue;
		}
		if (ip->di_size == 0) {
			printf("ls: zero length directory\n");
			continue;
		}
		ls(fd);
	}
}

#define CTRL(x)	(x&037)

getfile(prompt, mode)
	char *prompt;
	int mode;
{
	int fd;
	char buf[100];

	do {
		printf("%s: ", prompt);
		gets(buf);
		if (buf[0] == CTRL('d') && buf[1] == 0)
			return (-1);
	} while ((fd = open(buf, mode)) <= 0);
	return(fd);
}

typedef struct direct	DP;
static
ls(fd)
	register int fd;
{
	register int size;
	register char *dp;
	char dirbuf[DIRBLKSIZ];

	printf("\ninode\tname\n");
	while ((size = read(fd, dirbuf, DIRBLKSIZ)) == DIRBLKSIZ)
		for(dp = dirbuf; (dp < (dirbuf + size)) &&
		    (dp + ((DP *)dp)->d_reclen) < (dirbuf + size);
		    dp += ((DP *)dp)->d_reclen) {
			if (((DP *)dp)->d_ino == 0)
				continue;
			if (((DP *)dp)->d_namlen > MAXNAMLEN+1) {
				printf("Corrupt file name length!  Run fsck soon!\n");
				return;
			}
			printf("%d\t%s\n", ((DP *)dp)->d_ino,
			    ((DP *)dp)->d_name);
		}
}
