/*
 * Copyright (c) 1986 MICOM-Interlan, Inc., Boxborough Mass.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 */
#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1986 MICOM-Interlan, Inc., Boxborough Mass.\n\
 All rights reserved.\n";
#endif not lint

#ifndef lint
static char sccsid[] = "@(#)npdump.c	6.2 (Berkeley) 2/20/86";
#endif not lint

#include <stdio.h>
#include <sys/file.h>
#include "npcmd.h"
#include <sys/ioctl.h>

extern int errno;

#define IMAGESIZE (1024 * 256)

main(argc,argv)
int argc;
char **argv;
{

	int	totalread;		/* Total byte count of device reads */
	int	ed;			/* Device's file descriptor */
	int	fd;			/* Dumpfile device descriptor */
	int	nread;			/* Value returned from read() call */
	int 	nwritten;		/* Value returned from write() call */
	char	*fname;
	char	ibuf[1024];
	char	*devname = "/dev/np00";

	
	switch (argc) {
	case 3:
		/* Pathname for device to be dumped */
		devname = argv[2];
	case 2:
		/* Name of the dump file */
		fname = argv[1];
		break;
	default:
		printf("usage: npdump dumpfile [device]\n");
		exit(1);
	}

	/* Open the device to be dumped */

	if ((ed = open(devname, O_RDWR)) == -1) {
		char fullpath[50];
		(void) sprintf(fullpath, "/dev/%s", devname);
		if ((ed = open(devname,O_RDWR)) == -1) {
			fprintf(stderr,
				"%s unable to open device %s errno = %d\n",
				argv[0], devname, errno);
			exit(2);
		}
	}

	/* Open/create the dump file */

	if ((fd = open(fname, O_RDWR | O_CREAT)) == -1) {
		fprintf(stderr,"%s: unable to open file %s errno = %d\n",
		    argv[0], fname, errno);
		exit(2);
	}


	/* Read from the device and write to the dump file */
	
	totalread = 0;

	while (totalread < IMAGESIZE) {

		if ((nread = read(ed,ibuf,1024)) > 0) { 
		
			totalread += nread;

			nwritten = write(fd,ibuf,nread);

			if (nwritten != nread) {
				fprintf(stderr,"Bad write to %s errno = %d\n",
			    	argv[2],errno);
				exit(7);
			}
		}

		else {
			fprintf(stderr,"Bad read from %s errno = %d\n", argv[0],errno);
			exit(7);
			
		}
	}

	close(fd);
	close(ed);

	exit(0);
}
