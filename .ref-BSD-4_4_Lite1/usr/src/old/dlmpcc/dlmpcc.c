/*
 * Copyright (c) 1988 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
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
"@(#) Copyright (c) 1988 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)dlmpcc.c	5.6 (Berkeley) 1/17/91";
#endif /* not lint */

/*
 * MPCC Download and Configuration Program.
 */
#include <sys/ioctl.h>
#include <sys/types.h>
#include <tahoe/vba/mpreg.h>
#include <fcntl.h>
#include <errno.h>
#include <stdio.h>
#include <ctype.h>
#include "scnhdr.h"
#include "pathnames.h"

#define MAXMPCC 16

char	*MPCCTAB = _PATH_MPCCTAB;
int	resetflg = 0;

main(argc, argv)
	char *argv[];
{
	int bd;

	if (argc == 1) {
		for (bd = 0; bd < MAXMPCC; bd++)
			if (bldmap(bd) != -1)
				download(bd);
		exit(0);
	}
	for (argc--, argv++; argc > 0; argc--, argv++) {
		bd = atoi(argv[0]);
		if (strcmp(argv[0], "-r") == 0) {
			resetflg = 1;
			continue;
		}
		if (bd > MAXMPCC || bd < 0) {
			printf("Illegal Board Number=> %d\n", bd);
			continue;
		}
		if (bldmap(bd) == -1)
			continue;
		download(bd);
	}
	exit(0);
}

/*	
 * Build Load Module Map
 */
struct  bdcf cf;
struct	abdcf bdasy;

#define LINESIZE 128

bldmap(dlbd)
	int dlbd;		/* board to be downloaded */
{
	FILE *tabfp;
	int bd, port, count;
	char *bdstr, *strtok(), protocol, line[LINESIZE];
	char *lptr, *lptr1, *lptr2;
	
	protocol = '\0';
	/* open the configuration file for reading */
	if ((tabfp = fopen(MPCCTAB, "r")) == NULL) {
		printf("No Configuration File: %s\n", MPCCTAB);
		return (-1);
	}
	for (;;) {
		if (fgets(&line[0], LINESIZE-1, tabfp) == NULL) {
			fclose(tabfp);
			return (-1);
		}
		count++;
		line[strlen(line)-1] = '\0';
		lptr = strtok(line, ':');
		if (tolower(*lptr) != 'm')
			continue;
		lptr = strtok((char *)0, ':');
		bd = atoi(lptr);
		if (bd == dlbd)
			break;
	}
	cf.fccstimer = 20;      /* default to 1 sec (20 * 50ms) */
	cf.fccsports = 0;       /* no ports are fccs */
	cf.fccssoc = 0;         /* no ports switch on close */
	for (port = 0; port < MPMAXPORT; port++)
		cf.protoports[port] = MPPROTO_UNUSED;
	/* check for the keywords following the board number */
	lptr1 = (char *)0;
	lptr2 = (char *)0;
	while (*lptr) {
		lptr = strtok((char *)0, ':');
		if (!strncmp(lptr, "FCCS", 4)) {
			lptr1 = lptr;
			continue;
		}
		if (!strncmp(lptr, "SOC", 3)) {
			lptr2 = lptr;
			continue;
		}
	}
	/* process the board and port characteristics */
	while (fgets(&line[0], LINESIZE-1, tabfp) != NULL) {
		count++;
		line[strlen(line)-1] = '\0';
		if (!line[0])		/* if newline only */
			continue;
		lptr = strtok(line, ':');
		if (tolower(*lptr) == 'm')
			break;
		if (*lptr == '#')	/* ignore comment */
			continue;
		if (tolower(*lptr) == 'p' && tolower(*(lptr+1)) == 'o') {
			/* PORT */
			port = atoi(lptr = strtok((char *)0, ':'));
			protocol = *(lptr = strtok((char *)0, ':'));
			switch (cf.protoports[port] = protocol) {
			case '3' :		/* ASYNCH 32 port */
			case 'A' :		/* ASYNCH */
				break;
			case 'B':		/* BISYNCH */
				break;
			case 'S':		/* SDLC */
				snapargs(port, lptr);
				break;
			case 'X':		/* X25 */
				x25pargs(port, lptr);
				break;
			default:
				printf(
"No protocol specified on PROTOCOL line in configuration file %s:%d: %s\n",
				    MPCCTAB, count, line);
				protocol = 'A';
				break;
			}
			continue;
		}
		if (tolower(*lptr) == 'p' && tolower(*(lptr+1)) == 'r') {
			/* PROTOCOL */
#ifdef notdef
			if(protocol) {
				printf(
"second protocol specified on PROTOCOL line in configuration file %s:%d: %s\n",
				    MPCCTAB, count, line);
				continue;
			}
#endif
			lptr = strtok((char *) 0, ':');
			switch (protocol = *lptr) {
			case '3':		/* ASYNCH 32 port */
			case 'A':		/* ASYNCH */
				asybargs(lptr);
				break;
			case 'B':		/* BISYNCH */
				break;
			case 'S':		/* SDLC */
				snabargs(lptr);
				break;
			case 'X':		/* X25 */
				x25bargs(lptr);
				break;
			default:
				printf(
"No protocol specified on PROTOCOL line in configuration file %s:%d: %s\n",
				    MPCCTAB, count, line);
				protocol = 'A';
				break;
			}
			continue;
		}
		printf("Error in configuration file %s,line %d, %s\n",
		    MPCCTAB, count, line);
	}
	fclose(tabfp);
	mkldnm();
	return (0);
}

/*
 * decode x25 arguments for board
 *
 * for X.25, the arguments are N1, N2, T1, T2, T3, T4, K).
 */
x25bargs(args)
	char *args;
{
}

/*
 * decode sna arguments for board
 * for SNA, the arguments are N1, N2, T1, T2, T3, T4, K).
 */
snabargs(args)
	char *args;
{
}

/*
 * decode async arguments for board
 */
asybargs(args)
char *args;
{

	bdasy.xmtbsz = atoi(strtok((char *)0, ':'));
}

/*
 * decode x25 arguments for port
 */
x25pargs(port,args)
	int port;
	char *args;
{
}

/*
 * decode sna arguments for port
 */
snapargs(port, args)
	int port;
	char *args;
{
}

gethi()
{
	int i;

	for (i = MPMAXPORT-1; i >= 0 && cf.protoports[i] == 0; i--)
		;
	return (i);
}

getlo()
{
	int i;

	for (i = 0; i < MPMAXPORT && cf.protoports[i] == 0; i++)
		;
	return (i);
}

prntmap(board)
	int board;
{
	int j;

	printf("\nMPCC #: %d\n", board);
	for (j = 0; j < MPMAXPORT; j++) {
		printf("port: %d  %c", j, cf.protoports[j]);
		switch (cf.protoports[j]) {
		case '3': case 'A':
			printf("\n");
			break;
		case 'B':
			break;
		case 'S':
			break;
		case 'X':
			break;
		default:
			printf("Unused\n");
			break;
		}
	}
	printf("ldname: %s, ", cf.loadname);
	printf("hiport: %d, loport: %d\n", gethi(), getlo());
	if (cf.fccsports != 0)
		printf("FCCS\n");
	switch (cf.protoports[0]) {
	case '3': case 'A':
		printf("xmtsize: %d\n", bdasy.xmtbsz);
		break;
	case 'B':
		break;
	case 'S':
		break;
	case 'X':
		break;
	}
	printf("protoports: %s\n", cf.protoports);
}

/*
 * Make Load Module Name
 *
 * if any port is 'ASYNCH"
 * 	add 'a' to load module name
 * if any port is 'BISYNCH'
 * 	add 'b' to load module name
 * if any port is 'SDLC'
 * 	add 's' to load module name
 * if any port is 'X25'
 * 	add 'x' to load module name
 */
mkldnm()
{
	static char *pcols = "ABSX3";
	char *proto;
	int j, offset;

	offset = 0;
	for (proto = pcols; *proto; proto++) {
		for (j = 0; j < MPMAXPORT; j++) {
			if (cf.protoports[j] == *proto) {
				if (*proto == '3')
					cf.loadname[offset] = '3';
				else
					cf.loadname[offset] = tolower(*proto);
				offset++;
				break;
			}
		}
		cf.loadname[offset] = '\0';
	}
}

/*
 * if a string is passed as an argument,
 * 	save it in the local string area
 * 	set the local index to the start of the string
 * else
 * 	set start to the current character in the string
 * 	while the character is not the separator,
 * 		and the character is not NULL
 * 			skip the character
 */
static
char *
strtok(s, c)
	char *s, c;
{
	static char locals[LINESIZE];
	static int i;
	char *start;

	if (s != 0) {
		strcpy(locals, s);
		i = 0;
	}
	for (start = &locals[i] ; locals[i] && locals[i] != c; i++)
		;
	if (locals[i]) {
		locals[i] = '\0';
		i++;
	}
	while (*start == ' ')
		start++;
	return (start);
}

short	bits[] = { 1, 2, 4, 8, 16, 32, 64, 128 };
fccs(line, tptr, pptr)
	char *line, *tptr, *pptr;
{
	u_short ports, num, time;

	ports = 0;
	line = strtok(line, ',');
	while (*(line = strtok((char *) 0, ',')) != '\0') {
		num = (short) atoi(line);
		if (num >= 0 && num < 8)
			ports |= bits[num];
		else if (num >= 50 && num < 6400)
			time = num / 50;
		else
			printf("bad value for FCCS: %d\n", num);
	}
	*pptr = ports;
	*tptr = time;
}

soc(line, sptr)
	char *line, *sptr;
{
	u_short ports, num;

	ports = 0;
	line = strtok(line, ',');
	while (*(line = strtok((char *) 0, ',')) != '\0') {
		num = atoi(line);
		if (num >= 0 && num < 8)
			ports |= bits[num];
		else
			printf("bad value for SOC: %d\n",num);
	}
	*sptr = ports;
}

char	buffer[MPDLBUFSIZE];
extern	int errno;
struct head1 {
	long	magic;
	long	fill[12];
	struct	scnhdr text;
	struct	scnhdr data;
	struct	scnhdr bss;
} header1;

download(mpccnum)
	int mpccnum;
{
	char dlname[LINESIZE], fullname[LINESIZE];
	char *ldname, *ppmap;
	int dlfd, ldfd;
	char *it;
	short i;
	char hilo[2];
	long realsize;

	sprintf(dlname, "%s/mpcc%d", _PATH_DEV, mpccnum);
	if (*cf.loadname == '3')
		sprintf(fullname, _PATH_MPCC32);
	else
		sprintf(fullname, _PATH_MPCCDL);
	if ((cf.loadname[0]) == '\0')
		return (-1);
	if ((dlfd = open(dlname, O_RDWR)) == MP_DLERROR) {
		printf("Can not open %s\n",dlname);
		return (-1);
	}
	if ((ldfd = open(fullname, O_RDONLY)) == MP_DLERROR) {
		close(dlfd);
		printf("Can not access protocol code file: %s\n", fullname);
		return (-1);
	}
	if (dlokay(dlfd,mpccnum) == MP_DLERROR) {
		close(ldfd);
		close(dlfd);
		return (-1);
	}
	printf("Downloading MPCC #%x\n", mpccnum);
	/* read executable file header */
	if (read(ldfd, &header1, sizeof(header1)) != sizeof(header1)) {
		printf("Can not read %s\n", fullname);
		return (-1);
	}
	/* place at start of text space */
	if (lseek(ldfd, header1.text.s_scnptr , (int) 0) == -1) {
		printf("lseek error(text): %d", errno);
		return (-1);
	}
	/* send text */
	realsize = header1.data.s_paddr - header1.text.s_paddr;
	if (dl(ldfd, dlfd, realsize) == -1) {
		ioctl(dlfd, MPIORESETBOARD, 0L);
		return (-1);
	}
	/* place at start of data space	*/
	if (lseek(ldfd, header1.data.s_scnptr , (int) 0) == -1) {
		printf("lseek error(data): %d", errno);
		return (-1);
	}
	/* send initialized data */
	realsize = header1.bss.s_paddr - header1.data.s_paddr;
	if (dl(ldfd, dlfd, realsize) == -1) {
		ioctl(dlfd, MPIORESETBOARD, 0L);
		return (-1);
	}
	/* signal end of code */
	if (ioctl(dlfd, MPIOENDCODE, (char *) 0) == MP_DLERROR) {
		printf("MPIOENDCODE ioctl failed\n");
		ioctl(dlfd, MPIORESETBOARD, 0L);
		return (-1);
	}
	/* download configuration information	*/
	if (config(dlfd) == -1) {
		ioctl(dlfd, MPIORESETBOARD, 0L);
		return (-1);
	}
	/* write port/protocol map */
	ppmap = (char *)&cf.protoports[0];
	tknzmap(ppmap);
	if (ioctl(dlfd, MPIOPORTMAP, ppmap) == MP_DLERROR) {
		printf("MPIOPORTMAP ioctl failed\n");
		ioctl(dlfd, MPIORESETBOARD, 0L);
		return (-1);
	}
	/* signal end of download */
	if (ioctl(dlfd, MPIOENDDL, (char *) 0) == MP_DLERROR) {
		printf("MPIOENDDL ioctl failed\n");
		ioctl(dlfd, MPIORESETBOARD, 0L);
		return (-1);
	}
	close(dlfd);
	close(ldfd);
	printf("Download Complete and Successful\n");
	return (0);
}

dlokay(bdfd, mpccnum)
	int bdfd, mpccnum;
{
	char answer;

	if (resetflg) {
		printf("Reseting MPCC #%x\n",mpccnum);
		ioctl(bdfd, MPIORESETBOARD, 0L);
		sleep(10);
	}
	if (ioctl(bdfd, MPIOSTARTDL, 0) == MP_DLERROR) {
		if (errno == EBUSY) {
			printf("MPCC #%x has already been downloaded.\n",
			    mpccnum);
			printf("Do you want to re-download it?: ");
			fscanf(stdin,"%c",&answer);
			while (getchar() != '\n')
				;
			if ((answer | 0x60) != 'y')
				return (MP_DLERROR);
			ioctl(bdfd, MPIORESETBOARD, 0L);
			sleep(10);
			if (ioctl(bdfd, MPIOSTARTDL, (char *) 0) == MP_DLERROR) {
				printf("Can't download MPCC #%x\n", mpccnum);
				return (MP_DLERROR);
			}
		} else {
			switch (errno) {
			case ENODEV:
				printf("MPCC #%x not in system\n", mpccnum);
				break;
			case EACCES:
				printf("Download area in use, try later\n");
				break;
			case ENOSPC:
				printf("MPCC #%x already being downloaded\n",
				    mpccnum);
				break;
			default:
				printf("Unknown response from MPCC #%x\n",
				    mpccnum);
				break;
			}
			return (MP_DLERROR);
		}
	}
	return (0);
}

dl(dskfd, bdfd, size)
	int dskfd, bdfd;
	long size;
{
	int bytes;

	while (size > 0) {
		bytes = (size < MPDLBUFSIZE) ? (int) size : MPDLBUFSIZE;
		if ((bytes = read(dskfd, buffer, bytes)) == MP_DLERROR) {
			close(dskfd);
			close(bdfd);
			printf("Download-Can't read buffer\n");
			return (-1);
		}
		if (write(bdfd, buffer, bytes) == MP_DLERROR) {
			close(dskfd);
			close(bdfd);
			printf("Download-Can't write buffer\n");
			return (-1);
		}
		size -= bytes;
	}
	return (0);
}

/*
 * download each protocol's configuration data
 * and the configuration data for tboard.
 */
config(dlfd)
	int dlfd;
{
	register int i;
	char *ldname;

	for (ldname = cf.loadname; *ldname; ldname++) {
		switch (*ldname) {
		case '3': case 'a':
			if (ioctl(dlfd, MPIOASYNCNF, &bdasy) == MP_DLERROR) {
				printf("async ioctl failed\n");
				return (-1);
			}
			break;
		case 'b':
			break;
		case 'x':
			break;

		case 's':
			break;
		}
	}
}

/*
 * tokenize the protoport string,
 * (change from the letter to the corresponding number).
 */
tknzmap(map)
	char *map;
{
	short i;

	for (i = 0; i < MPMAXPORT; i++) {
		switch (*map) {
		case '3' :	*map = MPPROTO_ASYNC; break;
		case 'A' :	*map = MPPROTO_ASYNC; break;
		case 'B' :	*map = MPPROTO_BISYNC; break;
		case 'S' :	*map = MPPROTO_SNA; break;
		case 'X' :	*map = MPPROTO_X25; break;
		default:	*map = MPPROTO_UNUSED; break;
		}
		map++;
	}
}
