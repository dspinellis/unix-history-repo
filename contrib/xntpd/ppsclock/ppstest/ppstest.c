#ifndef lint
static char rcsid[] =
    "ppstest.c,v 3.1 1993/07/06 01:09:56 jbj Exp (LBL)";
#endif
/*
 * This software was developed by the Computer Systems Engineering group
 * at Lawrence Berkeley Laboratory under DARPA contract BG 91-66.
 *
 * Copyright (c) 1992 The Regents of the University of California.
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
 *	California, Lawrence Berkeley Laboratory.
 * 4. The name of the University may not be used to endorse or promote
 *    products derived from this software without specific prior
 *    written permission.
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

/*
 * ppsclock test program
 *
 * This program is used to test the SunOS 4 pps clock streams module.
 * It assumes a Magnavox MX4200 GPS receiver running at 4800 baud with
 * its 1 PPS signal driving carrier detect.
 */

#include <sys/types.h>
#include <sys/time.h>
#include <sys/ppsclock.h>
#include <sys/stropts.h>

#include <fcntl.h>
#include <stdio.h>
#include <termios.h>

char *prog;

main(argc, argv)
	int argc;
	char **argv;
{
	register int fd, i;
	char *tty ="/dev/gps0";
	char buf[1024];
	struct ppsclockev ev;

	prog = argv[0];
	fd = open(tty, O_RDWR, 0);
	if (fd < 0) {
		fprintf(stderr, "%s: fopen ", prog);
		perror(tty);
		exit(1);
	}
	if (!gpsinit(fd))
		exit(1);

	i = read(fd, buf, sizeof(buf) - 1);
	if (i < 0) {
		fprintf(stderr, "%s: ", prog);
		perror("read");
		exit(1);
	}
	buf[i] = '\0';

	if (ioctl(fd, CIOGETEV, (char *)&ev) < 0) {
		fprintf(stderr, "%s: ", prog);
		perror("CIOGETEV");
		exit(1);
	}
	printf("%d.%06d (serial %d)\n\t\"%s\"\n",
	    ev.tv.tv_sec, ev.tv.tv_usec, ev.serial, buf);

	exit(0);
}

int
gpsinit(fd)
	register int fd;
{
	struct termios termios;
	speed_t speed = B4800;

	memset((char *)&termios, 0, sizeof(termios));
	termios.c_cflag = CS8 | CREAD | CLOCAL;
	termios.c_iflag = IGNCR;
	termios.c_lflag = ICANON;
	if (cfsetispeed(&termios, speed) < 0) {
		fprintf(stderr, "%s: cfsetispeed failed\n", prog);
		perror("cfsetispeed");
		return (0);
	}
	if (cfsetospeed(&termios, speed) < 0) {
		fprintf(stderr, "%s: cfsetospeed failed\n", prog);
		perror("cfsetospeed");
		return (0);
	}
	if (tcsetattr(fd, TCSAFLUSH, &termios) < 0) {
		fprintf(stderr, "%s: ", prog);
		perror("tcsetattr");
		return (0);
	}

	if (ioctl(fd, I_PUSH, PPSCLOCKSTR) < 0) {
		fprintf(stderr, "%s: I_PUSH: ", prog);
		perror(PPSCLOCKSTR);
		return (0);
	}

	return (1);
}
