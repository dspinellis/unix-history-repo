/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Rick Adams.
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

/*
** Hacks to support "-a|c|n" flags on the command line which enalbe VJ
** header compresion and disable ICMP.  I use getopt to deal witht that
** stuff because I'm a lazy sob, I can't spell, and that's OK.
**
** If this is good all rights go to B & L Jolitz, otherwise send your
** comments to Reagan (/dev/null).
**
** nerd@percival.rain.com (Michael Galassi) 92.09.03
**
** Hacked to change from sgtty to POSIX termio style serial line control
** and added flag to enable cts/rts style flow control.
**
** blymn@awadi.com.au (Brett Lymn) 93.04.04
*/

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1988 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)slattach.c	4.6 (Berkeley) 6/1/90";
static char rcsid[] = "$Header: /a/cvs/386BSD/src/sbin/slattach/slattach.c,v 1.2 1993/07/06 16:46:02 jkh Exp $";
#endif /* not lint */

#include <sys/param.h>
#include <sys/ioctl.h>
#include <termios.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <net/if.h>
#include <net/if_slvar.h>
#include <netdb.h>
#include <fcntl.h>
#include <stdio.h>
#include <paths.h>

#define DEFAULT_BAUD	9600

static char usage_str[] = "\
usage: %s [-a ][-c ][-n ][-s <speed> ]<device>\n\
	-a -- autoenable VJ compression\n\
	-c -- enable VJ compression\n\
	-n -- throw out ICMP packets\n\
	-h -- turn on cts/rts style flow control\n\
	-s -- baud rate (default 9600)\n";

int main(int argc, char **argv)
{
	struct termios tty;
	int option;
	int fd;
	char devname[32];
	char *dev = (char *)0;
	int slipdisc = SLIPDISC;
	int speed = DEFAULT_BAUD;
	int slflags = 0;
	int flow_control = 0;	/* extra flags to enable hardware flow cont. */

	extern char *optarg;
	extern int optind;

	while ((option = getopt(argc, argv, "achns:")) != EOF) {
		switch (option) {
		case 'a':
			slflags |= SC_AUTOCOMP;
			slflags &= ~SC_COMPRESS;
			break;
		case 'c':
			slflags |= SC_COMPRESS;
			slflags &= ~SC_AUTOCOMP;
			break;
		case 'h':
			flow_control |= CRTSCTS;
			break;
		case 'n':
			slflags |= SC_NOICMP;
			break;
		case 's':
			speed = atoi(optarg);
			break;
		case '?':
		default:
			fprintf(stderr, usage_str, argv[0]);
			exit(1);
		}
	}

	if (optind == argc - 1)
		dev = argv[optind];


	if (dev == (char *)0) {
		fprintf(stderr, usage_str, argv[0]);
		exit(2);
	}

	if ((speed = findspeed(speed)) == 0) {
		fprintf(stderr, "unknown speed");
		exit(1);
	}

	if (strncmp(_PATH_DEV, dev, sizeof(_PATH_DEV) - 1)) {
		strcpy(devname, _PATH_DEV);
		strcat(devname, "/");
		strncat(devname, dev, 10);
		dev = devname;
	}

	if ((fd = open(dev, O_RDWR | O_NDELAY)) < 0) {
		perror(dev);
		exit(1);
	}

	tty.c_iflag = 0;
	tty.c_oflag = 0;
	tty.c_cflag = CREAD | CS8 | flow_control;
	tty.c_lflag = 0;
	tty.c_cc[VMIN] = 1; /* wait for one char */
	tty.c_cc[VTIME] = 0; /* wait forever for a char */
	if (ioctl(fd, TIOCSETA, &tty) < 0) {
		perror("ioctl(TIOCSETA)");
		close(fd);
		exit(1);
	}

	if (ioctl(fd, TIOCSDTR) < 0) {
                perror("ioctl(TIOCSDTR)");
                close(fd);
                exit(1);
        }

	cfsetispeed(&tty, speed);
	cfsetospeed(&tty, speed);
	if (tcsetattr(fd, TCSADRAIN, &tty) < 0) {
		perror("tcsetattr");
		close(fd);
		exit(1);
	}

	if (ioctl(fd, TIOCSETD, &slipdisc) < 0) {
		perror("ioctl(TIOCSETD)");
		close(fd);
		exit(1);
	}

	if (ioctl(fd, SLIOCSFLAGS, &slflags) < 0) {
		perror("ioctl(SLIOCSFLAGS)");
		close(fd);
		exit(1);
	}

	daemon(0, 1);

	for (;;)
		sigpause(0L);
}

struct sg_spds {
	int sp_val, sp_name;
}       spds[] = {
#ifdef B50
	{ 50, B50 },
#endif
#ifdef B75
	{ 75, B75 },
#endif
#ifdef B110
	{ 110, B110 },
#endif
#ifdef B150
	{ 150, B150 },
#endif
#ifdef B200
	{ 200, B200 },
#endif
#ifdef B300
	{ 300, B300 },
#endif
#ifdef B600
	{ 600, B600 },
#endif
#ifdef B1200
	{ 1200, B1200 },
#endif
#ifdef B1800
	{ 1800, B1800 },
#endif
#ifdef B2000
	{ 2000, B2000 },
#endif
#ifdef B2400
	{ 2400, B2400 },
#endif
#ifdef B3600
	{ 3600, B3600 },
#endif
#ifdef B4800
	{ 4800, B4800 },
#endif
#ifdef B7200
	{ 7200, B7200 },
#endif
#ifdef B9600
	{ 9600, B9600 },
#endif
#ifdef EXTA
	{ 19200, EXTA },
#endif
#ifdef EXTB
	{ 38400, EXTB },
#endif
	{ 0, 0 }
};

int findspeed(int speed)
{
	struct sg_spds *sp = spds;

	while ((sp->sp_val != 0) && (sp->sp_val != speed))
		sp++;

	return (sp->sp_name);
}
