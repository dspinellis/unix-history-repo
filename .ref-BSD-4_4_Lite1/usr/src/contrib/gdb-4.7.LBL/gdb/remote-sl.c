/*
 * Copyright (c) 1990, 1991, 1992 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Lawrence Berkeley Laboratory,
 * Berkeley, CA.  The name of the University may not be used to
 * endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char rcsid[] =
    "@(#) $Header: /usr/src/contrib/gdb-4.7.lbl/gdb/RCS/remote-sl.c,v 1.2 1993/06/03 03:01:03 mccanne Exp $ (LBL)";
#endif

#include <signal.h>
#include <sys/param.h>
#include <sys/file.h>
#include <sys/time.h>

#include <errno.h>
extern int errno;

#if BSD >= 199103		/* XXX ifdef on POSIX? */
#include <termios.h>
#elif defined(HAVE_TERMIO)
#include <termio.h>
#else
#include <sgtty.h>
#endif
#include <sys/ioctl.h>

#ifdef USG
#include <fcntl.h>
#endif

#ifndef FD_SETSIZE
#define FD_SET(n, fdp) ((fdp)->fds_bits[0] |= (1 << (n)))
#define FD_ISSET(n, fdp) ((fdp)->fds_bits[0] & (1 << (n)))
#define FD_ZERO(fdp) ((fdp)->fds_bits[0] = 0)
#endif

#include "defs.h"
#include "remote-sl.h"
#include "remote.h"

static int sl_send();
static int sl_recv();

/*
 * Descriptor for I/O to remote machine.
 */
static int sl_fd = -1;

/*
 * User-configurable baud rate of serial line.
 * Default is 38400.
 */
static int speed = EXTB;
/*
 * Command line argument.
 */
extern char *baud_rate;

/*
 * Statistics.
 */
static int sl_errs;
static int sl_inbytes;
static int sl_outbytes;;

static void
sl_close()
{
	if (sl_fd >= 0) {
		close(sl_fd);
		sl_fd = -1;
	}
}

static int getbaud();

/*
 * Configure the serial link.
 */
static void
sl_conf(fd)
	int fd;
{
#if BSD >= 199103
	struct termios tios;

	(void)tcgetattr(fd, &tios);
	cfmakeraw(&tios);
	/* Set 8 bit characters, enable receiver, non-dialup. */
	tios.c_cflag = CS8|CREAD|CLOCAL;
	cfsetspeed(&tios, speed);
	(void)tcsetattr(fd, TCSANOW, &tios);
#elif defined(HAVE_TERMIO)
	struct termio tios;

	ioctl(fd, TCGETA, &tios);
	tios.c_lflag &= ~(ICANON | ECHO);
	/* Set speed, 8 bit characters, enable receiver, non-dialup. */
	tios.c_cflag = (speed & CBAUD)|CS8|CREAD|CLOCAL;
	ioctl(fd, TCSETA, &tios);
#else
	struct sgttyb sg;

	ioctl(fd, TIOCGETP, &sg);
	sg.sg_flags = RAW | ANYP;
	sg.sg_ispeed = sg.sg_ospeed = speed;
	ioctl(fd, TIOCSETP, &sg);
#endif
}

/*
 * Open a serial line for remote debugging.
 */
void
sl_open(name, remote_fnp)
	char *name;
	struct remote_fn *remote_fnp;
{
	char device[80];

	sl_close();

	if (name[0] != '/') {
		(void)sprintf(device, "/dev/%s", name);
		name = device;
	}
	/*
	 * Use non-blocking mode so we don't wait for a carrier.
	 * This allows the open to complete, then we set CLOCAL
	 * mode since we don't need the modem control lines.
	 */
	sl_fd = open(name, O_RDWR|O_NONBLOCK);
	if (sl_fd < 0) {
		perror_with_name(name);
		/* NOTREACHED */
	}

	if (baud_rate != 0) {
		speed = getbaud(baud_rate);
		baud_rate = 0;
	}
	sl_conf(sl_fd);

	remote_fnp->send = sl_send;
	remote_fnp->recv = sl_recv;
	remote_fnp->close = sl_close;
	remote_fnp->maxdata = SL_MAXDATA;
	remote_fnp->rpcsize = SL_RPCSIZE;
	
	sl_errs = 0;
	sl_inbytes = 0;
	sl_outbytes = 0;
}

/*
 * Remote input buffering.
 */
static u_char rib[2 * SL_MTU];
static u_char *rib_cp, *rib_ep;
#define GETC(to) ((rib_cp < rib_ep) ? *rib_cp++ : rib_filbuf(to))

/*
 * Fill up the input buffer (with a non-blocking read).
 * On error, return the negation of the error code, otherwise
 * return the first character read and set things up so GETC will
 * read the remaining chars.
 */
static int
rib_filbuf(to)
	int to;
{
	int cc, fd = sl_fd;
	fd_set fds;
	struct timeval timeout, *tp;

	if (to < 0)
		tp = 0;
	else {
		timeout.tv_sec = to / 1000;
		timeout.tv_usec = to % 1000;
		tp = &timeout;
	}

	FD_ZERO(&fds);
	while (1) {
		FD_SET(fd, &fds);
		cc = select(fd + 1, &fds, (fd_set *)0, (fd_set *)0, tp);
		if (cc == 0)
			return (-EKGDB_TIMEOUT);
		else if (cc < 0)
			return (-EKGDB_IO);
		else {
			cc = read(fd, (caddr_t)rib, sizeof(rib));
			if (cc < 0)
				return (-EKGDB_IO);

			rib_cp = &rib[1];
			rib_ep = &rib[cc];

			sl_inbytes += cc;

			return (rib[0]);
		}
	}
}

#define PUTESC(p, c) { \
	if (c == FRAME_END) { \
		*p++ = FRAME_ESCAPE; \
		c = TRANS_FRAME_END; \
	} else if (c == FRAME_ESCAPE) { \
		*p++ = FRAME_ESCAPE; \
		c = TRANS_FRAME_ESCAPE; \
	} else if (c == FRAME_START) { \
		*p++ = FRAME_ESCAPE; \
		c = TRANS_FRAME_START; \
	} \
	*p++ = c; \
}


/*
 * Send a message to the remote host.  An error code is returned.
 */
static int
sl_send(type, bp, len)
	register u_char type;
	register u_char *bp;
	register int len;
{
	register u_char *p, *ep;
	register u_char csum, c;
	u_char buf[SL_MTU];

	/*
	 * Build a packet.  The framing byte comes first, then the command
	 * byte, the message, the checksum, and another framing character.
	 * We must escape any bytes that match the framing or escape chars.
	 */
	p = buf;
	*p++ = FRAME_START;
	csum = type;
	PUTESC(p, type);

	for (ep = bp + len; bp < ep; ) {
		c = *bp++;
		csum += c;
		PUTESC(p, c);
	}
	csum = -csum;
	PUTESC(p, csum);
	*p++ = FRAME_END;

	len = p - buf;
	sl_outbytes += len;
	if (write(sl_fd, (caddr_t)buf, len) != len)
		return (EKGDB_IO);
	return (0);
}

/*
 * Read a packet from the remote machine.  An error code is returned.
 */
static int
sl_recv(tp, ip, lenp, to)
	int *tp;
	u_char *ip;
	int *lenp;
	int to;
{
	register u_char csum, *bp;
	register int c;
	register int escape, len;
	register int type;
	u_char buf[SL_RPCSIZE + 1];	/* room for checksum at end of buffer */

	/*
	 * Allow immediate quit while reading from device, it could be hung.
	 */
	++immediate_quit;

	/*
	 * Throw away garbage characters until we see the start
	 * of a frame (i.e., don't let framing errors propagate up).
	 * If we don't do this, we can get pretty confused.
	 */
	while ((c = GETC(to)) != FRAME_START)
		if (c < 0)
			return (-c);
restart:
	csum = len = escape = 0;
	type = -1;
	bp = buf;
	while (1) {
		c = GETC(to);
		if (c < 0)
			return (-c);

		switch (c) {
			
		case FRAME_ESCAPE:
			escape = 1;
			continue;
			
		case TRANS_FRAME_ESCAPE:
			if (escape)
				c = FRAME_ESCAPE;
			break;
			
		case TRANS_FRAME_END:
			if (escape)
				c = FRAME_END;
			break;

		case TRANS_FRAME_START:
			if (escape)
				c = FRAME_START;
			break;

		case FRAME_START:
			goto restart;
			
		case FRAME_END:
			if (type < 0 || --len < 0) {
				csum = len = escape = 0;
				continue;
			}
			if (csum != 0) {
				++sl_errs;
				return (EKGDB_CSUM);
			}
			--immediate_quit;

			/* Store saved rpc reply type */
			*tp = type;

			/* Store length of rpc reply packet */
			if (lenp)
				*lenp = len;

			if (ip)
				bcopy((caddr_t)buf, (caddr_t)ip, len);
			return (0);
		}
		csum += c;
		if (type < 0) {
			type = c;
			escape = 0;
			continue;
		}
		if (++len > sizeof(buf)) {
			do {
				if ((c = GETC(to)) < 0)
					return (-c);
			} while (c != FRAME_END);

			return (EKGDB_2BIG);
		}
		*bp++ = c;

		escape = 0;
	}
}

static int
getbaud(s)
	char *s;
{
	switch (atoi(s)) {
	case 0:		return (B0);
	case 50:	return (B50);
	case 75:	return (B75);
	case 110:	return (B110);
	case 134:	return (B134);
	case 150:	return (B150);
	case 200:	return (B200);
	case 300:	return (B300);
	case 600:	return (B600);
	case 1200:	return (B1200);
	case 1800:	return (B1800);
	case 2400:	return (B2400);
	case 4800:	return (B4800);
	case 9600:	return (B9600);
	case 19200:	return (EXTA);
	case 38400:	return (EXTB);
	}
	return (-1);
}

static void
set_sl_baudrate_command(arg, from_tty)
	char           *arg;
	int             from_tty;
{
	int baudrate;

	if (arg == 0)
		error_no_arg("set remote-baudrate");
	while (*arg == ' ' || *arg == '\t')
		++arg;
	if (*arg == 0)
		error_no_arg("set remote-baudrate");
	if (*arg < '0' || *arg > '9')
		error("non-numeric arg to \"set remote-baudrate\".");

	baudrate = getbaud(arg);
	if (baudrate < 0)
		error("unknown baudrate for \"set remote-baudrate\".");
	speed = baudrate;
	/*
	 * Don't use command line option anymore.
	 */
	baud_rate = 0;
}

/* ARGSUSED */
static void
sl_info(arg, from_tty)
	char *arg;
	int from_tty;
{
	int linespeed;

	switch (speed) {
	default:	linespeed = 0; break;
	case B50:	linespeed = 50; break;
	case B75:	linespeed = 75; break;
	case B110:	linespeed = 110; break;
	case B134:	linespeed = 134; break;
	case B150:	linespeed = 150; break;
	case B200:	linespeed = 200; break;
	case B300:	linespeed = 300; break;
	case B600:	linespeed = 600; break;
	case B1200:	linespeed = 1200; break;
	case B1800:	linespeed = 1800; break;
	case B2400:	linespeed = 2400; break;
	case B4800:	linespeed = 4800; break;
	case B9600:	linespeed = 9600; break;
	case EXTA:	linespeed = 19200; break;
	case EXTB:	linespeed = 38400; break;
	}
	printf("sl-baudrate     %6d\n", linespeed);
	printf("bytes received  %6d\n", sl_inbytes);
	printf("bytes sent      %6d\n", sl_outbytes);
	printf("checksum errors %6d\n", sl_errs);
}

extern struct cmd_list_element *setlist;

void
_initialize_sl()
{
	add_info("sl", sl_info,
		 "Show current settings of serial line debugging options.");
	add_cmd("sl-baudrate", class_support, set_sl_baudrate_command,
		"Set remote debug serial line baudrate.", &setlist);
}
