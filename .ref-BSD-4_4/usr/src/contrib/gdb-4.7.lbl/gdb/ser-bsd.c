/* Remote serial interface for OS's with sgttyb
   Copyright 1992 Free Software Foundation, Inc.

This file is part of GDB.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include "defs.h"
#include <sys/types.h>
#include <sys/time.h>
#include <fcntl.h>
#include "serial.h"

static int desc = -1;

void
serial_raw(fd, oldstate)
     int fd;
     struct ttystate *oldstate;
{
  struct sgttyb sgttyb;

  oldstate->flags = fcntl(fd, F_GETFL, 0);

  fcntl(fd, F_SETFL, oldstate->flags|FNDELAY);

  if (ioctl(fd, TIOCGETP, &sgttyb))
    {
      fprintf(stderr, "TIOCGETP failed: %s\n", safe_strerror(errno));
    }

  oldstate->sgttyb = sgttyb;

  sgttyb.sg_flags = RAW;

  if (ioctl(fd, TIOCSETP, &sgttyb))
    {
      fprintf(stderr, "TIOCSETP failed: %s\n", safe_strerror(errno));
    }
}

void
serial_restore(fd, oldstate)
     int fd;
     struct ttystate *oldstate;
{
  fcntl(fd, F_SETFL, oldstate->flags);

  ioctl(fd, TIOCSETP, &oldstate->sgttyb);
}

static struct ttystate oldstate;

static fd_set readfds;

int
serial_open(name)
     const char *name;
{
  struct sgttyb sgttyb;

  desc = open (name, O_RDWR);
  if (desc < 0)
    error("Open of %s failed: %s", name, safe_strerror(errno));

  serial_raw(desc, &oldstate);

/* Setup constant stuff for select */

  FD_ZERO(&readfds);

  return desc;
}

/* Read a character with user-specified timeout.  TIMEOUT is number of seconds
   to wait, or -1 to wait forever.  Use timeout of 0 to effect a poll.  Returns
   char if successful.  Returns -2 if timeout expired, EOF if line dropped
   dead, or -3 for any other error (see errno in that case). */

int
serial_readchar(timeout)
     int timeout;
{
  static unsigned char buf[BUFSIZ];
  static unsigned char *bufp;
  static int bufcnt = 0;
  int numfds;
  struct timeval tv;

  if (bufcnt-- > 0)
    return *bufp++;

  tv.tv_sec = timeout;
  tv.tv_usec = 0;

  FD_SET(desc, &readfds);

  if (timeout >= 0)
    numfds = select(desc+1, &readfds, 0, 0, &tv);
  else
    numfds = select(desc+1, &readfds, 0, 0, 0);

  if (numfds <= 0)
    if (numfds == 0)
      return -2;		/* Timeout */
    else
      return -3;		/* Got an error from select */

  bufcnt = read(desc, buf, BUFSIZ);

  if (bufcnt <= 0)
    if (bufcnt == 0)
      return EOF;		/* 0 chars means end of file */
    else
      return -3;		/* Got an error from read */

  bufcnt--;
  bufp = buf;
  return *bufp++;
}

#ifndef B19200
#define B19200 EXTA
#endif

#ifndef B38400
#define B38400 EXTB
#endif

/* Translate baud rates from integers to damn B_codes.  Unix should
   have outgrown this crap years ago, but even POSIX wouldn't buck it.  */

static struct
{
  int rate;
  int code;
} baudtab[] = {
  {50, B50},
  {75, B75},
  {110, B110},
  {134, B134},
  {150, B150},
  {200, B200},
  {300, B300},
  {600, B600},
  {1200, B1200},
  {1800, B1800},
  {2400, B2400},
  {4800, B4800},
  {9600, B9600},
  {19200, B19200},
  {38400, B38400},
  {-1, -1},
};

static int 
rate_to_code(rate)
     int rate;
{
  int i;

  for (i = 0; baudtab[i].rate != -1; i++)
    if (rate == baudtab[i].rate)  
      return baudtab[i].code;

  return -1;
}

int
serial_setbaudrate(rate)
     int rate;
{
  struct sgttyb sgttyb;

  if (ioctl(desc, TIOCGETP, &sgttyb))
    error("TIOCGETP failed: %s\n", safe_strerror(errno));

  sgttyb.sg_ospeed = rate_to_code(rate);
  sgttyb.sg_ispeed = rate_to_code(rate);

  if (ioctl(desc, TIOCSETP, &sgttyb))
    error("TIOCSETP failed: %s\n", safe_strerror(errno));

  return 1;
}

int
serial_write(str, len)
     const char *str;
     int len;
{
  int cc;

  while (len > 0)
    {
      cc = write(desc, str, len);

      if (cc < 0)
	return 0;
      len -= cc;
      str += cc;
    }
  return 1;
}

void
serial_close()
{
  if (desc < 0)
    return;

  serial_restore(desc, oldstate);

  close(desc);
  desc = -1;
}
