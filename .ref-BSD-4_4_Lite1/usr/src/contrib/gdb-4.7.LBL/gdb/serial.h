/* Remote serial support interface definitions for GDB, the GNU Debugger.
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

#ifdef HAVE_TERMIO

#include <termios.h>
#include <unistd.h>

struct ttystate
{
  int flags;			/* Flags from fcntl F_GETFL */
  struct termios termios;	/* old tty driver settings */
};

#else /* not HAVE_TERMIO */

#include <sgtty.h>

struct ttystate {
  int flags;			/* Flags from fcntl F_GETFL */
  struct sgttyb sgttyb;		/* old tty driver settings */
};

#endif /* not HAVE_TERMIO */
/* Return a sensible default name for a serial device, something which
   can be used as an argument to serial_open.  */
   
const char *serial_default_name PARAMS ((void));

/* Try to open the serial device "name", return 1 if ok, 0 if not.  */

int serial_open PARAMS ((const char *name));

/* Turn the port into raw mode.  */

void serial_raw PARAMS ((int fd, struct ttystate *oldstate));

/* Turn the port into normal mode.  */

void serial_normal PARAMS ((void));

/* Read one char from the serial device with <TO>-second timeout.
   Returns char if ok, else EOF, -2 for timeout, -3 for anything else  */

int serial_readchar PARAMS ((int to));

/* Set the baudrate to the decimal value supplied, and return 1, or fail and
   return 0.  */

int serial_setbaudrate PARAMS ((int rate));

/* Return the next rate in the sequence, or return 0 for failure.  */

/* Write some chars to the device, returns 0 for failure.  See errno for
   details. */

int serial_write PARAMS ((const char *str, int len));

/* Close the serial port */

void serial_close PARAMS ((void));
