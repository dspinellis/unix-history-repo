/******************************************************************************
 * Copyright 1990, 1992 Free Software Foundation, Inc.
 *
 * This code was donated by Intel Corp.
 *
 * This include file provides BSD/USG-compatible tty control for a host utility
 * that interacts with NINDY.  As of this writing, it is used by the gdb960
 * remote communications module (remote.c) and the comm960 utility.
 * 
 * It is assumed that 'USG' is defined on the compiler invocation line if the
 * code should compile and run on a USG/SysV system.  Otherwise, BSD is assumed.
 *
 * The application code has access to these macros:
 *
 *	TTY_STRUCT	Data type used by tty functions (ioctls and the
 *			following macros).
 *
 *	TTY_NINDYTERM(tty)
 *			'tty' is assumed to be a TTY_STRUCT describing the
 *			terminal.  It is modified as appropriate to allow
 *			all user input to be passed through unmodified to NINDY
 *			as soon as it is typed, and to allow all NINDY output
 *			to be passed through unmodified to the display as soon
 *			as it is received.
 *
 *	TTY_REMOTE(tty,baud)
 *			'tty' is assumed to be a TTY_STRUCT describing the
 *			serial connection between the host and NINDY.  It is
 *			initialized as appropriate to allow communications
 *			between the host and NINDY at the specified baud rate
 *			(which must be one of the "B..." defined constants).
 *
 *	TTY_FLUSH(fd)	flush all pending input and output on the tty whose
 *			file descriptor is 'fd'.
 *
 *	TTY_NBREAD(fd,n,bufptr)
 *			Performs non-blocking read of 'n' characters on the
 *			file descriptor 'fd'.  Sets the integer 'n' to the
 *			number of characters actually read.  The characters
 *			are read into the buffer pointed at by bufptr.
 *
 * In addition, the BSD ioctl commands TIOCGETP and TIOCSETP are defined to
 * have the same meanings under USG: retrieve and set (respectively) the
 * parameters of a tty.
 ******************************************************************************/

#ifdef USG

#	include <termio.h>
#	define TTY_STRUCT	struct termio
#	define TIOCGETP		TCGETA
#	define TIOCSETP		TCSETAF

	/* NOTE!:
	 *	Remove CLOCAL from following macro if you will be accessing
	 *	the i960 system via a modem.
	 */
#       define TTY_REMOTE(tty,baud) {                   \
                tty.c_iflag = IXON | IXOFF;             \
                tty.c_oflag = 0;                        \
                tty.c_cflag = baud|CS8|CREAD|CLOCAL;    \
                tty.c_lflag = 0;                        \
                tty.c_cc[VEOF] = 1;                     \
                tty.c_cc[VEOL] = 0;                     \
        }

#	define TTY_NINDYTERM(tty) {		\
		tty.c_iflag = 0;		\
		tty.c_oflag = 0;		\
		tty.c_lflag = ISIG;		\
		tty.c_cc[VEOF] = 1;		\
		tty.c_cc[VEOL] = 0;		\
	}

#	define TTY_FLUSH(fd)	ioctl(fd,TCFLSH,2);

#       define TTY_NBREAD(fd,n,bufptr) {			\
		int _saveflags_;				\
		_saveflags_ = fcntl( fd, F_GETFL, 0 );		\
		fcntl( fd, F_SETFL, _saveflags_ | O_NDELAY );	\
		n = read( fd, bufptr, n );			\
		fcntl( fd, F_SETFL, _saveflags_ );		\
	}

#else	/* BSD */

#	include <sys/ioctl.h>
#	define TTY_STRUCT	struct sgttyb
#       define TTY_REMOTE(tty,baud){            \
                tty.sg_flags = RAW | TANDEM;    \
                tty.sg_ispeed = baud;           \
                tty.sg_ospeed = baud;           \
        }

#	define TTY_NINDYTERM(tty)	{	\
		tty.sg_flags |= CBREAK;		\
		tty.sg_flags &= ~(ECHO|CRMOD);	\
	}

#	define TTY_FLUSH(fd)	{ int _i_ = 0; ioctl(fd,TIOCFLUSH,&_i_); }

#       define TTY_NBREAD(fd,n,bufptr) {		\
		int _n_;				\
		ioctl(fd,FIONREAD,&_n_);		\
		n = (_n_>0) ? read(fd,bufptr,n) : 0;	\
	}
#endif



#ifndef B19200
#	define B19200 EXTA
#endif
#ifndef B38400
#	define B38400 EXTB
#endif
