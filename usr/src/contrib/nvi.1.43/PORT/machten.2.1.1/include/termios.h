/*
 * termios.h -- POSIX termios emulation using sgtty interface.
 */

#ifndef _termios_h_
#define _termios_h_


#include <sgtty.h>

/* rename to avoid name clash */
#ifdef ECHO
#  define _SGTTY_ECHO	ECHO
#  undef ECHO
#endif
#ifdef TOSTOP
#  define _SGTTY_TOSTOP	TOSTOP
#  undef TOSTOP
#endif
#ifdef NOFLSH
#  define _SGTTY_NOFLSH	NOFLSH
#  undef NOFLSH
#endif

/* these aren't defined in <sys/types.h> */
typedef unsigned int 	speed_t;
typedef unsigned long	tcflag_t;
typedef unsigned char 	cc_t;

/* control chars */
#define VINTR		1
#define VQUIT		2
#define VERASE		3
#define VKILL		4
#define VEOF		5
#define VEOL		6
#define VSTART		7
#define VSTOP		8
#define VSUSP		9
#ifndef _POSIX_SOURCE
#define VDSUSP          11
#define VREPRINT        12
#define VDISCARD        13
#define VWERASE         14
#define VLNEXT          15
#define VSTATUS         16
#endif

#define VMIN		VEOF
#define VTIME		VEOL
#define NCCS		17

/* input modes */
#define IGNBRK		0x00000001
#define BRKINT		0x00000002
#define IGNPAR  	0x00000004
#define PARMRK  	0x00000008
#define INPCK   	0x00000010
#define ISTRIP  	0x00000020
#define INLCR   	0x00000040
#define IGNCR   	0x00000080
#define ICRNL   	0x00000100
#define IXON    	0x00000400
#define IXANY   	0x00000800 
#define IXOFF   	0x00001000

/* output modes */
#define OPOST   	0x00000001
#define ONLCR		0x00000004

/* control modes */
#define CSIZE   	0x00000030
#define CS5     	0x00000000
#define CS6     	0x00000010
#define CS7     	0x00000020
#define CS8     	0x00000030
#define CSTOPB  	0x00000040
#define CREAD   	0x00000080
#define PARENB  	0x00000100
#define PARODD  	0x00000200
#define HUPCL   	0x00000400
#define CLOCAL  	0x00000800

/* line discipline modes */
#define ISIG    	0x00000001
#define ICANON  	0x00000002
#define _TERMIOS_ECHO   0x00000008
#define ECHO   		_TERMIOS_ECHO
#define ECHOE   	0x00000010
#define ECHOK   	0x00000020
#define ECHONL  	0x00000040
#define _TERMIOS_NOFLSH 0x00000080
#define NOFLSH 		_TERMIOS_NOFLSH
#define _TERMIOS_TOSTOP 0x00000100
#define TOSTOP 		_TERMIOS_TOSTOP
#define IEXTEN  	0x00008000

/* tcsetattr() actions */
#define TCSANOW         0
#define TCSADRAIN       1
#define TCSAFLUSH       2


struct termios {
	struct sgttyb	__sg;
	struct tchars	__tc;
	struct ltchars	__ltc;

	tcflag_t	c_iflag;	/* input modes */
	tcflag_t	c_oflag;	/* output modes */
	tcflag_t	c_cflag;	/* control modes */
	tcflag_t	c_lflag;	/* local modes */
	cc_t		c_cc[NCCS];	/* control chars */
};

extern int	tcgetattr(/* int fd, struct termios *t */);
extern int	tcsetattr(/* int fd, int action, struct termios *t */);
extern speed_t	cfgetospeed(/* struct termios *t */);


#endif /* _termios_h_ */
