#include <sys/termios.h>

set_ttydefaults(fd)
	int fd;
{
	struct termios term;

	tcgetattr(fd, &term);
	term.c_iflag = TTYDEF_IFLAG;
	term.c_oflag = TTYDEF_OFLAG;
	term.c_lflag = TTYDEF_LFLAG;
	term.c_cflag = TTYDEF_CFLAG;
	tcsetattr(fd, TCSAFLUSH, &term);
}
