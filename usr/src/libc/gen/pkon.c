#include <sgtty.h>

struct	piocb {
	unsigned t;
	short	psize;
	short	mode;
	short	state;
	char	window;
};
pkon(fd, size)
{
	if (size&037 || size > 128) {
		write(2,"bad packet size\n",16);
		return(-1);
	}
	return(turnon(fd, size, 2, 0));
}
turnon(fd,psize,window,mode)
{
struct piocb p;

	p.window = window;
	p.psize = psize;
	p.mode = mode;
	p.t = 1;
	if (ioctl(fd,TIOCSETD,&p) < 0 || ioctl(fd,DIOCGETP,&p) < 0) {
		return(-1);
	}
	return(p.psize);
}

pkoff(fd)
{
struct piocb p;

	close(fd);
	return;
/*
	p.t = 0;
	return(ioctl(fd,TIOCSETD,&p));
*/
}
