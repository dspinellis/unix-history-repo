gtty(fd, buf)
int fd;
int *buf;
{
	if (syscall(32, fd, 0, buf, 0, 0) < 0)
		return(-1);
	return(0);
}
