lseek(fd, off, ptr)
int fd, ptr;
long off;
{
	unsigned a;

	a = off;

	if (a == off)
		return (syscall(19, fd, 0, a, ptr, 0));
	a = off/512;
	syscall(19, fd, 0, a, ptr+3, 0);
	return(syscall(19, fd, 0, (int) (off%512), ptr, 0));
}
