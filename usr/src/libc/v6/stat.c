#include <sys/types.h>
#include <sys/stat.h>

struct ostat {
	short os_dev;
	short os_inum;
	short os_flags;
	char  os_nlinks;
	char  os_uid;
	char  os_gid;
	char  os_size0;
	short os_size1;
	short os_addr[8];
	long  os_actime;
	long  os_modtime;
} osbuf;

stat(name, buf)
char *name;
struct stat *buf;
{
	if (syscall(18, 0, 0, name, &osbuf, 0) < 0)
		return(-1);

	stcopyit(buf);
	return(0);
}

fstat(fd, buf)
int fd;
struct stat *buf;
{
	if (syscall(28, fd, 0, &osbuf, 0, 0) < 0)
		return(-1);
	stcopyit(buf);
	return(0);
}

static
stcopyit(buf)
struct stat *buf;
{
	buf->st_dev = osbuf.os_dev;
	buf->st_ino = osbuf.os_inum;
	buf->st_mode = osbuf.os_flags;
	buf->st_mode &= 067777;
	if ((buf->st_mode&060000) == 0)
		buf->st_mode |= 0100000;
	buf->st_nlink = osbuf.os_nlinks;
	buf->st_uid = osbuf.os_uid;
	buf->st_gid = osbuf.os_gid;
	buf->st_rdev = 0;
	buf->st_size = ( (long) osbuf.os_size0 << 16) | osbuf.os_size1;
	buf->st_atime = osbuf.os_actime;
	buf->st_mtime = osbuf.os_modtime;
	buf->st_ctime = buf->st_mtime;
}
