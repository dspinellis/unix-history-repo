#include <sys/types.h>
#include <sys/stat.h>

/* map to new stat structure format */
struct inode {
	int	i_device;
	int	i_number;
	int	i_flags;
	char	i_nlinks;
	char	i_uid;
	char	i_gid;
	char	i_size0;
	int	i_size1;
	int	i_addr[8];
	long	i_actime;
	long	i_modtime;
};

_stat(name, stbuff)
	char *name;
	struct stat *stbuff;
{
	struct inode inode;

	if (stat(name, &inode) < 0)
		return (-1);
	copy(stbuff, &inode);
	return (0);
}

_fstat(unit, stbuff)
	int unit;
	struct stat *stbuff;
{
	struct inode inode;

	if (fstat(unit, &inode) < 0)
		return (-1);
	copy(stbuff, &inode);
	return (0);
}

static
copy(stbuff, inode)
	register struct stat *stbuff;
	register struct inode *inode;
{

	stbuff->st_dev = inode->i_device;
	stbuff->st_ino = inode->i_number;
	stbuff->st_mode = (inode->i_flags & 067777);
	if ((stbuff->st_mode & 060000) == 0)
		stbuff->st_mode =| 0100000;
	stbuff->st_nlinks = (int)(inode->i_nlinks&0377);
	stbuff->st_uid = inode->i_uid & 0377;
	stbuff->st_gid = inode->i_gid & 0377;
/*
	stbuff->st_uid = (inode->i_gid << 8) | (inode->i_uid & 0377);
	stbuff->st_gid = 0;
*/
	stbuff->st_rdev = 0;
	if (inode->i_flags & 020000)
		stbuff->st_rdev = inode->i_addr[0];
	stbuff->st_size = ((long) (inode->i_size0 & 0377)) << 16;
	stbuff->st_size += (long) (unsigned) inode->i_size1;
	stbuff->st_atime = inode->i_actime;
	stbuff->st_mtime = inode->i_modtime;
	stbuff->st_ctime = 0;
}
