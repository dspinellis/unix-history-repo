#ifndef	DIRSIZ
#define	DIRSIZ	14
#endif
struct	dir
{
	ino_t	d_ino;
	char	d_name[DIRSIZ];
};
