struct	filsys
{
	int	s_isize;
	int	s_fsize;
	int	s_nfree;
	int	s_free[100];
	int	s_ninode;
	int	s_inode[100];
	char	s_flock;
	char	s_ilock;
	char	s_fmod;
	char	s_ronly;
	int	s_time[2];
};
