struct brbuf {
	int	nl, nr;
	char	*next;
	char	b[1024];
	int	fd;
};
long lseek();
