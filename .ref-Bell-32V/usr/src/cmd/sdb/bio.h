struct brbuf {
	int	nl, nr;
	char	*next;
	char	b[512];
	int	fd;
};
