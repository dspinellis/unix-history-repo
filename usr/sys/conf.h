struct
{
	char	d_minor;
	char	d_major;
};

struct	bdevsw
{
	int	(*d_open)();
	int	(*d_close)();
	int	(*d_strategy)();
	int	*d_tab;
} bdevsw[];
int	nblkdev;

struct	cdevsw
{
	int	(*d_open)();
	int	(*d_close)();
	int	(*d_read)();
	int	(*d_write)();
	int	(*d_sgtty)();
} cdevsw[];
int	nchrdev;
