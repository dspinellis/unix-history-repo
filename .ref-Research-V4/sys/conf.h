struct
{
	char	d_minor;
	char	d_major;
};

struct
{
	int	(*d_open)();
	int	(*d_close)();
	int	(*d_strategy)();
} bdevsw[];

struct
{
	int	(*d_open)();
	int	(*d_close)();
	int	(*d_read)();
	int	(*d_write)();
	int	(*d_sgtty)();
} cdevsw[];
