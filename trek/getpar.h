struct cvntab		/* used for getcodpar() paramater list */
{
	char	*abrev;
	char	*full;
	int	(*value)();
	int	value2;
};

extern double		getfltpar();
extern struct cvntab	*getcodpar();
