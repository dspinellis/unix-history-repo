/*	pdma.h	2.1	1/5/80	*/

struct pdma
{
	struct	device *p_addr;
	char	*p_mem;
	char	*p_end;
	int	p_arg;
	int	(*p_fcn)();
};
