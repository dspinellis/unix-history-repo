/*	pdma.h	4.1	11/9/80	*/

struct pdma
{
	struct	device *p_addr;
	char	*p_mem;
	char	*p_end;
	int	p_arg;
	int	(*p_fcn)();
};
