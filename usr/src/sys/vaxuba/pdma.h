/*	pdma.h	6.1	83/07/29	*/

struct pdma {
	struct	dzdevice *p_addr;
	char	*p_mem;
	char	*p_end;
	int	p_arg;
	int	(*p_fcn)();
};
