/*	map.h	4.4	81/02/27	*/

/*
 * Resource Allocation Maps
 */
struct map
{
	int	m_size;
	int	m_addr;
};

#ifdef KERNEL
struct	map *swapmap;
int	nswapmap;
struct	map *argmap;
struct	map *kernelmap;
#endif
