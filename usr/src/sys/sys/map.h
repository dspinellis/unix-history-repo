/*	map.h	4.3	81/02/27	*/

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
struct	map *argmap;
struct	map *kernelmap;
#endif
