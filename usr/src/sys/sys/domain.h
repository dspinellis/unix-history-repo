/*	domain.h	5.1	5.1	*/

/*
 * Structure for a communications domain.
 * Each communications domain supports a set or protocols,
 * which are contained in a protocol table located through
 * this structure.
 */
struct	domain {
	char	*dom_name;
	struct	protosw *dom_proto;
	int	dom_nproto;
};

#ifdef KERNEL
struct	domain *domains;
#endif
