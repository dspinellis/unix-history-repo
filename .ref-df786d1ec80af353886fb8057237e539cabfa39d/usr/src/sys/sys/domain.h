/*	domain.h	6.1	83/07/29	*/

/*
 * Structure per communications domain.
 */
struct	domain {
	int	dom_family;		/* AF_xxx */
	char	*dom_name;
	struct	protosw *dom_protosw, *dom_protoswNPROTOSW;
	struct	domain *dom_next;
};

#ifdef KERNEL
struct	domain *domains;
#endif
