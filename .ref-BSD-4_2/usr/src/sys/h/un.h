/*	un.h	6.1	83/07/29	*/

/*
 * Definitions for UNIX IPC domain.
 */
struct	sockaddr_un {
	short	sun_family;		/* AF_UNIX */
	char	sun_path[109];		/* path name (gag) */
};

#ifdef KERNEL
int	unp_discard();
#endif
