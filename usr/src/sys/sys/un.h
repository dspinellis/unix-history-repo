/*	un.h	5.3	83/06/13	*/

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
