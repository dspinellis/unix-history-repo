/*	un.h	5.2	83/05/27	*/

/*
 * Definitions for UNIX IPC domain.
 */
struct	sockaddr_un {
	short	sun_family;		/* AF_UNIX */
	char	sun_path[14];		/* path name */
};

#ifdef KERNEL
int	unp_discard();
#endif
