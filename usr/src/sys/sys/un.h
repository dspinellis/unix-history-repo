/*	un.h	6.2	84/05/07	*/

/*
 * Definitions for UNIX IPC domain.
 */
struct	sockaddr_un {
	short	sun_family;		/* AF_UNIX */
	char	sun_path[108];		/* path name (gag) */
};

#ifdef KERNEL
int	unp_discard();
#endif
