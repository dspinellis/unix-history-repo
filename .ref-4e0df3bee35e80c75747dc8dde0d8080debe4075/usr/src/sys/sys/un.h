/*	un.h	5.1	82/08/02	*/

/*
 * Definitions for UNIX IPC domain.
 */
struct	sockaddr_un {
	short	sun_family;		/* AF_UNIX */
	char	sun_path[14];		/* path name */
};
