/*
 * Copyright (c) 1982, 1986, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)un.h	8.3 (Berkeley) %G%
 */

/*
 * Definitions for UNIX IPC domain.
 */
struct	sockaddr_un {
	u_char	sun_len;		/* sockaddr len including null */
	u_char	sun_family;		/* AF_UNIX */
	char	sun_path[104];		/* path name (gag) */
};

#ifdef KERNEL
struct unpcb;

int	uipc_usrreq __P((struct socket *so, int req, struct mbuf *m,
		struct mbuf *nam, struct mbuf *control));
int	unp_attach __P((struct socket *so));
int	unp_bind __P((struct unpcb *unp, struct mbuf *nam, struct proc *p));
int	unp_connect __P((struct socket *so, struct mbuf *nam, struct proc *p));
int	unp_connect2 __P((struct socket *so, struct socket *so2));
void	unp_detach __P((struct unpcb *unp));
void	unp_discard __P((struct file *fp));
void	unp_disconnect __P((struct unpcb *unp));
void	unp_drop __P((struct unpcb *unp, int errno));
void	unp_gc __P((void));
void	unp_mark __P((struct file *fp));
void	unp_scan __P((struct mbuf *m0, void (*op) __P((struct file *))));
void	unp_shutdown __P((struct unpcb *unp));
#else /* !KERNEL */

/* actual length of an initialized sockaddr_un */
#define SUN_LEN(su) \
	(sizeof(*(su)) - sizeof((su)->sun_path) + strlen((su)->sun_path))
#endif /* KERNEL */
