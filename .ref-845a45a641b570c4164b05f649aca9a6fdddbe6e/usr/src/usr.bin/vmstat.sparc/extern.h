/*
 * Copyright (c) 1993 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)extern.h	5.1 (Berkeley) %G%
 */

struct dkinfo {
	struct	dkinfo *dk_next;
	char	*dk_name;	/* full-length name */
	char	dk_2c[3];	/* two-character version of dk_name */
	int	dk_sel;		/* 1 if we should print this one */
	u_long	dk_addr;	/* kernel addr of struct dkdevice */
	long	dk_oxfer;	/* previous xfer stat */
	long	dk_dxfer;	/* delta between oxfer and cur xfer */
};
extern struct dkinfo *dkinfo;
extern int ndrives;
extern kvm_t *kd;

void	dointr __P((void));
void	domem __P((void));
void	dosum __P((void));
void	dovmstat __P((u_int, int));
void	errexit __P((const char *, ...));
long	getuptime __P((void));
void	knlist __P((struct nlist *));
void	kread __P((u_long, void *, size_t, char *));
