/*-
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)kvm.h	5.4 (Berkeley) %G%
 */

#ifndef _KVM_H_
#define	_KVM_H_

/* Default version symbol. */
#define	VRS_SYM		"_version"
#define	VRS_KEY		"VERSION"

#include <sys/cdefs.h>

__BEGIN_DECLS
struct proc;
struct user;
char		*kvm_getargs __P((const struct proc *, const struct user *));
struct eproc	*kvm_geteproc __P((const struct proc *));
char		*kvm_geterr __P((void));
int		 kvm_getprocs __P((int, int));
struct user	*kvm_getu __P((const struct proc *));
struct proc	*kvm_nextproc __P((void));
int		 kvm_nlist __P((struct nlist *));
int		 kvm_openfiles __P((const char *, const char *, const char *));
int		 kvm_read __P((void *, void *, int));
__END_DECLS

#endif /* !_KVM_H_ */
