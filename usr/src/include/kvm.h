/*-
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)kvm.h	5.7 (Berkeley) %G%
 */

#ifndef _KVM_H_
#define	_KVM_H_

/* Default version symbol. */
#define	VRS_SYM		"_version"
#define	VRS_KEY		"VERSION"

#include <nlist.h>
#include <sys/cdefs.h>

__BEGIN_DECLS

typedef struct __kvm kvm_t;

kvm_t	*kvm_open __P((const char *, const char *, const char *, int,
		       const char *));
kvm_t	*kvm_openfiles __P((const char *, const char *, const char *, int,
			    char *));
int	 kvm_close __P((kvm_t *));
int	 kvm_read __P((kvm_t *, unsigned long, char *, unsigned int));
int	 kvm_write __P((kvm_t *, unsigned long, const char *, unsigned int));
int	 kvm_nlist __P((kvm_t *, struct nlist *));
char	*kvm_geterr __P((kvm_t *));

struct kinfo_proc *kvm_getprocs __P((kvm_t *, int, int, int *));
char	**kvm_getargv __P((kvm_t *, const struct kinfo_proc *, int));
char	**kvm_getenvv __P((kvm_t *, const struct kinfo_proc *, int));

__END_DECLS

#endif /* !_KVM_H_ */
