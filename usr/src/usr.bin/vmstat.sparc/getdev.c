/*
 * Copyright (c) 1993 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)getdev.c	5.1 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <sys/device.h>

#include <kvm.h>

#include "getdev.h"

extern kvm_t *kd;

extern void errexit __P((const char *, ...));
extern void kread __P((u_long, void *, size_t, char *));

/*
 * Read in devices and add those whose predicate matches.
 */
void
getdev(alladdr, take, add)
	u_long alladdr;
	int (*take) __P((struct device *));
	void (*add) __P((u_long, struct device *));
{
	register u_long addr;
	struct device dev, *alldevs;

	kread(alladdr, &alldevs, sizeof(alldevs), "alldevs");
	for (addr = (u_long)alldevs; addr != 0; addr = (u_long)dev.dv_next) {
		kread(addr, &dev, sizeof(dev), "(device)");
		if (take(&dev))
			add(addr, &dev);
	}
}
