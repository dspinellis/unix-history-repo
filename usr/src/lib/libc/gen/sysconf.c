/*-
 * Copyright (c) 1993 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Sean Eric Fagan of Cygnus Support.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)sysconf.c	5.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/param.h>
#include <sys/sysctl.h>

#include <errno.h>
#include <unistd.h>

long
sysconf(name)
	int name;
{
	struct clockinfo clk;
	size_t len;
	int mib[2], value;

	switch (name) {
	case _SC_ARG_MAX:
		mib[0] = CTL_KERN;
		mib[1] = KERN_ARGMAX;
		break;
	case _SC_CHILD_MAX:
		mib[0] = CTL_KERN;
		mib[1] = KERN_MAXPROC;
		break;
	case _SC_CLK_TCK:
		mib[0] = CTL_KERN;
		mib[1] = KERN_CLOCKRATE;
		len = sizeof(struct clockinfo);
		return (sysctl(mib, 2,
		    &clk, &len, NULL, 0) == -1 ? -1 : clk.hz);
	case _SC_NGROUPS_MAX:
		mib[0] = CTL_KERN;
		mib[1] = KERN_NGROUPS;
		break;
	case _SC_OPEN_MAX:
		mib[0] = CTL_KERN;
		mib[1] = KERN_MAXFILES;
		break;
	case _SC_JOB_CONTROL:
		mib[0] = CTL_KERN;
		mib[1] = KERN_JOB_CONTROL;
		break;
	case _SC_SAVED_IDS:
		mib[0] = CTL_KERN;
		mib[1] = KERN_SAVED_IDS;
		break;
	case _SC_VERSION:
		mib[0] = CTL_KERN;
		mib[1] = KERN_POSIX1;
		break;
	case _SC_BC_BASE_MAX:
		mib[0] = CTL_USER;
		mib[1] = USER_BC_BASE_MAX;
		break;
	case _SC_BC_DIM_MAX:
		mib[0] = CTL_USER;
		mib[1] = USER_BC_DIM_MAX;
		break;
	case _SC_BC_SCALE_MAX:
		mib[0] = CTL_USER;
		mib[1] = USER_BC_SCALE_MAX;
		break;
	case _SC_BC_STRING_MAX:
		mib[0] = CTL_USER;
		mib[1] = USER_BC_STRING_MAX;
		break;
	case _SC_COLL_WEIGHTS_MAX:
		mib[0] = CTL_USER;
		mib[1] = USER_COLL_WEIGHTS_MAX;
		break;
	case _SC_EXPR_NEST_MAX:
		mib[0] = CTL_USER;
		mib[1] = USER_EXPR_NEST_MAX;
		break;
	case _SC_LINE_MAX:
		mib[0] = CTL_USER;
		mib[1] = USER_LINE_MAX;
		break;
	case _SC_RE_DUP_MAX:
		mib[0] = CTL_USER;
		mib[1] = USER_RE_DUP_MAX;
		break;
	case _SC_2_VERSION:
		mib[0] = CTL_USER;
		mib[1] = USER_POSIX2_VERSION;
		break;
	case _SC_2_C_BIND:
		mib[0] = CTL_USER;
		mib[1] = USER_POSIX2_C_BIND;
		break;
	case _SC_2_C_DEV:
		mib[0] = CTL_USER;
		mib[1] = USER_POSIX2_C_DEV;
		break;
	case _SC_2_FORT_DEV:
		mib[0] = CTL_USER;
		mib[1] = USER_POSIX2_FORT_DEV;
		break;
	case _SC_2_FORT_RUN:
		mib[0] = CTL_USER;
		mib[1] = USER_POSIX2_FORT_RUN;
		break;
	case _SC_2_LOCALEDEF:
		mib[0] = CTL_USER;
		mib[1] = USER_POSIX2_LOCALEDEF;
		break;
	case _SC_2_SW_DEV:
		mib[0] = CTL_USER;
		mib[1] = USER_POSIX2_SW_DEV;
		break;
	default:
		errno = EINVAL;
		return (-1);
	}
	len = sizeof(value);
	return (sysctl(mib, 2, &value, &len, NULL, 0) == -1 ? -1 : value); 
}
