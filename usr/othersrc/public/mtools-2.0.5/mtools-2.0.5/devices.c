/*
 * Device tables.  See the Configure file for a complete description.
 */

#include <stdio.h>
#include "msdos.h"

#ifdef DELL
struct device devices[] = {
	{'A', "/dev/rdsk/f0d9dt", 0L, 12, 0, (int (*) ()) 0, 40, 2, 9},
	{'A', "/dev/rdsk/f0q15dt", 0L, 12, 0, (int (*) ()) 0, 80, 2, 15},
	{'A', "/dev/rdsk/f0d8dt", 0L, 12, 0, (int (*) ()) 0, 40, 2, 8},
	{'B', "/dev/rdsk/f13ht", 0L, 12, 0, (int (*) ()) 0, 80, 2, 18},
	{'B', "/dev/rdsk/f13dt", 0L, 12, 0, (int (*) ()) 0, 80, 2, 9},
	{'C', "/dev/rdsk/dos", 0L, 16, 0, (int (*) ()) 0, 0, 0, 0},
	{'\0', (char *) NULL, 0L, 0, 0, (int (*) ()) 0, 0, 0, 0}
};
#endif /* DELL */

#ifdef __386BSD__
struct device devices[] = {
	{'A', "/dev/rfd0a", 0L, 12, 0, (int (*) ()) 0, 80, 2, 18}, /* 1.44m */
	{'A', "/dev/rfd0a", 0L, 12, 0, (int (*) ()) 0, 80, 2, 15}, /* 1.2m */
	{'A', "/dev/rfd0a", 0L, 12, 0, (int (*) ()) 0, 80, 2, 9},  /* 720k */
	{'A', "/dev/rfd0a", 0L, 12, 0, (int (*) ()) 0, 40, 2, 9},  /* 360k */
	{'A', "/dev/rfd0a", 0L, 12, 0, (int (*) ()) 0, 40, 2, 8},  /* 320k */
	{'B', "/dev/rfd1a", 0L, 12, 0, (int (*) ()) 0, 80, 2, 18}, /* 1.44m */
	{'B', "/dev/rfd1a", 0L, 12, 0, (int (*) ()) 0, 80, 2, 15}, /* 1.2m */
	{'B', "/dev/rfd1a", 0L, 12, 0, (int (*) ()) 0, 80, 2, 9},  /* 720k */
	{'B', "/dev/rfd1a", 0L, 12, 0, (int (*) ()) 0, 40, 2, 9},  /* 360k */
	{'B', "/dev/rfd1a", 0L, 12, 0, (int (*) ()) 0, 40, 2, 8},  /* 320k */
	{'\0', (char *) NULL, 0L, 0, 0, (int (*) ()) 0, 0, 0, 0}
};
#endif /* __386BSD__ */

#ifdef ISC
struct device devices[] = {
	{'A', "/dev/rdsk/f0d9dt", 0L, 12, 0, (int (*) ()) 0, 40, 2, 9},
	{'A', "/dev/rdsk/f0q15dt", 0L, 12, 0, (int (*) ()) 0, 80, 2, 15},
	{'A', "/dev/rdsk/f0d8dt", 0L, 12, 0, (int (*) ()) 0, 40, 2, 8},
	{'B', "/dev/rdsk/f13ht", 0L, 12, 0, (int (*) ()) 0, 80, 2, 18},
	{'B', "/dev/rdsk/f13dt", 0L, 12, 0, (int (*) ()) 0, 80, 2, 9},
	{'C', "/dev/rdsk/0p1", 0L, 16, 0, (int (*) ()) 0, 0, 0, 0},
	{'D', "/usr/vpix/defaults/C:", 8704L, 12, 0, (int (*) ()) 0, 0, 0, 0},
	{'E', "$HOME/vpix/C:", 8704L, 12, 0, (int (*) ()) 0, 0, 0, 0},
	{'\0', (char *) NULL, 0L, 0, 0, (int (*) ()) 0, 0, 0, 0}
};
#endif /* ISC */

#ifdef MASSCOMP
struct device devices[] = {
	{'A', "/dev/rflp", 0L, 12, 0, (int (*) ()) 0, 80, 2, 8},
	{'\0', (char *) NULL, 0L, 0, 0, (int (*) ()) 0, 0, 0, 0}
};
#endif /* MASSCOMP */

#ifdef SPARC
struct device devices[] = {
	{'A', "/dev/rfd0c", 0L, 12, 0, (int (*) ()) 0, 80, 2, 18},
	{'A', "/dev/rfd0c", 0L, 12, 0, (int (*) ()) 0, 80, 2, 9},
	{'\0', (char *) NULL, 0L, 0, 0, (int (*) ()) 0, 0, 0, 0}
};
#endif /* SPARC */

#ifdef UNIXPC
#include <sys/gdioctl.h>
#include <fcntl.h>

int init_unixpc();

struct device devices[] = {
	{'A', "/dev/rfp020", 0L, 12, O_NDELAY, init_unixpc, 40, 2, 9},
	{'C', "/usr/bin/DOS/dvd000", 0L, 12, 0, (int (*) ()) 0, 0, 0, 0},
	{'\0', (char *) NULL, 0L, 0, 0, (int (*) ()) 0, 0, 0, 0}
};

int
init_unixpc(fd, ntracks, nheads, nsect)
int fd, ntracks, nheads, nsect;
{
	struct gdctl gdbuf;

	if (ioctl(fd, GDGETA, &gdbuf) == -1) {
		ioctl(fd, GDDISMNT, &gdbuf);
		return(1);
	}

	gdbuf.params.cyls = ntracks * nheads;
	gdbuf.params.heads = nheads;
	gdbuf.params.psectrk = nsect;

	gdbuf.params.pseccyl = gdbuf.params.psectrk * gdbuf.params.heads;
	gdbuf.params.flags = 1;		/* disk type flag */
	gdbuf.params.step = 0;		/* step rate for controller */
	gdbuf.params.sectorsz = 512;	/* sector size */

	if (ioctl(fd, GDSETA, &gdbuf) < 0) {
		ioctl(fd, GDDISMNT, &gdbuf);
		return(1);
	}
	return(0);
}
#endif /* UNIXPC */
