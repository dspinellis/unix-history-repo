#ifndef lint
static char version[] = "@(#)main.c	3.1 (Berkeley) %G%";
#endif

#include <sys/param.h>
#include <sys/inode.h>
#include <sys/fs.h>
#include "fsck.h"

checkfilesys(filesys)
	char *filesys;
{

	devname = filesys;
	if (setup(filesys) == 0) {
		if (preen)
			pfatal("CAN'T CHECK FILE SYSTEM.");
		return;
	}
/* 1: scan inodes tallying blocks used */
	if (preen == 0) {
		printf("** Last Mounted on %s\n", sblock.fs_fsmnt);
		if (hotroot)
			printf("** Root file system\n");
		printf("** Phase 1 - Check Blocks and Sizes\n");
	}
	pass1();

/* 1b: locate first references to duplicates, if any */
	if (enddup != &duplist[0]) {
		if (preen)
			pfatal("INTERNAL ERROR: dups with -p");
		printf("** Phase 1b - Rescan For More DUPS\n");
		pass1b();
	}

/* 2: traverse directories from root to mark all connected directories */
	if (preen == 0)
		printf("** Phase 2 - Check Pathnames\n");
	pass2();

/* 3: scan inodes looking for disconnected directories */
	if (preen == 0)
		printf("** Phase 3 - Check Connectivity\n");
	pass3();

/* 4: scan inodes looking for disconnected files; check reference counts */
	if (preen == 0)
		printf("** Phase 4 - Check Reference Counts\n");
	pass4();

/* 5: check resource counts in cylinder groups */
	if (preen == 0)
		printf("** Phase 5 - Check Cyl groups\n");
	pass5();

	if (fixcg) {
		if (preen == 0)
			printf("** Phase 6 - Salvage Cylinder Groups\n");
		makecg();
		n_ffree = sblock.fs_cstotal.cs_nffree;
		n_bfree = sblock.fs_cstotal.cs_nbfree;
	}

	pwarn("%d files, %d used, %d free (%d frags, %d blocks)\n",
	    n_files, n_blks - howmany(sblock.fs_cssize, sblock.fs_fsize),
	    n_ffree + sblock.fs_frag * n_bfree, n_ffree, n_bfree);
	if (dfile.mod) {
		(void)time(&sblock.fs_time);
		sbdirty();
	}
	ckfini();
	free(blockmap);
	free(freemap);
	free(statemap);
	free((char *)lncntp);
	if (!dfile.mod)
		return;
	if (!preen) {
		printf("\n***** FILE SYSTEM WAS MODIFIED *****\n");
		if (hotroot)
			printf("\n***** REBOOT UNIX *****\n");
	}
	if (hotroot) {
		sync();
		exit(4);
	}
}
