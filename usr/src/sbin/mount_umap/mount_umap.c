/*
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 * All rights reserved.
 *
 * This code is derived from software donated to Berkeley by
 * Jan-Simon Pendry.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)mount_umap.c	8.1 (Berkeley) %G%
 */

#include <sys/param.h>
#include <sys/mount.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <miscfs/umapfs/umap.h>

#include <errno.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>

void usage __P((void));

#define ROOTUSER 0

/* This define controls whether any user but the superuser can own and
 * write mapfiles.  If other users can, system security can be gravely
 * compromised.  If this is not a concern, undefine SECURITY.
 */

#define MAPSECURITY 1

/* This routine provides the user interface to mounting a umap layer.
 * It takes 4 mandatory parameters.  The mandatory arguments are the place 
 * where the next lower level is mounted, the place where the umap layer is to
 * be mounted, the name of the user mapfile, and the name of the group
 * mapfile.  The routine checks the ownerships and permissions on the
 * mapfiles, then opens and reads them.  Then it calls mount(), which
 * will, in turn, call the umap version of mount. 
 */

int
main(argc, argv)
	int argc;
	char *argv[];
{
	int ch, mntflags;
	int e, i, nentries, gnentries, count;
        int mapdata[MAPFILEENTRIES][2];
        int gmapdata[GMAPFILEENTRIES][2];
	char *fs_type="umap";
	char *source, *target;
	char *mapfile, *gmapfile;
        FILE *fp, *gfp, *fopen();
	struct stat statbuf;
	struct umap_args args;

	mntflags = 0;
	while ((ch = getopt(argc, argv, "F:")) != EOF)
		switch(ch) {
		case 'F':
			mntflags = atoi(optarg);
			break;
		case '?':
		default:
			usage();
		}
	argc -= optind;
	argv += optind;

	if (argc != 4)
		usage();

	source = argv[i++];
	target = argv[i++];
	mapfile = argv[i++];
	gmapfile = argv[i++];

#ifdef MAPSECURITY
	/*
	 * Check that group and other don't have write permissions on
	 * this mapfile, and that the mapfile belongs to root. 
	 */
	if ( stat(mapfile, &statbuf) )
	{
		printf("mount_umap: can't stat %s\n",mapfile);
		perror("mount_umap: error status");
		notMounted();
	}

	if (statbuf.st_mode & S_IWGRP || statbuf.st_mode & S_IWOTH)
	{
		printf("mount_umap: Improper write permissions for %s, mode %x\n",
		    mapfile, statbuf.st_mode);
		notMounted();
	}

	if ( statbuf.st_uid != ROOTUSER )
	{
		printf("mount_umap: %s does not belong to root\n", mapfile);
		notMounted();
	}
#endif MAPSECURITY

	/*
	 * Read in uid mapping data.
	 */

	if ((fp = fopen(mapfile, "r")) == NULL) {
		printf("mount_umap: can't open %s\n",mapfile);
		notMounted();
	}
	fscanf(fp, "%d\n", &nentries);
	if (nentries > MAPFILEENTRIES)
		printf("mount_umap: nentries exceeds maximum\n");
#if 0
	else
		printf("reading %d entries\n", nentries);
#endif

	for(count = 0; count<nentries;count++) {
		if ((fscanf(fp, "%d %d\n", &(mapdata[count][0]),
		    &(mapdata[count][1]))) == EOF) {
			printf("mount_umap: %s, premature eof\n",mapfile);
			notMounted();
		}
#if 0
		/* fix a security hole */
		if (mapdata[count][1] == 0) {
			printf("mount_umap: Mapping to UID 0 not allowed\n");
			notMounted();
		}
#endif
	}

	/*
	 * Check that group and other don't have write permissions on
	 * this group mapfile, and that the file belongs to root. 
	 */
	if ( stat(gmapfile, &statbuf) )
	{
		printf("mount_umap: can't stat %s\n",gmapfile);
		perror("mount_umap: error status");
		notMounted();
	}

	if (statbuf.st_mode & S_IWGRP || statbuf.st_mode & S_IWOTH)
	{
		printf("mount_umap: Improper write permissions for %s, mode %x\n",
		    gmapfile, statbuf.st_mode);
	}

	if ( statbuf.st_uid != ROOTUSER )
	{
		printf("mount_umap: %s does not belong to root\n", mapfile);
	}

	/*
	 * Read in gid mapping data.
	 */
	if ((gfp = fopen(gmapfile, "r")) == NULL) {
		printf("mount_umap: can't open %s\n",gmapfile);
		notMounted();
	}
	fscanf(gfp, "%d\n", &gnentries);
	if (gnentries > GMAPFILEENTRIES)
		printf("mount_umap: gnentries exceeds maximum\n");
#if 0
	else
		printf("reading %d group entries\n", gnentries);
#endif

	for(count = 0; count<gnentries;count++) {
		if ((fscanf(gfp, "%d %d\n", &(gmapdata[count][0]),
		    &(gmapdata[count][1]))) == EOF) {
			printf("mount_umap: %s, premature eof on group mapfile\n",
			    gmapfile);
			notMounted();
		}
	}


	/*
	 * Setup mount call args.
	 */
	args.target = source;
	args.nentries = nentries;
	args.mapdata = &(mapdata[0][0]);
	args.gnentries = gnentries;
	args.gmapdata = &(gmapdata[0][0]);

#if 0
	printf("calling mount_umap(%s,%d,<%s>)\n",target,mntflags,
	       args.target);
#endif
	if (mount(MOUNT_UMAP, argv[1], mntflags, &args)) {
		(void)fprintf(stderr, "mount_umap: %s\n", strerror(errno));
	}
	exit(0);
}

void
usage()
{
	(void)fprintf(stderr,
	    "usage: mount_umap [ -F fsoptions ] target_fs mount_point user_mapfile group_mapfile\n");
	exit(1);
}

int
notMounted()
{
	(void)fprintf(stderr, "file system not mounted\n");
}
