/*
 * $Id: mount.x,v 5.2 90/06/23 22:20:19 jsp Rel $
 *
 * Copyright (c) 1990 Jan-Simon Pendry
 * Copyright (c) 1990 Imperial College of Science, Technology & Medicine
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Jan-Simon Pendry at Imperial College, London.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that: (1) source distributions retain this entire copyright notice and
 * comment, and (2) distributions including binaries display the following
 * acknowledgement:  ``This product includes software developed by the
 * University of California, Berkeley and its contributors'' in the
 * documentation or other materials provided with the distribution and in
 * all advertising materials mentioning features or use of this software.
 * Neither the name of the University nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)mount.x	5.1 (Berkeley) 7/19/90
 */

/*
 * Protocol description for the mount program
 */


const MNTPATHLEN = 1024;	/* maximum bytes in a pathname argument */
const MNTNAMLEN = 255;		/* maximum bytes in a name argument */
const FHSIZE = 32;		/* size in bytes of a file handle */

/*
 * The fhandle is the file handle that the server passes to the client.
 * All file operations are done using the file handles to refer to a file
 * or a directory. The file handle can contain whatever information the
 * server needs to distinguish an individual file.
 */
typedef opaque fhandle[FHSIZE];	

/*
 * If a status of zero is returned, the call completed successfully, and 
 * a file handle for the directory follows. A non-zero status indicates
 * some sort of error. The status corresponds with UNIX error numbers.
 */
union fhstatus switch (unsigned fhs_status) {
case 0:
	fhandle fhs_fhandle;
default:
	void;
};

/*
 * The type dirpath is the pathname of a directory
 */
typedef string dirpath<MNTPATHLEN>;

/*
 * The type name is used for arbitrary names (hostnames, groupnames)
 */
typedef string name<MNTNAMLEN>;

/*
 * A list of who has what mounted
 */
typedef struct mountbody *mountlist;
struct mountbody {
	name ml_hostname;
	dirpath ml_directory;
	mountlist ml_next;
};

/*
 * A list of netgroups
 */
typedef struct groupnode *groups;
struct groupnode {
	name gr_name;
	groups gr_next;
};

/*
 * A list of what is exported and to whom
 */
typedef struct exportnode *exports;
struct exportnode {
	dirpath ex_dir;
	groups ex_groups;
	exports ex_next;
};

program MOUNTPROG {
	/*
	 * Version one of the mount protocol communicates with version two
	 * of the NFS protocol. The only connecting point is the fhandle 
	 * structure, which is the same for both protocols.
	 */
	version MOUNTVERS {
		/*
		 * Does no work. It is made available in all RPC services
		 * to allow server reponse testing and timing
		 */
		void
		MOUNTPROC_NULL(void) = 0;

		/*	
		 * If fhs_status is 0, then fhs_fhandle contains the
	 	 * file handle for the directory. This file handle may
		 * be used in the NFS protocol. This procedure also adds
		 * a new entry to the mount list for this client mounting
		 * the directory.
		 * Unix authentication required.
		 */
		fhstatus 
		MOUNTPROC_MNT(dirpath) = 1;

		/*
		 * Returns the list of remotely mounted filesystems. The 
		 * mountlist contains one entry for each hostname and 
		 * directory pair.
		 */
		mountlist
		MOUNTPROC_DUMP(void) = 2;

		/*
		 * Removes the mount list entry for the directory
		 * Unix authentication required.
		 */
		void
		MOUNTPROC_UMNT(dirpath) = 3;

		/*
		 * Removes all of the mount list entries for this client
		 * Unix authentication required.
		 */
		void
		MOUNTPROC_UMNTALL(void) = 4;

		/*
		 * Returns a list of all the exported filesystems, and which
		 * machines are allowed to import it.
		 */
		exports
		MOUNTPROC_EXPORT(void)  = 5;
	
		/*
		 * Identical to MOUNTPROC_EXPORT above
		 */
		exports
		MOUNTPROC_EXPORTALL(void) = 6;
	} = 1;
} = 100005;
