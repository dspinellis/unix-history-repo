/*	physio.c	1.6	83/07/18	*/

#include <stdio.h>
#include "cpmio.h"
#include <sys/file.h>

/*
 * Write physical sector to floppy disk file
 */

putpsect(tr, sect, buf)
	char buf[];
{

	long newpos;

	if (sect > sectrk || sect < 1) {
		fprintf(stderr, "putpsect: sector number out of range: %d\n",
									sect);
		return (EOF);
	}
	newpos = (long) (sect + (tr * sectrk) -1 ) * seclth;
	if (lseek(fid, newpos, 0) == -1) {
		perror("putpsect");
		return (EOF);
	}
	if (write(fid, buf, seclth) == seclth) 
		return (1);
	perror("putpsect");
	fprintf(stderr, "track %d, sect %d\n", tr, sect);
	return (EOF);
}

/*
 * Read physical sector from floppy disk file
 */

getpsect(tr, sect, buf)
	char buf[];
{

	long newpos;

	if (sect > sectrk || sect < 1) {
		fprintf("getpsect: sector number out of range: %d\n",sect);
		return (EOF);
	}
	newpos = (long) (sect + (tr * sectrk) -1 ) * seclth;
	if (lseek(fid, newpos, 0) == -1) {
		perror("getpsect");
		return (EOF);
	}
	if (read(fid, buf, seclth) != seclth) {
		perror("getpsect");
		fprintf(stderr, "track %d, sect %d\n", tr, sect);
		return (EOF);
	}
	return (1);
}

/* 
 * Initialize a new floppy disk file in "name",
 * return its file pointer.
 */

initcpm(name)
	char *name;
{
	int f, i;
	char buf[512];

	if ((f = open(name, O_CREAT|O_RDWR, 0644)) < 0)
		return (EOF);
	for (i=0; i<512; i++)
		buf[i] = '\345';
	/*
	 * Initialize (with 0xe5) the first four tracks
	 * on the `floppy'
	 */
	for (i=0; i < (4*seclth*sectrk); i += 512)
		write(f, buf, 512);
	return (f);
}
