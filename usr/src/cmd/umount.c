static char *sccsid = "@(#)umount.c	4.3 (Berkeley) 10/15/80";
#include <stdio.h>
#include <fstab.h>
/*
 * umount
 */

#define	NMOUNT	16
#define	NAMSIZ	32

struct mtab {
	char	file[NAMSIZ];
	char	spec[NAMSIZ];
} mtab[NMOUNT];

main(argc, argv)
char **argv;
{
	register struct mtab *mp;
	register char *p1, *p2;
	int mf;

	sync();
	mf = open("/etc/mtab", 0);
	read(mf, (char *)mtab, NMOUNT*2*NAMSIZ);
	if(argc != 2) {
		printf("arg count\n");
		return(1);
	}
	if (strcmp(argv[1], "-a") == 0){
		if (setfsent() == 0)
			perror(FSTAB), exit(1);
		umountall();
		endfsent();
	} else {
		int	back;
		if (back = umountfs(argv[1])){
			if (back < 0)
				perror("umount");
			exit(1);
		}
	}
	exit(0);
}
/*
 *	It is important to unmount the files in
 *	reverse! order from the order they were mounted,
 *	so that file systems mounted as children to other
 *	file systems get removed in the right order.
 */
umountall()
{
	struct	fstab	fs;
	struct	fstab	*fsp;
	if ( (fsp = getfsent()) == 0)
		return;
	fs = *fsp;	/* save info locally; it is static from getfsent() */
	umountall();
	if (strcmp(fs.fs_file, "/") == 0)
		return;
	if (strcmp(fs.fs_type, FSTAB_RW) &&
	    strcmp(fs.fs_type, FSTAB_RO))
		return;
	if (umountfs(fs.fs_spec) < 0)
		fprintf(stdout, "Unmount of special file %s FAILED\n", fs.fs_spec);
	else
		fprintf(stdout, "Unmounted special file %s\n", fs.fs_spec);
	fflush(stdout);
}

int umountfs(name)
	char	*name;
{
	register	char	*p1, *p2;
	register	struct	mtab	*mp;
	int	mf;

	if (umount(name) < 0) {
		return(-1);
	}
	p1 = name;
	while(*p1++)
		;
	p1--;
	while(*--p1 == '/')
		*p1 = '\0';
	while(p1 > name && *--p1 != '/')
		;
	if(*p1 == '/')
		p1++;
	name = p1;
	for (mp = mtab; mp < &mtab[NMOUNT]; mp++) {
		p1 = name;
		p2 = &mp->spec[0];
		while (*p1++ == *p2)
			if (*p2++ == 0) {
				for (p1 = mp->file; p1 < &mp->file[NAMSIZ*2];)
					*p1++ = 0;
				mp = &mtab[NMOUNT];
				while ((--mp)->file[0] == 0);
				mf = creat("/etc/mtab", 0644);
				write(mf, (char *)mtab, (mp-mtab+1)*2*NAMSIZ);
				return(0);
			}
	}
	printf("%s not in mount table\n", name);
	return(1);
}
