static char *sccsid = "@(#)umount.c	4.2 (Berkeley) %G%";
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
		FILE	*fs_file;
		struct	fstab	fs;
		if ((fs_file = fopen(FSTAB, "r")) == NULL){
			perror(FSTAB);
			exit(1);
		}
		while (!feof(fs_file)){
			fscanf(fs_file, FSTABFMT, FSTABARG(&fs));
			if (strcmp(fs.fs_file, "/") == 0)
				continue;
			if (strcmp(fs.fs_type, "rw") &&
			    strcmp(fs.fs_type, "ro"))
				continue;
			fprintf(stderr, "Unmounting special file %s\n",
				fs.fs_spec);
			fflush(stderr);
			if (umountfs(fs.fs_spec))
				continue;
		}
		fclose(fs_file);
	} else {
		if (umountfs(argv[1]))
			exit(1);
	}
	exit(0);
}

int umountfs(name)
	char	*name;
{
	register	char	*p1, *p2;
	register	struct	mtab	*mp;
	int	mf;

	if (umount(name) < 0) {
		perror("umount");
		return(1);
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
