static char *sccsid = "@(#)mount.c	4.3 (Berkeley) 10/15/80";
#include <stdio.h>
#include <fstab.h>

/*
 * mount
 */

int	mountall;
#define	NMOUNT	16
#define	NAMSIZ	32

struct mtab {
	char	file[NAMSIZ];
	char	spec[NAMSIZ];
} mtab[NMOUNT];

int	ro;
main(argc, argv)
char **argv;
{
	register struct mtab *mp;
	register char *np;
	int mf;

	mountall = 0;
	mf = open("/etc/mtab", 0);
	read(mf, (char *)mtab, NMOUNT*2*NAMSIZ);
	if (argc==1) {
		for (mp = mtab; mp < &mtab[NMOUNT]; mp++)
			if (mp->file[0])
				printf("%s on %s\n", mp->spec, mp->file);
		exit(0);
	}

	if (argc == 2){
		if (strcmp(argv[1], "-a") == 0)
			mountall++;
		else {
			fprintf(stdout,"arg count\n");
			exit(1);
		}
	}

	if (!mountall){
		ro = 0;
		if(argc > 3)
			ro++;
		if (mountfs(argv[1], argv[2], ro)){
			perror("mount");
			exit(1);
		}
	} else {
		struct	fstab	*fsp;
		close(2); dup(1);
		if (setfsent() == 0)
			perror(FSTAB), exit(1);
		while ( (fsp = getfsent()) != 0){
			if (strcmp(fsp->fs_file, "/") == 0)
				continue;
			ro = !strcmp(fsp->fs_type, FSTAB_RO);
			if (ro==0 && strcmp(fsp->fs_type, FSTAB_RW))
				continue;
			if (mountfs(fsp->fs_spec, fsp->fs_file, ro))
				failed(fsp);
			else
				succeed(fsp);
		}
		endfsent();
	}
	exit(0);
}
failed(fsp)
	register	struct	fstab *fsp;
{
	extern int errno;
	extern char *sys_errlist[];
	int err = errno;
	printf("Attempt to mount ");
	location(fsp);
	printf("FAILED: %s\n", sys_errlist[err]);
}
succeed(fsp)
	register	struct	fstab *fsp;
{
	printf("Mounted ");
	location(fsp);
	printf("\n");
}
location(fsp)
	register	struct	fstab *fsp;
{
	extern	int	ro;
	printf("%s on %s %s ",
		fsp->fs_file, fsp->fs_spec,
		ro ? "(Read Only)" : "");
}

mountfs(spec, name, ro)
	char	*spec, *name;
	int	ro;
{
	register	char	*np;
	register	struct	mtab	*mp;
	int	mf;

	if(mount(spec, name, ro) < 0) {
		return(1);
	}
	np = spec;
	while(*np++)
		;
	np--;
	while(*--np == '/')
		*np = '\0';
	while(np > spec && *--np != '/')
		;
	if(*np == '/')
		np++;
	spec = np;
	for (mp = mtab; mp < &mtab[NMOUNT]; mp++) {
		if (mp->file[0] == 0) {
			for (np = mp->spec; np < &mp->spec[NAMSIZ-1];)
				if ((*np++ = *spec++) == 0)
					spec--;
			for (np = mp->file; np < &mp->file[NAMSIZ-1];)
				if ((*np++ = *name++) == 0)
					name--;
			mp = &mtab[NMOUNT];
			while ((--mp)->file[0] == 0);
			mf = creat("/etc/mtab", 0644);
			write(mf, (char *)mtab, (mp-mtab+1)*2*NAMSIZ);
			return(0);
		}
	}
	return(0);
}
