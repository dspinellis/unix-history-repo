/*	ls.c	1.1	86/01/12	*/
/*	ls.c	6.1	83/07/29	*/

#include "param.h"
#include "inode.h"
#include "dir.h"
#include "fs.h"

#include "saio.h"

char line[100];

main()
{
	int io;
	register char	*ptr;
	struct inode	*ip;

	for(;;) {
		line[0] = 0;
		printf("\n\nls: ");
		gets(line);
		/* scan to end of line */
		for(ptr=line; *ptr; ptr++)
			;
		/* check to see if a file was specified */
		if(ptr == line) {
			printf("usage: dev(unit,0)/directory\n");
			continue;
		}
		/* do one correction first so the raw dev is not opened */
		if(*(--ptr) == ')') {
			*(++ptr) = '/';
			*(++ptr) = '.';
			*(++ptr) = (char)0;
		}
		if(*ptr == '/') {
			*(++ptr) = '.';
			*(++ptr) = (char)0;
		}
		if((io = open(line, 0)) >= 0)
			break;
	}
	if((io >= NFILES+3) || (io < 3))
		_stop("open returned corrupt file index!");
	ip = &iob[io-3].i_ino;
	if ((ip->i_mode & IFMT) != IFDIR) {
		printf("%s is not a directory!\n", line);
		_stop("");
	}
	if (ip->i_size == 0) {
		printf("%s is a zero length directory!\n", line);
		_stop("");
	}
	
	ls(io);
}

ls(io)
register io;
{

	register int	i, size;
	register char	*dp;
	static char	dirbuf[DIRBLKSIZ];

	printf ("\nInode -> Name\n");
	while ((size = read(io, dirbuf, DIRBLKSIZ)) == DIRBLKSIZ) {
		for(dp = dirbuf; (dp < (dirbuf + size)) &&
		    (dp + ((struct direct *)dp)->d_reclen) < (dirbuf + size);
		    dp += ((struct direct *)dp)->d_reclen) {
			if(((struct direct *)dp)->d_ino == 0)
				continue;
			if(((struct direct *)dp)->d_reclen > 
			    DIRSIZ(((struct direct *)dp)))
				continue;
			if(((struct direct *)dp)->d_namlen > MAXNAMLEN+1)
				_stop("Corrupt file name length!  Run fsck soon!\n");
			printf("%s -> %d\n", ((struct direct *)dp)->d_name,
			    ((struct direct *)dp)->d_ino);
		}
	}
}
