/* lnall-
 *	son of mvall, son of cpall, son of ...
 */

char	buf[100];

main(argc, argv)
char **argv;
{
	register char *c1, *c2, *cp;
	int i;

	if(argc < 3) {
		write(2, "arg count\n", 10);
		exit();
	}
	argc--;
	if (!dir(argv[argc]))
	{
		printf("%s: not a directory\n", argv[argc]);
		exit(1);
	}
	c1 = buf;
	c2 = argv[argc];
	while(*c1++ = *c2++);
	c1[-1] = '/';
	for(i=1; i<argc; i++)
	{
		c2 = c1;
		cp = actual(argv[i]);
		while (*c2++ = *cp++);
		if (link(argv[i],buf)) perror(argv[i]);
	}
}

dir(n)
	char *n;
{
     struct inode {
       char  minor;	    /* +0: minor device of i-node */
       char  major;	    /* +1: major device */
       int   inumber;	    /* +2 */
       int   flags;	    /* +4: see below */
       char  nlinks;	    /* +6: number of links to file */
       char  uid;	    /* +7: user ID of owner */
       char  gid;	    /* +8: group ID of owner */
       char  size0;	    /* +9: high byte of 24-bit size */
       int   size1;	    /* +10: low word of 24-bit size */
       int   addr[8];	    /* +12: block numbers or device number */
       int   actime[2];     /* +28: time of last access */
       int   modtime[2];    /* +32: time of last modification */
     } statbuf;
	if (stat(n,&statbuf)) return(0);
	return((statbuf.flags & 060000) == 040000);
}

actual(str)
	char *str;
{
	register char *f;
	register slash;
	f = str;
	slash = 0;
	while (*f) if (*f++ == '/') slash++;
	if (!slash) return(str);
	while (*f != '/') --f;
	++f;
	return(f);
}
