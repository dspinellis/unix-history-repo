/*
 * pq - print the quota file affecting you most immediately
 *
 * Kurt Shoens UCB July, 1977
 */
     struct  {
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
     } inode;
main(ct,av)
	char **av;
{
	int i,lastnode;
	if (ct==1)
	{
		lastnode=0;
		stat(".",&inode);
		while (lastnode != inode.inumber)
		{
			lastnode=inode.inumber;
			if (quota(".q"))
			{
				pq(".q");
				exit(0);
			}
			chdir("..");
			stat(".",&inode);
		}
		exit(9);
	}
	for (i=1;i<ct;++i) pq(av[i]);
}

pq(cp)
	char *cp;
{
	float cut,a,b;
	if (stat(cp,&inode))
	{
		perror(cp);
		return;
	}
	if (!quota(cp))
	{
		printf("%s: not a q-file.\n",cp);
		return;
	}
	a = inode.addr[1];
	b = inode.addr[2];
	cut = 100.0*a/b;
	printf("%d/%d %3.0f%%\n",inode.addr[1],inode.addr[2],cut);
	return;
}

quota(cp)
	char *cp;
{
	if (stat(cp,&inode)) return(0);
	if (((inode.flags & 060000) != 020000) || (inode.addr[0]!= -1))
		return(0);
	return(-1);
}
