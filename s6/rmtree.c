#
/* UCB rmtree
 * rmtree dir . . .
 * author: Kurt Shoens
 * recursively removes files and directories
 * in a subtree.  The -v flag prevents it from
 * removing the final directory, ie the one
 * you specify.  The incredible danger of
 * this program must by now be apparent to you.  It
 * asks you if you are sure on each argument.
 */


#define ENOTDIR 20
#define ENOENT 2

#define STACKSIZE 1024

int vflag;
int errs;
int fflag;
int cflag;
int active;

/* set warnflag true to get "last chance before . . . " */

int warnflag 1;

char pathname[STACKSIZE];

char *pathptr;
char *myname;

extern errno;

main(ct,av)
	char **av;
{
	int level,i,thiser;
	char crap[36];
	setuid(getuid());
	myname = av[0];
/*
 *	if (getuid())
 *	{
 *		printf("sorry, not super user\n");
 *		exit(1);
 *	}
 */
	if (ct<2)
	{
		usage(myname);
		exit(9);
	}
	for (i=1;i<ct;++i)
	{
		if (*av[i] == '-')
		{
			process(av[i]);
			continue;
		}
		++active;
		if (stat(av[i],crap))
		{
			perror(av[i]);
			errs++;
			continue;
		}
		if (!dir(av[i]))
		{
			errno = ENOTDIR;
			perror(av[i]);
			errs++;
		}
		else
		{
			if (warning(av[i],warnflag && !fflag))
			{
				if (cflag && !vflag) chmod(av[i], 0700);
				pathptr = pathname;
				pushstr(av[i]);
				thiser = descend();
				errs =+ thiser;
				if (!vflag && !thiser) errs =+ rmdir(av[i]);
			}
		}
	}
	if (!active)
	{
		usage(myname);
		exit(9);
	}
	exit(errs);
}

descend()
{
	int offset,j,err,markerr,xcnt;
	static f,inum,*ip;
	register char *cp;
	char name[16];
	markerr=0;
	offset=16;
	f=open(pathname,0);
	if (f==-1)
	{
		prs(pathname);
		prs(": no read permission\n");
		return(1);
	}
	j=read(f,name,16);
	xcnt=0;
	while (j)
	{
		ip=name;
		inum= *ip;
		if (!scomp(&name[2],".") && !scomp(&name[2],"..") && inum)
		{
			if (!xcnt++) if (access(pathname,3))
			{
				prs(pathname);
				prs(": need both X and W permission\n");
				close(f);
				return(1);
			}
			pushstr(&name[2]);
			if (dir(pathname))
			{
				close(f);
				if (cflag) chmod(pathname, 0700);
				err=descend();
				if (!err) rmdir(pathname);
				else markerr=1;
				popstr();
				f = open(pathname,0);
				seek(f,offset,0);
			}
			else
			{

				/* if unlink fails, keeps going ?? */

				if (rm(pathname)) markerr=1;
				popstr();
			}
		}
		j=read(f,name,16);
		offset=offset+16;
	}
	close(f);
	return(markerr);
}

rmdir(str)
	char *str;
{
	int p;
	p = fork();
	if (!p)
	{
		execl("/bin/rmdir","rmdir",str,0);
		perror("/bin/rmdir");
		exit(1);
	}
	else wait(&p);
	return(p);
}

rm(f)
	char *f;
{
	if (quota(f) && vflag) return(1);
	if (unlink(f))
	{
		perror(f);
		return(1);
	}
	return(0);
}

popstr()
{
	--pathptr;
	while (*pathptr != '/') --pathptr;
	while (*pathptr == '/') --pathptr;
	*++pathptr = 0;
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
     } buf;
	if (stat(n,&buf)) return(0);
	return((buf.flags & 060000) == 040000);
}

quota(n)
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
     } buf;
	if (stat(n,&buf)) return(0);
	return((buf.flags & 060000) == 020000 && buf.addr[0] == -1);
}

scomp(n1,n2)
	char *n1,*n2;
{
	char left,right;
	left= *n1;
	right= *n2;
	while (left && right)
	{
		if (left!=right) return(0);
		++n1;
		++n2;
		left= *n1;
		right= *n2;
	}
	return(left==right);
}

actual(str)
	char *str;
{
	register char *f;
	register slash;
	f = str;
	if (*f == '/') return(f);
	slash = 0;
	while (*f) if (*f++ == '/') slash++;
	if (!slash) return(str);
	while (*f != '/') --f;
	++f;
	return(f);
}

warning(file,f)
	char *file;
{
	char line[20];
	if (!f) return;
	if (gtty(0,line)) return;
	printf("last chance before OBLITERATING %s\n",file);
	printf("ok? ");
	line[read(0,line,20)] = 0;
	return(*line == 'y' || *line == 'Y');
}

process(str)
	char *str;
{
	while (*str) switch(*str++)
	{
	case 'c':
		cflag++;
		break;

	case 'v':
		vflag = 1;
		break;

	case 'f':
		fflag = 1;
		break;

	case 's':
		fflag = 0;
		break;

	case 'a':
		vflag = 0;
		break;

	case '-':
		break;

	default:
		printf("unknown switch: %c\n", *--str);
		exit(1);
	}
}

usage(str)
	char *str;
{
	printf("Usage: %s [ -acfsv ] dir ...\n",str);
	errs++;
}

prs(str)
	char *str;
{
	register char *f;
	f = str;
	while (*f) ++f;
	write(2, str, f-str);
}

pushstr(str)
	char *str;
{
	if (pathptr != pathname) *pathptr++ = '/';
	while (*pathptr++ = *str++) if (pathptr >= pathname + STACKSIZE)
		panic("path name too long");
	pathptr--;
}

panic(str)
	char *str;
{
	prs("PANIC: ");
	prs(str);
	prs("\n");
	exit(1);
}
