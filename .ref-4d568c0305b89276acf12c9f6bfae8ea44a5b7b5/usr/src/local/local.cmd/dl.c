static char *sccsid = "@(#)dl.c	4.2\t%G%";

#include <stdio.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <sys/dir.h>
#include <errno.h>
#include <signal.h>

#define	DELIM	'/'
#define MODEBITS 07777
#define	ISDIR(st)	(((st).st_mode&S_IFMT) == S_IFDIR)
#define	ISLNK(st)	(((st).st_mode&S_IFMT) == S_IFLNK)
#define	ISREG(st)	(((st).st_mode&S_IFMT) == S_IFREG)
#define	ISDEV(st) \
	(((st).st_mode&S_IFMT) == S_IFCHR || ((st).st_mode&S_IFMT) == S_IFBLK)

struct	stat s1, s2, buf;
extern	unsigned errno;
int	errcode;
char    *path;
char    *path2;
char    line[1024];
int     cflag = 0;   /* "copy" - same as cp  */
int     dflag = 0;   /* "debug" - enable trace printf statements */
int     fflag = 0;   /* "force" option: override protection, no messages */
int     mflag = 0;   /* "move" - same as mv  */
int     cs = 0;      /* flag used with cflag to prevent endless recursion */
int     ms = 0;      /* flag used with mflag to prevent endless recursion */
int     tflag = 0;   /* restore original time stamp */

main(argc, argv)     /* dl.c - delete, undelete, move, copy */ 
char *argv[];
{
	register char *arg;
	char *s;
	int mode;
	int aflg = 0;   /* "all" option: undelete all deleted files */
	int iflg = 0;	/* "interactive" option: send messages to user */
	int rflg = 0;   /* "recursive" opt., used only with n */
	int sflg = 0;   /* do not delete previous dlsave file */
	int uflg = 0;   /* undelete named files and/or directories */
	int nflg = 0;   /* do not provide back-up files */

	if (isatty(0) == 0)	/* if standard i/o is not a terminal, */
		fflag++;		/*  turn on the f flag                */

	if (strcmp(*argv,"ud") == 0)
		uflg++;

	if (strcmp(*argv,"copy") == 0)
		cflag++;

	if (strcmp(*argv,"move") == 0)
		mflag++;

	while(argc>1 && argv[1][0]=='-') {
		arg = *++argv;
		argc--;

		/*
		 *  all arguments following a null option (- ) are 
		 *   treated as file names, so that file names may
		 *   begin with a minus sign (-).
		 */
		if (*(arg+1) == '\0') break;

		while(*++arg != '\0')
			switch(*arg) {
			case 'a':	/* "all" */
				aflg++;
				break;
			case 'c':	/* "copy" */
				cflag++;
				break;
			case 'd':	/* "debug" */
				dflag++;
				break;
			case 'f':	/* "force" */
				fflag++;
				break;
			case 'i':	/* "interactive" */
				iflg++;
				break;
			case 'r':	/* "recursive" */
				rflg++;
				break;
			case 's':	/* "save" */
				sflg++;
				break;
			case 't':	/* "time" stamp */
				tflag++;
				break;
			case 'u':	/* "undelete" */
				uflg++;
				break;
			case 'm':	/* "move" */
				mflag++;
				break;
			case 'n':	/* "not save" */
				nflg++;
				break;
			default:
				printf("dl: unknown option %s\n", *argv);
				exit(1);
			}
	}
	if (cflag || mflag || nflg)  sflg++;
	/*
	 * set up home directory pathname
	 */
	setpath();
	/*
	 * process "undelete all" request
	 */
	if(aflg) {
		undelete_all(iflg);
		exit;
	}
	/*
	 * remove previously saved files unless "save" option,   
	 *  or ud or rm mode
	 */
	if(!sflg && !uflg) 
		dldir(path,0);
	if(!nflg && lstat(path,&buf) != 0) {
		/*
		 * set up .dlsave directory 
	 	 */
		mode = 0777;
          	if (mkdir(path,mode) != 0) {
		       	   fprintf(stderr,"dl: cannot mkdir ~/.dlsave\n");
			   perror(s);
			   exit(1);
		}
	}
     	while(--argc > 0) {
		if(!strcmp(*++argv, "..")) {
		    	if(!fflag) 
				fprintf(stderr, "dl: cannot remove `..'\n");
			continue;
		}
	       /*
	 	* process "undelete" request(s)
	 	*/
		if(uflg) {
			undelete(*argv,iflg);
			exit;
		}
		else {
		/*
		 * process delete request(s)
		 */
			if (cflag) {
				copy(*argv,argv[argc-1],iflg,rflg);
				exit(errcode);
			}
			if (mflag) {
				move(*argv,argv[argc-1],iflg);
				exit(errcode);
			}
			if (nflg) 
				rm(*argv, iflg, rflg);
			else 
				dl(*argv, iflg);
		}
	}
	exit(errcode);
}

setpath()
{
	char *home;
	char *suffix;
	char *getenv();

	home = "HOME";
	if ((path=getenv(home)) == NULL) {
		fprintf(stderr,"dl: getenv failed\n");
		exit(1);
	}
	suffix = "/.dlsave";
	strcat(path,suffix);	
	return;
}

package(argu)
char argu[];
{
	register int i, j, k;
	char  *slash, *slashdot;
	char  *place;
	char  line2[512];

	place = line2;
	strcpy(place,argu);
	path2 = line;
	strcpy(path2,path);
	slashdot = "/.#";
	strcat(path2,slashdot);
	i = strlen(argu);
	slash = "/";
	k = 0;
	for (j=0;j<i;j++) {
		if (place[j] == *slash) {
			k = j + 1;
		}
	}
	strcat(path2,argu+k);
	return;
}

undelete_all(iflg)
{
	struct direct *dp;
	DIR *dirp;
	char *filename;
	int x;

	/*
	 * undelete all saved files (a option)
	 */
	if((dirp = opendir(path)) == NULL) {
		if(!fflag)
			printf("uda: cannot read %s?\n", path);
		exit(1);
	}
	while((dp = readdir(dirp)) != NULL) {
             	if(dp->d_ino != 0 && !dotname(dp->d_name)) {
			filename = (dp->d_name)+2;
			package(filename);
			if(iflg) {
				printf("uda: undelete %s?", filename);
				if (!yes()) 
					goto no;
			}
			if(lstat(filename, &buf) == 0) {
				printf("uda: %s exists. Override?", filename);
				if(!yes())
					goto no;
			}
			x = move(path2,filename,0);
			if(iflg) {
			        if (x >= 0)
				       printf("uda: %s undeleted.\n", filename);
				else
				       printf("uda: unable to undelete %s\n", filename); 
			}
no:		continue;
	        }
	}
	closedir(dirp);
	return;
}

undelete(arg,iflg)
char arg[];
{
	struct stat buf1, buf2;
	int x;

	/*
	 * undelete a saved file (u option)
	 */
	package(arg);
	if(lstat(path2, &buf1)) {
		if (!fflag) 
			printf("ud: %s nonexistent\n", path2);
		++errcode;
	    	return;
	}
	if(iflg) {
		printf("ud: undelete %s?", arg);
		if(!yes())
			return;
	}
	if(lstat(arg, &buf2) == 0) {
			printf("ud: %s exists: overwrite?", arg);
			if(!yes())
				return;
	}
        x = move(path2,arg,0);
	if(iflg) {
		if (x >= 0)
      	      		printf("ud: %s undeleted.\n", arg);
        	else
       	      		printf("ud: unable to undelete %s\n", arg); 
	}
	return;
}

rm(arg, iflg, rflg)
char arg[];
{
	if (dflag) printf("rm entered: arg=%s fflag=%d iflg=%d rflg=%d\n",arg,fflag,iflg,rflg);
	if(lstat(arg, &buf)) {
		if (!fflag) 
			printf("rm: %s nonexistent\n", arg);
		++errcode;
	    	return;
	}
	/*
	 * unlink file named by arg
	 */
	if ((buf.st_mode&S_IFMT) == S_IFDIR || rflg) {  
		if(iflg) {
			printf("rm: remove directory %s?", arg);
			if(!yes())
				return;
		}
		dldir(arg, iflg); 
		return;
	}
	if (!fflag && iflg) {
		printf("rm: remove %s?", arg);
		if (!yes()) 
			return;
	}
	else if (!fflag) {
		if ((buf.st_mode&S_IFMT) != S_IFLNK && access(arg, 02) < 0) {
			printf("rm: override protection %o for %s?\n",buf.st_mode&0777,arg);
			if (!yes())
				return;
		}
	}
	if (unlink(arg) && !fflag) {
		printf ("rm: %s not removed.\n",arg);
		++errcode;
	}
	else {
		if (!fflag && iflg)
			printf ("rm: %s removed.\n",arg);
	}
	return;
} 

dl(arg, iflg)
char arg[];
{
	/*
	 * move the argument (file or directory) to 
	 *  .dlsave directory in user's home directory
	 */
	if (dflag) printf("dl entered: arg=%s fflag=%d iflg=%d\n",arg,fflag,iflg);
	package(arg);
	move(arg,path2,iflg);
	return;
}

dldir(arg, iflg)
char arg[];
{
	struct  stat  buf1, buf2;
	struct direct *dp;
	DIR *dirp;
	char name[BUFSIZ];

	if (dflag) printf("dldir entered: arg=%s fflag=%d iflg=%d\n",arg,fflag,iflg);
	if(lstat(arg, &buf1)) {
		if (!fflag && iflg) 
			printf("dldir: %s nonexistent\n", arg);
		++errcode;
	    	return;
	}
	/*
	 * if the argument is a directory,
	 * recursively remove the directory's contents 
	 * and then the directory.
	 */
	if ((buf1.st_mode&S_IFMT) == S_IFDIR) {  
		if (access(arg, 02) < 0) {
			if(!fflag) {
			 	printf("dldir: %s not accessable\n",arg);
			}
			errcode++;
			return;
		}
		if((dirp = opendir(arg)) == NULL) 
			exit(1);
		while((dp = readdir(dirp)) != NULL) {
			if(dp->d_ino != 0 && !dotname(dp->d_name)) {
				(void) sprintf(name, "%s/%s", arg, dp->d_name);
				if (dflag) printf("dldir: name= %s\n",name);
				if(lstat(name, &buf2)) {
					if (!fflag) 
					   printf("dldir: %s nonexistent\n", name);
					++errcode;
	    				return;
				}
				if ((buf2.st_mode&S_IFMT) == S_IFDIR) {  
    				if(!fflag && iflg) {  
					printf("dldir: delete directory %s?", name);
					if(!yes())
						return;
   				}    
  				dldir(name, iflg);
				}
				else {
    					if(!fflag && iflg) {  
					   printf("dldir: delete file %s?", name);
						if(!yes())
							return;
   					}    
			       		/*
	 				 * permanently remove the file
	 				 */
					if (unlink(name)) {   
					  if (!fflag)
					   printf("dldir: %s not removed\n", name);
					   ++errcode;
					}
					else {
					   if (!fflag && iflg)
					     printf("dldir: %s removed.\n", name);
					}
				}
			}
		}
		closedir(dirp);
		if (dotname(arg))
			return;
		if (rmdir(arg) < 0) { 
			if(!fflag && iflg) {
				fprintf(stderr, "dldir: rmdir:");
				perror(arg);
			}
			errcode++;
		}
		else {
			if(!fflag && iflg) 
				printf("dldir: directory %s removed.\n",arg);
		}
	return;
	}
}

dotname(s)
char *s;
{
	if(s[0] == '.')
		if(s[1] == '.')
			if(s[2] == '\0')
				return(1);
			else
				return(0);
		else if(s[1] == '\0')
			return(1);
	return(0);
}

yes()
{
	int i, b;

	i = b = getchar();
	while(b != '\n' && b != EOF)
		b = getchar();
	return(i == 'y');
}

move(source, target, iflag)
	char *source, *target;
{
	int targetexists;
	int sw = 0;

	if (dflag) printf("move entered: source=%s target=%s fflag=%d iflag=%d\n",source,target,fflag,iflag);
	if (lstat(source, &s1) < 0) {
		if (!fflag)
			error("cannot access %s", source);
		return (1);
	}
	if (dflag) printf("move: lstat(%s) successful\n",source);
	/*
	 * First, try to rename source to target.
	 */
	targetexists = lstat(target, &s2) >= 0;
	if (targetexists) {
		if (dflag) printf("move: lstat(%s) successful\n",target);
		if (s1.st_dev == s2.st_dev && s1.st_ino == s2.st_ino) {
			if (!fflag)
			      error("%s and %s are identical", source, target);
			return (1);
		}
		if (access(target, 2) < 0 && !fflag && isatty(fileno(stdin))) {
			if (query("override protection %o for %s? ",
			  s2.st_mode & MODEBITS, target) == 0) 
				return (1);
			sw++;
		}
		if (mflag && ms == 0) {
			if (ISREG(s2)) {
				if (!fflag && sw == 0) {
		   		printf("overwrite file %s?", target);
				if(!yes())
					return;
				}
			ms = 1;
			dl (target, 0);
			}
			else
			if (s1.st_dev != s2.st_dev && ISDIR(s2))
				goto copyit;
		}
	}
	if (!fflag && iflag && !mflag) {
		if (ISDIR(s1))
			printf("dl: delete directory %s?", source);
		else 
			printf("dl: delete file %s?", source);
		if(!yes())
			return;
	}
	if(dflag) printf("move(1)rename: source=%s, target=%s\n",source, target);
	if (rename(source, target) >= 0) {
		if (!fflag && iflag && !mflag)
			printf("dl: %s deleted. \n",source);
		if (dflag) printf("move: %s renamed %s.\n",source,target);
		return (0);
	}
	if (dflag) printf("move/rename: errno=%d\n",errno);
	if (errno != EXDEV) {
		if (!fflag && iflag) {
			Perror2(source, "rename");
		}
		goto copyit;
	}
	if (targetexists && unlink(target) < 0) {
		if(!fflag) 
			error("cannot unlink %s", target);
		return (1);
	}
	if (dflag) printf("move: target unlinked\n");

	/*
	 * If file or directory cannot be renamed:
	 *  If directory, copy it with r option 
	 *   and delete the source
	 */
copyit:	if (ISDIR(s1)) {
	     if (dflag) printf("move: directory copy %s to %s\n",source,target);
		copy (source, target, iflag, 1);
		dldir (source, iflag);
		return(0);
	}

	/*
	 *  If link, recreate symbolic link
	 */
	if (ISLNK(s1)) {
		register m;
		char symln[MAXPATHLEN];

		if (readlink(source, symln, sizeof (symln)) < 0) {
			if (!fflag) 
				Perror(source);
				return (1);
		}
		m = umask(~(s1.st_mode & MODEBITS));
		if (symlink(symln, target) < 0) {
			if (!fflag) 
				Perror(target);
				return (1);
		}
		if (dflag) printf("move: symlink to target successful\n");
		(void) umask(m);
		goto cleanup;
	}

	/*
	 *  If device
	 */
	if (ISDEV(s1)) {
		if (mknod(target, s1.st_mode, s1.st_rdev) < 0) {
			if (!fflag) 
				Perror(target);
			return (1);
		}
		if (dflag) printf("move: mknod for target successful\n");
		goto cleanup;
	}

	/*
	 *  If regular file, copy it
	 */
	if (ISREG(s1)) {
	     	if(dflag) printf("move: file copy %s to %s\n",source,target);
		copy(source, target, iflag, 0);
		goto cleanup;
 	}  

	if (!fflag) 
		error("%s: unknown file type %o", source, s1.st_mode);
	return (1);

	/*
	 *  If a move has been successful, erase the source
	 */
cleanup:
	if (dflag) printf("move: cleanup\n");
	if (unlink(source) < 0) {
		if (!fflag) 
			error("cannot unlink %s", source);
		return (1);
	}
	if (dflag) printf("move: %s unlinked.\n",source);
	if (!fflag && iflag && !mflag) 
		printf("dl: %s deleted.\n",source);
	return (0);
}

/*VARARGS*/
query(prompt, a1, a2)
	char *a1;
{
	register char i, c;

	fprintf(stderr, prompt, a1, a2);
	i = c = getchar();
	while (c != '\n' && c != EOF)
		c = getchar();
	return (i == 'y');
}

error(fmt, a1, a2)
	char *fmt;
{
	fprintf(stderr, "dl: ");
	fprintf(stderr, fmt, a1, a2);
	fprintf(stderr, "\n");
}

Perror(s)
	char *s;
{
	char buf[MAXPATHLEN + 10];

	(void) sprintf(buf, "move: %s", s);
	perror(buf);
}

Perror2(s1, s2)
	char *s1, *s2;
{
	char buf[MAXPATHLEN + 20];

	(void) sprintf(buf, "dl: %s: %s", s1, s2);
	perror(buf);
}

#define	BSIZE	8192

char	*rindex();

copy(from, to, iflag, rflag)
	char *from, *to;
{
	int fold, fnew, n;
	char *last, destname[BSIZE], buf[BSIZE];
	struct stat stfrom, stto;
	time_t tv[2];

	if (dflag) printf("copy entered: from=%s to=%s iflag=%d rflag=%d\n",from,to,iflag,rflag);
	fold = open(from, 0);
	if (fold < 0) {
		Cerror(from,fflag);
		return (1);
	}
	if (fstat(fold, &stfrom) < 0) {
		Cerror(from,fflag);
		(void) close(fold);
		return (1);
	}
	if (dflag) printf("copy: fstat(%s) OK.\n",from);
	if (stat(to, &stto) >= 0 &&
	   (stto.st_mode&S_IFMT) == S_IFDIR) {
		last = rindex(from, '/');
		if (last) last++; else last = from;
		if (strlen(to) + strlen(last) >= BSIZE - 1) {
			fprintf(stderr, "cp: %s/%s: Name too long", to, last);
			(void) close(fold);
			return(1);
		}
		(void) sprintf(destname, "%s/%s", to, last);
		if (dflag) printf("copy: stat %s & %s is dir., to=%s.\n",to,to,destname);
		to = destname;
	}
	if (!rflag && (stfrom.st_mode&S_IFMT) == S_IFDIR) {
		fprintf(stderr, "cp: %s is a directory, and option -r not chosen.  Job aborted.\n", from);
		return(1);
	}
	if (rflag && (stfrom.st_mode&S_IFMT) == S_IFDIR) {
		(void) close(fold);
		if (dflag) printf("copy: rflag & from is dir., %s closed.\n",from);
		if (stat(to, &stto) < 0) {
			if (mkdir(to, (int)stfrom.st_mode) < 0) {
				Cerror(to,fflag);
				return (1);
			}
			if (dflag) printf("copy: stat(%s) failed & mkdir % successful.\n",to,to);
		}
		else {
			if (!fflag) {
				if ((stto.st_mode&S_IFMT) == S_IFDIR) {
				dl(to, iflag);
				}
				else {
			      		fprintf(stderr, "cp: %s: Not a directory.\n", to);
			      		return (1);
				}
			}
		}
		if (dflag) printf("copy: return with rcopy(%s,%s,%d,%d)\n",from,to,iflag,rflag);
		return (rcopy(from, to, iflag, rflag));
	}
	if (stat(to, &stto) >= 0) {
		if (dflag) printf("cp:stat(%s) o.k.\n",to);
		if (stfrom.st_dev == stto.st_dev &&
		   stfrom.st_ino == stto.st_ino) {
			fprintf(stderr, "cp: Cannot copy file to itself.\n");
			(void) close(fold);
			return (1);
		}
		if (cflag && cs == 0) {
			if (!fflag)
			     fprintf (stderr, "cp: %s exists: overwrite? ", to);
			     if (!yes()) {
			        	(void) close(fold);
					return(1);
			}
			dl(to, 0);
			cs = 1;
		}
	}
	fnew = creat(to, (int)stfrom.st_mode);
	if (fnew < 0) {
		Cerror(to,fflag);
		(void) close(fold); return(1);
	}
	if (dflag) printf("copy: creat(%s,%d) successful.\n",to,stfrom.st_mode);
	for (;;) {
		n = read(fold, buf, BSIZE);
		if (n == 0)
			break;
		if (n < 0) {
			Cerror(from,fflag);
			(void) close(fold); (void) close(fnew); return (1);
		}
		if (write(fnew, buf, n) != n) {
			Cerror(to,fflag);
			(void) close(fold); (void) close(fnew); return (1);
		}
	}
	if (dflag) printf("copy: %s copied to %s.\n",from,to);
	if (!tflag) {
		/* restore original time-stamp */
		tv[0] = stfrom.st_atime;
		tv[1] = stfrom.st_mtime;
		(void) utime(to, tv);
		if (dflag) printf("copy: tflag on, tv[0]=%d, tv[1]=%d.\n",tv[0], tv[1]);
	}
	if (dflag) printf("copy: returning from copy, from=%s, to=%s.\n",from,to);
	(void) close(fold); (void) close(fnew); return (0);
}

rcopy(from, to, iflag, rflag)
	char *from, *to;
{
	DIR *fold = opendir(from);
	struct direct *dp;
	int errs = 0;
	char fromname[BUFSIZ];

	if (dflag) printf("rcopy: entered: from=%s, to=%s.\n",from,to);
	if (fold == 0) {
		Cerror(from,fflag);
		return (1);
	}
	for (;;) {
		dp = readdir(fold);
		if (dp == 0) {
			closedir(fold);
			return (errs);
		}
		if (dp->d_ino == 0)
			continue;
		if (!strcmp(dp->d_name, ".") || !strcmp(dp->d_name, ".."))
			continue;
		if (strlen(from) + 1 + strlen(dp->d_name) >= BUFSIZ - 1) {
			if (!fflag) {
				fprintf(stderr, "cp: %s/%s: Name too long.\n",
			    		from, dp->d_name);
			}
			errs++;
			continue;
		}
		(void) sprintf(fromname, "%s/%s", from, dp->d_name);
		if (dflag) printf("rcopy: copy(%s,%s,%d,%d)\n",fromname,to,iflag,rflag);
		errs += copy(fromname, to, iflag, rflag);
	}
}

Cerror(s)
	char *s;
{
	if (!fflag) {
	 	fprintf(stderr, "cp: ");
		perror(s);
	}
}
