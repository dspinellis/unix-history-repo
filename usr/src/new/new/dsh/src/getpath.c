/*(@(#)getpath.c	1.2		/ra/csr/presotto/hacks/src/worm/sccs/s.getpath.c)*/
#define MAXPATHS 20
#define MAXPATHSIZE 100

int		npaths;			/* number of paths to search */
char		*paths[MAXPATHS];	/* pointers to the paths */
char		thepath[256];		/* where to copy path */

/*
 *	input the execution search path
 */
getpath ()
{
    char	*path;

    path = (char *)getenv ("PATH");
    strcpy (thepath, path);
    path = thepath;
    for (npaths = 0; npaths < MAXPATHS; npaths++) {
	paths[npaths] = path;
	while (*path != ':' && *path != 0) path++;
	if (*path == 0) {
	    npaths++;
	    break;
	} else {
	    *path++ = 0;
	}
    }
}

/*
 *	see if we can execute a command
 */
findcmd (path)
char	*path;			/* path to the command */
{
    char	abspath[MAXPATHSIZE];
    int		rv;			/* return value */
    int		indp;			/* path index */

    if (*path == 0)
	return (0);

    if (*path == '/') {
	
	/* absolute path,  don't fool around */
	rv = access(path, 1);
    } else {

	/* relative path,  do some work */
	for (indp = 0; indp < npaths; indp++) {
	    strcpy (abspath, paths[indp]);
	    strcat (abspath, "/");
	    strcat (abspath, path);
	    rv = access(abspath, 1);
	    if (rv == 0)
		break;
	}
    }

    return (rv);
}
