/*
** Program to produce reliable locks for shell scripts.
** Algorithmn suggested by Peter Honeyman, January 1984, in connection
** with HoneyDanBer UUCP.
**
** Erik E. Fair <ucbvax!fair>
*/

#include <stdio.h>
#include <sys/file.h>
#include <errno.h>

#define	LOCK_SET	0
#define	LOCK_FAIL	1

#define TRUE	1
#define	FALSE	0

int	Verbose = FALSE;
char	*Pname;
char	*USAGE = "%s: USAGE: shlock -f file -p pid [-v]\n";
char	*errmsg();
char	*tmpfile();

#define	vprintf	if (Verbose) printf

extern int	errno;
extern char	*rindex();
extern char	*strcpy();
extern char	*strcat();

main(ac,av)
int	ac;
char	*av[];
{
	register int	x;
	char	*file;
	int	pid;

	Pname = ((Pname = rindex(av[0], '/')) ? Pname + 1 : av[0]);

	for(x = 1; x < ac; x++) {
		if (av[x][0] == '-') {
			switch(av[x][1]) {
			case 'v':
				Verbose = TRUE;
				break;
			case 'p':
				if (strlen(av[x]) > 2) {
					pid = atoi(&av[x][2]);
				} else {
					pid = atoi(av[x + 1]);
					x++;
				}
				break;
			case 'f':
				if (strlen(av[x]) > 2) {
					file = &av[x][2];
				} else {
					file = av[x + 1];
					x++;
				}
				break;
			default:
				fprintf(stderr, USAGE, Pname);
				exit(LOCK_FAIL);
			}
		}
	}
	if (pid == 0 || file == (char *)NULL) {
		fprintf(stderr, USAGE, Pname);
		exit(LOCK_FAIL);
	}
	if (mklock(file, pid))
		exit(LOCK_SET);
	exit(LOCK_FAIL);
}

mklock(file, pid)
char	*file;
int	pid;
{
	register int	fd;
	register int	len;
	register char	*tmp = tmpfile(file);
	char	*buf[BUFSIZ];

	vprintf("%s: attempting to get lock <%s> for process %d\n", Pname, file, pid);
	sprintf(buf, "%d\n", pid);
	len = strlen(buf);
loop:
	if ((fd = open(tmp, O_RDWR|O_CREAT|O_EXCL, 0644)) < 0) {
		switch(errno) {
		case EEXIST:
			vprintf("%s: temporary file %s exists already.\n", Pname, tmp);
			if (unlink(tmp) < 0) {
				fprintf(stderr,"%s: unlink(%s): %s\n", Pname, tmp, errmsg(errno));
				return(FALSE);
			}
			/*
			** I hereby profane the god of structured programming
			** Edsgar Djikstra
			*/
			goto loop;
		default:
			fprintf(stderr,"%s: open(%s): %s\n", Pname, tmp, errmsg(errno));
			return(FALSE);
		}
	}

	/*
	** Write the PID into the temporary file before attempting to link
	** to the actual lock file. That way we have a valid lock the instant
	** the link succeeds.
	*/
	if (write(fd, buf, len) < 0) {
		fprintf(stderr, "%s: write(%s,%d): %s\n", Pname, tmp, pid, errmsg(errno));
		close(fd);
		return(FALSE);
	}
	close(fd);

loop2:
	if (link(tmp, file) < 0) {
		switch(errno) {
		case EEXIST:
			vprintf("%s: lock <%s> already exists\n", Pname, file);
			if (cklock(file)) {
				vprintf("%s: extant lock is valid\n", Pname);
				if (unlink(tmp) < 0) {
					fprintf(stderr,"%s: unlink(%s): %s\n", Pname, tmp, errmsg(errno));
				}
				return(FALSE);
			} else {
				vprintf("%s: extant lock is invalid, removing\n", Pname);
				if (unlink(file) < 0) {
					fprintf(stderr,"%s: unlink(%s): %s\n", Pname, file, errmsg(errno));
					return(FALSE);
				}
			}
			goto loop2;
		default:
			fprintf(stderr,"%s: link(%s, %s): %s\n", Pname, tmp, file, errmsg(errno));
			return(FALSE);
		}
	}
	if (unlink(tmp) < 0) {
		fprintf(stderr,"%s: unlink(%s): %s\n", Pname, tmp, errmsg(errno));
	}
	vprintf("%s: got lock <%s>\n", Pname, file);
	return(TRUE);
}

/*
** Check the validity of an existing lock file.
**
**	Read the PID out of the lock
**	Send a null signal to determine whether that PID still exists
**	Existence (or not) determines the validity of the lock.
**
**	Two bigs wins to this algorithmn:
**
**	o	Locks do not survive crashes of either the system or the
**			application by any appreciable period of time.
**
**	o	No clean up to do if the system or application crashes.
**
*/

cklock(file)
char	*file;
{
	register int	fd = open(file, O_RDONLY);
	register int	len;
	char	buf[BUFSIZ];

	vprintf("%s: checking extant lock <%s>\n", Pname, file);
	if (fd < 0) {
		fprintf(stderr,"%s: open(%s): %s\n", Pname, file, errmsg(errno));
		return(TRUE);	/* might or might not; conservatism */
	}
	
	if ((len = read(fd, buf, sizeof(buf))) <= 0) {
		close(fd);
		vprintf("%s: lock file format error\n", Pname);
		return(FALSE);
	}
	close(fd);
	buf[len + 1] = '\0';
	return(p_exists(atoi(buf)));
}

/*
** Does the PID exist?
** Send null signal to find out.
*/

p_exists(pid)
int	pid;
{
	vprintf("%s: locking process %d is ", Pname, pid);
	if (kill(pid, 0) < 0) {
		switch(errno) {
		case ESRCH:
			vprintf("dead\n");
			return(FALSE);	/* pid does not exist */
		case EPERM:
			vprintf("alive\n");
			return(TRUE);	/* pid exists */
		default:
			vprintf("state unknown: %s\n", errmsg(errno));
			return(TRUE);	/* be conservative */
		}
	}
	vprintf("alive\n");
	return(TRUE);	/* pid exists */
}

char *
errmsg(n)
register int	n;
{
	extern	int	sys_nerr;
	extern 	char	*sys_errlist[];

	return((n >= 0 && n < sys_nerr) ? sys_errlist[n] : "unknown error");
}

char *
tmpfile(file)
char *file;
{
	register char	*cp;
	static char	buf[BUFSIZ];
	char	tempname[BUFSIZ];

	if ((cp = rindex(strcpy(buf, file), '/')) != (char *)NULL) {
		*(cp + 1) = '\0';
	} else
		buf[0] = '\0';
	sprintf(tempname, "shlock%d", getpid());
	vprintf("%s: temporary filename: %s\n", Pname, tempname);
	return(strcat(buf, tempname));
}
