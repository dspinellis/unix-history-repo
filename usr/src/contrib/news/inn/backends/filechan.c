/*  $Revision: 1.12 $
**
**  An InterNetNews channel process that splits a funnel entry into
**  separate files.  Originally from Robert Elz <kre@munnari.oz.au>.
*/
#include "configdata.h"
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include <fcntl.h>
#include <sys/file.h>
#include "paths.h"
#include "clibrary.h"
#include "libinn.h"
#include "macros.h"

extern void	MAPread();
extern char	*MAPname();

int
main(ac, av)
    int			ac;
    char		*av[];
{
    char		buff[2048];
    register char	*p;
    register char	*next;
    register int	i;
    register int	fd;
    int			Fields;
    STRING		Directory;
    BOOL		Map;
    FILE		*F;
    struct stat		Sb;
    UID_T		uid;
    GID_T		gid;
    UID_T		myuid;

    /* Set defaults. */
    Fields = 1;
    Directory = _PATH_BATCHDIR;
    Map = FALSE;
    myuid = geteuid();
    (void)umask(NEWSUMASK);

    /* Parse JCL. */
    while ((i = getopt(ac, av, "d:f:m:p:")) != EOF)
	switch (i) {
	default:
	    (void)fprintf(stderr, "Usage error.\n");
	    exit(1);
	case 'd':
	    Directory = optarg;
	    break;
	case 'f':
	    Fields = atoi(optarg);
	    break;
	case 'm':
	    Map = TRUE;
	    MAPread(optarg);
	    break;
	case 'p':
	    if ((F = fopen(optarg, "w")) == NULL) {
		(void)fprintf(stderr, "filechan cant fopen %s %s\n",
			optarg, strerror(errno));
		exit(1);
	    }
	    (void)fprintf(F, "%ld\n", (long)getpid());
	    if (ferror(F) || fclose(F) == EOF) {
		(void)fprintf(stderr, "filechan cant fclose %s %s\n",
			optarg, strerror(errno));
		exit(1);
	    }
	    break;
	}

    /* Move, and get owner of current directory. */
    if (chdir(Directory) < 0) {
	(void)fprintf(stderr, "Can't chdir to %s, %s\n",
	    Directory, strerror(errno));
	exit(1);
    }
    if (stat(".", &Sb) < 0) {
	(void)fprintf(stderr, "Can't stat %s, %s\n",
	    Directory, strerror(errno));
	exit(1);
    }
    uid = Sb.st_uid;
    gid = Sb.st_gid;

    /* Read input. */
    while (fgets(buff, sizeof buff, stdin) != NULL) {
	if ((p = strchr(buff, '\n')) != NULL)
	    *p = '\0';

	/* Skip the right number of leading fields. */
	for (i = Fields, p = buff; *p; p++)
	    if (*p == ' ' && --i <= 0)
		break;
	if (*p == '\0')
	    /* Nothing to write.  Probably shouldn't happen. */
	    continue;

	/* Add a newline, get the length of all leading fields. */
	*p++ = '\n';
	i = p - buff;

	/* Rest of the line is space-separated list of filenames. */
	for (; *p; p = next) {
	    /* Skip whitespace, get next word. */
	    while (*p == ' ')
		p++;
	    for (next = p; *next && *next != ' '; next++)
		continue;
	    if (*next)
		*next++ = '\0';

	    if (Map)
		p = MAPname(p);
	    fd = open(p, O_CREAT | O_WRONLY | O_APPEND, BATCHFILE_MODE);
	    if (fd >= 0) {
		/* Try to lock it and set the ownership right. */
		(void)LockFile(fd, TRUE);
		if (myuid == 0 && uid != 0)
		    (void)chown(p, uid, gid);

		/* Just in case, seek to the end. */
		(void)lseek(fd, 0L, SEEK_END);

		errno = 0;
		if (write(fd, (POINTER)buff, (SIZE_T)i) != i) {
		    perror("write");
		    exit(1);
		}

		(void)close(fd);
	    }
	}
    }

    exit(0);
    /* NOTREACHED */
}
