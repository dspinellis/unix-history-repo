/* bbtar.c - generate the names of archive files to be put to tape */

/* Usage:

	% cd ~bboards/archive		# followed by one of:

	% tar cv `bbtar private`	# to save private BBoard archives
	% tar cv `bbtar public`		# to save public BBoard archives
	% tar cv `bbtar`		# to save all BBoard archives

 */


#include <pwd.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "../zotnet/bboards.h"


#define	NOTOK	(-1)


static int  private = 0;

static char archives[BUFSIZ];


struct passwd  *getpwnam ();

/*  */

/* ARGSUSED */

main (argc, argv)
int     argc;
char  **argv;
{
    struct bboard  *bb;
    struct passwd  *pw;

    if ((pw = getpwnam (BBOARDS)) == NULL)
	exit (1);
    (void) sprintf (archives, "%s/archive/", pw -> pw_dir);

    if (argc > 1)
	private = strcmp (argv[1], "private") == 0 ? 1
	    : strcmp (argv[1], "public") == 0 ? -1
	    : 0;

    (void) setbbent (SB_STAY);
    while (bb = getbbent ())
	process (bb);
    (void) endbbent ();

    exit (0);
}

/*  */

static  process (bb)
struct bboard  *bb;
{
    struct stat st;

    if (stat (bb -> bb_archive, &st) == NOTOK)
	return;
    if (strncmp (archives, bb -> bb_archive, strlen (archives)) == 0)
	bb -> bb_archive += strlen (archives);

    if (private == 0)
	printf ("%s\n", bb -> bb_archive);
    else
	if ((st.st_mode & 0444) != 0444 ? (private > 0) : (private < 0))
	    printf ("%s\n", bb -> bb_archive);
}
