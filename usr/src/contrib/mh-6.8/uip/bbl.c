/* bbl.c - ease the tasks of a BBleader */
#ifndef	lint
static char ident[] = "@(#)$Id: bbl.c,v 2.4 1992/11/04 00:39:25 jromine Exp $";
#endif	lint

#include "../h/mh.h"
#include "../h/local.h"
#include "../zotnet/bboards.h"
#include <ctype.h>
#include <pwd.h>
#include <stdio.h>
#ifdef LOCALE
#include	<locale.h>
#endif

/*  */

static struct swit switches[] = {
#define	SHELLSW	0
    "shell program", 0,

#define	VERBSW	1
    "verbose", 0,
#define	NVERBSW	2
    "noverbose", 0,

#define	HELPSW	3
    "help", 4,

    NULL, NULL
};

/*  */

static	int     verbosw = 0;

static	int     sub_ok = 0;

static	char   *bboards = BBOARDS;

static	char   *cwd = NULL;

static	char   *current_folder = NULL;

static	char   *bbfolder = NULL;
static	char    subfolder[BUFSIZ];

static	struct stat bbstat;
static	struct stat substat;

static	char   *shell = "/bin/sh";

static	struct bboard  *bb = NULL;


#ifndef	__STDC__
#ifdef	SYS5
struct	passwd	*getpwnam (), *getpwuid ();
#endif	/* SYS5 */
#endif

/*  */

/* ARGSUSED */

main (argc, argv)
int     argc;
char  **argv;
{
    char   *cp,
          **ap,
          **argp,
            buffer[80],
           *arguments[MAXARGS];
    struct passwd  *pw;

#ifdef LOCALE
	setlocale(LC_ALL, "");
#endif
    invo_name = r1bindex (argv[0], '/');
    if ((cp = m_find (invo_name)) != NULL) {
	ap = brkstring (cp = getcpy (cp), " ", "\n");
	ap = copyip (ap, arguments);
    }
    else
	ap = arguments;
    (void) copyip (argv + 1, ap);
    argp = arguments;

    if ((shell = getenv ("SHELL")) == NULL)
	if ((pw = getpwuid (getuid ())) != NULL
		&& pw -> pw_shell
		&& *pw -> pw_shell)
	    shell = getcpy (pw -> pw_shell);

    if ((pw = getpwnam (bboards)) == NULL)
	adios (NULLCP, "no entry for ~%s", bboards);
    if (pw -> pw_uid != geteuid ())
	adios (NULLCP, "not running setuid to %s", bboards);

    current_folder = ((cp = m_find (pfolder)) || (cp = m_find (inbox)))
	    ? getcpy (cp) : defalt;

/*  */

    while (cp = *argp++) {
	if (*cp == '-')
	    switch (smatch (++cp, switches)) {
		case AMBIGSW: 
		    ambigsw (cp, switches);
		    done (1);
		case UNKWNSW: 
		    adios (NULLCP, "-%s unknown", cp);
		case HELPSW: 
		    (void) sprintf (buffer, "%s [+folder] [switches] bboard",
			    invo_name);
		    help (buffer, switches);
		    done (1);

		case SHELLSW: 
		    if (!(shell = *argp++) || *shell == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    continue;

		case VERBSW: 
		    verbosw++;
		    continue;
		case NVERBSW: 
		    verbosw = 0;
		    continue;
	    }
	if (*cp == '+')
	    if (bbfolder)
		adios (NULLCP, "only one folder at a time!");
	    else
		bbfolder = cp;
	else
	    if (bb != NULL)
		adios (NULLCP, "only one BBoard a time!");
	    else
		if ((bb = getbbnam (cp)) == NULL
			&& (bb = getbbaka (cp)) == NULL)
		    adios (NULLCP, "no such BBoard as '%s'", cp);
    }

/*  */

    if (!bb)
	adios (NULLCP, "no BBoard specified");
    if (!bbfolder)
	bbfolder = "+bbl";
    (void) sprintf (subfolder, "%s/arc", bbfolder);

    if (!m_find ("path"))
	free (path ("./", TFOLDER));
    cwd = getcpy (pwd ());

    process ();

    m_replace (pfolder, current_folder);
    m_update ();

    done (0);
}

/*  */

process () {
    int     child_id;
    char    buffer[BUFSIZ];

    if (!ldrbb (bb) && !ldrchk (bb))
	return;

    if (stat (bb -> bb_file, &bbstat) == NOTOK)
	adios (NULLCP, "no such file as %s", bb -> bb_file);

    if (stat (bb -> bb_archive, &substat) != NOTOK
	    && substat.st_size > 0)
	sub_ok++;
/*  else */
    substat.st_mode = bbstat.st_mode;/* archive should always match */
    substat.st_gid = bbstat.st_gid;/* actual bboard mode & gid */

/* do subfolder first, since you will lose otherwise... */
    (void) sprintf (buffer, "Remove messages currently in %s? ", subfolder);
    if (check_folder (subfolder) && getanswer (buffer))
	rmf (subfolder);

    (void) sprintf (buffer, "Remove messages currently in %s? ", bbfolder);
    if (check_folder (bbfolder) && getanswer (buffer))
	rmf (bbfolder);

    switch (child_id = fork ()) {
	case NOTOK: 
	    adios ("fork", "unable to");

	case OK: 
	    do_child ();	/* NOTREACHED */

	default: 
	    do_parent (child_id);
	    break;
    }
}

/*  */

int     check_folder (folder)
char   *folder;
{
    char   *maildir;
    struct stat st;
    struct msgs *mp;

    maildir = m_maildir (folder + 1);

    if (stat (maildir, &st) == NOTOK)
	return 0;

    if ((st.st_mode & S_IFMT) != S_IFDIR)
	adios (NULLCP, "not a directory '%s'", maildir);
    check_mode (maildir, (st.st_mode | 0555) & 0777);

    if (chdir (maildir) == NOTOK)
	adios (maildir, "unable to change to");
    if (!(mp = m_gmsg (folder + 1)))
	adios (NULLCP, "unable to read %s", folder);

    if (chdir (cwd) == NOTOK)
	admonish (cwd, "could not change back to");
    return (mp -> hghmsg != 0);
}

/*  */

do_parent (child_id)
int     child_id;
{
    int     zap = 0;
    char    buffer[BUFSIZ];

    if (pidwait (child_id, NOTOK) == NOTOK)
	done (1);

    (void) putchar ('\n');

    (void) check_folder (bbfolder);
    if (getanswer ("Incorporate changes? "))
	update (&bbstat, bb -> bb_file, bbfolder, bb -> bb_info, bb -> bb_map);
    (void) sprintf (buffer, "Remove %s? ", bbfolder);
    if (getanswer (buffer))
	zap++;

    if (check_folder (subfolder)) {
	if (getanswer ("Update archives? "))
	    update (&substat, bb -> bb_archive, subfolder, NULLCP, NULLCP);
    (void) sprintf (buffer, "Remove %s? ", subfolder);
    if (getanswer (buffer))
	rmf (subfolder);
    }
    else
	if (sub_ok
		&& getanswer ("Remove archives? ")
		&& getanswer ("Are you sure? "))
	    if (unlink (bb -> bb_archive) == NOTOK)
		admonish (bb -> bb_archive, "unable to remove %s");
    if (zap)
	rmf (bbfolder);
}

/*  */

check_mode (dir, mode)
char   *dir;
unsigned int    mode;
{
    int     child_id;
    struct stat st;
#ifdef SYS5DIR
    struct dirent  *dp;
#else  SYS5DIR
    struct direct  *dp;
#endif SYS5DIR
    DIR * dd;

    if (verbosw)
	fprintf (stderr, "chmod %o %s\n", mode, dir);

    switch (child_id = fork ()) {
	case NOTOK: 
	    adios ("fork", "unable to");

	case OK: 
	    (void) setgid (getgid ());
	    (void) setuid (getuid ());

	    if (chmod (dir, (int) mode) == NOTOK)
		adios (dir, "unable to change mode of");
	    if (chdir (dir) == NOTOK)
		adios (dir, "unable to change to");
	    if ((dd = opendir (dir)) == NULL)
		adios (dir, "unable to read");
	    while (dp = readdir (dd))
		if (dp -> d_name[0] != '.') {
		    if (stat (dp -> d_name, &st) == NOTOK) {
			admonish (dp -> d_name, "unable to stat");
			continue;
		    }
		    if (chmod (dp -> d_name, (int) ((st.st_mode | 0444) & 0777))
			    == NOTOK)
			admonish (dp -> d_name, "unable to change mode of");
		}
	    closedir (dd);
	    done (0);

	default: 
	    if (pidwait (child_id, OK))
		done (1);
	    break;
    }
}

/*  */

/* ARGSUSED */

update (stp, file, folder, info, map)
struct stat *stp;
char   *file,
       *folder,
       *info,
       *map;
{
    int     fd;
    struct stat st;

    if (stat (file, &st) != NOTOK
	    && st.st_mtime != stp -> st_mtime) {
	printf ("File '%s' has changed...\n", file);
	if (getanswer ("Append to it instead? "))
	    goto work;
	else
	    if (!getanswer ("Still update it? "))
		return;
    }
    if ((fd = creat (file, BBMODE)) == NOTOK)
	adios (file, "unable to re-create");
    else {
	(void) close (fd);
	if (map)
	    (void) unlink (map);
    }
#ifdef	notdef
    if (info)
	check_info (folder, info);
#endif	notdef

work: ;
    pack (folder, file);
    if (chmod (file, (int) (stp -> st_mode & 0777)) == NOTOK)
	admonish (file, "unable to change mode of");
    if (stat (file, &st) != NOTOK && st.st_gid != stp -> st_gid)
	chgrp (file, stp -> st_gid);
}

/*  */

#ifdef	notdef
check_info (folder, info)
char   *folder,
       *info;
{
    int     id,
            state;
    char   *hdrptr,
           *maildir,
           *msgnam,
            posted[BUFSIZ],
            name[NAMESZ],
            buf[BUFSIZ];
    struct msgs *mp;
    FILE * fp;

    if (chdir (maildir = m_maildir (folder + 1)) == NOTOK)
	adios (maildir, "unable to change to");

    if (!(mp = m_gmsg (folder + 1)))
	adios (NULL, "unable to read %s", folder);
    if (mp -> hghmsg) {
	if ((fp = fopen (msgnam = m_name (mp -> hghmsg), "r")) == NULL)
	    adios (NULL, "unable to read message %s in %s",
		    msgnam, folder);
	id = 0;
	posted[0] = NULL;
	for (state = FLD;;) {
	    switch (state = m_getfld (state, name, buf, sizeof buf, fp)) {
		case FLD: 
		case FLDEOF: 
		case FLDPLUS: 
		    hdrptr = add (buf, NULL);
		    while (state == FLDPLUS) {
			state = m_getfld (state, name, buf, sizeof buf, fp);
			hdrptr = add (buf, hdrptr);
		    }
		    if (uleq (name, "BBoard-ID")) {
			id = atoi (buf);
			if (id > 0 && posted[0])
			    break;
		    }
		    if (uleq (name, "BB-Posted")) {
			strncpy (posted, buf, sizeof posted - 2);
			if (posted[strlen (posted) - 1] == '\n')
			    posted[strlen (posted) - 1] = NULL;
			if (id > 0 && posted[0])
			    break;
		    }
		    continue;

		default: 
		    admonish (NULL, "unable to find BBoard-info in message %s",
			    msgnam);
		    (void) fclose (fp);
		    goto no_risk;
	    }
	    break;
	}
	(void) fclose (fp);

	if (verbosw)
	    fprintf (stderr,
		    "[ Highest message has %s%d and\n\t\t      %s%s ]\n",
		    "BBoard-ID: ", id, "BB-Posted: ", posted);

	if ((fp = lkfopen (info, "w")) == NULL)
	    adios (info, "unable to lock and fopen");
	fprintf (fp, "%d\n%s\n", id, posted);
	(void) lkfclose (fp, info);
    }

no_risk: ;
    if (chdir (cwd) == NOTOK)
	admonish (cwd, "could not change back to");
}
#endif	notdef

/*  */

pack (folder, file)
char   *folder,
       *file;
{
    int     child_id;

    switch (child_id = fork ()) {
	case NOTOK: 
	    admonish ("fork", "unable to");
	    return;

	case OK: 
	    if (verbosw)
		fprintf (stderr, "pack %s -file %s\n", folder, file);

	    execlp (packproc, r1bindex (packproc, '/'),
		    folder, "-file", file, NULLCP);
	    fprintf (stderr, "unable to exec ");
	    perror (packproc);
	    _exit (-1);

	default: 
	    (void) pidXwait (child_id, packproc);
	    break;
    }
}

/*  */

chgrp (file, gid)
char   *file;
short	gid;
{
    int     child_id;
    char    group[BUFSIZ];

    switch (child_id = fork ()) {
	case NOTOK: 
	    admonish ("fork", "unable to");
	    return;

	case OK: 
	    (void) setuid (geteuid ());/* make sure chgrp works */
	    (void) sprintf (group, "%d", gid);
	    if (verbosw)
		fprintf (stderr, "chgrp %s %s\n", group, file);

	    execlp ("/bin/chgrp", "chgrp", group, file, NULLCP);
	    fprintf (stderr, "unable to exec ");
	    perror ("/bin/chgrp");
	    _exit (-1);

	default: 
	    (void) pidXwait (child_id, "chgrp");
	    break;
    }
}

/*  */

rmf (folder)
char   *folder;
{
    int     child_id;

    switch (child_id = fork ()) {
	case NOTOK: 
	    admonish ("fork", "unable to");
	    return;

	case OK: 
	    (void) setgid (getgid ());
	    (void) setuid (getuid ());
	    if (verbosw)
		fprintf (stderr, "rmf %s\n", folder);

	    execlp (rmfproc, r1bindex (rmfproc, '/'), folder, NULLCP);
	    fprintf (stderr, "unable to exec ");
	    perror (rmfproc);
	    _exit (-1);

	default: 
	    (void) pidXwait (child_id, rmfproc);
	    break;
    }
}

/*  */

do_child () {
    char    buffer[BUFSIZ];

    (void) setgid (getgid ());		/* become the user, not bboards */
    (void) setuid (getuid ());

    inc (bb -> bb_file, bbfolder);
    if (sub_ok)
	inc (bb -> bb_archive, subfolder);
/*  else
	create the folder */

    if (verbosw)
	(void) putchar ('\n');
    printf ("[ Working folder is %s, Archive folder is %s ]\n",
	    bbfolder, subfolder);
    printf ("[ Type CTRL-D to finish ]\n");

    m_replace (pfolder, bbfolder + 1);
    m_update ();

    (void) sprintf (buffer, "=> %s: %s", invo_name, bb -> bb_name);
    execlp (shell, buffer, NULLCP);
    fprintf (stderr, "unable to exec ");
    perror (shell);
    _exit (-1);
}

/*  */

inc (file, folder)
char   *file,
       *folder;
{
    int     child_id;

    switch (child_id = fork ()) {
	case NOTOK: 
	    adios ("fork", "unable to");

	case OK: 
	    if (verbosw)
		fprintf (stderr, "inc %s -file %s -silent\n", folder, file);
	    execlp (incproc, r1bindex (incproc, '/'),
		    folder, "-file", file, "-silent", NULLCP);
	    fprintf (stderr, "unable to exec ");
	    perror (incproc);
	    _exit (-1);

	default: 
	    if (pidXwait (child_id, incproc))
		done (1);
	    break;
    }
}
