/* spooler.c --  MH style mailer to write to /usr/spool/mail/<user>
 *
 *     Mlocal, P=/etc/spooler,  F=lsDFSmn, S=10, R=20, A=spooler $u
 */


#define FLOCK   /* uses flock() if defined, lockf() otherwise. */

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <pwd.h>
#include "sysexits.h"

#define SPOOLDIR "/usr/spool/mail"

char *delim = "\1\1\1\1\n";
char *ME = "/etc/spooler";

FILE *mboxopen();
FILE *fp_in;

main( argc, argv )
int argc;
char **argv;
{
    register int rc, anyerrs;
    char tmpfile[100];

    if( *++argv == NULL ) {
	fprintf( stderr, "%s: no recipients.\n", ME );
	exit( EX_USAGE );
    }

    if( argc == 2 ) {           /* single recipient, don't need tmp copy */
	fp_in = stdin;
	rc = localmail( *argv );
    }
    else {
	if(( rc = copyfile( tmpfile )) == 0 ) {     /* sets fp_in */
	    for( anyerrs = 0; *argv; argv++ ) {
		rewind( fp_in );
		if(( rc = localmail( *argv )))
		    anyerrs++;
	    }
	    if( !anyerrs )
		unlink( tmpfile );
	}
    }

    exit( rc );
}



localmail( user )
char   *user;
{
    register FILE *fp;
    register int count, n;
    register char buf[BUFSIZ];
    register char mailbox[BUFSIZ];
    register struct stat sb;

    sprintf( mailbox, "%s/%s", SPOOLDIR, user );

    if( stat( mailbox, &sb ) == -1 ) {
	if( create_mbox( mailbox, user ))
	    return(EX_TEMPFAIL);
    }

    if(( fp = mboxopen( mailbox )) == 0 )
	return(EX_TEMPFAIL);
    fwrite( delim, sizeof(char), strlen(delim), fp );
    while(( count = fread( buf, sizeof(char), BUFSIZ, fp_in))) {
	n = fwrite( buf, sizeof(char), count, fp );
	if( n != count ) {
	    fprintf( stderr, "%s write error: %d read, %d written\n",
		ME, count, n );
	    mboxclose(fp);
	    return(-1);
	}
    }
    fwrite( delim, sizeof(char), strlen(delim), fp );
    mboxclose(fp);

    return(0);
}


create_mbox( mbox, user )
register char *mbox, *user;
{
    register int fd;
    register struct passwd *pw;

    if(( fd = creat( mbox, 0600 )) == -1 )
	return -1;
    if(( pw = getpwnam( user )) != NULL ) {
	fchown( fd, pw->pw_uid, pw->pw_gid );
    }
    close( fd );

    return 0;
}


FILE *
mboxopen( mailbox )
register char *mailbox;
{
    register FILE *fp;
    register int i;

    if(( fp = fopen( mailbox, "a" )) == NULL ) {
	fprintf( stderr, "%s:  Can't open %s for appending\n", ME, mailbox );
	return(0);
    }

    /* lock the mailbox */

#define NLOCKTRYS 20        /* almost a 2 min. wait should be enough even */
			    /* after a long vacation */

    for( i = 0; i < NLOCKTRYS; i++ ) {
#ifdef FLOCK
	if( flock( fileno(fp), LOCK_EX | LOCK_NB ) == 0)    /* got it ok */
#else
	if( lockf( fileno(fp), F_TLOCK, 0) == 0 )
#endif FLOCK
	    break;
	/* fprintf(stderr, "can't lock, sleeping 5 sec...\n"); */
	sleep(5);
    }
/*
 *  If lockf worked perfectly (ie, rpc.lockd didn't die) this would be
 *  just fine.
 */
#ifdef DontDoThis
    if( i == NLOCKTRYS ) {
	fprintf( stderr, "%s:  Can't lock %s\n", ME, mailbox);
	return(0);
    }
#endif

    fseek( fp, 0, 2 );

    return(fp);
}


mboxclose(fp)
FILE *fp;
{
/*todo: unlockit.  --not really necessary */
    fclose(fp);
}



/*
 *  collect stdin to a tmp file
 */
copyfile( tmpfile )
register char *tmpfile;
{
    struct stat sb;
    register int pid, count, n;
    register char buf[BUFSIZ];
    register FILE *fp;

    pid = getpid();

    while( 1 ) {
	sprintf( tmpfile, "/tmp/spooler.%05d", pid );
	if( stat( tmpfile, &sb ) == -1 )    /* ok, this file doesn't exist */
	    break;
	pid++;
    }

    if(( fp = fopen( tmpfile, "w" )) == NULL ) {
	fprintf( stderr, "%s:  Can't open %s\n", ME, tmpfile );
	return(EX_TEMPFAIL);
    }

    while(( count = fread( buf, sizeof(char), BUFSIZ, stdin ))) {
	n = fwrite( buf, sizeof(char), count, fp );
	if( n != count ) {
	    fprintf( stderr, "%s: copyfile, write error: %d read, %d written\n",
		ME, count, n );
	    fclose(fp);
	    return(-1);
	}
    }

    fp_in = freopen( tmpfile, "r", fp );
    if( fp_in == NULL ) {
	fprintf( stderr, "%s: freopen on %s failed\n", ME, tmpfile );
	return(-1);
    }

    return(0);
}
