/*
 * $Source: /usr/src/kerberosIV/krb/RCS/in_tkt.c,v $
 * $Author: karels $
 *
 * Copyright 1985, 1986, 1987, 1988 by the Massachusetts Institute
 * of Technology.
 *
 * For copying and distribution information, please see the file
 * <mit-copyright.h>.
 */

#ifndef lint
static char *rcsid_in_tkt_c =
"$Id: in_tkt.c,v 4.13 91/07/05 16:39:32 karels Exp $";
#endif /* lint */

#include <mit-copyright.h>
#include <stdio.h>
#include <des.h>
#include <krb.h>
#include <sys/file.h>
#include <sys/param.h>
#include <sys/stat.h>
#ifdef TKT_SHMEM
#include <sys/param.h>
#endif

extern int krb_debug;

/*
 * in_tkt() is used to initialize the ticket store.  It creates the
 * file to contain the tickets and writes the given user's name "pname"
 * and instance "pinst" in the file.  in_tkt() returns KSUCCESS on
 * success, or KFAILURE if something goes wrong.
 */

in_tkt(pname,pinst)
    char *pname;
    char *pinst;
{
    int tktfile;
    uid_t me, metoo, getuid(), geteuid();
    struct stat buf;
    int count;
    char *file = TKT_FILE;
    int fd, remove = 0;
    register int i;
    char charbuf[BUFSIZ];
#ifdef TKT_SHMEM
    char shmidname[MAXPATHLEN];
#endif /* TKT_SHMEM */

    me = getuid();
    metoo = geteuid();
    if (lstat(file,&buf) == 0) {
	/*
	 * If called by root to set up ticket file for other user,
	 * don't bitch, just zap the file; login/su run as root
	 * until after authentication succeeds.
	 */
	if (buf.st_uid != me && me == 0) {
		remove = 1;
		buf.st_uid = me;
	}
	if (buf.st_uid != me || (buf.st_mode & S_IFMT) != S_IFREG ||
	    buf.st_mode & 077) {
	    if (krb_debug)
		fprintf(stderr,"%s exists, wrong owner/mode",file);
	    return(KFAILURE);
	}
	/* file already exists, and permissions appear ok, so nuke it */
	if ((fd = open(file, O_RDWR, 0)) < 0)
	    goto out; /* can't zero it, but we can still try truncating it */

	bzero(charbuf, sizeof(charbuf));

	for (i = 0; i < buf.st_size; i += sizeof(charbuf))
	    if (write(fd, charbuf, sizeof(charbuf)) != sizeof(charbuf))
		break;
	
	(void) fsync(fd);
	(void) close(fd);
	if (remove)
		(void) unlink(file);
    }
 out:
    /* arrange so the file is owned by the ruid
       (seteuid or swap real & effective uid if necessary).
       This isn't a security problem, since the ticket file, if it already
       exists, has the right uid (== ruid) and mode. */
    /* THIS IS HIGHLY QUESTIONABLE, though, as the current uid is probably
       wrong; this is done from login or su, which are doing this before
       adopting the new ID (we're called during the initial authentication). */
    if (me != metoo) {
#if defined(BSD) && BSD >= 199006
	if (seteuid(me) < 0) {
	    if (krb_debug)
		perror("in_tkt: seteuid");
	    return(KFAILURE);
	} else
	    if (krb_debug)
		printf("switched euid %d to %d\n",metoo,me);
#else
	if (setreuid(metoo, me) < 0) {
	    /* can't switch??? barf! */
	    if (krb_debug)
		perror("in_tkt: setreuid");
	    return(KFAILURE);
	} else
	    if (krb_debug)
		printf("swapped UID's %d and %d\n",metoo,me);
#endif
    }
    if ((tktfile = creat(file,0600)) < 0) {
	if (krb_debug)
	    fprintf(stderr,"Error initializing %s",TKT_FILE);
        return(KFAILURE);
    }
    if (me != metoo) {
#if defined(BSD) && BSD >= 199006
	if (seteuid(metoo) < 0) {
	    if (krb_debug)
		perror("in_tkt: seteuid2");
	    return(KFAILURE);
	} else
	    if (krb_debug)
		printf("switched euid back to %d\n",metoo);
#else
	if (setreuid(me, metoo) < 0) {
	    /* can't switch??? barf! */
	    if (krb_debug)
		perror("in_tkt: setreuid2");
	    return(KFAILURE);
	} else
	    if (krb_debug)
		printf("swapped UID's %d and %d\n",me,metoo);
#endif
    }
    if (fstat(tktfile,&buf) < 0) {
	if (krb_debug)
	    fprintf(stderr,"Error initializing %s",TKT_FILE);
        return(KFAILURE);
    }

    if (buf.st_uid != me || (buf.st_mode & S_IFMT) != S_IFREG ||
        buf.st_mode & 077) {
	if (krb_debug)
	    fprintf(stderr,"Error initializing %s",TKT_FILE);
        return(KFAILURE);
    }

    count = strlen(pname)+1;
    if (write(tktfile,pname,count) != count) {
        (void) close(tktfile);
        return(KFAILURE);
    }
    count = strlen(pinst)+1;
    if (write(tktfile,pinst,count) != count) {
        (void) close(tktfile);
        return(KFAILURE);
    }
    (void) close(tktfile);
#ifdef TKT_SHMEM
    (void) strcpy(shmidname, file);
    (void) strcat(shmidname, ".shm");
    return(krb_shm_create(shmidname));
#else /* !TKT_SHMEM */
    return(KSUCCESS);
#endif /* TKT_SHMEM */
}
