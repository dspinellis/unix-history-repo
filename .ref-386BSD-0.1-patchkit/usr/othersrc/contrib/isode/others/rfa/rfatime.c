/*
 * RFA - Remote File Access
 *
 * Access and Management for a partial file system tree that exists
 * at two sites either as master files or slave files
 *
 * rfatime.c : external program to set time of local site
 *
 * Contributed by Oliver Wenzel, GMD Berlin, 1990
 *
 * $Header: /f/osi/others/rfa/RCS/rfatime.c,v 7.3 91/02/22 09:28:27 mrose Interim $
 *
 * $Log:	rfatime.c,v $
 * Revision 7.3  91/02/22  09:28:27  mrose
 * Interim 6.8
 * 
 * Revision 7.2  91/01/14  13:55:05  mrose
 * update
 * 
 * Revision 1.1  91/01/04  16:07:59  ow
 * Initial revision
 * 
 */

#ifndef       lint
static char *rcsid = "$Header: /f/osi/others/rfa/RCS/rfatime.c,v 7.3 91/02/22 09:28:27 mrose Interim $";
#endif

/*
 *                              NOTICE
 *
 *    Acquisition, use, and distribution of this module and related
 *    materials are subject to the restrictions of a license agreement.
 *    Consult the Preface in the User's Manual for the full terms of
 *    this agreement.
 *
 */


#include <stdio.h>
#include <sys/types.h>
#include <sys/time.h>

extern int errno;

main(ac, av)
    int ac;
    char **av;
{

    struct timeval tv;
    time_t dt;
    int err;


    if (ac < 2) {
	fprintf(stderr,
		"*** can't set time : options for rfatime missing ***\n");
	exit(-1);
    }
    if ((dt = atol(av[1])) == 0L) {
	fprintf(stderr,"*** can't set time : invalid time delta %s ***\n",
		av[1]);
	exit(-2);
    }
    if (dt > 0) {
	/*--- clock "jumps" forwards ---*/
	gettimeofday(&tv, NULL);
	tv.tv_sec += dt;
	if (settimeofday(&tv, NULL) == -1) {
	    err = errno;
	    fprintf(stderr,"*** can't set time : %s ***\n", sys_errname(errno));
	    exit(err);
	}
    } else {
	tv.tv_sec = dt;
	tv.tv_usec = 0L;
	if (adjtime(&tv, NULL) == -1) {
	    err = errno;
	    fprintf(stderr,"*** can't set time : %s ***\n", sys_errname(errno));
	    exit(err);
	}
    }
    exit(0);
}

