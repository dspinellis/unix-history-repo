/*-
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)read_password.c	5.1 (Berkeley) %G%";
#endif /* not lint */

/*
 * $Source: /mit/kerberos/src/lib/des/RCS/read_password.c,v $
 * $Author: jon $
 *
 * Copyright 1985, 1986, 1987, 1988 by the Massachusetts Institute
 * of Technology.
 *
 * For copying and distribution information, please see the file
 * <mit-copyright.h>.
 *
 * This routine prints the supplied string to standard
 * output as a prompt, and reads a password string without
 * echoing.
 */

#include <stdio.h>
#include <strings.h>
#include <sys/ioctl.h>
#include <signal.h>
#include <setjmp.h>

static jmp_buf env;

/*** Routines ****************************************************** */
/*
 * This version just returns the string, doesn't map to key.
 *
 * Returns 0 on success, non-zero on failure.
 */

int
local_des_read_pw_string(s,max,prompt,verify)
    char *s;
    int	max;
    char *prompt;
    int	verify;
{
    int ok = 0;
    char *ptr;
    
    jmp_buf old_env;
    struct sgttyb tty_state;
    char key_string[BUFSIZ];

    if (max > BUFSIZ) {
	return -1;
    }

    /* XXX assume jmp_buf is typedef'ed to an array */
    bcopy((char *)old_env, (char *)env, sizeof(env));
    if (setjmp(env))
	goto lose;

    /* save terminal state*/
    if (ioctl(0,TIOCGETP,(char *)&tty_state) == -1) 
	return -1;
/*
    push_signals();
*/
    /* Turn off echo */
    tty_state.sg_flags &= ~ECHO;
    if (ioctl(0,TIOCSETP,(char *)&tty_state) == -1)
	return -1;
    while (!ok) {
	(void) printf(prompt);
	(void) fflush(stdout);
	while (!fgets(s, max, stdin));

	if ((ptr = index(s, '\n')))
	    *ptr = '\0';
	if (verify) {
	    printf("\nVerifying, please re-enter %s",prompt);
	    (void) fflush(stdout);
	    if (!fgets(key_string, sizeof(key_string), stdin)) {
		clearerr(stdin);
		continue;
	    }
            if ((ptr = index(key_string, '\n')))
	    *ptr = '\0';
	    if (strcmp(s,key_string)) {
		printf("\n\07\07Mismatch - try again\n");
		(void) fflush(stdout);
		continue;
	    }
	}
	ok = 1;
    }

lose:
    if (!ok)
	bzero(s, max);
    printf("\n");
    /* turn echo back on */
    tty_state.sg_flags |= ECHO;
    if (ioctl(0,TIOCSETP,(char *)&tty_state))
	ok = 0;
/*
    pop_signals();
*/
    bcopy((char *)env, (char *)old_env, sizeof(env));
    if (verify)
	bzero(key_string, sizeof (key_string));
    s[max-1] = 0;		/* force termination */
    return !ok;			/* return nonzero if not okay */
}
