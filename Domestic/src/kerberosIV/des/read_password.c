/*
 * $Source: /usr/src/kerberosIV/des/RCS/read_password.c,v $
 * $Author: kfall $
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

#ifndef	lint
static char rcsid_read_password_c[] =
"$Header: /usr/src/kerberosIV/des/RCS/read_password.c,v 4.13 90/06/23 03:09:35 kfall Exp $";
#endif	lint

#include <sys/param.h>
#include <stdio.h>
#include <string.h>
#include <setjmp.h>
#include <pwd.h>
#include <mit-copyright.h>
#include <des.h>
#include "conf.h"

extern char	*getpass();

#ifdef	BSDUNIX
static jmp_buf env;
#endif

int
des_read_password(k, prompt, verify)
	des_cblock *k;
	char *prompt;
	int verify;
{
	int ok;
	char key_string[_PASSWORD_LEN];

#ifdef BSDUNIX
	if (setjmp(env)) {
		ok = -1;
		goto lose;
    	}
#endif

    	ok = des_read_pw_string(key_string, _PASSWORD_LEN, prompt, verify);
    	if (ok == 0)
		des_string_to_key(key_string, k);

lose:
    	bzero(key_string, sizeof (key_string));
    	return ok;
}

/*
 * This version just returns the string, doesn't map to key.
 *
 * Returns 0 on success, non-zero on failure.
 */

int
des_read_pw_string(s, max, prompt, verify)
	char *s;
	int max;
	char *prompt;
	int verify;
{
	register int len = MIN(_PASSWORD_LEN, max);
	char	*ptr = getpass(prompt);

	(void)strncpy(s, ptr, len);
	if (verify) {
		while (strncmp(getpass(prompt), s, len) != 0) {
			printf("\n\07\07Mismatch - try again\n");
			(void)fflush(stdout);
			continue;
		}
	}
	s[len-1] = '\0';	/* just in case */
	return 0;
}
