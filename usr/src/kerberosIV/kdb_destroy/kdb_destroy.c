/*
 * $Source: /usr/src/kerberosIV/kdb_destroy/RCS/kdb_destroy.c,v $
 * $Author: kfall $
 *
 * Copyright 1988 by the Massachusetts Institute of Technology.
 *
 * For copying and distribution information, please see the file
 * <mit-copyright.h>.
 *
 * Description.
 */

#ifndef	lint
static char rcsid_kdb_destroy_c[] =
"$Header: /usr/src/kerberosIV/kdb_destroy/RCS/kdb_destroy.c,v 4.1 90/06/25 21:03:10 kfall Exp $";
#endif	lint

#include <mit-copyright.h>
#include <strings.h>
#include <stdio.h>
#include "des.h"
#include "krb.h"
#include "krb_db.h"

main()
{
    char    answer[10];		/* user input */
    char    dbm[256];		/* database path and name */
    char    dbm1[256];		/* database path and name */
    char   *file1, *file2;	/* database file names */

    strcpy(dbm, DBM_FILE);
    strcpy(dbm1, DBM_FILE);
    file1 = strcat(dbm, ".dir");
    file2 = strcat(dbm1, ".pag");

    printf("You are about to destroy the Kerberos database ");
    printf("on this machine.\n");
    printf("Are you sure you want to do this (y/n)? ");
    fgets(answer, sizeof(answer), stdin);

    if (answer[0] == 'y' || answer[0] == 'Y') {
	if (unlink(file1) == 0 && unlink(file2) == 0)
	    fprintf(stderr, "Database deleted at %s\n", DBM_FILE);
	else
	    fprintf(stderr, "Database cannot be deleted at %s\n",
		    DBM_FILE);
    } else
	fprintf(stderr, "Database not deleted.\n");
}
