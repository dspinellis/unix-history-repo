/* $Id: getactive.c,v 3.0 1991/09/09 20:18:23 davison Trn $
 */
/* This software is Copyright 1991 by Stan Barber. 
 *
 * Permission is hereby granted to copy, reproduce, redistribute or otherwise
 * use this software as long as: there is no monetary profit gained
 * specifically from the use or reproduction of this software, it is not
 * sold, rented, traded or otherwise marketed, and this copyright notice is
 * included prominently in any copy made. 
 *
 * The author make no claims as to the fitness or correctness of this software
 * for any use whatsoever, and it is provided as is. Any use of this software
 * is at the user's own risk. 
 */

#include "EXTERN.h"
#include "common.h"
#include "nntpclient.h"

void finalize _((int));

int debug = 0;			/* make nntpclient.c happy */

int
main(argc, argv)
int argc;
char *argv[];
{
    char command[32];
    char *action;
    register FILE *actfp;

    if (argc < 2 || argc > 3) {
	fprintf(stderr, "Usage: getactive [active|distributions|newsgroups] filename\n");
	exit(1);
    }
    if (argc == 2)
	action = "ACTIVE";
    else {
	action = argv[1];
	argc--;
	argv++;
    }
    nntp_connect();
    sprintf(command,"LIST %s",action);
    nntp_command(command); 
#ifdef HAS_SIGHOLD
    sighold(SIGINT);
#endif
    if (nntp_check(FALSE) != NNTP_CLASS_OK) {
	fprintf(stderr,"getactive: Can't get %s file from server.\n",action);
	fprintf(stderr, "Server said: %s\n", ser_line);
	finalize(1);
    }

    actfp = fopen(argv[1], "w");
    if (actfp == NULL) {
	perror(argv[1]);
	finalize(1);
    }

    while (nntp_gets(ser_line, sizeof ser_line) >= 0) {
	if (ser_line[0] == '.')		/* while there's another line */
	    break;			/* get it and write it to */
	if (actfp != NULL) {		/* the temporary active file */
	    fputs(ser_line, actfp);
	    putc('\n', actfp);
	}
    }

    if (ferror(actfp)) {
	perror(argv[1]);
	finalize(1);
    }
    if (fclose(actfp) == EOF) {
	perror(argv[1]);
	finalize(1);
    }

#ifdef HAS_SIGHOLD
    sigrelse(SIGINT);
#endif
    nntp_close();
    return 0;
}

void
finalize(num)
int num;
{
    nntp_close();
    exit(num);
}
