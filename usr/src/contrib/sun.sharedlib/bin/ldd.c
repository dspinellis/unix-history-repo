/*
 * This source code is a product of Sun Microsystems, Inc. and is provided
 * for unrestricted use provided that this legend is included on all tape
 * media and as a part of the software program in whole or part.  Users
 * may copy or modify this source code without charge, but are not authorized
 * to license or distribute it to anyone else except as part of a product or
 * program developed by the user.
 *
 * THIS PROGRAM CONTAINS SOURCE CODE COPYRIGHTED BY SUN MICROSYSTEMS, INC.
 * SUN MICROSYSTEMS, INC., MAKES NO REPRESENTATIONS ABOUT THE SUITABLITY
 * OF SUCH SOURCE CODE FOR ANY PURPOSE.  IT IS PROVIDED "AS IS" WITHOUT
 * EXPRESS OR IMPLIED WARRANTY OF ANY KIND.  SUN MICROSYSTEMS, INC. DISCLAIMS
 * ALL WARRANTIES WITH REGARD TO SUCH SOURCE CODE, INCLUDING ALL IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.  IN
 * NO EVENT SHALL SUN MICROSYSTEMS, INC. BE LIABLE FOR ANY SPECIAL, INDIRECT,
 * INCIDENTAL, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING
 * FROM USE OF SUCH SOURCE CODE, REGARDLESS OF THE THEORY OF LIABILITY.
 * 
 * This source code is provided with no support and without any obligation on
 * the part of Sun Microsystems, Inc. to assist in its use, correction, 
 * modification or enhancement.
 *
 * SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
 * INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY THIS
 * SOURCE CODE OR ANY PART THEREOF.
 *
 * Sun Microsystems, Inc.
 * 2550 Garcia Avenue
 * Mountain View, California 94043
 */

/* @(#)ldd.c 1.6 91/04/07 SMI */

/* Copyright (c) 1991 by Sun Microsystems, Inc. */

/*
 * List Dynamic Dependencies
 */

#include <sys/types.h>
#include <stdio.h>
#include <a.out.h>
#include <link.h>
#include <sys/wait.h>
#include <sys/file.h>

char *nargv[] = {0, 0};			/* dummy argument vector */

char *malloc();				/* malloc return value */

main(argc, argv, envp)
	int argc;			/* count of arguments */
	char *argv[];			/* argument values */
	char *envp[];			/* array of environment strings */
{
	unsigned int i, j;		/* general temporaries */
	int fd;				/* file descriptor on targets */
	int pid;			/* child process id */
	char **nenvp;			/* new environment */
	struct exec exec;		/* template for inspecting target */
	union wait status;		/* dummy for wait() */
	struct exec a;

	/*
	 * Initial sanity check.
	 */
	if (argc <= 1) {
		(void) fprintf(stderr, "Usage: ldd file ... \n");
		exit(1);
	}
	(void) setlinebuf(stdout);
	(void) setlinebuf(stderr);

	/*
	 * Add an environment variable.  Count the number of existing
	 * ones, and then allocate up a new vector of environment strings
	 * and add ours to it.
	 */
	for (i = 0; envp[i]; i++)
		;
	if ((nenvp = (char **)malloc((i + 2) * sizeof (char *))) == NULL) {
		perror("ldd: out of memory for environment");
		exit(1);
	}
	nenvp[0] = "LD_TRACE_LOADED_OBJECTS=placeholder";
	for (j = 0; j < (i + 1); j++)
		nenvp[j + 1] = envp[j];

	/*
	 * For each argument, examine it and determine if it is an
	 * executable with a __DYNAMIC structure and an entry point.
	 * If it is, simply execute it and let the dynamic linker
	 * report its status.
	 */
	for (i = 1; i < argc; i++) {
		if ((fd = open(argv[i], O_RDONLY)) == -1) {
			fprintf(stderr, "ldd: %s unreadable: ", argv[1]);
			perror("");
			continue;
		}
		j = read(fd, (char *)&exec, sizeof (struct exec));
		(void) close(fd);
		if (j != sizeof (struct exec)) {
			(void) fprintf(stderr, "ldd: %s: not an executable\n",
			    argv[i]);
			continue;
		}
		if (N_BADMAG(exec)) {
			(void) fprintf(stderr, "ldd: %s: not an executable\n",
			    argv[i]);
			continue;
		}
		if (!exec.a_dynamic) {
			(void) printf("%s: statically linked\n", argv[i]);
			continue;
		}
		if (exec.a_entry == 0) {
			(void) fprintf("%s: not an executable\n", argv[i]);
			continue;
		}
		if ((pid = fork()) == 0) {
			if (argc > 2)
				(void) printf("%s:\n", argv[i]);
			execve(argv[i], nargv, nenvp);
			(void) fprintf(stderr,
			    "ldd: %s execution failed: ", argv[i]);
			perror("");
			continue;
		} else
			if (pid != -1) {
				while (wait(&status) != pid)
					;
				if (status.w_T.w_Termsig)
					(void) fprintf(stderr,
			"ldd: %s execution failed due to signal %d %s\n", 
					    argv[i], 
					    status.w_T.w_Termsig,
					    status.w_T.w_Coredump ? 
					    "(core dumped)" : "");
				else if (status.w_T.w_Retcode)
					(void) printf(stderr,
			"ldd: %s execution failed: exit status %d\n",
					    argv[i], 
					    status.w_T.w_Retcode);
			} else 
				perror("ldd: fork() failed");
	}
	exit(0);
	/*NOTREACHED*/
}
