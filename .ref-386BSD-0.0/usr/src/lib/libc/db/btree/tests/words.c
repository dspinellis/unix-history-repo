/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Mike Olson.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)words.c	5.1 (Berkeley) 4/12/91";
#endif /* LIBC_SCCS and not lint */

/*
 *  test1.c -- simple btree test program.
 */

#include <stdio.h>
#include <ctype.h>
#include <sys/param.h>
#include <sys/types.h>
#include <sys/file.h>
#include <db.h>
#include <btree.h>

#define	DICTIONARY	"/usr/share/dict/words"

typedef struct cmd_table {
	char *cmd;
	int nargs;
	int (*func)();
	char *descrip;
} cmd_table;

extern int cursor(), delcur(), delete(), first(), help(), insert();
extern int last(), lookup(), next(), previous();

cmd_table Commands[] = {
	"cursor", 2, cursor,
		"cursor <word>:  point the scan cursor at <word>",
	"delcur", 1, delcur,
		"delcur:  delete the word under the scan cursor",
	"delete", 2, delete,
		"delete <word>:  delete <word> from the dictionary",
	"first", 1, first,
		"first: point the scan cursor at the first dictionary entry",
	"help", 1, help,
		"help:  print this command summary",
	"insert", 3, insert,
		"insert <word> <def>:  insert <word> into the dictionary with definition <def>",
	"last", 1, last,
		"last:  point the scan cursor at the last dictionary entry",
	"lookup", 2, lookup,
		"lookup <word>:  look up <word> in the dictionary",
	"next", 1, next,
		"next:  move the scan cursor forward one word",
	"previous", 1, previous,
		"previous:  move the scan cursor back one word",
	(char *) NULL, 0, NULL, (char *) NULL,
};

char *Usage = "[-p pagesize] [-c cachesize] [-u] [-l|b|n] [dbname]";

main(argc, argv)
	int argc;
	char **argv;
{
	char *dbname;
	int c;
	char *progname;
	extern int strcmp();
	extern char *optarg;
	extern int optind;
	DB *t;
	BTREEINFO b;

	progname = *argv;

	b.psize = 0;
	b.cachesize = 0;
	b.lorder = 0;
	b.flags = R_DUP;
	b.compare = strcmp;

	while ((c = getopt(argc, argv, "p:c:ulb")) != EOF) {
		switch (c) {
		  case 'p':
			b.psize = atoi(optarg);
			break;

		  case 'c':
			b.cachesize = atoi(optarg);
			break;

		  case 'u':
			b.flags = 0;
			break;

		  case 'l':
			b.lorder = LITTLE_ENDIAN;
			break;

		  case 'b':
			b.lorder = BIG_ENDIAN;
			break;

		  default:
			fprintf(stderr, "%s: usage: %s\n", progname, Usage);
			exit (1);
		}
	}

	if (argv[optind] != (char *) NULL)
		dbname = argv[optind];

	if ((t = btree_open(dbname, O_CREAT|O_RDWR, 0600, &b)) == (DB *) NULL) {
		perror(progname);
		exit (1);
	}

	load(t);

	user(t);
}

load(t)
	DB *t;
{
	char *lbuf;
	int i, l;
	int status;
	FILE *fp;
	DBT key;
	DBT data;
	char word[64];
	char drow[64];

	printf("loading %s...\n", DICTIONARY);
	fflush(stdout);
	if ((fp = fopen(DICTIONARY, "r")) == (FILE *) NULL) {
		perror("/usr/dict/words");
		(void) (*(t->close))(t->internal);
		exit (1);
	}

	key.data = &word[0];
	data.data = &drow[0];
	while ((lbuf = fgets(word, 64, fp)) != (char *) NULL) {
		l = strlen(lbuf) - 1;
		lbuf[l] = '\0';
		for (i = 0; i < l; i++)
			drow[l - (i + 1)] = word[i];
		drow[l] = '\0';

		key.size = data.size = l + 1;

		status = (*(t->put))(t->internal, &key, &data, R_NOOVERWRITE);

		switch (status) {
		  case RET_SUCCESS:
			break;

		  case RET_ERROR:
			perror("put");
			break;

		  case RET_SPECIAL:
			fprintf(stderr, "%s is a duplicate key!\n", lbuf);
			fflush(stderr);
			break;
		}
	}

	(void) fclose(fp);
	printf("done\n");
	fflush(stdout);
}

user(t)
	DB *t;
{
	char *lbuf;
	int argc;
	int i;
	char *argv[4];
	char buf[512];

	for (;;) {
		printf("> ");
		fflush(stdout);
		if ((lbuf = fgets(&buf[0], 512, stdin)) == (char *) NULL)
			break;
		lbuf[strlen(lbuf) - 1] = '\0';

		if (strcmp(lbuf, "quit") == 0)
			break;

		argc = parse(lbuf, &argv[0], 3);
		if (argc == 0)
			continue;

		for (i = 0; Commands[i].cmd != (char *) NULL; i++) {
			if (strcmp(Commands[i].cmd, argv[0]) == 0)
				break;
		}

		if (Commands[i].cmd == (char *) NULL) {
			fprintf(stderr,
				"%s: command unknown ('help' for help)\n",
				lbuf);
			fflush(stderr);
			continue;
		}

		if (Commands[i].nargs != argc) {
			fprintf(stderr, "arg count\n");
			fflush(stderr);
			continue;
		}

		switch (argc) {
		  case 1:
			(*(Commands[i].func))(t);
			break;
		  case 2:
			(*(Commands[i].func))(t, argv[1]);
			break;
		  case 3:
			(*(Commands[i].func))(t, argv[1], argv[2]);
			break;
		  case 4:
			(*(Commands[i].func))(t, argv[1], argv[2], argv[3]);
			break;
		}
	}
	(void) (*(t->close))(t->internal);
	exit (0);
}

int
parse(lbuf, argv, maxargc)
	char *lbuf;
	char **argv;
	int maxargc;
{
	int argc = 0;
	char *c;

	c = lbuf;
	while (isspace(*c))
		c++;
	while (*c != '\0' && argc < maxargc) {
		*argv++ = c;
		argc++;
		while (!isspace(*c) && *c != '\0') {
			c++;
		}
		while (isspace(*c))
			*c++ = '\0';
	}
	return (argc);
}

int
cursor(t, arg)
	DB *t;
	char *arg;
{
	int status;
	DBT key;
	DBT data;

	key.data = arg;
	key.size = strlen(arg + 1);
	status = (*(t->seq))(t->internal, &key, &data, R_CURSOR);
	if (status == RET_SUCCESS)
		show(&key, &data);
	else
		perror("cursor");
}

int
delcur(t)
	DB *t;
{
	int status;

	status = (*(t->delete))(t->internal, (DBT *) NULL, R_CURSOR);

	if (status == RET_ERROR)
		perror("delcur");
}

int
delete(t, arg)
	DB *t;
	char *arg;
{
	int status;
	DBT key;

	key.data = arg;
	key.size = strlen(arg) + 1;

	status = (*(t->delete))(t->internal, &key, 0);
	switch (status) {
	  case RET_SUCCESS:
		break;

	  case RET_ERROR:
		perror("delete");
		break;

	  case RET_SPECIAL:
		fprintf(stderr, "%s not found\n", arg);
		fflush(stderr);
		break;
	}
}

int
first(t)
	DB *t;
{
	int status;
	DBT key;
	DBT data;

	status = (*(t->seq))(t->internal, &key, &data, R_FIRST);

	switch (status) {
	  case RET_ERROR:
		perror("first");
		break;

	  case RET_SPECIAL:
		printf("no more keys");
		break;

	  case RET_SUCCESS:
		show(&key, &data);
		break;
	}
}
int
help(t)
	DB *t;
{
	int i;

#ifdef lint
	t = t;
#endif /* lint */
	for (i = 0; Commands[i].cmd != (char *) NULL; i++)
		printf("%s\n", Commands[i].descrip);
	printf("type 'quit' to quit\n");
}

int
insert(t, arg, def)
	DB *t;
	char *arg;
	char *def;
{
	int status;
	DBT key;
	DBT data;

	key.data = arg;
	key.size = strlen(arg) + 1;
	data.data = def;
	data.size = strlen(def) + 1;

	status = (*(t->put))(t->internal, &key, &data, R_NOOVERWRITE);
	switch (status) {
	  case RET_SUCCESS:
		break;

	  case RET_ERROR:
		perror("put");
		break;

	  case RET_SPECIAL:
		fprintf(stderr, "%s is a duplicate key!\n", arg);
		fflush(stderr);
		break;
	}
}

int
last(t)
	DB *t;
{
	int status;
	DBT key;
	DBT data;

	status = (*(t->seq))(t->internal, &key, &data, R_LAST);

	switch (status) {
	  case RET_ERROR:
		perror("last");
		break;

	  case RET_SPECIAL:
		printf("no more keys");
		break;

	  case RET_SUCCESS:
		show(&key, &data);
		break;
	}
}

int
lookup(t, arg)
	DB *t;
	char *arg;
{
	int status;
	DBT key;
	DBT data;

	key.data = arg;
	key.size = strlen(arg) + 1;

	status = (*(t->get))(t->internal, &key, &data, 0);

	switch (status) {
	  case RET_SPECIAL:
		printf("not found\n");
		break;
	  case RET_SUCCESS:
		show(&key, &data);
		break;
	  case RET_ERROR:
		perror("get");
		break;
	}
}

int
next(t)
	DB *t;
{
	int status;
	DBT key;
	DBT data;

	status = (*(t->seq))(t->internal, &key, &data, R_NEXT);

	switch (status) {
	  case RET_ERROR:
		perror("next");
		break;

	  case RET_SPECIAL:
		printf("no more keys");
		break;

	  case RET_SUCCESS:
		show(&key, &data);
		break;
	}
}

int
previous(t)
	DB *t;
{
	int status;
	DBT key;
	DBT data;

	status = (*(t->seq))(t->internal, &key, &data, R_PREV);

	switch (status) {
	  case RET_ERROR:
		perror("previous");
		break;

	  case RET_SPECIAL:
		printf("no more keys");
		break;

	  case RET_SUCCESS:
		show(&key, &data);
		break;
	}
}

show(key, data)
	DBT *key;
	DBT *data;
{
	if (key->size > 0)
		printf("%s", key->data);
	if (data->size > 0)
		printf("/%s", data->data);
	printf("\n");
}
