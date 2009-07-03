/*-
 * Copyright (c) 2003 Mike Barcroft <mike@FreeBSD.org>
 * Copyright (c) 2008 Bjoern A. Zeeb <bz@FreeBSD.org>
 * Copyright (c) 2009 James Gritton <jamie@FreeBSD.org>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#include <sys/cdefs.h>
__FBSDID("$FreeBSD$");

#include <sys/param.h>
#include <sys/jail.h>
#include <sys/socket.h>
#include <sys/sysctl.h>

#include <arpa/inet.h>
#include <netinet/in.h>

#include <err.h>
#include <errno.h>
#include <jail.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define	JP_USER		0x01000000
#define	JP_OPT		0x02000000

#define	PRINT_DEFAULT	0x01
#define	PRINT_HEADER	0x02
#define	PRINT_NAMEVAL	0x04
#define	PRINT_QUOTED	0x08
#define	PRINT_SKIP	0x10
#define	PRINT_VERBOSE	0x20

static struct jailparam *params;
static int *param_noparent;
static int nparams;

static int add_param(const char *name, void *value, size_t valuelen,
		struct jailparam *source, unsigned flags);
static int sort_param(const void *a, const void *b);
static char *noname(const char *name);
static char *nononame(const char *name);
static int print_jail(int pflags, int jflags);
static void quoted_print(char *str);

int
main(int argc, char **argv)
{
	char *dot, *ep, *jname, *nname;
	int c, i, jflags, jid, lastjid, pflags, spc;

	jname = NULL;
	pflags = jflags = jid = 0;
	while ((c = getopt(argc, argv, "adj:hnqsv")) >= 0)
		switch (c) {
		case 'a':
		case 'd':
			jflags |= JAIL_DYING;
			break;
		case 'j':
			jid = strtoul(optarg, &ep, 10);
			if (!*optarg || *ep)
				jname = optarg;
			break;
		case 'h':
			pflags = (pflags & ~PRINT_SKIP) | PRINT_HEADER;
			break;
		case 'n':
			pflags = (pflags & ~PRINT_VERBOSE) | PRINT_NAMEVAL;
			break;
		case 'q':
			pflags |= PRINT_QUOTED;
			break;
		case 's':
			pflags = (pflags & ~(PRINT_HEADER | PRINT_VERBOSE)) |
			    PRINT_NAMEVAL | PRINT_QUOTED | PRINT_SKIP;
			break;
		case 'v':
			pflags = (pflags & ~(PRINT_NAMEVAL | PRINT_SKIP)) |
			    PRINT_VERBOSE;
			break;
		default:
			errx(1, "usage: jls [-dhnqv] [-j jail] [param ...]");
		}

	/* Add the parameters to print. */
	if (optind == argc) {
		if (pflags & PRINT_VERBOSE) {
			add_param("jid", NULL, (size_t)0, NULL, JP_USER);
			add_param("host.hostname", NULL, (size_t)0, NULL,
			    JP_USER);
			add_param("path", NULL, (size_t)0, NULL, JP_USER);
			add_param("name", NULL, (size_t)0, NULL, JP_USER);
			add_param("dying", NULL, (size_t)0, NULL, JP_USER);
			add_param("cpuset.id", NULL, (size_t)0, NULL, JP_USER);
			add_param("ip4.addr", NULL, (size_t)0, NULL, JP_USER);
			add_param("ip6.addr", NULL, (size_t)0, NULL,
			    JP_USER | JP_OPT);
		} else {
			pflags = (pflags &
			    ~(PRINT_NAMEVAL | PRINT_SKIP | PRINT_VERBOSE)) |
			    PRINT_DEFAULT;
			add_param("jid", NULL, (size_t)0, NULL, JP_USER);
			add_param("ip4.addr", NULL, (size_t)0, NULL, JP_USER);
			add_param("host.hostname", NULL, (size_t)0, NULL,
			    JP_USER);
			add_param("path", NULL, (size_t)0, NULL, JP_USER);
		}
	} else
		while (optind < argc)
			add_param(argv[optind++], NULL, (size_t)0, NULL,
			    JP_USER);

	if (pflags & PRINT_SKIP) {
		/* Check for parameters with boolean parents. */
		for (i = 0; i < nparams; i++) {
			if ((params[i].jp_flags & JP_USER) &&
			    (dot = strchr(params[i].jp_name, '.'))) {
				*dot = 0;
				nname = noname(params[i].jp_name);
				*dot = '.';
				param_noparent[i] =
				    add_param(nname, NULL, (size_t)0, NULL,
					JP_OPT);
				free(nname);
			}
		}
	}

	/* Add the index key parameters. */
	if (jid != 0)
		add_param("jid", &jid, sizeof(jid), NULL, 0);
	else if (jname != NULL)
		add_param("name", jname, strlen(jname), NULL, 0);
	else
		add_param("lastjid", &lastjid, sizeof(lastjid), NULL, 0);

	/* Print a header line if requested. */
	if (pflags & PRINT_VERBOSE)
		printf("   JID  Hostname                      Path\n"
		       "        Name                          State\n"
		       "        CPUSetID\n"
		       "        IP Address(es)\n");
	else if (pflags & PRINT_DEFAULT)
		printf("   JID  IP Address      "
		       "Hostname                      Path\n");
	else if (pflags & PRINT_HEADER) {
		for (i = spc = 0; i < nparams; i++)
			if (params[i].jp_flags & JP_USER) {
				if (spc)
					putchar(' ');
				else
					spc = 1;
				fputs(params[i].jp_name, stdout);
			}
		putchar('\n');
	}

	/* Fetch the jail(s) and print the paramters. */
	if (jid != 0 || jname != NULL) {
		if (print_jail(pflags, jflags) < 0)
			errx(1, "%s", jail_errmsg);
	} else {
		for (lastjid = 0;
		     (lastjid = print_jail(pflags, jflags)) >= 0; )
			;
		if (errno != 0 && errno != ENOENT)
			errx(1, "%s", jail_errmsg);
	}

	return (0);
}

static int
add_param(const char *name, void *value, size_t valuelen,
    struct jailparam *source, unsigned flags)
{
	struct jailparam *param, *tparams;
	int i, tnparams;

	static int paramlistsize;

	/* The pseudo-parameter "all" scans the list of available parameters. */
	if (!strcmp(name, "all")) {
		tnparams = jailparam_all(&tparams);
		if (tnparams < 0)
			errx(1, "%s", jail_errmsg);
		qsort(tparams, (size_t)tnparams, sizeof(struct jailparam),
		    sort_param);
		for (i = 0; i < tnparams; i++)
			add_param(tparams[i].jp_name, NULL, (size_t)0,
			    tparams + i, flags);
		free(tparams);
		return -1;
	}

	/* Check for repeat parameters. */
	for (i = 0; i < nparams; i++)
		if (!strcmp(name, params[i].jp_name)) {
			if (value != NULL && jailparam_import_raw(params + i,
			    value, valuelen) < 0)
				errx(1, "%s", jail_errmsg);
			params[i].jp_flags |= flags;
			if (source != NULL)
				jailparam_free(source, 1);
			return i;
		}

	/* Make sure there is room for the new param record. */
	if (!nparams) {
		paramlistsize = 32;
		params = malloc(paramlistsize * sizeof(*params));
		param_noparent =
		    malloc(paramlistsize * sizeof(*param_noparent));
		if (params == NULL || param_noparent == NULL)
			err(1, "malloc");
	} else if (nparams >= paramlistsize) {
		paramlistsize *= 2;
		params = realloc(params, paramlistsize * sizeof(*params));
		param_noparent = realloc(param_noparent,
		    paramlistsize * sizeof(*param_noparent));
		if (params == NULL || param_noparent == NULL)
			err(1, "realloc");
	}

	/* Look up the parameter. */
	param_noparent[nparams] = -1;
	param = params + nparams++;
	if (source != NULL) {
		*param = *source;
		param->jp_flags |= flags;
		return param - params;
	}
	if (jailparam_init(param, name) < 0)
		errx(1, "%s", jail_errmsg);
	param->jp_flags = flags;
	if ((value != NULL ? jailparam_import_raw(param, value, valuelen)
	     : jailparam_import(param, value)) < 0) {
		if (flags & JP_OPT) {
			nparams--;
			return (-1);
		}
		errx(1, "%s", jail_errmsg);
	}
	return param - params;
}

static int
sort_param(const void *a, const void *b)
{
	const struct jailparam *parama, *paramb;
	char *ap, *bp;

	/* Put top-level parameters first. */
	parama = a;
	paramb = b;
	ap = strchr(parama->jp_name, '.');
	bp = strchr(paramb->jp_name, '.');
	if (ap && !bp)
		return (1);
	if (bp && !ap)
		return (-1);
	return (strcmp(parama->jp_name, paramb->jp_name));
}

static char *
noname(const char *name)
{
	char *nname, *p;

	nname = malloc(strlen(name) + 3);
	if (nname == NULL)
		err(1, "malloc");
	p = strrchr(name, '.');
	if (p != NULL)
		sprintf(nname, "%.*s.no%s", (int)(p - name), name, p + 1);
	else
		sprintf(nname, "no%s", name);
	return nname;
}

static char *
nononame(const char *name)
{
	char *nname, *p;

	p = strrchr(name, '.');
	if (strncmp(p ? p + 1 : name, "no", 2))
		return NULL;
	nname = malloc(strlen(name) - 1);
	if (nname == NULL)
		err(1, "malloc");
	if (p != NULL)
		sprintf(nname, "%.*s.%s", (int)(p - name), name, p + 3);
	else
		strcpy(nname, name + 2);
	return nname;
}

static int
print_jail(int pflags, int jflags)
{
	char *nname;
	char **param_values;
	int i, ai, jid, count, spc;
	char ipbuf[INET6_ADDRSTRLEN];

	jid = jailparam_get(params, nparams, jflags);
	if (jid < 0)
		return jid;
	if (pflags & PRINT_VERBOSE) {
		printf("%6d  %-29.29s %.74s\n"
		       "%6s  %-29.29s %.74s\n"
		       "%6s  %-6d\n",
		    *(int *)params[0].jp_value,
		    (char *)params[1].jp_value,
		    (char *)params[2].jp_value,
		    "",
		    (char *)params[3].jp_value,
		    *(int *)params[4].jp_value ? "DYING" : "ACTIVE",
		    "",
		    *(int *)params[5].jp_value);
		count = params[6].jp_valuelen / sizeof(struct in_addr);
		for (ai = 0; ai < count; ai++)
			if (inet_ntop(AF_INET,
			    &((struct in_addr *)params[6].jp_value)[ai],
			    ipbuf, sizeof(ipbuf)) == NULL)
				err(1, "inet_ntop");
			else
				printf("%6s  %-15.15s\n", "", ipbuf);
		if (!strcmp(params[7].jp_name, "ip6.addr")) {
			count = params[7].jp_valuelen / sizeof(struct in6_addr);
			for (ai = 0; ai < count; ai++)
				if (inet_ntop(AF_INET6,
				    &((struct in_addr *)params[7].jp_value)[ai],
				    ipbuf, sizeof(ipbuf)) == NULL)
					err(1, "inet_ntop");
				else
					printf("%6s  %-15.15s\n", "", ipbuf);
		}
	} else if (pflags & PRINT_DEFAULT)
		printf("%6d  %-15.15s %-29.29s %.74s\n",
		    *(int *)params[0].jp_value,
		    params[1].jp_valuelen == 0 ? "-"
		    : inet_ntoa(*(struct in_addr *)params[1].jp_value),
		    (char *)params[2].jp_value,
		    (char *)params[3].jp_value);
	else {
		param_values = alloca(nparams * sizeof(*param_values));
		for (i = 0; i < nparams; i++) {
			if (!(params[i].jp_flags & JP_USER))
				continue;
			param_values[i] = jailparam_export(params + i);
			if (param_values[i] == NULL)
				errx(1, "%s", jail_errmsg);
		}
		for (i = spc = 0; i < nparams; i++) {
			if (!(params[i].jp_flags & JP_USER))
				continue;
			if ((pflags & PRINT_SKIP) &&
			    ((!(params[i].jp_ctltype &
				(CTLFLAG_WR | CTLFLAG_TUN))) ||
			     (param_noparent[i] >= 0 &&
			      *(int *)params[param_noparent[i]].jp_value)))
				continue;
			if (spc)
				putchar(' ');
			else
				spc = 1;
			if (pflags & PRINT_NAMEVAL) {
				/*
				 * Generally "name=value", but for booleans
				 * either "name" or "noname".
				 */
				if (params[i].jp_flags &
				    (JP_BOOL | JP_NOBOOL)) {
					if (*(int *)params[i].jp_value)
						printf("%s", params[i].jp_name);
					else {
						nname = (params[i].jp_flags &
						    JP_NOBOOL) ?
						    nononame(params[i].jp_name)
						    : noname(params[i].jp_name);
						printf("%s", nname);
						free(nname);
					}
					continue;
				}
				printf("%s=", params[i].jp_name);
			}
			if (params[i].jp_valuelen == 0) {
				if (pflags & PRINT_QUOTED)
					printf("\"\"");
				else if (!(pflags & PRINT_NAMEVAL))
					putchar('-');
			} else
				quoted_print(param_values[i]);
		}
		putchar('\n');
		for (i = 0; i < nparams; i++)
			if (params[i].jp_flags & JP_USER)
				free(param_values[i]);
	}
	for (i = 0; i < nparams; i++)
		if (!(params[i].jp_flags & JP_RAWVALUE)) {
			free(params[i].jp_value);
			params[i].jp_value = NULL;
		}
	return (jid);
}

static void
quoted_print(char *str)
{
	int c, qc;
	char *p = str;

	/* An empty string needs quoting. */
	if (!*p) {
		fputs("\"\"", stdout);
		return;
	}

	/*
	 * The value will be surrounded by quotes if it contains spaces
	 * or quotes.
	 */
	qc = strchr(p, '\'') ? '"'
	    : strchr(p, '"') ? '\''
	    : strchr(p, ' ') || strchr(p, '\t') ? '"'
	    : 0;
	if (qc)
		putchar(qc);
	while ((c = *p++)) {
		if (c == '\\' || c == qc)
			putchar('\\');
		putchar(c);
	}
	if (qc)
		putchar(qc);
}
