/*-
 * Copyright (c) 1993
 *	The Regents of the University of California.  All rights reserved.
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

#ifndef lint
static char copyright[] =
"@(#) Copyright (c) 1993\n\
	The Regents of the University of California.  All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)nvi.recover.c	8.1 (Berkeley) 6/6/93";
#endif /* not lint */

#include <sys/param.h>
#include <sys/stat.h>

#include <dirent.h>
#include <err.h>
#include <pwd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "pathnames.h"

void usage __P((void));
void mail __P((char *, uid_t, time_t, char *));

int
main(argc, argv)
	int argc;
	char *argv[];
{
	extern int optind;
	extern char *optarg;
	struct dirent *dp;
	struct stat sb;
	DIR *dirp;
	int ch;
	char host[MAXHOSTNAMELEN], *p;

	p = _PATH_PRESERVE;
	while ((ch = getopt(argc, argv, "d:")) != EOF)
		switch(ch) {
		case 'd':
			p = optarg;
			break;
		case '?':
		default:
			usage();
		}
	argc -= optind;
	argv += optind;

	if (argc != 0)
		usage();

	if (chdir(p) || (dirp = opendir(".")) == NULL)
		err(1, "%s", p);

	(void)gethostname(host, sizeof(host));
	while ((dp = readdir(dirp)) != NULL) {
		if (dp->d_namlen <= 3 || strncmp(dp->d_name, "vi.", 3))
			continue;
		if ((p = strrchr(dp->d_name + 3, '.')) == NULL)
			continue;
		if (stat(dp->d_name, &sb)) {
			warn("%s", dp->d_name);
			continue;
		}
		*p = '\0';

		mail(host, sb.st_uid, sb.st_mtime, dp->d_name + 3);
	}
	(void)closedir(dirp);
	return (0);
}

void
mail(host, id, when, fname)
	char *host;
	uid_t id;
	time_t when;
	char *fname;
{
	struct passwd *pw;
	FILE *pf;
	char *to, *user, buf[MAXPATHLEN + 100], ubuf[50];

	if ((pw = getpwuid(id)) == NULL) {
		to = "root";
		(void)snprintf(ubuf, sizeof(ubuf), "%u", id);
		user = ubuf;
	} else
		user = to = pw->pw_name;
	
	(void)snprintf(buf, sizeof(buf),
	    "%s -i -t -F \"Nvi recovery program\" -f root", _PATH_SENDMAIL);
	if ((pf = popen(buf, "w")) == NULL)
		err(1, "%s", _PATH_SENDMAIL);
	(void)fprintf(pf, "%s\n%s\n%s%s\n%s%s\n%s\n\n",
	    "Reply-To: root",
	    "From: root (Nvi recovery program)",
	    "To: ",
	    user,
	    "Subject: Nvi saved the file ",
	    fname,
	    "Precedence: bulk");			/* For vacation(1). */
	(void)fprintf(pf, "%s%.24s%s%s\n%s%s%s\n",
	    "On ", ctime(&when),
	    ", the user ", user,
	    "was editing a file named ", fname,
	    " on the machine ");
	(void)fprintf(pf, "%s%s\n",
	    host, ", when it was saved for\nrecovery.");
	(void)fprintf(pf, "\n%s\n%s\n%s\n\n%s\n",
	    "You can recover most, if not all, of the changes",
	    "to this file using the -l and -r options to nvi(1)",
	    "or nex(1).",
	    "-- The nvi recovery program (nvi.recover(8))");
	(void)pclose(pf);
}

void
usage()
{
	(void)fprintf(stderr, "usage: vi.recover [-d directory]\n");
	exit(1);
}
