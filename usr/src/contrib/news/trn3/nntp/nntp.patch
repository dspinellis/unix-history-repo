Here's my latest patch to modify NNTP 1.5.11 to support the transmission
of thread databases.  

This patch supports both mthread's .thread file format (XTHREAD) and News
OVerview's .overfile file format (XOVER).  It also adds the LISTGROUP
command for listing the numbers in a group, and the DATE command for
finding out the time on the server.  Finally, the NEWGROUPS command is
filtered based the user's access privileges.

Apply this patch from the root directory of your NNTP source.  While it
only affects your server (the clientlib is no longer modified), it does
modify things in the common directory.

To apply this:
	cd nntp-1.5.11
	patch -p < xthread.patch
  OR	unipatch < xthread.patch | patch -p

This patch changes the conf.h.dist file, so you will need to edit your
conf.h file to add the new defines.  For example, you could:

	cd common
	diff -c conf.h.dist.orig conf.h.dist | patch conf.h

and then edit conf.h to make sure the new stuff is configured properly.

Then you are ready to compile the new server with "make server" as usual.

Wayne Davison                                        davison@borland.com
---8<------8<------8<------8<---cut here--->8------>8------>8------>8---
Index:common/version.c
Prereq: "1.5.11
@@ -2,4 +2,4 @@
  * Provide the version number of this release.
  */
 
-char	nntp_version[] = "1.5.11 (10 February 1991)";
+char	nntp_version[] = "1.5.11t3 (10 February 1993)";
Index:CHANGES
@@ -2,6 +2,22 @@
 since the initial release.  Individuals who either reported the bug or
 inspired the bug fix are in square brackets.
 
+1.5.11+XOVER1   (Wayne Davison <davison@borland.com> 18 Jan 93)
+	Fixes, optimizations, and enhancements to the first patch.
+
+1.5.11+XOVER0	(Rich $alz <rsalz@uunet.uu.net> 23 dec 92)
+	This adds the XOVER command to the server.  The XOVER command
+	is used to retrieve data from the .overview file that is part
+	of Geoff Collyer's "nov" package (that package is not provided
+	here; the official archive for it is
+	world.std.com:pub/src/news/nov.dist.tar.Z).  This command
+	has the following syntax:
+		XOVER [first[-last]]
+	Where first and last are article numbers; the default is to
+	return data for the current article.  This command is only
+	valid after a GROUP command.  It returns a 224 code followed
+	by a multi-line response.
+
 1.5.11
 	Fixes to spawn.c and batch.c for those system that need
 	execle() to specifically call /bin/sh to exectute a sh script.
Index:common/README
@@ -340,6 +340,10 @@
 Authorization process. Read the file AUTHORIZATION in the root directory of
 the NNTP distribution for more information.
 
+XOVER		(defined)
+     Defines whether we want to include the XOVER command, described
+in the top-level README file of this distribution.
+
 SERVER_FILE	("/usr/local/lib/rn/server")
 
      This file contains the name of the machine which runs the
Index:common/conf.h.dist
@@ -94,6 +94,21 @@
 			/* the server more.  If your server is heavily */
 			/* loaded already, defining this may be a bad idea */
 
+/* XTHREAD defines:  if XTHREAD is defined, THREAD_DIR controls where the
+ * thread files will be read from.
+ */
+#define XTHREAD		/* Optional XTHREAD command.  This allows trn to
+			 * keep all data on the server. */
+
+/* Leave this undefined to indicate that thread files go in the spool
+ * directory.  However, if you want a separate hierarchy of thread files,
+ * define it here.
+ */
+/*#define THREAD_DIR	"/usr/spool/threads"		/* base directory */
+
+/* if LONG_THREAD_NAMES & THREAD_DIR are defined, create threads in one dir */
+#undef LONG_THREAD_NAMES		/* not for short-name systems */
+
 /* Things that vary in network implementations */
 #define	SUBNET		/* If you have 4.3 subnetting */
 #undef	DAMAGED_NETMASK	/* If your subnet mask is not a multiple of */
@@ -201,6 +216,10 @@
 /* Things that relate to authentication and access */
 /* Define AUTH to use the proposed NNTP Version 2 authentication protocol. */
 #define	AUTH	
+
+/* Various protocol extensions */
+#define XOVER		/* xover -- Return .overview data */
+
 /*
  * A file containing the name of the host which is running
  * the news server.  This will have to match what rrn thinks,
@@ -332,6 +351,24 @@
 #			endif
 #		endif
 #	endif
+#endif
+
+#ifdef XTHREAD
+# ifdef THREAD_DIR
+#   ifdef LONG_THREAD_NAMES
+#	undef SUFFIX
+#   else
+#     ifndef SUFFIX
+#	define SUFFIX ".th"
+#     endif
+#   endif
+# else
+#   define THREAD_DIR	SPOOLDIR
+#   ifndef SUFFIX
+#     define SUFFIX	"/.thread"
+#   endif
+#   undef LONG_THREAD_NAMES
+# endif
 #endif
 
 /*
Index:common/nntp.h
@@ -20,6 +20,7 @@
  *	x2x	Article selection
  *	x3x	Distribution
  *	x4x	Posting
+ *	x8x	Authorization
  */
 
 #define	CHAR_INF	'1'
@@ -29,6 +30,7 @@
 #define	CHAR_FATAL	'5'
 
 #define	INF_HELP	100	/* Help text on way */
+#define	INF_DATE	111	/* Date */
 #define	INF_AUTH	180	/* Authorization capabilities */
 #define	INF_DEBUG	199	/* Debug output */
 
@@ -42,6 +44,7 @@
 #define	OK_HEAD		221	/* Head follows */
 #define	OK_BODY		222	/* Body follows */
 #define	OK_NOTEXT	223	/* No text sent -- stat, next, last */
+#define	OK_OVER		224	/* Overview data follows */
 #define	OK_NEWNEWS	230	/* New articles by message-id follow */
 #define	OK_NEWGROUPS	231	/* New newsgroups follow */
 #define	OK_XFERED	235	/* Article transferred successfully */
@@ -48,6 +51,7 @@
 #define	OK_POSTED	240	/* Article posted successfully */
 #define	OK_AUTHSYS	280	/* Authorization system ok */
 #define	OK_AUTH		281	/* Authorization (user/pass) ok */
+#define	OK_BIN		282	/* binary data follows */
 
 #define	CONT_XFER	335	/* Continue to send article */
 #define	CONT_POST	340	/* Continue to post article */
Index:server/Makefile
@@ -6,13 +6,13 @@
 	ahbs.o globals.o group.o help.o ihave.o list.o misc.o netaux.o \
 	newgroups.o newnews.o nextlast.o ngmatch.o post.o parsit.o scandir.o \
 	slave.o spawn.o strcasecmp.o subnet.o time.o xhdr.o fakesyslog.o \
-	batch.o auth.o timer.o ../common/version.o
+	batch.o auth.o timer.o xthread.o ../common/version.o
 
 SRVRSRC = main.c serve.c access.c access_inet.c access_dnet.c active.c \
 	ahbs.c globals.c group.c help.c ihave.c list.c misc.c netaux.c \
 	newgroups.c newnews.c nextlast.c ngmatch.c post.c parsit.c scandir.c \
 	slave.c spawn.c strcasecmp.c subnet.c time.c xhdr.c fakesyslog.c \
-	batch.c auth.c timer.c ../common/version.c
+	batch.c auth.c timer.c xthread.c ../common/version.c
 
 SRVRINC = common.h ../common/conf.h ../common/nntp.h timer.h
 
Index:server/ahbs.c
@@ -56,8 +56,9 @@
 			(void) fclose(fp);
 			return;
 		}
-		printf("%d 0 %s Article retrieved, %s.\r\n",
-			OK_ARTICLE + method, argv[1], verbiage[method]);
+		printf("%d %ld %s Article retrieved, %s.\r\n",
+			OK_ARTICLE + method, group_artnum, argv[1],
+			verbiage[method]);
 		spew(fp, method);
 		(void) fclose(fp);
 #ifdef LOG
Index:server/batch.c
@@ -461,7 +461,7 @@
 #ifdef POSTER
 	sprintf(user, "USER=%s", POSTER);
 	sprintf(logname, "LOGNAME=%s", POSTER);
-	if ((home = (char *)malloc(strlen(home_poster)+5)) != NULL)
+	if ((home = (char *)malloc(strlen(home_poster)+5+1)) != NULL)
 		sprintf(home, "HOME=%s", home_poster);
 	envp[0] = user;
 	envp[1] = logname;
Index:server/common.h
@@ -164,11 +164,18 @@
 extern	char	inews[];
 extern	char	rnews[];
 
+#ifdef	XTHREAD
+extern	char	*threaddir;
+extern	char	*threadfile;
+#endif
+
 extern	char	**group_array;
 extern	char	*actbuf;
 extern	int	num_groups;
 extern	char	*homedir;
 extern	int	ingroup;
+extern	char	*group_name;
+extern	long	group_artnum;
 extern	int	maxgroups;
 #ifdef DYNAMIC_ART_ARRAY
 extern	int	*art_array;
Index:server/globals.c
@@ -26,6 +26,11 @@
 char	inews[] = INEWS;
 char	rnews[] = RNEWS;
 
+#ifdef	XTHREAD
+char	*threaddir = THREAD_DIR;
+char	*threadfile = NULL;
+#endif
+
 /*
  * Other random externals.
  */
@@ -34,6 +39,8 @@
 char 	*actbuf;
 int	num_groups;
 int	ingroup = 0;
+char	*group_name = NULL;
+long	group_artnum = 0;
 int	art_ptr;
 int	num_arts;
 #ifdef DYNAMIC_ART_ARRAY
Index:server/group.c
@@ -4,6 +4,12 @@
 
 #include "common.h"
 
+#ifdef	XTHREAD
+extern char *thread_name();
+#endif
+
+extern char *malloc();
+
 /*
  * GROUP newsgroup
  *
@@ -67,8 +73,15 @@
 		return;
 	}
 
+#ifdef XOVER
+	close_xover();
+#endif
 	close_crnt();
+	if (group_name)
+		free(group_name);
 	(void) chdir(spooldir);
+	if ((group_name = malloc(strlen(argv[1])+1)) != NULL)
+		strcpy(group_name, argv[1]);
 
 #ifdef LOG
 	syslog(LOG_INFO, "%s group %s", hostname, argv[1]);
@@ -97,6 +110,10 @@
 
 	ingroup = 1;
 
+#ifdef	XTHREAD
+	threadfile = thread_name(argv[1]);
+#endif
+
 	while ((cp = index(argv[1], '/')) != (char *) NULL)
 		*cp = '.';
 
@@ -108,3 +125,177 @@
 		argv[1]);
 	(void) fflush(stdout);
 }
+
+
+#ifdef XOVER
+static FILE *xover_fp;
+static int xover_num;
+
+doxover(argc, argv)
+	int		argc;
+	char		*argv[];
+{
+	register FILE	*fp;
+	register int	c, first, last;
+	int		artnum, artp;
+	char		*p;
+
+	if (!canread) {
+		printf("%d You only have permission to transfer, sorry.\r\n",
+			ERR_ACCESS);
+		(void) fflush(stdout);
+		return;
+	}
+
+	if (!ingroup) {
+		printf("%d You are not currently in a newsgroup.\r\n",
+			ERR_NCING);
+		(void) fflush(stdout);
+		return;
+	}
+	if (argc != 1 && argc != 2) {
+		printf("%d Usage: XOVER [first[-last]].\r\n", ERR_CMDSYN);
+		(void) fflush(stdout);
+		return;
+	}
+
+	if (xover_fp)
+		fp = xover_fp;
+	else {
+		fp = xover_fp = fopen(".overview", "r");
+		if (fp == NULL) {
+			printf("%d No overview available.\r\n", ERR_FAULT);
+			(void) fflush(stdout);
+#ifdef SYSLOG
+			syslog(LOG_ERR, "xover: fopen %s: %m", ".overview");
+#endif
+			return;
+		}
+		xover_num = 0;
+	}
+
+	if (argc == 1) {
+		if (art_ptr < 0 || art_ptr >= num_arts) {
+			printf("%d No article is currently selected.\r\n",
+				ERR_NOCRNT);
+			(void) fflush(stdout);
+			return;
+		}
+		first = last = art_array[art_ptr];
+	} else {
+		p = index(argv[1], '-');
+		if (p == NULL)
+			first = last = atoi(argv[1]);
+		else {
+			*p++ = '\0';
+			first = atoi(argv[1]);
+			last = atoi(p);
+		}
+		if (first < 1)
+		    first = 1;
+		if (last > art_array[num_arts-1])
+		    last = art_array[num_arts-1];
+	}
+	/* Return the desired data.  This is written carefully to avoid
+	 * over-long lines. */
+	printf("%d overview data follows\r\n", OK_OVER);
+	if (first < xover_num || !xover_num) {
+		fseek(fp, 0L, 0);
+		xover_num = 0;
+	}
+	if (xover_num) {
+		artnum = xover_num;
+		xover_num = 0;
+	} else
+		fscanf(fp, "%d", &artnum);
+	artp = 0;
+	while (!feof(fp)) {
+		if (artnum > last) {
+			xover_num = artnum;
+			break;
+		}
+		while (art_array[artp] < artnum)
+		    artp++;
+		if (artnum >= first && artnum == art_array[artp]) {
+			printf("%d", artnum);
+			while ((c = getc(fp)) != EOF && c != '\n')
+				putchar(c);
+			printf("\r\n");
+		} else
+			while ((c = getc(fp)) != EOF && c != '\n')
+				continue;
+		fscanf(fp, "%d", &artnum);
+	}
+	printf(".\r\n");
+	(void) fflush(stdout);
+}
+
+close_xover()
+{
+	if (xover_fp) {
+		fclose(xover_fp);
+		xover_fp = NULL;
+	}
+}
+#endif
+
+#ifdef LISTGROUP
+/*
+ * LISTGROUP [group]
+ *
+ * Lists all article numbers (filenames) in the given group. Used by
+ * newsreaders such as nn and trn for fast validation of a database.
+ * If a group name is given it becomes the current group.
+ *
+ * This command is an extention, and not included in RFC 977.
+ */
+
+listgroup(argc, argv)
+	int		argc;
+	char		*argv[];
+{
+	register int i;
+
+	if (argc == 2) {
+		ingroup = 0;
+		/* This will output a success or failure message */
+		group(argc, argv);
+		if (!ingroup) {
+			return;
+		}
+	} else if (argc > 2) {
+		printf("%d Usage: LISTGROUP [newsgroup].\r\n", ERR_CMDSYN);
+		(void) fflush(stdout);
+		return;
+	} else if (!ingroup) {
+		printf("%d You are not currently in a newsgroup.\r\n",
+			ERR_NCING);
+		(void) fflush(stdout);
+		return;
+	} else if (!canread) {
+		printf("%d You only have permission to transfer, sorry.\r\n",
+			ERR_ACCESS);
+		(void) fflush(stdout);
+		return;
+	} else {
+		/* output a success message when no group name is given */
+		printf("%d %d %d %d (current group)\r\n",
+			OK_GROUP,
+			num_arts,
+			(num_arts > 0 ? art_array[0] : 0),
+			(num_arts > 0 ? art_array[num_arts-1] : 0));
+	}
+
+#ifdef LOG
+	syslog(LOG_INFO, "%s listgroup", hostname);
+#endif
+	for (i = 0; i < num_arts; i++) {
+		printf("%d\r\n", art_array[i]);
+	}
+	putchar('.');
+	putchar('\r');
+	putchar('\n');
+	(void) fflush(stdout);
+}
+
+#endif /* LISTGROUP */
Index:server/help.c
@@ -21,8 +21,22 @@
 	printf("NEXT        POST         QUIT\r\n");
 	printf("STAT        NEWGROUPS    HELP\r\n");
 	printf("IHAVE       NEWNEWS      SLAVE\r\n");
-	printf("\r\nAdditionally, the following extention is supported:\r\n\r\n");
+	printf("DATE\r\n");
+#if defined(XHDR) || defined(XTHREAD) || defined(LISTGROUP) || defined(XOVER)
+	printf("\r\nAdditionally, the following extension(s) are supported:\r\n\r\n");
+# ifdef LISTGROUP
+	printf("LISTGROUP   Retrieve a list of valid article-numbers.\r\n");
+# endif
+# ifdef	XHDR
 	printf("XHDR        Retrieve a single header line from a range of articles.\r\n");
+# endif
+# ifdef	XOVER
+	printf("XOVER       Return news overview data\r\n");
+# endif
+# ifdef	XTHREAD
+	printf("XTHREAD     Retrieve trn thread file for the current group.\r\n");
+# endif
+#endif
 	printf("\r\n");
 	printf("Bugs to Stan Barber (Internet: nntp@tmc.edu; UUCP: ...!bcm!nntp)\r\n");
 	printf(".\r\n");
Index:server/misc.c
@@ -100,7 +100,7 @@
 	int		lookup;
 {
 	char		line[MAXBUFLEN];
-	char		*tmp;
+	char		*start, *end;
 	register char	*cp;
 	long		ltmp;
 	static char	path[MAXPATHLEN];
@@ -118,6 +118,7 @@
 #endif USGHIST
 	static FILE	*hfp = NULL;	/* history file, text version */
 
+	group_artnum = 0;
 #ifdef CNEWS
 	cp = rindex(msg_id,'@');	/* look for @ in message id */
 	if( cp != NULL)
@@ -234,17 +235,30 @@
 			ltmp, msg_id);
 #endif SYSLOG
 	if (cp == NULL) return(NULL); /* this article has expired */
-	tmp = cp+1;
 
-	if ((cp = index(tmp, ' ')) != NULL)
+	if (group_name) {
+		end = cp;
+		do {
+			if ((end = index(start = end+1, ' ')) != NULL)
+				*end = '\0';
+
+			if ((cp = index(start, '/')) != NULL) {
+				*cp = '\0';
+				if (!strcmp(start, group_name))
+					group_artnum = atol(cp+1);
+				*cp = '/';
+			}
+		} while (!group_artnum && end != NULL);
+	}
+	else if ((cp = index(start = cp+1, ' ')) != NULL)
 		*cp = '\0';
-	
-	while ((cp = index(tmp, '.')) != NULL)
+
+	while ((cp = index(start, '.')) != NULL)
 		*cp = '/';
 
 	(void) strcpy(path, spooldir);
 	(void) strcat(path, "/");
-	(void) strcat(path, tmp);
+	(void) strcat(path, start);
 #ifdef USGHIST
 	(void) fclose(hfp);
 #endif
@@ -481,9 +495,10 @@
 
 close_crnt()
 {
-	if (art_fp != NULL)
+	if (art_fp != NULL) {
 		(void) fclose(art_fp);
-	art_fp = NULL;
+		art_fp = NULL;
+	}
 }
 
 
Index:server/newgroups.c
@@ -23,6 +23,7 @@
 	int		i;
 	long		date;
 	register FILE	*date_fp;
+	char		*reqlist[2];
 
 	if (argc < 3) {
 printf("%d Usage: NEWGROUPS yymmdd hhmmss [\"GMT\"] [<distributions>].\r\n",
@@ -96,6 +97,22 @@
 #endif
 
 		if (distcount == 0) {
+#ifdef ACTIVE_TIMES_FILE
+			reqlist[0] = line;
+#else
+			reqlist[0] = cp + 1;
+#endif
+			reqlist[1] = NULL;
+
+			if (ngpermcount) {
+			    if (ngmatch(s1strneql, ALLBUT,
+			        ngpermlist, ngpermcount, reqlist, 1) == 0) {
+			         continue;
+			    }
+			} else if (ALLBUT == 0) {
+			    continue;
+			}
+
 #ifdef ACTIVE_TIMES_FILE
 			putline(line);
 #else
Index:server/serve.c
@@ -24,9 +24,9 @@
 #include "timer.h"
 #endif
 
-extern	int	ahbs(), group(), help(), ihave();
+extern	int	ahbs(), dodate(), group(), help(), ihave();
 extern	int	list(), newgroups(), newnews(), nextlast(), post();
-extern	int	slave(), stat(), xhdr();
+extern	int	slave(), stat(), listgroup(), xhdr(), doxover(), xthread();
 
 extern int errno;
 
@@ -33,6 +33,9 @@
 #ifdef AUTH
 extern	int	doauth();
 #endif AUTH
+#ifdef XOVER
+extern	int	doxover();
+#endif
 
 static struct cmdent {
 	char	*cmd_name;
@@ -46,6 +49,7 @@
 #endif AUTH
 	"article",	0,	ahbs,
 	"body",		0,	ahbs,
+	"date",		0,	dodate,
 	"group",	0,	group,
 	"head",		0,	ahbs,
 	"help",		0,	help,
@@ -52,6 +56,9 @@
 	"ihave",	1,	ihave,
 	"last",		0,	nextlast,
 	"list",		0,	list,
+#ifdef LISTGROUP
+	"listgroup",	0,	listgroup,
+#endif LISTGROUP
 	"newgroups",	0,	newgroups,
 	"newnews",	0,	newnews,
 	"next",		0,	nextlast,
@@ -61,6 +68,12 @@
 #ifdef XHDR
 	"xhdr",		0,	xhdr,
 #endif XHDR
+#ifdef XOVER
+	"xover",	0,	doxover,
+#endif
+#ifdef XTHREAD
+	"xthread",	0,	xthread,
+#endif
 };
 #define NUMCMDS (sizeof(cmdtbl) / sizeof(struct cmdent))
 
Index:server/time.c
@@ -14,6 +14,28 @@
 #include <sys/time.h>
 #endif not USG
 
+dodate(ac, av)
+	int	ac;
+	char	*av[];
+{
+	struct tm	*gmt;
+#ifdef USG
+	time_t		now;
+
+	(void) time(&now);
+	gmt = gmtime(&now);
+#else /* not USG */
+	struct timeval	now;
+
+	(void) gettimeofday(&now, (struct timezone *)NULL);
+	gmt = gmtime(&now.tv_sec);
+#endif /* not USG */
+	printf("%d %04.4d%02.2d%02.2d%02.2d%02.2d%02.2d\r\n", INF_DATE,
+		gmt->tm_year + 1900, gmt->tm_mon + 1, gmt->tm_mday,
+		gmt->tm_hour, gmt->tm_min, gmt->tm_sec);
+	(void) fflush(stdout);
+}
+
 /*
  * dtol -- convert date to long integer.  This is not implicitly
  * local time, or any other kind of time, for that matter.  If you
Index:server/xthread.c
@@ -0,0 +1,149 @@
+/* This file (and only this file - not the entire nntp distribution) is
+ * hereby placed in the public domain.  Use it as you see fit, but if you
+ * manage to find some wonderful use for this code elsewhere, it would be
+ * nice to receive mention for it.
+ *
+ * - Tim Iverson
+ *   iverson@xstor.com -/- uunet!xstor!iverson
+ *   3/28/91
+ *   modified by Wayne Davison (davison@borland.com) to work with trn 2.0
+ *   10/6/91
+ */
+
+#include "common.h"
+
+#ifdef XTHREAD
+
+# ifdef __GNUC__
+#  define alloca __builtin_alloca
+# endif
+
+char *thread_name();
+
+/* Usage: XTHREAD [DBINIT|THREAD]
+ *
+ * DBINIT	dump the contents of the db.init file to stdout
+ * THREAD	dump the contents of the thread file for the current
+ *		newsgroup to stdout (default if no argument).
+ *
+ * N.B. These two files are not ascii and no attempt is made at converting
+ *	native byte size to any type of standard whatsoever.  This leaves it
+ *	up to the receiver (i.e. trn) to translate the data based on db.init.
+ *
+ * This command is not documented in rfc977.
+ */
+
+void
+xthread(argc, argv)
+int	argc;
+char	*argv[];
+{
+	register FILE	*fp;
+	struct stat	s;
+	char		*buf, *file, *what;
+
+	/* can't transfer threads, only read 'em */
+	if (!canread)
+	{
+		printf("%d You only have permission to transfer, sorry.\r\n",
+			ERR_ACCESS);
+		(void) fflush(stdout);
+		return;
+	}
+
+	/* "parse" the argument */
+	if (argc == 2 && !strcasecmp(argv[1], "dbinit"))
+	{
+		file = thread_name("*******");
+		what = "db.init";
+		strcpy(index(file, '*'), what);
+	}
+	else if (argc == 1 || argc == 2 && !strcasecmp(argv[1], "thread"))
+	{
+		if (!threadfile)
+		{
+			printf("%d You are not currently in a newsgroup.\r\n",
+				ERR_NCING);
+			(void) fflush(stdout);
+			return;
+		}
+		file = threadfile;
+		what = "thread";
+	}
+	else
+	{
+		printf("%d Usage: XTHREAD [DBINIT|THREAD]\r\n", ERR_CMDSYN);
+		(void) fflush(stdout);
+		return;
+	}
+
+	/* try to open the file to be transfered */
+	if (!(fp = fopen(file, "r")))
+	{
+		printf("%d %s file is not available.\r\n", ERR_FAULT, what);
+		(void) fflush(stdout);
+#ifdef SYSLOG
+		if (!strcmp(what, "db.init"))
+		    syslog(LOG_ERR, "xthread: fopen %s: %m", file);
+#endif
+		return;
+	}
+
+	/* tell 'em how much binary data is coming down the pike */
+	fstat(fileno(fp), &s);
+	printf("%d %u bytes of %s file follows verbatim (binary!)\r\n",
+		OK_BIN, s.st_size, what);
+
+	/* copy the file verbatim to stdout */
+#ifdef __GNUC__
+	if (buf = alloca(s.st_size))
+	{
+		/* ah-so! got lotsa memoree */
+		read(fileno(fp), buf, s.st_size);
+		fwrite(buf, s.st_size, 1, stdout);
+	}
+	else
+#endif
+	{
+		int		bytes;
+		char		buf[BUFSIZ];
+
+		while (bytes = fread(buf, 1, sizeof buf, fp))
+			fwrite(buf, bytes, 1, stdout);
+	}
+
+	fputs("\r\n.\r\n", stdout);
+	fflush(stdout);
+	fclose(fp);
+}
+
+/* Change a newsgroup name into the name of the thread data file.  We
+** subsitute any '.'s in the group name into '/'s (unless LONG_THREAD_NAMES
+** is defined), prepend the path, and append the '/.thread' (or '.th') on to
+** the end.
+*/
+char *
+thread_name(group)
+char *group;
+{
+	static char name_buff[MAXPATHLEN];
+#ifdef LONG_THREAD_NAMES
+	char group_buff[512];
+	register char *ptr;
+
+	strcpy(group_buff, group);
+	ptr = group = group_buff;
+	while ((ptr = index(ptr, '/'))) {
+		*ptr = '.';
+	}
+#endif
+#ifdef SUFFIX
+	sprintf(name_buff, "%s/%s%s", threaddir, group, SUFFIX);
+#else
+	sprintf(name_buff, "%s/%s", threaddir, group);
+#endif
+
+	return name_buff;
+}
+
+#endif /* not XTHREAD */
---8<------8<------8<------8<---cut here--->8------>8------>8------>8---
