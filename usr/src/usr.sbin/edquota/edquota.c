#ifndef lint
static char sccsid[] = "@(#)edquota.c	4.1 (Berkeley, from Melbourne) %G%";
#endif

/*
 * Disk quota editor.
 */
#include <stdio.h>
#include <signal.h>
#include <errno.h>
#include <pwd.h>
#include <ctype.h>
#include <fstab.h>

#include <sys/param.h>
#define	QUOTA
#include <sys/quota.h>
#include <sys/stat.h>
#include <sys/file.h>

#ifdef MELBOURNE
#define	DEFEDITOR	"/bin/ed"
#else
#define	DEFEDITOR	"/usr/ucb/vi"
#endif

struct	dquot dq[NMOUNT];
struct	dquot odq[NMOUNT];
char	dqf[NMOUNT][MAXPATHLEN + 1];
char	odqf[NMOUNT][MAXPATHLEN + 1];

char	tmpfil[] = "/tmp/EdP.aXXXXX";
char	*arg0;
char	*getenv();

main(argc, argv)
	char **argv;
{

	mktemp(tmpfil);
	close(creat(tmpfil, 0600));
	chown(tmpfil, getuid(), getgid());
	arg0 = *argv++;
	if (argc < 2) {
		fprintf(stderr, "Usage: %s username ...\n", arg0);
		unlink(tmpfil);
		exit(1);
	}
	--argc;
	if (getuid()) {
		fprintf(stderr, "%s: permission denied\n", arg0);
		unlink(tmpfil);
		exit(1);
	}
	while (--argc >= 0)
		doedit(*argv++);
	unlink(tmpfil);
	exit(0);
}

doedit(name)
	register char *name;
{
	register uid;
	register struct passwd *pw;

	if (alldigits(name))
		uid = atoi(name);
	else if (pw = getpwnam(name))
		uid = pw->pw_uid;
	else {
		fprintf(stderr, "%s: no such user\n");
		sleep(1);
		return;
	}
	getprivs(uid);
	if (editit())
		putprivs(uid);
}

editit()
{
	register pid, xpid;
	int stat;

	sighold(SIGINT);
	sighold(SIGQUIT);
	sighold(SIGHUP);

 top:
	if ((pid = fork()) < 0) {
		extern errno;

		if (errno == EPROCLIM) {
			fprintf(stderr, "You have too many processes\n");
			return(0);
		}
		if (errno == EAGAIN) {
			sleep(1);
			goto top;
		}
		perror("fork");
		return (0);
	}
	if (pid == 0) {
		register char *ed;

		sigrelse(SIGINT);
		sigrelse(SIGQUIT);
		sigrelse(SIGHUP);
		setgid(getgid());
		setuid(getuid());

		if ((ed = getenv("EDITOR")) == (char *)0)
			ed = DEFEDITOR;
		execlp(ed, ed, tmpfil, 0);
		perror(ed);
		exit(1);
	}
	while ((xpid = wait(&stat)) >= 0)
		if (xpid == pid)
			break;
	sigrelse(SIGINT);
	sigrelse(SIGQUIT);
	sigrelse(SIGHUP);
	return (!stat);
}

getprivs(uid)
	register uid;
{
	register i;
	FILE *fd;

	getdiscq(uid, dq, dqf);
	for (i = 0; i < NMOUNT; i++) {
		odq[i] = dq[i];
		strcpy(odqf[i], dqf[i]);
	}
	if ((fd = fopen(tmpfil, "w")) == NULL) {
		fprintf(stderr, "edquota: ");
		perror(tmpfil);
		exit(1);
	}
	for (i = 0; i < NMOUNT; i++) {
		if (*dqf[i] == '\0')
			continue;
		fprintf(fd,
"fs %s blocks (soft = %d, hard = %d) inodes (soft = %d, hard = %d)\n"
			, dqf[i]
			, dq[i].dq_bsoftlimit
			, dq[i].dq_bhardlimit
			, dq[i].dq_isoftlimit
			, dq[i].dq_ihardlimit
		);
	}
	fclose(fd);
}

putprivs(uid)
	register uid;
{
	register i, j;
	int n;
	FILE *fd;
	char line[BUFSIZ];

	fd = fopen(tmpfil, "r");
	if (fd == NULL) {
		fprintf(stderr, "Can't re-read temp file!!\n");
		return;
	}
	for (i = 0; i < NMOUNT; i++) {
		char *cp, *dp, *next();

		if (fgets(line, sizeof (line), fd) == NULL)
			break;
		cp = next(line, " \t");
		if (cp == NULL)
			break;
		*cp++ = '\0';
		while (*cp && *cp == '\t' && *cp == ' ')
			cp++;
		dp = cp, cp = next(cp, " \t");
		if (cp == NULL)
			break;
		*cp++ = '\0';
		while (*cp && *cp == '\t' && *cp == ' ')
			cp++;
		strcpy(dqf[i], dp);
		n = sscanf(cp,
"blocks (soft = %d, hard = %d) inodes (soft = %hd, hard = %hd)\n"
			, &dq[i].dq_bsoftlimit
			, &dq[i].dq_bhardlimit
			, &dq[i].dq_isoftlimit
			, &dq[i].dq_ihardlimit
		);
		if (n != 4)
			break;
	}
	fclose(fd);
	n = i;
	for (i = 0; i < n; i++) {
		if (*dqf[i] == '\0')
			break;
		for (j = 0; j < NMOUNT; j++) {
			if (strcmp(dqf[i], odqf[j]) == 0)
				break;
		}
		if (j >= NMOUNT)
			continue;
		*odqf[j] = '\0';
		if (dq[i].dq_isoftlimit == odq[j].dq_isoftlimit &&
		    dq[i].dq_ihardlimit == odq[j].dq_ihardlimit &&
		    dq[i].dq_bsoftlimit == odq[j].dq_bsoftlimit &&
		    dq[i].dq_bhardlimit == odq[j].dq_bhardlimit) {
			for (j = i; j < 15; j++) {
				dq[j] = dq[j+1];
				strcpy(dqf[j], dqf[j+1]);
			}
			*dqf[j] = '\0';
			i--;
			continue;
		}
		/*
		 * This isn't really good enough, it is quite likely
		 * to have changed while we have been away editing,
		 * but it's not important enough to worry about at
		 * the minute.
		 */
		dq[i].dq_curblocks = odq[j].dq_curblocks;
		dq[i].dq_curinodes = odq[j].dq_curinodes;
		/*
		 * If we've upped the inode or disk block limits
		 * and the guy is out of warnings, reinitialize.
		 */
		if (dq[i].dq_bsoftlimit > odq[j].dq_bsoftlimit &&
		    dq[i].dq_bwarn == 0)
			dq[i].dq_bwarn = MAX_DQ_WARN;
		if (dq[i].dq_isoftlimit > odq[j].dq_isoftlimit &&
		    dq[i].dq_iwarn == 0)
			dq[i].dq_iwarn = MAX_IQ_WARN;
	}
	if (i < NMOUNT) {
		for (j = 0; j < NMOUNT; j++) {
			if (*odqf[j] == '\0')
				continue;
			strcpy(dqf[i], odqf[j]);
			dq[i].dq_isoftlimit = 0;
			dq[i].dq_ihardlimit = 0;
			dq[i].dq_bsoftlimit = 0;
			dq[i].dq_bhardlimit = 0;
			/*
			 * Same applies as just above
			 * but matters not at all, as we are just
			 * turning quota'ing off for this filesys.
			 */
			dq[i].dq_curblocks = odq[j].dq_curblocks;
			dq[i].dq_curinodes = odq[j].dq_curinodes;
			if (++i >= NMOUNT)
				break;
		}
	}
	if (*dqf[0])
		putdiscq(uid, dq, dqf);
}

char *
next(cp, match)
	register char *cp;
	char *match;
{
	register char *dp;

	while (cp && *cp) {
		for (dp = match; dp && *dp; dp++)
			if (*dp == *cp)
				return (cp);
		cp++;
	}
	return ((char *)0);
}

alldigits(s)
	register char *s;
{
	register c;

	c = *s++;
	do {
		if (!isdigit(c))
			return (0);
	} while (c = *s++);
	return (1);
}

getdiscq(uid, dq, dqf)
	register uid;
	register struct dquot *dq;
	register char (*dqf)[MAXPATHLEN + 1];
{
	register struct fstab *fs;
	char qfname[MAXPATHLEN + 1];

	setfsent();
	while (fs = getfsent()) {
		struct	stat statb;
		struct	dqblk dqblk;
		dev_t	fsdev;

		if (stat(fs->fs_spec, &statb) < 0)
			continue;
		fsdev = statb.st_rdev;
		if (fs->fs_quotafile == 0 || *fs->fs_quotafile == '\0')
			continue;
		sprintf(qfname, "%s/%s", fs->fs_file, fs->fs_quotafile);
		if (stat(qfname, &statb) < 0 || statb.st_dev != fsdev)
			continue;
		if (quota(Q_GETDLIM, uid, fsdev, &dqblk) != 0) {
			register fd = open(qfname, FRDONLY);

			if (fd < 0)
				continue;
			lseek(fd, (long)(uid * sizeof dqblk), FSEEK_ABSOLUTE);
			if (read(fd, &dqblk, sizeof dqblk) != sizeof (dqblk)) {
				close(fd);
				continue;
			}
			close(fd);
#ifdef notdef
			if (dqblk.dqb_isoftlimit == 0 && dqblk.dqb_bsoftlimit == 0)
				continue;
#endif
		}
		dq->dq_dqb = dqblk;
		dq->dq_dev = fsdev;
		strcpy(*dqf, fs->fs_file);
		dq++, dqf++;
	}
	endfsent();
	**dqf = '\0';
}

putdiscq(uid, dq, dqf)
	register uid;
	register struct dquot *dq;
	register char (*dqf)[MAXPATHLEN + 1];
{
	register fd, cnt;
	struct stat sb;
	struct fstab *fs;

	cnt = 0;
	for (cnt = 0; ++cnt <= NMOUNT && **dqf; dq++, dqf++) {
		fs = getfsfile(*dqf);
		if (fs == NULL)
			goto nofile;
		strcat(*dqf, fs->fs_quotafile);
		if (stat(*dqf, &sb) >= 0 && (fd = open(*dqf, 1)) >= 0) {
			lseek(fd, (long)uid * (long)sizeof (struct dqblk), 0);
			if (write(fd, &dq->dq_dqb, sizeof (struct dqblk)) !=
			    sizeof (struct dqblk)) {
				fprintf(stderr, "edquota: ");
				perror(*dqf);
			}
			close(fd);
		}
nofile:
		quota(Q_SETDLIM, uid, sb.st_dev, &dq->dq_dqb);
	}
}
