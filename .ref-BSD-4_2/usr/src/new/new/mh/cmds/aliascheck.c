#include <stdio.h>
#include <ctype.h>
#include <grp.h>
#include <sys/param.h>
#include <dir.h>

char    *AliasFile =    "/usr/local/lib/MailAliases";
char    *Password  =    "/etc/passwd";
char    *MailDir   =    "/usr/spool/mail";

char *termptr;

struct  mailname {
	struct mailname *m_next;
	char            *m_name;
	int              m_seen;
}  users, bad;


char *parse(ptr, buf)
register char *ptr;
char *buf;
{
	register char *cp;

	cp = buf;
	while(isspace(*ptr) || *ptr == ',' || *ptr == ':')
		ptr++;
	while(isalnum(*ptr) || *ptr == '/' || *ptr == '-' || *ptr == '.')
		*cp++ = *ptr++;
	if(cp == buf) {
		switch(*ptr) {
		case '<':
		case '*':
		case '=':
			*cp++ = *ptr++;
		}
	}
	*cp = 0;
	if(cp == buf)
		return 0;
	termptr = ptr;
	return buf;
}


char   *mail = 0;
FILE   *out;
int     donecd = 0;

main(argc, argv)
	int argc;
	char *argv[];
{
	register char *cp, *pp, **cpp;
	register struct mailname *lp;
	register struct group *gp;
	struct direct *dir;
	DIR *dirp;
	char line[256], pbuf[64];
	FILE *a;

	if(argc == 3 && strcmp(argv[1], "-mail") == 0)
		mail = argv[2];
	if(!mail)
		out = stdout;
	if((a = fopen(Password, "r")) == NULL) {
		om();
		fprintf(out, "Can't open password file ");
		perror(Password);
		done(1);
	}
	while(fgets(line, sizeof line, a)) {
		if(line[0] == '\n' || line[0] == ';')
			continue;
		cp = parse(line, pbuf);
		add(cp, &users);
	}
	fclose(a);
	if((a = fopen(AliasFile, "r")) == NULL) {
		om();
		fprintf(out, "Can't open alias file: %s\n", AliasFile);
		donecd = 1;
	} else {
		while(fgets(line, sizeof line, a)) {
			if(line[0] == '\n' || line[0] == ';')
				continue;
			cp = parse(line, pbuf);
			if(check(cp, 0)) {
				add(line, &bad);
				donecd = 1;
			}
		}
		fclose(a);
		if(donecd < 1) {
			if(out)
				fprintf(out, "No Alias Inconsistencies.\n");
		} else {
			om();
			fprintf(out, "%s :: %s Collisions:\n",
				Password, AliasFile);
			fprintf(out, "Colliding alias lines:\n");
			for(lp = bad.m_next; lp; lp = lp->m_next)
				fprintf(out, "%s", lp->m_name);
			donecd = 1;
		}
	}
	while(gp = getgrent()) {
		for(cpp = gp->gr_mem; *cpp; cpp++)
			if(!check(*cpp, 1)) {
				om();
				fprintf(out, "Group: %s--User: %s not in /etc/passwd\n",
					gp->gr_name, *cpp);
				donecd = 2;
			}
	}
	if(donecd < 2 && out)
		fprintf(out, "No extraneous group entries.\n");
#ifdef RAND
	for(lp = users.m_next; lp; lp = lp->m_next)
		if(lp->m_seen == 0) {
			om();
			fprintf(out, "User: %s not in a group.\n", lp->m_name);
			donecd = 3;
		}
	if(donecd < 3 && out)
		fprintf(out, "No Users not in any group.\n");
#endif
	if ((dirp = opendir(MailDir)) == 0) {
		om();
		fprintf(out, "Can't open mail directory: %s\n", MailDir);
		donecd = 4;
	} else {
		(void) readdir(dirp);		/* skip . */
		(void) readdir(dirp);		/* skip .. */
		while (dir = readdir(dirp)) {
			if (!check(dir->d_name, 0)) {
				om();
				fprintf(out, "Mail drop: %s--Nonexistent user.\n",
					dir->d_name);
				donecd = 4;
			}
		}
		closedir(dirp);
	}
	if(donecd < 4 && out)
		fprintf(out, "No Extra mail drops.\n");

	done(donecd);
}


add(name, list)
char *name;
struct mailname *list;
{
	register struct mailname *mp;
	char *getcpy();

	for(mp = list; mp->m_next; mp = mp->m_next)
		;
	mp->m_next = (struct mailname *) malloc(sizeof *mp->m_next);
	mp = mp->m_next;
	mp->m_next = 0;
	mp->m_name = getcpy(name);
}

check(name, mark)
	char *name;
	int mark;
{
	register struct mailname *mp;

	for(mp = users.m_next; mp; mp = mp->m_next)
		if(uleq(name, mp->m_name)) {
			if(mark)
				mp->m_seen = 1;
			return 1;
		}
	return 0;
}


om()
{
	int pipes[2], child;

	if(out)
		return;
	if(mail) {
		pipe(pipes);
		out = fdopen(pipes[1], "w");
		if((child = fork()) == -1) {
			fprintf(stderr, "Aliascheck: no forks!\n");
			done(1);
		}
		if(child == 0) {
			close(pipes[1]);
			close(0);
			dup(pipes[0]);
			close(pipes[0]);
			execl("/bin/mail", "mail", mail, 0);
			execl("/usr/bin/mail", "mail", mail, 0);
			perror("mail");
			done(1);
		}
		fprintf(out, "Aliascheck: ");
	}
}
