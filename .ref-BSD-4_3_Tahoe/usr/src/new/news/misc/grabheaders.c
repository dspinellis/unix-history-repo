#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>

#define HISTORY	"/usr/new/lib/news/history"
#define SPOOL	"/usr/spool/news"

#include <sys/types.h>
#include <sys/timeb.h>
#include <ctype.h>
#include <sys/time.h>

#define	NULL	0
#define daysec (24L*60L*60L)

main()
{
	FILE *Hfile, *Afile;
	char buffer[BUFSIZ], datestr[BUFSIZ];
	char *index();
	struct stat stbuf;
	struct timeb now;
	long t;

	Hfile = fopen(HISTORY, "r");
	if (Hfile == NULL) {
		perror(HISTORY);
		exit(1);
	}

	if (chdir(SPOOL) < 0) {
		perror(SPOOL);
		exit(1);
	}

	(void) ftime(&now);

	while (fgets(buffer, BUFSIZ, Hfile) != NULL) {
		register char *p, *file;

		p = index(buffer, '\t');
		if (p == NULL)
			continue;
		file = index(p+1, '\t');
		if (file == NULL || file[1] == '\n')
			continue;
		*file = '\0';
		t = getdate(p, &now);
		if ( (t+daysec*14L) < now.time)
			continue;
		strcpy(datestr, p);
		p = file;
		while (*++p != ' ' && *p != '\n')
			if (*p == '.')
				*p = '/';
		*p = '\0';
		file++;
		if (       strncmp(file, "net", 3)  && strncmp(file, "mod", 3)
			&& strncmp(file, "comp", 4) && strncmp(file, "sci", 3)
			&& strncmp(file, "news", 4) && strncmp(file, "rec", 3)
			&& strncmp(file, "talk", 4) && strncmp(file, "misc", 4)
			&& strncmp(file, "soc", 3)
			)
			continue;
		Afile = fopen(file, "r");
		if (Afile == NULL)
			continue;
		while (fgets(buffer, BUFSIZ, Afile) != NULL &&
		    buffer[0] != '\n') {
		if (strncmp(buffer, "From: ", 5) == 0) {
			register char *cp = index(buffer, '@');
			if (cp)
				while (*++cp && *cp != '.' && *cp != ' ')
					if (isupper(*cp))
						*cp = tolower(*cp);
				cp--;
				while (*++cp && *cp != ' ')
					if (islower(*cp))
						*cp = toupper(*cp);
		    }
		    fputs(buffer, stdout);
		}
		fstat(fileno(Afile), &stbuf);
		printf("Date-Received: %s\n", datestr);
		printf("Bytes: %ld\n\n", stbuf.st_size - ftell(Afile));
		fclose(Afile);
	}
	printf("\n");
}
