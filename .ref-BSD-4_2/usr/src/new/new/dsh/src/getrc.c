#include <stdio.h>

#define LEN 100

FILE *
getrcfile (file)
char	*file;
{
    char	path[LEN];
    FILE	*fp;

    if (*file != '/') {
	strcpy (path, getenv ("HOME"));
	strcat (path, "/");
	strcat (path, file);
	fp = fopen (path, "r");
    } else {
	fp = fopen (file, "r");
    }
    return (fp);
}

rckeymatch (line, rc)
char	*line;
char	*rc;
{
    while (*line == ' ' || *line == '\t')
	line++;
    while (*rc != 0) {
	if (*rc++ != *line++) 
	    return (0);
    }
    while (*line == ' ' || *line == '\t')
	line++;
    if (*line == '=')
	return (1);
    else
	return (0);
}

char *
rcval (line)
char	*line;
{
    while (*line++ != '=');
    while (*line == ' ' || *line == '\t')
	line++;
    return (line);
}

getintrc(file, rc, rvptr)
char	*file;
char	*rc;
char	*rvptr;
{
    char	line[LEN];
    FILE	*fp;

    fp = getrcfile (file);
    if (fp != NULL) {
	while (fgets(line, LEN, fp) != NULL) {
	    if (rckeymatch (line, rc)) {
		*rvptr = atoi (rcval(line));
		return (0);
	    }
	}
    }
    return (-1);
}

getstringrc(file, rc, rvptr)
char	*file;
char	*rc;
char	*rvptr;
{
    char	line[LEN];
    char	*p1;
    FILE	*fp;

    fp = getrcfile (file);
    if (fp != NULL) {
	while (fgets(line, LEN, fp) != NULL) {
	    if (rckeymatch (line, rc)) {
		p1 = rcval(line);
		while (*p1 != '\n' && *p1 != 0) 
		    *rvptr++ = *p1++;
		*rvptr = 0;
		return (0);
	    }
	}
    }
    return (-1);
}
