/*
 * A program to create a manifest (shiping list) that is a shell script
 * to return a Unix file name to it's original state after it has been
 * clobbered by MSDOS's file name restrictions.
 *
 *	This code also used in arc, mtools, and pcomm
 */

#ifdef BSD
#define strrchr rindex
#endif /* BSD */

#include <stdio.h>
#include <ctype.h>

main(argc, argv)
int argc;
char *argv[];
{
	int i;
	char *name, *new_name, *dos_name(), *strrchr();
	void exit();

	if (argc == 1) {
		fprintf(stderr, "Usage: mkmanifest <list-of-files>\n");
		exit(1);
	}

	for (i=1; i<argc; i++) {
					/* zap the leading path */
		if (name = strrchr(argv[i], '/'))
			name++;
		else
			name = argv[i];
					/* create new name */
		new_name = dos_name(name);

		if (strcmp(new_name, name))
			printf("mv %s %s\n", new_name, name);
	}
	exit(0);
}

char *
dos_name(name)
char *name;
{
	static char *dev[9] = {"con", "aux", "com1", "com2", "lpt1", "prn",
	"lpt2", "lpt3", "nul"};
	char *s, *temp, *ext, *strcpy(), *strpbrk(), *strcat();
	char buf[15];
	int i, dot;
	static char ans[13];

	strcpy(buf, name);
	temp = buf;
					/* separate the name from extention */
	ext = "";
	dot = 0;
	for (i=strlen(buf)-1; i>=0; i--) {
		if (buf[i] == '.' && !dot) {
			dot = 1;
			buf[i] = '\0';
			ext = &buf[i+1];
		}
		if (isupper(buf[i]))
			buf[i] = tolower(buf[i]);
	}
					/* if no name */
	if (*temp == '\0')
		temp = "x";
					/* if name is a device */
	for (i=0; i<9; i++) {
		if (!strcmp(temp, dev[i])) 
			*temp = 'x';
	}
					/* name too long? */
	if (strlen(temp) > 8)
		*(temp+8) = '\0';
					/* extention too long? */
	if (strlen(ext) > 3)
		*(ext+3) = '\0';
					/* illegal characters? */
	while (s = strpbrk(temp, "^+=/[]:',?*\\<>|\". "))
		*s = 'x';

	while (s = strpbrk(ext, "^+=/[]:',?*\\<>|\". "))
		*s = 'x';

	strcpy(ans, temp);
	if (*ext) {
		strcat(ans, ".");
		strcat(ans, ext);
	}
	return(ans);
}

#ifdef BSD
/*
 * Return ptr to first occurrence of any character from `brkset'
 * in the character string `string'; NULL if none exists.
 */

char *
strpbrk(string, brkset)
register char *string, *brkset;
{
	register char *p;

	if (!string || !brkset)
		return(0);
	do {
		for (p = brkset; *p != '\0' && *p != *string; ++p)
			;
		if (*p != '\0')
			return(string);
	}
	while (*string++);
	return(0);
}
#endif /* BSD */
