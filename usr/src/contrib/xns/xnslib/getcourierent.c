/*	$Header: getcourierent.c,v 2.0 85/11/21 07:22:07 jqj Exp $	*/

#include <stdio.h>
#include "courierdb.h"
#include <ctype.h>

/*
 *
 */
#ifdef CSERVICES
static char *COURIERDB = CSERVICES;
#else
static char *COURIERDB = "/usr/new/lib/xnscourier/Courierservices";
#endif
static FILE *courierdbf = NULL;
static char line[BUFSIZ+1];
static struct courierdbent service;
int _courier_stayopen;
static char *skipspace(), *skipitem();

setcourierdbent(f)
	int f;
{
	if (courierdbf != NULL)
		rewind(courierdbf);
	_courier_stayopen |= f;
}

endcourierdbent()
{
	if (courierdbf != NULL) {
		fclose(courierdbf);
		courierdbf = NULL;
	}
	_courier_stayopen = 0;
}

struct courierdbent *
getcourierdbent()
{
	char *p;
	register char *cp, c;

	if (courierdbf == NULL 
	    && (courierdbf = fopen(COURIERDB, "r" )) == NULL)
		return (NULL);

	do {
		if ((p = fgets(line, BUFSIZ, courierdbf)) == NULL)
			return (NULL);
		p = skipspace(p);
		cp = p;		/* end of whitespace */
		while ((c = *cp) != '\0' && c != '\n' && c != '#')
			cp++;
		*cp = '\0';	/* end of data */
	} while (*p == '\0');

	service.cr_programname = p;		/* a string */
	cp = skipitem(p);
	if (*cp != '\0') {
		*cp = '\0';
		cp = skipspace(++cp);
	}
	service.cr_programnumber = (unsigned long) atol(cp);	/* a long */
	cp = skipitem(cp);  cp = skipspace(cp);
	service.cr_version = (unsigned short) atoi(cp);		/* an int */
	cp = skipitem(cp);  cp = skipspace(cp);
	service.cr_description = (*cp) ? cp : (char*) 0;
	cp = skipitem(cp);
	if (*cp != '\0') {
		*cp = '\0';
		cp = skipspace(++cp);
	}
	service.cr_serverbin = (*cp) ? cp : (char*) 0;
	cp = skipitem(cp);
	if (*cp != '\0') {
		*cp = '\0';
		/* etc. for more fields */
	}
	return (&service);
}

static char * skipspace(p)
/* move the pointer past leading whitespace, returning the updated ptr */
	register char *p;
{
	register char c;
	while ((c = *p) == ' ' || c == '\t')
		p++;
	return(p);
}

static char* skipitem(p)
/* move the pointer, p, past non-whitespace */
	register char *p;
{
	register char c;
	while ((c = *p) && c != ' ' && c != '\t')
		p++;
	return(p);
}

setcourierdbfile(file)
	char *file;
{
	COURIERDB = file;
}

