/* $RCSfile: popen.c,v $$Revision: 4.0.1.1 $$Date: 91/06/07 11:22:52 $
 *
 *    (C) Copyright 1988, 1990 Diomidis Spinellis.
 *
 *    You may distribute under the terms of either the GNU General Public
 *    License or the Artistic License, as specified in the README file.
 *
 * $Log:	popen.c,v $
 * Revision 4.0.1.1  91/06/07  11:22:52  lwall
 * patch4: new copyright notice
 * 
 * Revision 4.0  91/03/20  01:34:50  lwall
 * 4.0 baseline.
 * 
 * Revision 3.0.1.2  90/08/09  04:04:42  lwall
 * patch19: various MSDOS and OS/2 patches folded in
 * 
 * Revision 3.0.1.1  90/03/27  16:11:57  lwall
 * patch16: MSDOS support
 * 
 * Revision 1.1  90/03/18  20:32:20  dds
 * Initial revision
 *
 */

/*
 * Popen and pclose for MS-DOS
 */

#include <stdlib.h>
#include <stdio.h>
#include <process.h>

/*
 * Possible actions on an popened file
 */
enum action {
	delete, 			/* Used for "r". Delete the tmp file */
	execute				/* Used for "w". Execute the command. */
};

/*
 * Linked list of things to do at the end of the program execution.
 */
static struct todo {
	FILE *f;			/* File we are working on (to fclose) */
	const char *name;		/* Name of the file (to unlink) */
	const char *command;		/* Command to execute */
	enum action what;		/* What to do (execute or delete) */
	struct todo *next;		/* Next structure */
} *todolist;


/* Clean up function */
static int close_pipes(void);

/*
 * Add a file f running the command command on file name to the list
 * of actions to be done at the end.  The action is specified in what.
 * Return -1 on failure, 0 if ok.
 */
static int
add(FILE *f, const char *command, const char *name, enum action what)
{
	struct todo    *p;

	if ((p = (struct todo *) malloc(sizeof(struct todo))) == NULL)
		return -1;
	p->f = f;
	p->command = command;
	p->name = name;
	p->what = what;
	p->next = todolist;
	todolist = p;
	return 0;
}

FILE *
mypopen(const char *command, const char *t)
{
	char buff[256];
	char *name;
	FILE *f;
	static init = 0;

	if (!init)
		if (onexit(close_pipes) == NULL)
			return NULL;
		else
			init++;

	if ((name = tempnam((char*)NULL, "pp")) == NULL)
		return NULL;

	switch (*t) {
	case 'r':
		sprintf(buff, "%s >%s", command, name);
		if (system(buff) || (f = fopen(name, "r")) == NULL) {
			free(name);
			return NULL;
		}
		if (add(f, command, name, delete)) {
			(void)fclose(f);
			(void)unlink(name);
			free(name);
			return NULL;
		}
		return f;
	case 'w':
		if ((f = fopen(name, "w")) == NULL) {
			free(name);
			return NULL;
		}
		if (add(f, command, name, execute)) {
			(void)fclose(f);
			(void)unlink(name);
			free(name);
			return NULL;
		}
		return f;
	default:
		free(name);
		return NULL;
	}
}

int
mypclose(FILE *f)
{
	struct todo *p, **prev;
	char buff[256];
	const char *name;
	int status;

	for (p = todolist, prev = &todolist; p; prev = &(p->next), p = p->next)
		if (p->f == f) {
			*prev = p->next;
			name = p->name;
			switch (p->what) {
			case delete:
				free(p);
				if (fclose(f) == EOF) {
					(void)unlink(name);
					status = EOF;
				} else if (unlink(name) < 0)
					status = EOF;
				else
					status = 0;
				free((void*)name);
				return status;
			case execute:
				(void)sprintf(buff, "%s <%s", p->command, p->name);
				free(p);
				if (fclose(f) == EOF) {
					(void)unlink(name);
					status = EOF;
				} else if (system(buff)) {
					(void)unlink(name);
					status = EOF;
				} else if (unlink(name) < 0)
					status = EOF;
				else
					status = 0;
				free((void*)name);
				return status;
			default:
				return EOF;
			}
		}
	return EOF;
}

/*
 * Clean up at the end.  Called by the onexit handler.
 */
static int
close_pipes(void)
{
	struct todo    *p;

	for (p = todolist; p; p = p->next)
		(void)mypclose(p->f);
	return 0;
}
