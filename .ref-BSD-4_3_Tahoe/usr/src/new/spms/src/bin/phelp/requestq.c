/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */
#include <ctype.h>
#include <stdio.h>
#include "macro.h"
#include "null.h"
#include "path.h"
#include "phelp.h"
#include "slist.h"
#include "spms.h"
#include "system.h"
#include "yesno.h"

char *helpstack[MAXHELPLEVEL];		/* stack of topics and subtopics */
extern int HELPLEVEL;			/* current level in help hierarchy */
static SLIST *requestqueue = NULL;	/* queue of topics to process */

/*
 * gettopic() parses a help request from standard input into tokens and
 * adds the tokens to a previously purged request queue.
 */
void
gettopic()
{
	char *gets();			/* get a line from standard input */
	char *getword();		/* get next word on line from stdin */
	char request[REQUESTSIZE];	/* input line buffer */
	char *rp;			/* request buffer pointer */
	char *topic;			/* topic word pointer */
	void initrq();			/* initialize request queue */
	void puttopic();		/* add topic to request queue */
	void purgrq();			/* purge request queue */
	
	if (requestqueue == NULL)
		initrq();
	else
		purgrq();
	if ((rp = gets(request)) != NULL)
		{
		do	{
			rp = getword(&topic, rp);
			if (*topic != '\0')
				puttopic(topic);
			} while (*rp != '\0');
		}
}



/*
 * getword() gets the next word on a line from standard input and returns
 * a pointer to the next word.
 */
char *
getword(word, bp)
	char **word;			/* receiving pointer for word */
	register char *bp;		/* buffer pointer */
{

	for (; *bp != '\0' && isspace(*bp); bp++)
		continue;
	*word = bp;
	for (; *bp != '\0' && !isspace(*bp); bp++)
		continue;
	if (*bp != '\0')
		*bp++ = '\0';
	return(bp);
}



/*
 * printnotopics() prints a "no topics" error message.
 */
void
printnotopics(ppathname)
	char *ppathname;		/* project pathname */
{
	if (HELPLEVEL < 1)
		{
		if (EQUAL(ppathname, CURPROJECT))
			{
			warn("There is no help available for this project");
			}
		else	{
			warn("There is no help available for project %s",
			     ppathname);
			}
		}
	else	{
		warn("%s doesn't have any subtopics", helpstack[HELPLEVEL-1]);
		}
}



/*
 * processtopic() interprets the request queue and controls the help
 * stack.
 */
processtopic()
{
	extern char PHELP_CMD[];	/* help command file pathname */
	extern char PHELP_HELP[];	/* help introduction file pathname */
	char *getcwp();			/* get current working project */
	char *mkndir();			/* make a directory name */
	char *ppathname;		/* project pathname */
	char *slget();			/* get a list item */
	char *topic;			/* next topic from request queue */
	char *strsav();			/* save a string somewhere */
	int chproject();		/* change project */
	int mkindex();			/* make topic index */
	int nrq;			/* no. items in request queue */
	int printtopic();		/* print topic file and index */
	int status = 0;			/* return status */
	void printindex();		/* print topic index */
	void printnotopics();		/* print "no topics" error message */
	void slrewind();		/* rewind list */

	if ((nrq = SLNUM(requestqueue)) < 1)
		{
		/* go up one level */
		if (HELPLEVEL > 0)
			{
			chdir(PARENTDIR);
			free(helpstack[--HELPLEVEL]);
			}
		else	{
			fprintf(stderr, "You are at the top level of help topics. ");
			printtopic(PHELP_CMD, CURDIR);
			}

		}
	else	{
		slrewind(requestqueue);

		topic = slget(requestqueue);
		if (EQUAL(topic, "help"))
			{
			/* print help introduction */
			printtopic(PHELP_HELP, CURDIR);
			return(status);
			}
		else if (EQUAL(topic, "?"))
			{
			/* print command summary */
			printtopic(PHELP_CMD, CURDIR);
			return(status);
			}
		else if (EQUAL(topic, "q"))
			{
			/* quit phelp */
			exit(status);
			}
		else if (EQUAL(topic, "~"))
			{
			/* return to top level help directory */
			for (; HELPLEVEL > 0; HELPLEVEL--)
				{
				chdir(PARENTDIR);
				free(helpstack[HELPLEVEL-1]);
				}
			if (mkindex(CURDIR) == YES)
				printindex(stdout);
			return(status);
			}
		else if (EQUAL(topic, "P"))
			{
			/* change project */
			if (nrq < 2)
				{
				warn("Missing project name");
				status = 1;
				}
			else if (nrq > 2)
				{
				warn("Too many arguments");
				status = 1;
				}
			else	{
				ppathname = slget(requestqueue);
				if (chghelp(ppathname) == NO)
					{
					status = 1;
					}
				else if (mkindex(CURDIR) == NO)
					{
					printnotopics(ppathname);
					status = 1;
					}
				else	{
					printindex(stdout);
					}
				}
			return(status);
			}
		else	{
			/* travel to the requested level */
			while (nrq-- > 1)
				{
				if (CHDIR(mkndir(topic)))
					helpstack[HELPLEVEL++] = strsav(topic);
				else if (FILEXIST(topic))
					{
					warn("%s doesn't have any subtopics", topic);
					status = 1;
					}
				else if (mkindex(CURDIR) == NO)
					{
					printnotopics(CURPROJECT);
					status = 1;
					}
				else	{
					warn("Can't find %s", topic);
					printindex(stderr);
					status = 1;
					}
				if (status != 0)
					break;
				topic = slget(requestqueue);
				}
			}

		if (status != 0)
			return(status);

		/* process help topic request */
		if (EQUAL(topic, "index"))
			{
			/* print topic index */
			if (mkindex(CURDIR) == YES)
				printindex(stdout);
			else	{
				printnotopics(CURPROJECT);
				if (HELPLEVEL > 0)
					{
					chdir(PARENTDIR);
					free(helpstack[--HELPLEVEL]);
					}
				status = 1;
				}
			}
		else	{
			helpstack[HELPLEVEL++] = strsav(topic);
			switch (printtopic(topic, mkndir(topic)))
				{
				case 0:		/* help file and subtopics */
				case 1:		/* subtopics only */
					chdir(mkndir(topic));
					break;
				case 2:		/* help file, no subtopics */
					free(helpstack[--HELPLEVEL]);
					break;
				case 3:		/* no help available at all */
					free(helpstack[--HELPLEVEL]);
						/* is there any help at */
						/* current level? */
					if (mkindex(CURDIR) == NO)
						{
						printnotopics(CURPROJECT);
						status = 1;
						}
					else	{
						warn("Sorry, %s is not available",
						     topic);
						printindex(stderr);
						status = 1;
						}
					break;
				}
			}
		}
	return(status);
}



/*
 * prompt() prints the stack of topics and subtopics to the current
 * level and prompts for the next command.
 */
void
prompt()
{
	int tsindex;			/* topic stack index */

	if (HELPLEVEL > 3)
		printf("-->");
	for (tsindex = MAX(0, HELPLEVEL-3); tsindex < HELPLEVEL; tsindex++)
		printf("%s-->", helpstack[tsindex]);
	printf("??? ");
}



/*
 * puttopic() adds a help topic to the end of the request queue.
 */
void
puttopic(topic)
	char *topic;			/* topic string */
{
	char *slappend();		/* append item to list */

	if (requestqueue == NULL)
		initrq();
	slappend(topic, requestqueue);
}



/*
 * initrq() initializes the request queue.
 */
void
initrq()
{
	SLIST *slinit();		/* initialize list */

	requestqueue = slinit();
}



/*
 * purgrq() empties the request queue.
 */
void
purgrq()
{
	int slpop();			/* pop items off list */

	while (slpop(CNULL, 0, requestqueue) == YES)
		continue;
}
