/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */
#include <sys/types.h>
#include <sys/dir.h>
#include <stdio.h>
#include "null.h"
#include "phelp.h"
#include "slist.h"
#include "tree.h"
#include "yesno.h"

extern char *helpstack[MAXHELPLEVEL];	/* stack of topics and subtopics */
extern int HELPLEVEL;			/* current level in help hierarchy */
static SLIST *index = NULL;		/* index list */

/*
 * mkindex() reads topicdir and creates a list of topics. Topics may be
 * regular file names or subtopic directories (`.d' suffix) or both. The
 * topics are inserted into a binary tree (this eliminates duplicate
 * topic names) and the resulting tree is copied to a list for printing
 * purposes. Return integer NO if the directory can not be read or there
 * aren't any topics, otherwise YES.
 */
mkindex(topicdir)
	char *topicdir;			/* directory in which to find topics */
{
	char *strcpy();			/* string copy */
	char topic[MAXNAMLEN];		/* topic name buffer */
	DIR *dirp;			/* directory stream */
	DIR *opendir();			/* open directory stream */
	int status = YES;		/* return status */
	int treetolist();		/* copy tree to singly-linked list */
	SLIST *slinit();		/* initialize list */
	struct direct *dp;		/* directory entry pointer */
	struct direct *readdir();	/* read a directory entry */
	TREE *topicroot;		/* root of topic tree */
	TREE *tree();			/* insert into topic tree */
	TREE *treerm();			/* remove tree */
	void slrm();			/* remove list */

	if ((dirp = opendir(topicdir)) == NULL)
		return(NO);
	if (index != NULL)
		slrm(CNULL, index);
	topicroot = NULL;
	for (dp = readdir(dirp); dp != NULL; dp = readdir(dirp))
		if (*dp->d_name != '.')
			if (dp->d_namlen > 2 &&
			    (dp->d_name)[dp->d_namlen-1] == 'd' &&
			    (dp->d_name)[dp->d_namlen-2] == '.')
				{
				strcpy(topic, dp->d_name);
				topic[dp->d_namlen-2] = '\0';
				topicroot = tree(topicroot, topic);
				}
			else
				topicroot = tree(topicroot, dp->d_name);
	closedir(dirp);

	index = slinit();
	if (treetolist(topicroot) == NO)
		status = NO;
	treerm(topicroot, CNULL);
	if (SLNUM(index) < 1)
		status = NO;
	return(status);
}



/*
 * printindex() prints an index of topics.
 */
void
printindex(ofp)
	FILE *ofp;			/* output stream */
{
	char *slget();			/* get a list item */
	char *topic;			/* next topic from index list */
	int available_space;		/* amount of space left on title line */
	int colwidth;			/* topic column width */
	int ncol;			/* number of columns to be printed */ 
	int strlen();			/* string length */
	void slprint();			/* print list */
	void slrewind();		/* rewind list */

	colwidth = index->maxkey + MINIMUM_GAP;
	if (HELPLEVEL < 1)
		{
		fprintf(ofp, "\nHelp topics available:");
		available_space = MAXLINE - 22;
		}
	else	{
		fprintf(ofp, "\n%s subtopics:", helpstack[HELPLEVEL-1]);
		available_space = MAXLINE - strlen(helpstack[HELPLEVEL-1]) - 11;
		}
	if ((available_space/colwidth) > SLNUM(index))
		{			/* topics on title line if possible */
		slrewind(index);
		while ((topic = slget(index)) != NULL)
			fprintf(ofp, "  %s", topic);
		fprintf(ofp, "\n\n");
		}
	else	{
		fprintf(ofp, "\n\n");
		if (colwidth % TABSIZE)
			colwidth = TABSIZE * (colwidth/TABSIZE + 1);
		ncol = MAXLINE/colwidth;
		slprint(ncol, colwidth, YES, ofp, index);
		putc('\n', ofp);
		}
	fflush(ofp);
}



/*
 * treetolist() copies a binary tree to singly linked list index. Returns
 * YES if successful, otherwise NO.
 */
treetolist(p)
	TREE *p;			/* current tree node */
{
	char *slappend();		/* append key to list */

	if (p != NULL)
		{
		if (treetolist(p->left) == NO)
			return(NO);
		if (slappend(p->key, index) == NULL)
			return(NO);
		if (treetolist(p->right) == NO)
			return(NO);
		}
	return(YES);
}
