/*
 *
 * loop.c - loop execution
 *
 * This file is part of zsh, the Z shell.
 *
 * This software is Copyright 1992 by Paul Falstad
 *
 * Permission is hereby granted to copy, reproduce, redistribute or otherwise
 * use this software as long as: there is no monetary profit gained
 * specifically from the use or reproduction of this software, it is not
 * sold, rented, traded or otherwise marketed, and this copyright notice is
 * included prominently in any copy made. 
 *
 * The author make no claims as to the fitness or correctness of this software
 * for any use whatsoever, and it is provided as is. Any use of this software
 * is at the user's own risk. 
 *
 */

#include "zsh.h"

int execfor(cmd) /**/
Cmd cmd;
{
List list;
struct forcmd *node;
char *str;
Lklist args;
int cj = thisjob;

	loops++;
	exiting = 0;
	node = cmd->u.forcmd;
	args = cmd->args;
	if (!node->inflag)
		{
		char **x;

		args = newlist();
		for (x = pparams; *x; x++)
			addnode(args,ztrdup(*x));
		}
	pushheap();
	while (str = ugetnode(args))
		{
		setsparam(node->name,ztrdup(str));
		list = dupstruct(node->list);
		execlist(list);
		if (breaks)
			{
			breaks--;
			if (breaks || !contflag)
				break;
			contflag = 0;
			}
		if (errflag)
			{
			lastval = 1;
			break;
			}
		freeheap();
		}
	popheap();
	thisjob = cj;
	loops--;
	return lastval;
}

int execselect(cmd) /**/
Cmd cmd;
{
List list;
struct forcmd *node;
char *str,*s;
Lklist args;
Lknode n;
int cj = thisjob,t0;
FILE *inp;

	node = cmd->u.forcmd;
	args = cmd->args;
	if (!node->inflag) {
		char **x;

		args = newlist();
		for (x = pparams; *x; x++)
			addnode(args,ztrdup(*x));
	}
	if (!full(args))
		return 1;
	loops++;
	exiting = 0;
	pushheap();
	inp = fdopen(dup((SHTTY==-1)?0:SHTTY),"r");
	for (;;)
		{
		do
			{
			int pl;
			selectlist(args);
			str = putprompt(prompt3,&pl);
			if (full(bufstack)) str = (char *) getnode(bufstack);
			else if (interact && SHTTY != -1 && isset(USEZLE)) {
				str = (char *)zleread(str,NULL,pl);
			} else {
				fprintf(stderr,"%s",str);
				fflush(stderr);
				str = fgets(zalloc(256),256,inp);
			}
			if (!str || errflag)
				{
				fprintf(stderr,"\n");
				fflush(stderr);
				goto done;
				}
			if (s = strchr(str,'\n'))
				*s = '\0';
			}
		while (!*str);
		setsparam("REPLY",ztrdup(str));
		t0 = atoi(str);
		if (!t0)
			str = "";
		else
			{
			for (t0--,n = firstnode(args); n && t0; incnode(n),t0--);
			if (n)
				str = getdata(n);
			else
				str = "";
			}
		setsparam(node->name,ztrdup(str));
		list = dupstruct(node->list);
		execlist(list);
		freeheap();
		if (breaks)
			{
			breaks--;
			if (breaks || !contflag)
				break;
			contflag = 0;
			}
		if (errflag)
			break;
		}
done:
	popheap();
	fclose(inp);
	thisjob = cj;
	loops--;
	return lastval;
}
 
int execwhile(cmd) /**/
Cmd cmd;
{
List list;
struct whilecmd *node;
int cj = thisjob; 

	node = cmd->u.whilecmd;
	exiting = 0;
	pushheap();
	loops++;
	for(;;)
		{
		list = dupstruct(node->cont);
		execlist(list);
		if (!((lastval == 0) ^ node->cond))
			break;
		list = dupstruct(node->loop);
		execlist(list);
		if (breaks)
			{
			breaks--;
			if (breaks || !contflag)
				break;
			contflag = 0;
			}
		freeheap();
		if (errflag)
			{
			lastval = 1;
			break;
			}
		}
	popheap();
	thisjob = cj;
	loops--;
	return lastval;
}
 
int execrepeat(cmd) /**/
Cmd cmd;
{
List list;
int cj = thisjob,count;

	exiting = 0;
	if (!full(cmd->args) || nextnode(firstnode(cmd->args)))
		{
		zerr("bad argument for repeat",NULL,0);
		return 1;
		}
	count = atoi(peekfirst(cmd->args));
	pushheap();
	loops++;
	while (count--)
		{
		list = dupstruct(cmd->u.list);
		execlist(list);
		freeheap();
		if (breaks)
			{
			breaks--;
			if (breaks || !contflag)
				break;
			contflag = 0;
			}
		if (lastval)
			break;
		if (errflag)
			{
			lastval = 1;
			break;
			}
		}
	popheap();
	thisjob = cj;
	loops--;
	return lastval;
}
 
int execif(cmd) /**/
Cmd cmd;
{
struct ifcmd *node;
int cj = thisjob;

	node = cmd->u.ifcmd;
	exiting = 0;
	while (node)
		{
		if (node->ifl)
			{
			execlist(node->ifl);
			if (lastval)
				{
				node = node->next;
				continue;
				}
			}
		execlist(node->thenl);
		break;
		}
	thisjob = cj;
	return lastval;
}
 
int execcase(cmd) /**/
Cmd cmd;
{
struct casecmd *node;
char *word;
Lklist args;
int cj = thisjob;

	node = cmd->u.casecmd;
	args = cmd->args;
	exiting = 0;
	if (firstnode(args) && nextnode(firstnode(args)))
		{
		zerr("too many arguments to case",NULL,0);
		errflag = 1;
		return 1;
		}
	if (!full(args))
		word = strdup("");
	else
		word = peekfirst(args);
	while (node)
		{
		singsub(&(node->pat));
		if (matchpat(word,node->pat))
			break;
		else
			node = node->next;
		}
	if (node && node->list)
		execlist(node->list);
	thisjob = cj;
	return lastval;
}
