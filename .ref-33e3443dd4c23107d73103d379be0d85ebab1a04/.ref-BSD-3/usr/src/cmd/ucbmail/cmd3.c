#

#include "rcv.h"
#include <sys/stat.h>

/*
 * Mail -- a mail program
 *
 * Still more user commands.
 */

/*
 * Process a shell escape by saving signals, ignoring signals,
 * and forking a sh -c
 */

shell(str)
	char *str;
{
	int (*sig[2])(), stat[1];
	register int t;
	char *Shell;

	if ((Shell = value("SHELL")) == NOSTR)
		Shell = SHELL;
	for (t = 2; t < 4; t++)
		sig[t-2] = signal(t, SIG_IGN);
	t = vfork();
	if (t == 0) {
		for (t = 2; t < 4; t++)
			if (sig[t-2] != SIG_IGN)
				signal(t, SIG_DFL);
		execl(Shell, Shell, "-c", str, 0);
		perror(Shell);
		_exit(1);
	}
	while (wait(stat) != t)
		;
	if (t == -1)
		perror("fork");
	for (t = 2; t < 4; t++)
		signal(t, sig[t-2]);
	printf("!\n");
	return(0);
}

/*
 * Fork an interactive shell.
 */

dosh(str)
	char *str;
{
	int (*sig[2])(), stat[1];
	register int t;
	char *Shell;

	if ((Shell = value("SHELL")) == NOSTR)
		Shell = SHELL;
	for (t = 2; t < 4; t++)
		sig[t-2] = signal(t, SIG_IGN);
	t = vfork();
	if (t == 0) {
		for (t = 2; t < 4; t++)
			if (sig[t-2] != SIG_IGN)
				signal(t, SIG_DFL);
		execl(Shell, Shell, 0);
		perror(Shell);
		_exit(1);
	}
	while (wait(stat) != t)
		;
	if (t == -1)
		perror("fork");
	for (t = 2; t < 4; t++)
		signal(t, sig[t-2]);
	putchar('\n');
	return(0);
}

/*
 * Print out a nice help message from some file or another.
 */

help()
{
	register c;
	register FILE *f;

	if ((f = fopen(HELPFILE, "r")) == NULL) {
		printf("No help just now.\n");
		return(1);
	}
	while ((c = getc(f)) != EOF)
		putchar(c);
	fclose(f);
	return(0);
}

/*
 * Change user's working directory.
 */

schdir(str)
	char *str;
{
	register char *cp;

	for (cp = str; *cp == ' '; cp++)
		;
	if (*cp == '\0')
		cp = homedir;
	else
		if ((cp = expand(cp)) == NOSTR)
			return(1);
	if (chdir(cp) < 0) {
		perror(cp);
		return(1);
	}
	return(0);
}

/*
 * Reply to a list of messages.  Extract each name from the
 * message header and send them off to mail1()
 */

respond(msgvec)
	int *msgvec;
{
	struct message *mp;
	char *cp, buf[2 * LINESIZE], *rcv;
	struct name *np;
	struct header head;
	char *netmap();

	if (msgvec[1] != 0) {
		printf("Sorry, can't reply to multiple messages at once\n");
		return(1);
	}
	mp = &message[msgvec[0] - 1];
	rcv = nameof(mp);
	strcpy(buf, "");
	cp = hfield("to", mp);
	if (cp != NOSTR)
		strcpy(buf, cp);
	np = elide(extract(buf));
	mapf(np, rcv);
	np = delname(np, myname);
	head.h_seq = 1;
	cp = detract(np);
	if (cp != NOSTR) {
		strcpy(buf, cp);
		strcat(buf, " ");
		strcat(buf, rcv);
	}
	else
		strcpy(buf, rcv);
	head.h_to = buf;
	head.h_subject = hfield("subject", mp);
	if (head.h_subject == NOSTR)
		head.h_subject = hfield("subj", mp);
	head.h_cc = NOSTR;
	cp = hfield("cc", mp);
	if (cp != NOSTR) {
		np = elide(extract(cp));
		mapf(np, rcv);
		np = delname(np, myname);
		head.h_cc = detract(np);
	}
	head.h_bcc = NOSTR;
	mail1(&head);
	return(0);
}

/*
 * Preserve the named messages, so that they will be sent
 * back to the system mailbox.
 */

preserve(msgvec)
	int *msgvec;
{
	register struct message *mp;
	register int *ip, mesg;

	if (edit) {
		printf("Cannot \"preserve\" in edit mode\n");
		return(1);
	}
	for (ip = msgvec; *ip != NULL; ip++) {
		mesg = *ip;
		mp = &message[mesg-1];
		mp->m_flag |= MPRESERVE;
		dot = mp;
	}
	return(0);
}

/*
 * Print the size of each message.
 */

messize(msgvec)
	int *msgvec;
{
	register struct message *mp;
	register int *ip, mesg;

	for (ip = msgvec; *ip != NULL; ip++) {
		mesg = *ip;
		mp = &message[mesg-1];
		printf("%d: %d\n", mesg, msize(mp));
	}
	return(0);
}

/*
 * Quit quickly.  If we are sourcing, just pop the input level
 * by returning an error.
 */

rexit(e)
{
	if (sourcing)
		return(1);
	exit(e);
}

/*
 * Set or display a variable value.  Syntax is similar to that
 * of csh.
 */

set(arglist)
	char **arglist;
{
	register struct var *vp;
	register char *cp, *cp2;
	char varbuf[BUFSIZ], **ap, **p;
	int errs, h, s;

	if (argcount(arglist) == 0) {
		for (h = 0, s = 1; h < HSHSIZE; h++)
			for (vp = variables[h]; vp != NOVAR; vp = vp->v_link)
				s++;
		ap = (char **) salloc(s * sizeof *ap);
		for (h = 0, p = ap; h < HSHSIZE; h++)
			for (vp = variables[h]; vp != NOVAR; vp = vp->v_link)
				*p++ = vp->v_name;
		*p = NOSTR;
		sort(ap);
		for (p = ap; *p != NOSTR; p++)
			printf("%s\t%s\n", *p, value(*p));
		return(0);
	}
	errs = 0;
	for (ap = arglist; *ap != NOSTR; ap++) {
		cp = *ap;
		cp2 = varbuf;
		while (*cp != '=' && *cp != '\0')
			*cp2++ = *cp++;
		*cp2 = '\0';
		if (*cp == '\0')
			cp = "";
		else
			cp++;
		if (equal(varbuf, "")) {
			printf("Non-null variable name required\n");
			errs++;
			continue;
		}
		assign(varbuf, cp);
	}
	return(errs);
}

/*
 * Unset a bunch of variable values.
 */

unset(arglist)
	char **arglist;
{
	register struct var *vp, *vp2;
	register char *cp;
	int errs, h;
	char **ap;

	errs = 0;
	for (ap = arglist; *ap != NOSTR; ap++) {
		if ((vp2 = lookup(*ap)) == NOVAR) {
			if (!sourcing) {
				printf("\"%s\": undefined variable\n", *ap);
				errs++;
			}
			continue;
		}
		h = hash(*ap);
		if (vp2 == variables[h]) {
			variables[h] = variables[h]->v_link;
			vfree(vp2->v_name);
			vfree(vp2->v_value);
			cfree(vp2);
			continue;
		}
		for (vp = variables[h]; vp->v_link != vp2; vp = vp->v_link)
			;
		vp->v_link = vp2->v_link;
		vfree(vp2->v_name);
		vfree(vp2->v_value);
		cfree(vp2);
	}
	return(errs);
}

/*
 * Put add users to a group.
 */

group(argv)
	char **argv;
{
	register struct grouphead *gh;
	register struct group *gp;
	register int h;
	int s;
	char **ap, *gname, **p;

	if (argcount(argv) == 0) {
		for (h = 0, s = 1; h < HSHSIZE; h++)
			for (gh = groups[h]; gh != NOGRP; gh = gh->g_link)
				s++;
		ap = (char **) salloc(s * sizeof *ap);
		for (h = 0, p = ap; h < HSHSIZE; h++)
			for (gh = groups[h]; gh != NOGRP; gh = gh->g_link)
				*p++ = gh->g_name;
		*p = NOSTR;
		sort(ap);
		for (p = ap; *p != NOSTR; p++)
			printgroup(*p);
		return(0);
	}
	if (argcount(argv) == 1) {
		printgroup(*argv);
		return(0);
	}
	gname = *argv;
	h = hash(gname);
	if ((gh = findgroup(gname)) == NOGRP) {
		gh = (struct grouphead *) calloc(sizeof *gh, 1);
		gh->g_name = vcopy(gname);
		gh->g_list = NOGE;
		gh->g_link = groups[h];
		groups[h] = gh;
	}

	/*
	 * Insert names from the command list into the group.
	 * Who cares if there are duplicates?  They got tossed
	 * later anyway.
	 */

	for (ap = argv+1; *ap != NOSTR; ap++) {
		gp = (struct group *) calloc(sizeof *gp, 1);
		gp->ge_name = vcopy(*ap);
		gp->ge_link = gh->g_list;
		gh->g_list = gp;
	}
	return(0);
}

/*
 * Sort the passed string vecotor into ascending dictionary
 * order.
 */

sort(list)
	char **list;
{
	register char **ap;
	int diction();

	for (ap = list; *ap != NOSTR; ap++)
		;
	if (ap-list < 2)
		return;
	qsort(list, ap-list, sizeof *list, diction);
}

/*
 * Do a dictionary order comparison of the arguments from
 * qsort.
 */

diction(a, b)
	register char **a, **b;
{
	return(strcmp(*a, *b));
}

/*
 * The do nothing command for comments.
 */

null(e)
{
	return(0);
}

/*
 * Print out the current edit file, if we are editting.
 * Otherwise, print the name of the person who's mail
 * we are reading.
 */

file(e)
{
	register char *cp;

	if (edit)
		printf("Reading \"%s\"\n", editfile);
	else
		printf("Reading %s's mail\n", rindex(mailname, '/') + 1);
	return(0);
}

/*
 * Expand file names like echo
 */

echo(argv)
	char **argv;
{
	register char **ap;
	register char *cp;

	for (ap = argv; *ap != NOSTR; ap++) {
		cp = *ap;
		if ((cp = expand(cp)) != NOSTR)
			printf("%s\n", cp);
	}
	return(0);
}

/*
 * Reply to a series of messages by simply mailing to the senders
 * and not messing around with the To: and Cc: lists as in normal
 * reply.
 */

Respond(msgvec)
	int msgvec[];
{
	struct header head;
	struct message *mp;
	register int s, *ap;
	register char *cp, *subject;

	for (s = 0, ap = msgvec; *ap != 0; ap++) {
		mp = &message[*ap - 1];
		s += strlen(nameof(mp)) + 1;
	}
	if (s == 0)
		return(0);
	cp = salloc(s + 2);
	head.h_to = cp;
	for (ap = msgvec; *ap != 0; ap++) {
		mp = &message[*ap - 1];
		cp = copy(nameof(mp), cp);
		*cp++ = ' ';
	}
	*--cp = 0;
	mp = &message[msgvec[0] - 1];
	subject = hfield("subject", mp);
	if (subject == NOSTR)
		subject = hfield("subj", mp);
	head.h_subject = subject;
	head.h_cc = NOSTR;
	head.h_bcc = NOSTR;
	head.h_seq = 0;
	mail1(&head);
	return(0);
}
