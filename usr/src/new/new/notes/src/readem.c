static char *sccsid = "%W%";

/*
 *	this particular collection of junk handles the basic idea
 *	of what to do when you are showing a note.
 *	It displays the note, and then manages to collect enough info
 *	from the terminal to either progress to the next note or
 *	show some of the responses.
 *
 *	original author : rob kolstad
 *	modified	: ray essick
 *	yet again	: lou salkind
 */

#include "parms.h"
#include "structs.h"
#include "newsgate.h"
#include <signal.h>
#include <sgtty.h>

readem(io, readnum, firstdis, resp)
struct io_f *io;
int *firstdis;
{
	struct note_f note;
	struct resp_f rsprec;
	struct io_f io2;
	struct when_f whendump;
	FILE *txtfile;
	FILE *ptxtfile;
	FILE *pipeopen();
	int rrecnum, roffset;
	char tonf[NNLEN + 1];			/* for forwarding */
	char ntitle[TITLEN + 1];		/* scratch space */
	char author[NAMESZ + 1];
	char system[SYSSZ + 1];
	char cmdline[CMDLEN];			/* build-a-command */
	char tmpfile[CMDLEN + 1];
	int c;			/* input char */
	int forward;		/* scroll forward/backward on deleted note */
	int toauth;		/* send to author */
	int znum;		/* forward as resp to this note */
	int znote, zresp;	/* scratch for asearch */
	int i, j;
	int wtext;		/* send mail with text */
	int retcode;
	int retstat;
	struct notesenv oldenv;
	char *delfile;
	extern char *myshell;

	retcode = -1;		/* init so grabs character */
	retstat = 0;		/* default return code */
	forward = 1;		/* default to scroll forward */
	ignsigs = 0;
	delfile = NULL;
	txtfile = NULL;
	replot = 1;

	oldenv = curenv;
	if (setjmp(jenv)) {
		/* KLUDGE */
		if (txtfile) {
			fclose(txtfile);
			txtfile = NULL;
		}
		if (delfile) {
			unlink(delfile);
			delfile = NULL;
		}
	}

	while (1) {
		x (readnum < 0, "readem: given bad readnum");
		if (readnum > io->descr.d_nnote) {
			/* HMM... THIS SHOULD REALLY BE AN ERROR */
			readnum = io->descr.d_nnote;
		}
		if (readnum == 0 && io->descr.d_plcy == 0)
			goto out;	/* empty notesfile */
		getnrec(io, readnum, &note);
		if (note.n_stat & DELETED) {
			if (forward)
				goto nextnt;		/* forward scroll */
			else
				goto prevnt;		/* backward scroll */
		}
		if (replot) {
		    replot = 0;		/* set later if want replot */
		    if (resp) {
			/* response */
			if (resp > note.n_nresp)
				resp = note.n_nresp;	/* set to the end */
			if (lrsp(io, readnum, resp, &rsprec, &roffset,
			    &rrecnum) == -1) {
				/* bad response chain - print note instead */
				resp = 0;
				getnrec(io, readnum, &note);
				retcode = dspnote(io, &note, readnum);
			} else
				retcode = dspresp(io, &note, &rsprec, roffset,
					resp, readnum);
		    } else
			/* show the note if we need new one */
			retcode = dspnote(io, &note, readnum);
		}
		forward = 1;
		if (retcode < 0) {
			cmdprompt();		/* no previous command */
			c = gchar();
		} else {
			c = retcode;		/* previous command */
			retcode = -1;
		}

		switch (c) {

		case 'z':	/* update sequencer and exit, RLS */
			retstat = QUITUPD;
			goto out;

		case 'u':	/* unsubscribe from this notesgroup, RLS */
			if (unsubscribe(io->nf) < 0)
				continue;
			retstat = QUITSEQ;
			goto out;

		case '?': 		/* if he doesn't know what to type */
		case 'h':
			help(RDMHLP);		/* print the pseudo-man page */
			continue;

		case 'D': 			/* delete this note/response */
			if (resp) {
				/* check to see if his note */
				if ((rsprec.r_auth[roffset].aid & UIDMASK) != globuid && (allow(io,DRCTOK) == 0)) {
					warn("Not your response");
					continue;
				}
			} else {
				if ((note.n_auth.aid & UIDMASK) != globuid &&
				    (allow(io,DRCTOK) == 0)) {
					warn("Not your note");
					continue;
				}
				if (readnum == 0) {
					warn("Use 'Z' to delete policy");
					continue;
				}
			}

			prompt("Delete? ");
			if (askyn() == 'n')
				continue;
			lock(io, 'n');
			/* this should catch most */
			getnrec(io, readnum, &note);
			/* and an up to date descriptor */
			getdscr(io, &io->descr);
			if (resp) {		/* go about deleting it */
				if (resp == note.n_nresp &&
				    inorder(&io->descr.d_lstxmit,
				    &rsprec.r_when[roffset])) {
					delresp(io, readnum, rrecnum, roffset,
						0);
					/* adjust note response count */
					note.n_nresp--;
					unlock(io, 'n');
					/* show next response */
					replot = 1;
					continue;
				}
		warn("Can't delete: networked, or not last response");
				unlock(io, 'n');
				continue;
			}
			/* it's a note */
			if (note.n_nresp ||
			    inorder(&note.n_date, &io->descr.d_lstxmit)) {
		warn("Can't delete; note has responses or is networked");
				unlock(io, 'n');
				continue;
			}
			delnote(io, readnum++, 0);
			resp = 0;
			unlock(io, 'n');
			replot = 1;
			continue;

		case 'E': 				/* edit an article */
			if (resp) {		/* check to see if his note */
				if ((rsprec.r_auth[roffset].aid & UIDMASK) != globuid && (allow(io,DRCTOK) == 0)) {
					warn("Not your response");
					continue;
				}
			} else {
				if ((note.n_auth.aid & UIDMASK) != globuid && (allow(io,DRCTOK) == 0)) {
					warn("Not your note");
					continue;
				}
				if (readnum == 0) {
			warn("Sorry, E doesn't work for policy notes yet");
					continue;
				}
			}

			lock(io, 'n');
			getnrec(io, readnum, &note);
			/* this should catch most */
			getdscr(io, &io->descr);
			/* and an up to date descriptor */
			if (resp) {		/* go about deleting it */
				if (resp == note.n_nresp &&
				    inorder(&io->descr.d_lstxmit,
				    &rsprec.r_when[roffset])) {
					delresp(io, readnum, rrecnum, roffset, 0);
					/* adjust note response count */
					note.n_nresp--;
					unlock(io, 'n');
					sprintf(tmpfile , "/tmp/nfe%d",
						getpid());
					delfile = tmpfile;
					/* build scr file */
					x ((txtfile = fopen(tmpfile, "w")) == NULL, "readem: scrfile");
					x (chmod(tmpfile, 0666) < 0,
					    "readem: chmod");
					pageout(io, &rsprec.r_addr[roffset],
					    txtfile);
					/* dump it */
					fclose(txtfile);  /* also flushes it */
					x ((txtfile = fopen(tmpfile, "r")) == NULL, "readem: edit reopen");
					resp = addresp(io, txtfile, readnum);
					fclose(txtfile);
					txtfile = NULL;
					/* up to date */
					getnrec(io, readnum, &note);
					/* add it back in ! */
					x (unlink(tmpfile) < 0,
					    "readem: edit unlink");
					delfile = NULL;
					continue; /* show next response */
				}
			warn("Can't edit: networked, or not last response");
				unlock(io, 'n');
				continue;
			}
			/* its a note */
			if (note.n_nresp ||
			    inorder(&note.n_date, &io->descr.d_lstxmit)) {
		warn("Can't edit; note has responses or is networked");
				unlock(io, 'n');
				continue;
			}
			delnote(io, readnum++, 0);
			resp = 0;
			unlock(io, 'n');
			sprintf(tmpfile, "/tmp/nfe%d", getpid());
			delfile = tmpfile;
			/* build scr file */
			x ((txtfile = fopen(tmpfile, "w")) == NULL,
			    "readem: scrfile");
			x(chmod(tmpfile, 0666) < 0, "readem: chmod");
			pageout(io, &note.n_addr, txtfile);
			/* dump it */
			fclose(txtfile);	/* also flushes it */
			x((txtfile = fopen(tmpfile, "r")) == NULL,
			    "readem: edit reopen");
			znum = addnote(io, txtfile, "Edit note text:",
			    "Note title: ");
			fclose(txtfile);
			txtfile = NULL;
			x(unlink(tmpfile) < 0, "readem: edit unlink");
			delfile = NULL;
			if (znum > 0)
				readnum = znum;	/* this is the one */
			continue;

		case 'Z': 	/* zap notes/responses - directors only */
			/* kills any note/response */
			getdscr(io, &io->descr);  /* up to date descriptor */
			if (allow(io, DRCTOK) == 0) {
				warn("Not a director");
				continue;
			}

			prompt("Delete? ");
			if (askyn() == 'n')
				continue;
			/*
			 *	should log the deletion here, so the
			 *	"meta-director" can watch for fascist
			 *	directors preying on the peasants.
			 */
			if (readnum == 0) {		/* deleting policy */
				lock(io, 'n');
				getdscr (io, &io->descr);  /* grab up-to-date */
				io->descr.d_plcy = 0;	/* its gone now */
				/* replace descriptor */
				putdscr (io, &io->descr);
				unlock(io, 'n');
				goto out;
			}
			if (resp) {			/* delete a response */
				delresp(io, readnum, rrecnum, roffset, 1);
				/* kill it */
				note.n_nresp--;		/* and response count */
			} else
				delnote(io, readnum++, 1);
			replot = 1;
			continue;

		case 'r': 	/* replot the current note/response */
		case '\014':	/* everyone else uses ^L, might as well */
			replot = 1;
			continue;

		case '\r': 			/* wants the next note */
		case '\n': 
			goto nextnt;

		case 'm': 		/* mail a note/response via Unix mail */
			toauth = 0;
			wtext = 0;		/* to others and no text */
			goto sendmail;
		case 'M': 			/* same as 'm' but with text */
			toauth = 0;
			wtext = 1;		/* to others with text */
			goto sendmail;
		case 'P': 
			toauth = 1;
			wtext = 1;		/* to author with text */
			goto sendmail;
		case 'p': 
			toauth = 1;
			wtext = 0;		/* to author, no text */

		sendmail:
			/* jump to here once set mail parms */
			if (resp) {
				mailit(io, &rsprec.r_addr[roffset],
				    &rsprec.r_auth[roffset],
				    &rsprec.r_id[roffset],
				    &rsprec.r_when[roffset], io->nf, toauth,
				    wtext);
			} else
				mailit(io, &note.n_addr, &note.n_auth,
				    &note.n_id, &note.n_date, io->nf, toauth,
				    wtext);
			continue;

		case '!': 			/* wants to fork a shell */
			gshell();
			continue;

		case 'q': 		/* quit this, maybe whole system */
		case 'k': 
			retstat = QUITSEQ;
			goto out;

		case '\04':        			/* control D */
			retstat = QUITFAST;
			goto out;

		case 'Q': 	/* exit system without updating sequencer */
		case 'K': 
			retstat = QUITNOSEQ;
			goto out;

		case 'i': 			/* go back to note index */
			*firstdis = readnum;
			goto out;

		case '\010': 
		case '-': 			/* display previous response */
			resp--;
			if (resp >= 0) {
				replot = 1;
				continue;	/* show the base note */
			}
			goto prevnt;

		case 'x': 
		case 'X': 
			if (readnum == 0)
				goto out;
			retcode = tsearch(io, readnum - 1, c == 'x');
			/* look it up */
			if (retcode > 0) {
				readnum = retcode;
				resp = 0;
				replot = 1;
			}
			continue;

		case 'a': 
		case 'A':		/* author search from current spot */
			if (readnum == 0)
				goto out;
			znote = readnum;
			zresp = resp;
			if (zresp == 0)
				znote--;
			else
				zresp++;		/* select 'next' */
			if (asearch(io, &znote, &zresp, (c == 'a')) > 0) {
				readnum = znote;
				resp = zresp;	/* set returned values */
				/* grab right descriptor */
				getnrec(io, readnum, &note);
				replot = 1;
			}
			continue;

		case 'd': 		/* toggle a notes director status */
			if (allow (io, DRCTOK) == 0) {
				prompt("Anonymous: %s	Networked: %s",
				    (io->descr.d_stat & ANONOK) ? "YES" : "NO",
				    (io->descr.d_stat & NETWRKD) ? "YES" : "NO");
				continue;
			}
			if (resp == 0) {		/* toggle a note */
				lock(io, 'n');
				getnrec (io, readnum, &note);
				if (note.n_stat & DIRMES)
					note.n_stat &= NOT DIRMES;
				else
					note.n_stat |= DIRMES;
				putnrec (io, readnum, &note);    /* replace */
				unlock(io, 'n');
				replot = 1;
				continue;
			}
			/* toggle a response */
			lock(io, 'n');
			/* grab that block */
			getrrec(io, rrecnum, &rsprec);
			if (rsprec.r_stat[roffset] & DIRMES)
				rsprec.r_stat[roffset] &= NOT DIRMES;
			else
				rsprec.r_stat[roffset] |= DIRMES;
			putrrec (io, rrecnum, &rsprec);     /* replace */
			unlock(io, 'n');
			replot = 1;
			continue;

		case 'e': 		/* allow him to edit his title */
			if (readnum == 0)
				continue;		/* don't touch */
			if (resp) {
				warn("Not reading note");
				continue;
			}
			if (allow (io, DRCTOK) == 0 &&
			    /* check uid */
			    (globuid != (note.n_auth.aid & UIDMASK) ||
			    strcmp (SYSTEM, note.n_id.sys) != 0)) {
				/* other sys */
				warn("Not your note");
				continue;
			}
			prompt("New Title: ");
			if ((i = gline(ntitle, TITLEN)) == 1)
				/* empty title, leave alone */
				continue;
			lock(io, 'n');
			getnrec (io, readnum, &note);	/* well, update it */
			for (j = 0; j < i - 1 && j < TITLEN; j++)
				note.ntitle[j] = ntitle[j];
			for (; j < TITLEN; j++)
				note.ntitle[j] = ' ';	/* space fill */
			putnrec (io, readnum, &note);  /* and replace */
			unlock(io, 'n');
			replot = 1;
			continue;

		case 'F':		/* change author and system, RLS */

			if (allow(io, DRCTOK) == 0) {
				warn("Not a director");
				continue;
			}
			prompt("New Author: ");
			if ((i = gline(author, NAMESZ)) == 1)
				continue;
			prompt("New System: ");
			if ((i = gline(system, SYSSZ)) == 1)
				continue;
			lock(io, 'n');
			if (resp == 0) {
				/* well, update it */
				getnrec(io, readnum, &note);
				strcpy(note.n_auth.aname, author);
				strcpy(note.n_from, system);
				strcpy(note.n_id.sys, system);
				note.n_auth.aid = 0;
				putnrec(io, readnum, &note);  /* and replace */
			} else {
				/* grab that block */
				getrrec (io, rrecnum, &rsprec);
				strcpy(rsprec.r_auth[roffset].aname, author);
				strcpy(rsprec.r_from[roffset], system);
				strcpy(rsprec.r_id[roffset].sys, system);
				rsprec.r_auth[roffset].aid = 0;
				putrrec(io, rrecnum, &rsprec);     /* replace */
			}
			unlock(io, 'n');
			replot = 1;
			continue;

		case 't':		/* talk to the author of a note */
			if (resp)
				replot = talkto(&rsprec.r_auth[roffset],
					    &rsprec.r_id[roffset]);
			else
				replot = talkto(&note.n_auth, &note.n_id);
			continue;

		case 'W': 		/* write a response with the text */
		case 'w': 		/* let him write a response */
			getdscr(io, &io->descr);	/* get up to date */
			if (allow(io, RESPOK) == 0) {
				warn("Sorry, you are not allowed to write");
				continue;	/* back to key processing */
			}
			if (readnum == 0) {
				warn("No responses allowed to policy note");
				continue;
			}

			if (c == 'w') {
				txtfile = NULL;		/* no preface text */
			} else {
				sprintf(tmpfile, "/tmp/nfx%d", getpid ());
				delfile = tmpfile;
				x((txtfile = fopen(tmpfile, "w")) == NULL,
				    "readem: bad scrfile");
				x(chmod(tmpfile, 0666) < 0,
				    "readem: chmod failed");
				if (resp) {
					preptxt(io, txtfile, io->nf,
					    &rsprec.r_auth[roffset],
					    &rsprec.r_id[roffset],
					    &rsprec.r_when[roffset],
					    &rsprec.r_addr[roffset]);
				} else {
					preptxt(io, txtfile, io->nf,
					    &note.n_auth, &note.n_id,
					    &note.n_date, &note.n_addr);
				}
				fclose(txtfile);
				x ((txtfile = fopen(tmpfile, "r")) == NULL,
				    "readem: reopen");
			}
			zresp = addresp(io, txtfile, readnum); /* put it in */
			if (zresp > 0) {
				/* update descriptor */
				getnrec (io, readnum, &note);
			}

			if (txtfile != NULL) {
				fclose(txtfile);	/* toss out scratch */
				txtfile = NULL;
				x (unlink(tmpfile) < 0,
				    "readem: couldnt unlink scratch");
				delfile = NULL;
			}
			if (zresp)
				resp = zresp;		/* show the new */

			if (zresp == 0)
				continue;

			if ((io->descr.d_stat & NETWRKD) == 0) {  
			/*
				prompt("Not networked");
				fflush(stdout);
				sleep(2);
			*/
				continue;
			}

#ifdef NEWS
			prompt("Send to news? ");
			if (askyn() == 'y') {

#ifdef DEMANDNEWS
				/* send it to the news */
				sprintf(cmdline, "%s/%s/newsoutput", MSTDIR,
				    UTILITY);
				dounix(0, 0, cmdline, io->nf, 0, 0, 0);
#endif DEMANDNEWS
			} else {
				/* don't send it to the network */
				gettime(&whendump);
				fixlast(&whendump, io->nf, 1 , NEWSSYS);
			}
#endif NEWS
			continue;

		case 'B':			/* bitch, bitch, bitch */
			/* check gripe file */
			if (init(&io2, GRIPES) < 0) {
				warn("No gripe file");
				continue;
			}
			addnote(&io2, NULL, "Edit Gripe text:",
			    "Gripe Header: ");
			/* let him put the note in */
			finish(&io2);	/* close up the gripe file */
			continue;

		case 'C': 	/* copy to other notefile with text */
		case 'c':	/* copy to other notefile without text */
			if (c == 'C')
				wtext = 1;
			else
				wtext = 0;		/* determine which */
			prompt("Forward to: ");
			if (gline(tonf, NNLEN) == 1)
				continue;
			if (init(&io2, tonf) < 0) {
				warn("Can't find notesfile %s", tonf);
				continue;
			}
			if (wtext == 0) {
				txtfile = NULL;
			} else {
				sprintf(tmpfile, "/tmp/nfx%d", getpid());
				delfile = tmpfile;
				x ((txtfile = fopen(tmpfile, "w")) == NULL,
				    "readem:creat scratch failed");
				x (chmod(tmpfile, 0666) < 0,
				    "readem: chmod failed");
				if (resp) {
					preptxt(io, txtfile, io->nf,
					    &rsprec.r_auth[roffset],
					    &rsprec.r_id[roffset],
					    &rsprec.r_when[roffset],
					    &rsprec.r_addr[roffset]);
				} else {
					preptxt(io, txtfile, io->nf,
					&note.n_auth, &note.n_id, &note.n_date,
					&note.n_addr);
				}
				fclose(txtfile);	/* close it */
				x ((txtfile = fopen(tmpfile, "r")) == NULL,
				    "readem: couldnt reopen");
			}
			c = 'n';			/* default to note */
			if (allow(&io2, WRITOK) && allow (&io2, READOK) &&
			    allow (&io2, RESPOK)) {
				prompt("Forward as response? ");
				c = askyn();
			}
			if (c == 'n' && allow(&io2, WRITOK)) {
				addnote(&io2, txtfile, "Edit forwarded text:",
				    "Forwarded Title: ");
			} else if (c == 'y') {
				if (znum = limindx (&io2)) {
					addresp (&io2, txtfile, znum);
				}
			} else
				warn ("You haven't permission");
			if (strcmp(io->nf, io2.nf) == 0) {
				/* if was this notefile */
				/* get new descriptor */
				getdscr (io, &io->descr);
			}
			finish(&io2);		/* close up that notefile */
			if (txtfile != NULL) {
				fclose(txtfile);	/* throw it away */
				txtfile = NULL;
				x (unlink(tmpfile) < 0,
				    "readem: couldnt unlink scratch");
				delfile = NULL;
			}
			continue;

		case 'n': 			/* nest notesfiles - a stack */
			prompt("New notesfile: ");
			if (gline(tonf, NNLEN) == 1)
				continue;
			closenf(io);				/* save fids */
			i = control(tonf, NOSEQ);
			if (opennf(io, io->nf) < 0) {
				warn("Couldn't reopen notesfile");
				wfchar();
				retstat = QUITNOSEQ;
				goto out;
			}
			if (i == QUITBAD || i == QUITNEX) {
				warn("Can not open notesfile `%s'", tonf);
				continue;
			}
			if (i == QUITFAST || i == QUITUPD) {
				retstat = i;
				goto out;
			}
			replot = 1;
			continue;

		case 's': 		/* place text at end of 'nfsave' */
		case 'S': 		/* place the whole string */
		case '|':		/* pipe into command */
		case '^':		/* whole string into command */
		case '%':		/* joke translator */
			switch (c) {
			case 's':
			case 'S':
				prompt("File name: ");
				if ((znum = gline(tmpfile, CMDLEN)) == 1)
					continue;
				prompt("Saving...");
				sprintf(cmdline, "cat >> %s", tmpfile);
				break;
			case '|':
			case '^':
				prompt("Command: ");
				if ((znum = gline(cmdline, CMDLEN)) == 1)
					continue;
				printf("\nStarting up...\n");
				break;
			case '%':
				prompt("Translation...");
				putchar('\n');
				strcpy(cmdline, "tr 'A-Za-z' 'N-ZA-Mn-za-m'");
				break;
			}
			fflush(stdout);
			if ((ptxtfile = pipeopen(cmdline, "w")) == NULL) {
				warn("pipe open failed");
				continue;
			}
			if (c == 'S' || c == '^') {
				/* save whole string */
				znum = savnote(io, ptxtfile, &note);
				for (i = 1; i <= note.n_nresp; i++) {
					if (lrsp(io, readnum, i, &rsprec,
					    &roffset, &rrecnum) == -1)
						/* hit end of chain */
						continue;
					znum += savresp(io, ptxtfile, &rsprec,
						roffset);
				}
			} else {
				/* save single page */
				if (resp)
					znum = savresp(io, ptxtfile, &rsprec,
						roffset);
				else
					znum = savnote(io, ptxtfile, &note);
			}
			i = pipeclose();
			if (i == 0 && (c == 's' || c == 'S')) {
				prompt("Saved %d lines", znum);
				replot = 0;
			} else
				wfchar();
			continue;

		case 'j': 			/* go on to next note/resp */
		case 'l': 			/* universal seq, RLS */
			if (readnum == 0)
				goto out;
			if (resp != note.n_nresp) {
				if ((resp = nxtresp(io, readnum, resp,
				    &io->stime)) > 0) {
					replot = 1;
					continue;
				}
			}
			/* fall into ... */

		case 'L':
		case 'J': 				/* next unread note */
			if (readnum == 0)
				goto out;
			resp = 0;
			if ((readnum = nxtnote(io, readnum, &io->stime)) > 0) {
				replot = 1;
				continue;
			}
			if (c == 'l' || c == 'L')
				retstat = QUITSEQ;
			else
				/* put him on last page */
				*firstdis = io->descr.d_nnote;
			goto out;

		case '+': 
		case ';': 
		case ' ': 
			if (readnum == 0)
				/* such is the fate of policy notes */
				goto out;
			resp++;
			if (resp > note.n_nresp)
				goto nextnt;
			replot = 1;
			continue;

		case '=': 			/* go back to the base note */
			if (resp == 0) {
				warn("Already on base note");
				continue;
			}
			resp = 0;		/* reset index into responses */
			replot = 1;
			continue;

		case '1': 			/* skip n responses */
		case '2': 
		case '3': 
		case '4': 
		case '5': 
		case '6': 
		case '7': 
		case '8': 
		case '9': 
			if (note.n_nresp < 1)
				goto nextnt;
			/* let him skip all over responses */
			resp += c - '0';
			replot = 1;
			continue;

		default: 		/* something we haven't covered */
			/* so can jump down here */
			warn("q to quit, ? for help");
			continue;
		}

	nextnt: 
		if (readnum == 0)
			goto out;
		if (++readnum > io->descr.d_nnote) {
			*firstdis = io->descr.d_nnote;
			goto out;
		}
		resp = 0;		/* reset response index */
		replot = 1;
		continue;

	prevnt:
		/* display previous note */
		if (readnum == 0)
			goto out;
		/* set to scroll backwards on deleted note */
		forward = 0;
		if (--readnum < 1) {
			/* zero is policy, so stop at 1 */
			readnum = 1;
			forward = 1;	/* bounce off bottom end */
		}
		resp = 0;		/* was else resp = 0; */
		replot = 1;
		continue;
	}
out:
	ignsigs++;
	curenv = oldenv;
	ignsigs = 0;

	return(retstat);
}
