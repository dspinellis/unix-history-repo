/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1984. */
static char rcsid[] = "$Header: demo.c,v 2.6 85/08/22 16:01:21 timo Exp $";

/*
 * B editor -- Editor command processor.
 */

#include <ctype.h>

#include "b.h"
#include "feat.h"
#include "erro.h"
#include "bobj.h"
#include "node.h"
#include "gram.h"
#include "keys.h"
#include "supr.h"

#ifdef BTOP
#include <setjmp.h>

#ifndef CMDPROMPT
#define CMDPROMPT ">>> " /* Prompt user for immediate command */
#endif CMDPROMPT
#endif BTOP


value editqueue();

/* Command line flags */
extern bool dflag;
extern bool slowterminal;

#ifdef SAVEBUF
extern char copysavefile[];
#endif SAVEBUF


Visible bool lefttorite;
	/* Saves some time in nosuggtoqueue() for read from file */

#define MAXHIST 101 /* One more than the number of UNDO's allowed. */

#define Mod(k) (((k)+MAXHIST) % MAXHIST)
#define Succ(k) (((k)+1) % MAXHIST)
#define Pred(k) (((k)+MAXHIST-1) % MAXHIST)

Hidden environ *tobesaved;
Hidden string savewhere;


#ifdef BTOP

extern jmp_buf jumpback;
extern bool interrupted;
extern bool canjump;

/*
 * Main loop, called from main program if -t option present.
 */

Visible Procedure
mainloop()
{
	environ env;
	environ *ep = &env;
	FILE *pdown;
	FILE *pup;
	int cmdchar;

	savewhere = (string)NULL;
	tobesaved = (environ*)NULL;
	start_b(&pdown, &pup);
	clrenv(ep);
#ifdef SAVEBUF
	ep->copybuffer = editqueue(copysavefile);
	if (ep->copybuffer)
		ep->copyflag = Yes;
#endif SAVEBUF

	for (;;) {
		cmdchar = sleur();
		if (cmdchar == EOF)
			break;
		getinput(ep, cmdchar, pdown, pup);
	}
#ifdef SAVEBUF
	if (ep->copyflag)
		savequeue(ep->copybuffer, copysavefile);
	else
		savequeue(Vnil, copysavefile);
#endif SAVEBUF
	Erelease(*ep);
}


/*
 * Provide input for the interpreter.
 */

Hidden Procedure
getinput(ep, cmdchar, pdown, pup)
	environ *ep;
	int cmdchar;
	FILE *pdown;
	FILE *pup;
{
	int n;
	char buffer[100];
	char filename[100];
	int lineno;


	switch (cmdchar) {

	case '>': /* Immediate command */
	case 'E': /* Expression */
	case 'R': /* Raw input */
	case 'Y': /* Yes/No */
		if (cmdchar == '>')
			setroot("Imm_cmd");
		else if (cmdchar == 'E')
			setroot("Expression");
		else
			setroot("Raw_input");
		delfocus(&ep->focus);
		initshow();
		if (cmdchar == '>')
			cmdprompt(CMDPROMPT);
		editdocument(ep);
		endshow();
		top(&ep->focus);
		ep->mode = WHOLE;
		if (!interrupted)
			send(ep->focus, pdown);
		delete(ep);
		break;

	case ':':
	case '=':
		fgets(buffer, sizeof buffer, pup);
		if (index(buffer, '+'))
			n = sscanf(buffer, " +%d %s", &lineno, filename) - 1;
		else {
			n = sscanf(buffer, " %s", filename);
			lineno = 0;
		}
		if (n == 1) {
			initshow();
			dofile(ep, filename, lineno);
			endshow();
			top(&ep->focus);
			ep->mode = WHOLE;
			delete(ep);
			if (!ep->copyflag) {
				release(ep->copybuffer);
				ep->copybuffer = Vnil;
			}
		}
		putc('\n', pdown);
		interrupted = No; /* Interrupts handled locally in editdocument! */
		break;

	default:
		printf("[Unrecognized command character '%c' (0%o)]\n",
			cmdchar&0177, cmdchar);

	}
}

#endif BTOP


#ifdef FILEARGS

/*
 * Edit a single unit or target, called from main program if file name
 * arguments are present.
 */

Visible Procedure
demo(filename, linenumber)
	string filename;
	int linenumber;
{
	environ env;
	environ *ep = &env;
	bool ok;

	clrenv(ep);
#ifdef SAVEBUF
	ep->copybuffer = editqueue(copysavefile);
	if (ep->copybuffer)
		ep->copyflag = Yes;
#endif SAVEBUF
	initshow();
	ok = dofile(ep, filename, linenumber);
	endshow();
	if (!ok)
		return No;
#ifdef SAVEBUF
	if (ep->copyflag)
		savequeue(ep->copybuffer, copysavefile);
	else
		savequeue(Vnil, copysavefile);
#endif SAVEBUF
	Erelease(*ep);
	return Yes;
}

#endif !FILEARGS


/*
 * Edit a unit or target, using the environment offered as a parameter.
 */

Hidden bool
dofile(ep, filename, linenumber)
	environ *ep;
	string filename;
	int linenumber;
{
#ifdef HELPFUL
	static bool didmessage;

	if (!didmessage) {
		didmessage = Yes;
		message("[Press ? or ESC-? for help]");
	}
#endif HELPFUL
#ifdef SAVEPOS
	if (linenumber <= 0)
		linenumber = getpos(filename);
#endif SAVEPOS
	setroot(filename[0] == '=' ? "Target_edit" : "Unit_edit");
	savewhere = filename;
	tobesaved = (environ*)NULL;

	lefttorite = Yes;
	edit(ep, filename, linenumber);
#ifdef USERSUGG
	readsugg(ep->focus);
#endif USERSUGG
	lefttorite = No;

	ep->generation = 0;
	if (!editdocument(ep))
		return No;
	if (ep->generation > 0) {
		if (!save(ep->focus, filename))
			error("Cannot save unit: %s", unixerror(filename));
#ifdef USERSUGG
		writesugg(ep->focus);
#endif USERSUGG
	}
#ifdef SAVEPOS
	savepos(filename, lineno(ep)+1);
#endif SAVEPOS
	savewhere = (char*)NULL;
	tobesaved = (environ*)NULL;
	return Yes;
}


/*
 * Call the editor for a given document.
 */

Hidden bool
editdocument(ep)
	environ *ep;
{
	int k;
	int first = 0;
	int last = 0;
	int current = 0;
	int onscreen = -1;
	bool reverse = No;
	environ newenv;
	int cmd;
	bool errors = No;
	int undoage = 0;
	bool done = No;
	environ history[MAXHIST];

	Ecopy(*ep, history[0]);

	for (;;) { /* Command interpretation loop */
		if (onscreen != current)
			virtupdate(onscreen < 0 ? (environ*)NULL : &history[onscreen],
				&history[current],
				reverse && onscreen >= 0 ?
					history[onscreen].highest : history[current].highest);
		onscreen = current;
		if (done)
			break;
#ifdef BTOP
		if (!interrupted && !moreinput())
#else BTOP
		if (!moreinput())
#endif BTOP
				actupdate(history[current].copyflag ?
						history[current].copybuffer : Vnil,
#ifdef RECORDING
					history[current].newmacro != Vnil,
#else !RECORDING
					No,
#endif !RECORDING
					No);
#ifdef BTOP
		if (interrupted || setjmp(jumpback))
			break;
		canjump = Yes;
#endif BTOP
		cmd = inchar();
#ifdef BTOP
		canjump = No;
#endif BTOP
		errors = No;

		switch (cmd) {

#ifndef NDEBUG
		case Ctl(@): /* Debug exit with variable dump */
			tobesaved = (environ*)NULL;
			return No;
#endif NDEBUG

#ifndef SMALLSYS
		case Ctl(^): /* Debug status message */
			dbmess(&history[current]);
			errors = Yes; /* Causes clear after new keystroke seen */
			continue;
#endif !SMALLSYS

		case UNDO:
			if (current == first)
				errors = Yes;
			else {
				if (onscreen == current)
					reverse = Yes;
				current = Pred(current);
				undoage = Mod(last-current);
			}
			break;

		case REDO:
			if (current == last)
				errors = Yes;
			else {
				if (current == onscreen)
					reverse = No;
				if (history[Succ(current)].generation <
						history[current].generation)
					error(REDO_OLD); /***** Should refuse altogether??? *****/
				current = Succ(current);
				undoage = Mod(last-current);
			}
			break;

#ifdef HELPFUL
		case HELP:
			if (help())
				onscreen = -1;
			break;
#endif HELPFUL

		case REDRAW:
			onscreen = -1;
			trmundefined();
			break;

		case EOF:
			done = Yes;
			break;

		default:
			Ecopy(history[current], newenv);
			newenv.highest = Maxintlet;
			newenv.changed = No;
			if (cmd != EXIT)
				errors = !execute(&newenv, cmd) || !checkep(&newenv);
			else
				done = Yes;
			if (errors) {
				switch (cmd) {
				case '\r':
				case '\n':
					if (newenv.mode == ATEND && !parent(newenv.focus)) {
						errors = !checkep(&newenv);
						if (!errors)
							done = Yes;
					}
					break;
#ifdef HELPFUL
				case '?':
					if (help())
						onscreen = -1;
#endif HELPFUL
				}
			}
			if (errors)
				Erelease(newenv);
			else {
#ifndef SMALLSYS
				if (done)
					done = canexit(&newenv);
#endif SMALLSYS
				if (newenv.changed)
					++newenv.generation;
				last = Succ(last);
				current = Succ(current);
				if (last == first) {
					/* Array full (always after a while). Discard "oldest". */
					if (current == last
						|| undoage < Mod(current-first)) {
						Erelease(history[first]);
						first = Succ(first);
						if (undoage < MAXHIST)
							++undoage;
					}
					else {
						last = Pred(last);
						Erelease(history[last]);
					}
				}
				if (current != last
					&& newenv.highest < history[current].highest)
					history[current].highest = newenv.highest;
				/* Move entries beyond current one up. */
				for (k = last; k != current; k = Pred(k)) {
					if (Pred(k) == onscreen)
						onscreen = k;
					Emove(history[Pred(k)], history[k]);
				}
				Ecopy(newenv, history[current]);
				Erelease(history[current]);
			}
			break; /* default */

		} /* switch */

		if (errors && cmd != '?') {
			if (!slowterminal && isascii(cmd)
				&& (isprint(cmd) || cmd == ' '))
				error(INS_BAD, cmd);
			else
				error((char*)NULL);
		}
		if (savewhere)
			tobesaved = &history[current];
	} /* for (;;) */

	actupdate(Vnil, No, Yes);
	Erelease(*ep);
	Ecopy(history[current], *ep);
	if (savewhere)
		tobesaved = ep;
	for (current = first; current != last; current = Succ(current))
		Erelease(history[current]);
	Erelease(history[last]);
	/* endshow(); */
	return Yes;
}


/*
 * Execute a command, return success or failure.
 */

Hidden bool
execute(ep, cmd)
	register environ *ep;
	register int cmd;
{
	register bool spflag = ep->spflag;
	register int i;
	environ env;
	char buf[2];
	register char *cp;
#ifdef USERSUGG
	bool sugg = symbol(tree(ep->focus)) == Suggestion;
#define ACCSUGG(ep) if (sugg) accsugg(ep)
#define KILLSUGG(ep) if (sugg) killsugg(ep)
#else !USERSUGG
#define ACCSUGG(ep) /* NULL */
#define KILLSUGG(ep) /* NULL */
#endif !USERSUGG

#ifdef RECORDING
	if (ep->newmacro && cmd != USEMACRO && cmd != SAVEMACRO) {
		buf[0] = cmd;
		buf[1] = 0;
		concato(&ep->newmacro, buf);
	}
#endif RECORDING
	ep->spflag = No;

	switch (cmd) {

#ifdef RECORDING
	case SAVEMACRO:
		ep->spflag = spflag;
		if (ep->newmacro) { /* End definition */
			release(ep->oldmacro);
			if (ep->newmacro && Length(ep->newmacro) > 0) {
				ep->oldmacro = ep->newmacro;
				message(REC_OK);
			}
			else {
				release(ep->newmacro);
				ep->oldmacro = Vnil;
			}
			ep->newmacro = Vnil;
		}
		else /* Start definition */
			ep->newmacro = mk_text("");
		return Yes;

	case USEMACRO:
		if (!ep->oldmacro || Length(ep->oldmacro) <= 0) {
			error(PLB_NOK);
			return No;
		}
		ep->spflag = spflag;
		cp = Str(ep->oldmacro);
		for (i = 0; i < Length(ep->oldmacro); ++i) {
			Ecopy(*ep, env);
			if (execute(ep, cp[i]&0377) && checkep(ep))
				Erelease(env);
			else {
				Erelease(*ep);
				Emove(env, *ep);
				if (!i)
					return No;
				error((char*)NULL); /* Just a bell */
				/* The error must be signalled here, because the overall
				   command (USEMACRO) succeeds, so the main loop
				   doesn't ring the bell; but we want to inform the
				   that not everything was done either. */
				return Yes;
			}
		}
		return Yes;
#endif RECORDING

#ifndef SMALLSYS
	case Ctl(_): /* 'Touch', i.e. set modified flag */
		ep->changed = Yes;
		return Yes;
#endif SMALLSYS

	case GOTO:
		ACCSUGG(ep);
#ifdef RECORDING
		if (ep->newmacro) {
			error(GOTO_REC);
			return No;
		}
#endif RECORDING
		return gotocursor(ep);

	case NEXT:
		ACCSUGG(ep);
		return next(ep);

	case PREVIOUS:
		ACCSUGG(ep);
		return previous(ep);

	case LEFTARROW:
		ACCSUGG(ep);
		return leftarrow(ep);

	case RITEARROW:
		ACCSUGG(ep);
		return ritearrow(ep);

	case WIDEN:
		ACCSUGG(ep);
		return widen(ep);

	case EXTEND:
		ACCSUGG(ep);
		return extend(ep);

	case NARROW:
		ACCSUGG(ep);
		return narrow(ep);

	case RNARROW:
		ACCSUGG(ep);
		return rnarrow(ep);

	case UPARROW:
		ACCSUGG(ep);
		return uparrow(ep);

	case DOWNARROW:
		ACCSUGG(ep);
		return downarrow(ep);

	case UPLINE:
		ACCSUGG(ep);
		return upline(ep);

	case DOWNLINE:
		ACCSUGG(ep);
		return downline(ep);

	case COPY:
		ACCSUGG(ep);
		ep->spflag = spflag;
		return copyinout(ep);

	case DELETE:
		ACCSUGG(ep);
		return delete(ep);

	case ACCEPT:
		ACCSUGG(ep);
		return accept(ep);

	default:
		if (!isascii(cmd) || !isprint(cmd))
			return No;
		ep->spflag = spflag;
		return ins_char(ep, cmd, islower(cmd) ? toupper(cmd) : -1);

	case ' ':
		ep->spflag = spflag;
		return ins_char(ep, ' ', -1);

	case RETURN:
	case NEWLINE:
		KILLSUGG(ep);
		return ins_newline(ep);
	}
}


/*
 * Initialize an environment variable.  Most things are set to 0 or NULL.
 */

Hidden Procedure
clrenv(ep)
	environ *ep;
{
	ep->focus = newpath(Pnil, gram(Optional), 1);
	ep->mode = WHOLE;
	ep->copyflag = ep->spflag = ep->changed = No;
	ep->s1 = ep->s2 = ep->s3 = 0;
	ep->highest = Maxintlet;
	ep->copybuffer = Vnil;
#ifdef RECORDING
	ep->oldmacro = ep->newmacro = Vnil;
#endif RECORDING
	ep->generation = 0;
	ep->changed = No;
}


/*
 * Save parse tree and copy buffer.
 */

Visible Procedure
enddemo()
{
	register environ *ep = tobesaved;

	tobesaved = (environ*)NULL;
		/* To avoid loops if saving is interrupted. */
	if (savewhere && ep) {
		if (ep->generation > 0) {
			save(ep->focus, savewhere);
#ifdef USERSUGG
			writesugg(ep->focus);
#endif USERSUGG
		}
#ifdef SAVEBUF
		if (ep->copyflag)
			savequeue(ep->copybuffer, copysavefile);
		else
			savequeue(Vnil, copysavefile);
#endif SAVEBUF
#ifdef SAVEPOS
		savepos(savewhere, lineno(ep)+1);
#endif SAVEPOS
	}
#ifdef BTOP
	waitchild();
#endif BTOP
}


/*
 * Find out if the current position is higher in the tree
 * than `ever' before (as remembered in ep->highest).
 * The algorithm of pathlength() is repeated here to gain
 * some efficiency by stopping as soon as it is clear
 * no change can occur.
 * (Higher() is called VERY often, so this pays).
 */

Visible Procedure
higher(ep)
	register environ *ep;
{
	register path p = ep->focus;
	register int pl = 0;
	register int max = ep->highest;

	while (p) {
		++pl;
		if (pl >= max)
			return;
		p = parent(p);
	}
	ep->highest = pl;
}


/*
 * Issue debug status message.
 */

Visible Procedure
dbmess(ep)
	register environ *ep;
{
#ifndef SMALLSYS
	char stuff[80];
	register string str = stuff;

	switch (ep->mode) {
	case VHOLE:
		sprintf(stuff, "VHOLE:%d.%d", ep->s1, ep->s2);
		break;
	case FHOLE:
		sprintf(stuff, "FHOLE:%d.%d", ep->s1, ep->s2);
		break;
	case ATBEGIN:
		str = "ATBEGIN";
		break;
	case ATEND:
		str = "ATEND";
		break;
	case WHOLE:
		str = "WHOLE";
		break;
	case SUBRANGE:
		sprintf(stuff, "SUBRANGE:%d.%d-%d", ep->s1, ep->s2, ep->s3);
		break;
	case SUBSET:
		sprintf(stuff, "SUBSET:%d-%d", ep->s1, ep->s2);
		break;
	case SUBLIST:
		sprintf(stuff, "SUBLIST...%d", ep->s3);
		break;
	default:
		sprintf(stuff, "UNKNOWN:%d,%d,%d,%d",
			ep->mode, ep->s1, ep->s2, ep->s3);
	}
	message(
#ifdef SAVEBUF
		"%s, %s, wi=%d, hi=%d, (y,x,l)=(%d,%d,%d) %s",
		symname(symbol(tree(ep->focus))),
#else !SAVEBUF
		"%d, %s, wi=%d, hi=%d, (y,x,l)=(%d,%d,%d) %s",
		symbol(tree(ep->focus)),
#endif SAVEBUF
		str, width(tree(ep->focus)), ep->highest,
		Ycoord(ep->focus), Xcoord(ep->focus), Level(ep->focus),
			ep->spflag ? "spflag on" : "");
#endif !SMALLSYS
}

#ifndef SMALLSYS

Hidden bool
canexit(ep)
	environ *ep;
{
	environ env;

	shrink(ep);
	if (ishole(ep))
		delete(ep);
	Ecopy(*ep, env);
	top(&ep->focus);
	higher(ep);
	ep->mode = WHOLE;
	if (findhole(&ep->focus)) {
		Erelease(env);
		error(EXIT_HOLES); /* There are holes left */
		return No;
	}
	Erelease(*ep);
	Emove(env, *ep);
	return Yes;
}


Hidden bool
findhole(pp)
	register path *pp;
{
	register node n = tree(*pp);

	if (Type(n) == Tex)
		return No;
	if (symbol(n) == Hole)
		return Yes;
	if (!down(pp))
		return No;
	for (;;) {
		if (findhole(pp))
			return Yes;
		if (!rite(pp))
			break;

	}
	up(pp) || Abort();
	return No;
}

#endif !SMALLSYS
