#ifndef lint
static char sccsid[] = "@(#)save.c	4.1 12/24/82";
#endif

#include	"mille.h"
#include	"unctrl.h"
#include	<sys/types.h>
#include	<sys/stat.h>
#include	<time.h>

typedef	struct stat	STAT;
typedef	struct tm	TIME;

char	*ctime();

int	read(), write();

/*
 *	This routine saves the current game for use at a later date
 */
extern int	errno;
extern char	*sys_errlist[];

save() {

	reg char	*sp;
	reg int		outf;
	reg TIME	*tp;
	char		buf[80];
	TIME		tme;
	STAT		junk;

	tp = &tme;
	if (Fromfile && getyn("Same file? "))
		strcpy(buf, Fromfile);
	else {
over:
		mvaddstr(MOVE_Y, MOVE_X, "file: ");
		clrtoeol();
		leaveok(Board, FALSE);
		refresh();
		sp = buf;
		while ((*sp = getch()) != '\n') {
			if (*sp == _tty.sg_kill)
				goto over;
			else if (*sp == _tty.sg_erase) {
				if (--sp < buf)
					sp = buf;
				else {
					addch('\b');
					/*
					 * if the previous char was a control
					 * char, cover up two characters.
					 */
					if (*sp < ' ')
						addch('\b');
					clrtoeol();
				}
			}
			else
				addstr(unctrl(*sp++));
			refresh();
		}
		*sp = '\0';
		leaveok(Board, TRUE);
	}

	/*
	 * check for existing files, and confirm overwrite if needed
	 */

	if (sp == buf || (!Fromfile && stat(buf, &junk) > -1
	    && getyn("Overwrite File? ") == FALSE))
		return FALSE;

	if ((outf = creat(buf, 0644)) < 0) {
		error(sys_errlist[errno]);
		return FALSE;
	}
	mvwaddstr(Score, ERR_Y, ERR_X, buf);
	wrefresh(Score);
	time(tp);			/* get current time		*/
	strcpy(buf, ctime(tp));
	for (sp = buf; *sp != '\n'; sp++)
		continue;
	*sp = '\0';
	varpush(outf, write);
	close(outf);
	wprintw(Score, " [%s]", buf);
	wclrtoeol(Score);
	wrefresh(Score);
	return TRUE;
}

/*
 *	This does the actual restoring.  It returns TRUE if the
 * backup was made on exiting, in which case certain things must
 * be cleaned up before the game starts.
 */
rest_f(file)
reg char	*file; {

	reg char	*sp;
	reg int		inf;
	char		buf[80];
	STAT		sbuf;

	if ((inf = open(file, 0)) < 0) {
		perror(file);
		exit(1);
	}
	if (fstat(inf, &sbuf) < 0) {		/* get file stats	*/
		perror(file);
		exit(1);
	}
	varpush(inf, read);
	close(inf);
	strcpy(buf, ctime(&sbuf.st_mtime));
	for (sp = buf; *sp != '\n'; sp++)
		continue;
	*sp = '\0';
	/*
	 * initialize some necessary values
	 */
	sprintf(Initstr, "%s [%s]\n", file, buf);
	Fromfile = file;
	return !On_exit;
}
