#include "ex.h"
/*
 * Ex - a text editor
 * Bill Joy UCB June/September 1977
 *
 * Pointers for lines deleted by the last buffer editing
 * command are salted away between dol and unddol.  Unddel
 * marks the line which was before this group prior to the last
 * command.  If undap1 != undap2 then the lines from undap1
 * to undap2-1 were appended by the last command.
 */

undo(c)
	char c;
{
	register int i, *jp, *kp;
	int *dolp1, *newdol, *newadot;

	if (inglobal)
		error("Can't undo in global@commands");
	if (!c)
		somechange();
	change();
	if (undkind == UNDMOVE) {
 		/*
		 * Command to be undone is a move command.
		 * This is handled as a special case by noting that
		 * a move "a,b m c" can be inverted by another move.
		 */
		if ((i = (jp = unddel) - undap2) > 0) {
			/*
			 * when c > b inverse is a+(c-b),c m a-1
			 */
			addr2 = jp;
			addr1 = (jp = undap1) + i;
			unddel = jp-1;
		} else {
			/*
			 * when b > c inverse is  c+1,c+1+(b-a) m b
			 */
			addr1 = ++jp;
			addr2 = jp + ((unddel = undap2) - undap1);
		}
		kp = undap1;
		move1(0, unddel);
		dot = kp;
		Command = "move";
		killed();
	} else {
		int cnt;

		newadot = dot;
		cnt = dol - zero;
		newdol = dol;
		dolp1 = dol + 1;
		/*
		 * Command to be undone is a non-move.
		 * All such commands are treated as a combination of
		 * a delete command and a append command.
		 * We first move the lines appended by the last command
		 * from undap1 to undap2-1 so that they are just before the
		 * saved deleted lines.
		 */
		if ((i = (kp = undap2) - (jp = undap1)) > 0) {
			reverse(jp, kp);
			reverse(kp, dolp1);
			reverse(jp, dolp1);
			/*
			 * Account for possible backward motion of target
			 * for restoration of saved deleted lines.
			 */
			if (unddel >= jp)
				unddel =- i;
			newdol =- i;
			/*
			 * For the case where no lines are restored, dot
			 * is the line before the first line deleted.
			 */
			dot = jp-1;
		}
		/*
		 * Now put the deleted lines, if any, back where they were.
		 * Basic operation is: dol+1,unddol m unddel
		 */
		jp = unddel + 1;
		if ((i = (kp = unddol) - dol) > 0) {
			if (jp != dolp1) {
				reverse(jp, dolp1);
				reverse(dolp1, ++kp);
				reverse(jp, kp);
			}
			/*
			 * Account for possible forward motion of the target
			 * for restoration of the deleted lines.
			 */
			if (undap1 >= jp)
				undap1 =+ i;
			/*
			 * Dot is the first resurrected line.
			 */
			dot = jp;
			newdol =+ i;
		}
		/*
		 * Clean up so we are invertible
		 */
		unddel = undap1 - 1;
		undap1 = jp;
		undap2 = jp + i;
		dol = newdol;
		netchHAD(cnt);
		if (undkind == UNDALL) {
			dot = undadot;
			undadot = newadot;
		}
	}
	if (dot == zero && dot != dol)
		dot = one;
}

somechange()
{
	register int *ip, *jp;

	switch (undkind) {
		case UNDMOVE:
			return;
		case UNDCHANGE:
			if (undap1 == undap2 && dol == unddol)
				break;
			return;
		case UNDALL:
			if (unddol - dol != dol - zero)
				return;
			for (ip = one, jp = dol + 1; ip <= dol; ip++, jp++)
				if ((*ip &~ 01) != (*jp &~ 01))
					return;
			break;
		case UNDNONE:
			error("Nothing to undo");
	}
	error("Nothing changed|Last undoable command didn't change anything");
}
