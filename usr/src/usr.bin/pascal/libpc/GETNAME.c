/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)GETNAME.c 1.2 %G%";

#include "h00vars.h"
#include "h01errs.h"

/*
 * GETNAME - activate a file
 *
 * takes a name, name length, element size, and variable
 * level and returns a pointer to a file structure.
 *
 * a new file structure is initialized if necessary.
 * temporary names are generated, and given
 * names are blank trimmed.
 */

struct iorec *
GETNAME(filep, name, maxnamlen, datasize)

	register struct iorec	*filep;
	char			*name;
	int			maxnamlen;
	int			datasize;
{
	struct iorec	*prev;
	struct iorec	*next;
	register int	cnt;
	struct iorec	locvar;
	extern char	*mktemp();

	if (filep->fblk >= MAXFILES || _actfile[filep->fblk] != filep) {
		/*
		 * initialize a new filerecord
		 */
		filep->funit = 0;
		if (datasize == 0) {
			filep->funit |= FTEXT;
			datasize = 1;
		}
		filep->fsize = datasize;
		filep->fbuf = 0;
		filep->lcount = 0;
		filep->llimit = 0x7fffffff;
		filep->fileptr = &filep->window[0];
		/*
		 * check to see if file is global, or allocated in
		 * the stack by checking its address against the
		 * address of one of our routine's local variables.
		 */
		if (filep < &locvar)
			filep->flev = GLVL;
		else
			filep->flev = filep;
		do {
			if (++_filefre == MAXFILES)
				_filefre = PREDEF + 1;
		} while (_actfile[_filefre] != FILNIL);
		filep->fblk = _filefre;
		_actfile[_filefre] = filep;
		/*
		 * link the newrecord into the file chain
		 */
		prev = (struct iorec *)&_fchain;
		next = _fchain.fchain;
		while (filep->flev > next->flev) {
			prev = next;
			next = next->fchain;
		}
		filep->fchain = next;
		prev->fchain = filep;
	} else {
		if (filep->funit & FDEF) {
			filep->funit &= (TEMP | FTEXT);
		} else {
			/*
			 * have a previous buffer, close associated file
			 */
			fclose(filep->fbuf);
			if (ferror(filep->fbuf)) {
				ERROR(ECLOSE, filep->pfname);
				return;
			}
			/*
			 * renamed temporary files are discarded
			 */
			if ((filep->funit & TEMP) &&
			    (name != NULL) &&
			    (unlink(filep->pfname))) {
				ERROR(EREMOVE, filep->pfname);
				return;
			}
			filep->funit &= (TEMP | FTEXT);
		}
	}
	/*
	 * get the filename associated with the buffer
	 */
	if (name == NULL) {
		if (*filep->fname != NULL) {
			return(filep);
		}
		/*
		 * no name given and no previous name, so generate
		 * a new one of the form tmp.xxxxxx
		 */
		filep->funit |= TEMP;
		name = mktemp("tmp.XXXXXX");
		maxnamlen = 10;
	} else {
		/*
		 * trim trailing blanks, and insure that the name 
		 * will fit into the file structure
		 */
		for (cnt = 0; cnt < maxnamlen; cnt++)
			if (name[cnt] == '\0' || name[cnt] == ' ')
				break;
		if (cnt >= NAMSIZ) {
			ERROR(ENAMESIZE, name);
			return;
		}
		maxnamlen = cnt;
		filep->funit &= ~TEMP;
	}
	/*
	 * put the new name into the structure
	 */
	for (cnt = 0; cnt < maxnamlen; cnt++)
		filep->fname[cnt] = name[cnt];
	filep->fname[cnt] = '\0';
	filep->pfname = &filep->fname[0];
	return(filep);
}
