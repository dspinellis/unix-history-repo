/*-
 * Copyright (c) 1979, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#ifndef lint
static char sccsid[] = "@(#)GETNAME.c	8.1 (Berkeley) 6/6/93";
#endif /* not lint */

#include "h00vars.h"
#include "libpc.h"

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

static char *tmpname = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";

struct iorec *
GETNAME(filep, name, namlim, datasize)

	register struct iorec	*filep;
	char			*name;
	long			namlim;
	long			datasize;
{
	int		maxnamlen = namlim;
	struct iorec	*prev;
	struct iorec	*next;
	register int	cnt;
	struct iorec	locvar;

	if (filep->fblk < MAXFILES && _actfile[filep->fblk] == filep) {
		/* 
		 * Close and immediately reactivate the file.
		 */
		PFCLOSE(filep, name != NULL);
		_actfile[filep->fblk] = filep;
		filep->funit &= (TEMP | FTEXT);
	} else {
		/*
		 * Initialize a new file record.
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
		*filep->fname = NULL;
		/*
		 * Check to see if file is global, or allocated in
		 * the stack by checking its address against the
		 * address of one of our routine's local variables.
		 */
		if (filep < &locvar)
			filep->flev = GLVL;
		else
			filep->flev = filep;
		for (_filefre++; _filefre < MAXFILES; _filefre++)
			if (_actfile[_filefre] == FILNIL)
				goto gotone;
		for (_filefre = PREDEF + 1; _filefre < MAXFILES; _filefre++)
			if (_actfile[_filefre] == FILNIL)
				goto gotone;
		ERROR("File table overflow\n");
		return;
gotone:
		filep->fblk = _filefre;
		_actfile[_filefre] = filep;
		/*
		 * Link the new record into the file chain.
		 */
		prev = (struct iorec *)&_fchain;
		next = _fchain.fchain;
		while (filep->flev > next->flev) {
			prev = next;
			next = next->fchain;
		}
		if (filep->flev == GLVL)
			/*
			 * Must order global files so that all dynamic files
			 * within a record are grouped together.
			 */
			while ((next != FILNIL) &&
			       (next->flev == GLVL) &&
			       ((struct iorec *)filep > next)) {
				prev = next;
				next = next->fchain;
			}
		filep->fchain = next;
		prev->fchain = filep;
	}
	/*
	 * Get the filename associated with the buffer.
	 */
	if (name == NULL) {
		if (*filep->fname != NULL) {
			return(filep);
		}
		/*
		 * No name given and no previous name, so generate
		 * a new one of the form #tmp.xxxxxx.
		 */
		filep->funit |= TEMP;
		sprintf(filep->fname, "#tmp.%c%d", tmpname[filep->fblk],
		    getpid());
		filep->pfname = &filep->fname[0];
		return(filep);
	}
	/*
	 * Trim trailing blanks, and insure that the name 
	 * will fit into the file structure.
	 */
	for (cnt = 0; cnt < maxnamlen; cnt++)
		if (name[cnt] == '\0' || name[cnt] == ' ')
			break;
	if (cnt >= NAMSIZ) {
		ERROR("%s: File name too long\n", name);
		return;
	}
	maxnamlen = cnt;
	filep->funit &= ~TEMP;
	/*
	 * Put the new name into the structure.
	 */
	for (cnt = 0; cnt < maxnamlen; cnt++)
		filep->fname[cnt] = name[cnt];
	filep->fname[cnt] = '\0';
	filep->pfname = &filep->fname[0];
	return(filep);
}
