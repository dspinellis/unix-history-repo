#include "ex.h"
#include "ex_io.h"
/*
 * Ex - a text editor
 * Bill Joy UCB June 1977
 */

wop()
{
	register c;
	register exclam;
	register int bl;
	struct stb stbuf;

	c = 0;
	exclam = 0;
	if (peekchar() == '!') {
		exclam++;
		getchar();
	}
	bl = skipwh();
	while (peekchar() == '>') {
		getchar();
		c++;
		skipwh();
	}
	if (c == 0 && bl == 0 && !endcmd(peekchar()))
		error("Blank required@before filename");
	if (c != 0 && c != 2)
		error("Write forms are 'w' and 'w>>'@- use 'w!' and 'w!>>' to override checks");
	filename('w');
	switch (c) {
		case 0:
			if (exclam)
				goto cre;
			if (value(EDITED) && eq(file, savedfile)) {
				if (addr1 != one || addr2 != dol)
					error(" Can't 'w' partial buffer to edited file@- use 'w!' to override checks");
				goto cre;
			}
			if (stat(file, &stbuf))
				goto cre;
			if ((stbuf.flags & FILETYP) == FCHSPEC) {
				if (stbuf.dmajor == DTTYMAJ && stbuf.dminor == DTTYMIN)
					goto cre;
				if (stbuf.dmajor == DVNLMAJ && stbuf.dminor == DVNLMIN)
					goto cre;
			}
			io = open(file, 1);
			if (io < 0)
				ioerror();
			if (gTTY(io) != 0)
				error(" File exists@- use \"write! %s\" if you really want to overwrite it", file);
			close(io);
cre:
			synctmp();
			io = creat(file, value(MODE));
			if (io < 0)
				ioerror();
			break;
		case 2:
			io = open(file, 1);
			if (io < 0 || seek(io, 0, 2)) {
				if (exclam /* || value(WRITEANY) */)
					goto cre;
				ioerror();
			}
			break;
	}
	putfile();
	iostats();
	if (c != 2 && addr1 == one && addr2 == dol) {
		if (eq(file, savedfile))
			value(EDITED) = 1;
		sync();
	}
}
