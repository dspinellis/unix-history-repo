#include "ex.h"
#include "ex_io.h"
/*
 * Ex - a text editor
 * Bill Joy UCB June 1977
 */

rop(c)
	char c;
{
	register int i;
	struct stb stbuf;
	int magic;

	io = open(file, 0);
	if (io < 0) {
		if (c == 'e' & errno == ENOENT)
			value(EDITED)++;
		ioerror();
	}
	if (value(EDITANY) == 0) {
		if (fstat(io, &stbuf))
			ioerror();
		switch (stbuf.flags & FILETYP) {
			case FBLSPEC:
				error(" Block special file");
			case FCHSPEC:
				if (gTTY(io) == 0)
					error(" Teletype");
				if (stbuf.dmajor == DVNLMAJ && stbuf.dminor == DVNLMIN)
					break;
				error(" Character special file");
			case FDIRECT:
				error(" Directory");
			case FPLAIN:
				i = read(io, &magic, 2);
				seek(io, 0, 0);
				if (i != 2)
					break;
				switch (magic) {
					case 0404:
						error(" Pascal object");
					case 0405:
						error(" Overlay executable");
					case 0407:
						error(" Executable");
					case 0410:
						error(" Pure executbable");
					case 0411:
						error(" Separate executable");
					case 0177545:
						error(" Archive");
					case 0177555:
						error(" Old archive");
					default:
						if (magic & 0100200)
							error(" Non-ascii file");
						break;
				}
		}
	}
	if (c == 'r')
		setdot();
	else
		setall();
	rop2();
	rop3(c);
}

rop2()
{

	deletenone();
	clrstats();
	append(getfile, addr2);
}

rop3(c)
	char c;
{

	if (iostats() == 0)
		if (c == 'e')
			value(EDITED)++;
	if (c == 'e')
		undkind = UNDNONE;
	if (laste) {
		laste = 0;
		sync();
	}
}
