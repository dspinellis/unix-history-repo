/* These are routines that are used in various places within libg++, which
   do not actually appear within curses anywhere.  */

struct	_kb_st
{
	int	_id;
	unsigned char	_flags;
};

struct _kb_st * stdkb __asm("_$$PsectAttributes_NOSHR$$stdkb");

extern int stdscr; /* This is the wrong type, but it does not matter.  */

mvcur(int ly,int lx,int ny,int nx)
{
	wmove (stdscr, ny, nx);
}

_setecho(int i)
{
	if(i) stdkb->_flags &= ~1;
	else stdkb->_flags |= 1;
}

_setnonl(int i)
{
	if(i) stdkb->_flags &= ~2;
	else stdkb->_flags |= 2;
}


cbreak()
{
	stdkb->_flags &= ~4;
	stdkb->_flags |= 2;
}

nocbreak()
{
	stdkb->_flags |= 4;
}
