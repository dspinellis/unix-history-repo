/*  $Revision: 1.8 $
**
**  InterNetNews replacement for C news system locking.
*/
#include <stdio.h>

static char	COMMAND[] = "ctlinnd %s Expire process %ld";
static int	IsLocked;


/*
**  C News debugging function.
/* ARGSUSED */
void
lockdebug(state)
    int		state;
{
}


/*
**  Lock the news system by telling the server to throttle input.
*/
void
newslock()
{
    char	buff[72];
    int		i;

    (void)sprintf(buff, COMMAND, "throttle", (long)getpid());
    i = system(buff) >> 8;
    if (i)
	error("Can't lock");
    IsLocked = 1;
}


/*
**  Unlock the system and reload the files.
*/
void
newsunlock()
{
    char	buff[72];
    int		i;

    if (IsLocked) {
	(void)sprintf(buff, COMMAND, "go", (long)getpid());
	i = system(buff) >> 8;
	if (i)
	    error("Can't reload");
	(void)sprintf(buff, "ctlinnd go");
	i = system(buff) >> 8;
	if (i)
	    error("Can't unlock");
	IsLocked = 0;
    }
}


/*
**  Print an error message, then unlock the system.
*/
void
errunlock(text, arg)
    char	*text;
    char	*arg;
{
    warning(text, arg);
    newsunlock();
    exit(1);
    /* NOTREACHED */
}
