/* @(#)fstab.c	4.1 (Berkeley) 12/21/80 */
#include <fstab.h>
#include <stdio.h>
#include <ctype.h>

static	struct	fstab fs;
static	FILE	*fs_file = 0;

static	char	*fs_string(back, string, lg, end)
	char	*string, *back;
	int	lg;	/* length of field to stuff into */
	char	end;
{
	register	char	*cp;
	for (cp = string; *cp && *cp != end; cp++)
		continue;
	if (*cp == '\0') return(0);
	*cp = '\0';
	strncpy(back, string, lg-1);
	return(cp+1);
}
static char *fs_digit(backp, string, end)
	int	*backp;
	char	*string;
	char	end;
{
	register	int	value = 0;
	register	char	*cp;
	for (cp = string; *cp && isdigit(*cp); cp++){
		value *= 10;
		value += *cp - '0';
	}
	if (*cp == '\0') return(0);
	*backp = value;
	while ( *cp && *cp != end)
		cp++;
	if (*cp == '\0') return(0);
	return(cp+1);
}

static	int	fstabscan(fsp)
	struct	fstab *fsp;
{
	register	char	*cp;
	char	buf[256];
	if (fgets(buf, 256, fs_file) == NULL)
		return(EOF);
	cp = buf;
	cp = fs_string(&fsp->fs_spec[0], cp, FSNMLG, ':');
	if (cp == 0)	return(0);
	cp = fs_string(&fsp->fs_file[0], cp, FSNMLG, ':');
	if (cp == 0)	return(1);
	cp = fs_string(&fsp->fs_type[0], cp, 3, ':');
	if (cp == 0)	return(2);
	cp = fs_digit(&fsp->fs_freq, cp, ':');
	if (cp == 0)	return(3);
	cp = fs_digit(&fsp->fs_passno, cp, '\n');
	if (cp == 0)	return(4);
	return(5);
}
	
int	setfsent()
{
	if (fs_file)
		endfsent();
	if ( (fs_file = fopen(FSTAB, "r")) == NULL){
		fs_file = 0;
		return(0);
	}
	return(1);
}

int endfsent()
{
	if (fs_file){
		fclose(fs_file);
	}
	return(1);
}

struct fstab *getfsent()
{
	int	nfields;

	if ( (fs_file == 0) && (setfsent() == 0) )
		return(0);
	nfields = fstabscan(&fs);
	if (nfields == EOF || nfields != FSTABNARGS)
		return(0);
	return(&fs);
}
struct fstab *getfsspec(name)
	char	*name;
{
	register	struct	fstab *fsp;
	if (setfsent() == 0)	/* start from the beginning */
		return(0);
	while( (fsp = getfsent()) != 0){
		if (strncmp(fsp->fs_spec, name, sizeof(fsp->fs_spec)) == 0)
			return(fsp);
	}
	return(0);
}
struct fstab *getfsfile(name)
	char	*name;
{
	register	struct fstab *fsp;
	if (setfsent() == 0)	/* start from the beginning */
		return(0);
	while ( (fsp = getfsent()) != 0){
		if (strncmp(fsp->fs_file, name, sizeof(fsp->fs_spec)) == 0)
			return(fsp);
	}
	return(0);
}
