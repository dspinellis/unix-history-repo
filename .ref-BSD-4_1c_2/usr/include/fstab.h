/*
 * File system table, see fstab (5)
 *
 * Used by dump, mount, umount, swapon, fsck, df, ...
 *
 * The fs_spec field is the block special name.
 * Programs that want to use the character special name must
 * create that name by prepending a 'r' after the right most slash.
 */

#define	FSTAB		"/etc/fstab"
#define	FSNMLG		32

#define	FSTABFMT	"%32s:%32s:%2s:%d:%d\n"
#define	FSTABARG(p)	(p)->fs_spec, (p)->fs_file, \
			(p)->fs_type, &(p)->fs_freq, &(p)->fs_passno
#define FSTABNARGS	5

#define	FSTAB_RW	"rw"	/* read write device */
#define	FSTAB_RO	"ro"	/* read only device */
#define	FSTAB_SW	"sw"	/* swap device */
#define	FSTAB_XX	"xx"	/* ignore totally */

struct	fstab{
	char	fs_spec[FSNMLG];	/* block special device name */
	char	fs_file[FSNMLG];	/* file system path prefix */
	char	fs_type[3];		/* rw,ro,sw or xx */
	int	fs_freq;		/* dump frequency, in days */
	int	fs_passno;		/* pass number on parallel dump */
};

struct	fstab *getfsent();
struct	fstab *getfsspec();
struct	fstab *getfsfile();
int	setfsent();
int	endfsent();
