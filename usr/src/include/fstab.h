/*	fstab.h	4.2	83/05/19	*/

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

#define	FSTAB_RW	"rw"	/* read write device */
#define	FSTAB_RO	"ro"	/* read only device */
#define	FSTAB_SW	"sw"	/* swap device */
#define	FSTAB_XX	"xx"	/* ignore totally */

struct	fstab{
	char	*fs_spec;		/* block special device name */
	char	*fs_file;		/* file system path prefix */
	char	*fs_type;		/* rw,ro,sw or xx */
	char	*fs_quotafile;		/* name of quota file if used */
	int	fs_freq;		/* dump frequency, in days */
	int	fs_passno;		/* pass number on parallel dump */
};

struct	fstab *getfsent();
struct	fstab *getfsspec();
struct	fstab *getfsfile();
int	setfsent();
int	endfsent();
