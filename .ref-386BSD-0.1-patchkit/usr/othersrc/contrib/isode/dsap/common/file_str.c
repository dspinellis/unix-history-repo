#include "quipu/util.h"
#include "quipu/attrvalue.h"
#ifdef	CHECK_FILE_ATTRIBUTES
#include <sys/stat.h>
#endif

extern LLog * log_dsap;
extern char dsa_mode;

str2file_aux (str,at,x)
char * str;
AttributeType at;
AttributeValue x;
{
struct file_syntax * fs;
#ifdef CHECK_FILE_ATTRIBUTES
struct stat filestat;
#endif

	x->av_syntax = AV_FILE;
	fs = (struct file_syntax *) smalloc (sizeof(struct file_syntax));
	fs->fs_ref = 1;
	fs->fs_attr = NULLAttrV;
	x->av_struct = (caddr_t) fs;
	if ((fs->fs_real_syntax = at->oa_syntax) >= AV_WRITE_FILE)
		fs->fs_real_syntax -= AV_WRITE_FILE; 

	if (*str != 0) {
		fs->fs_mode = 0;
		fs->fs_name = strdup (str);
	} else {
		if (dflt_attr_file (at,x,1) == NOTOK) 
			return (NOTOK);
		fs->fs_mode = FS_DEFAULT;
	}

#ifdef CHECK_FILE_ATTRIBUTES
	/* now check the file is OK */
	if (stat(fs->fs_name,&filestat) != -1) 
		return (OK);
	else {
		parse_error ("can't find attribute file %s",fs->fs_name);
		return (NOTOK);
	}
	/* NOTREACHED */
#else
	return (OK);
#endif

}

AttributeValue str2file (str,at)
char * str;
AttributeType at;
{
AttributeValue x;

	x = AttrV_alloc();
	if (str2file_aux (str,at,x) == OK)
		return (x);
	free ((char *)x);
	return (NULLAttrV);
}

file_free (fs)
struct file_syntax * fs;
{
	if (fs == (struct file_syntax *) NULL)
		return;

	fs->fs_ref--;

	if (fs->fs_ref > 0)
		return;

	if (fs->fs_attr) 
		AttrV_free (fs->fs_attr);

	if ( !dsa_mode && (fs->fs_mode & FS_CREATE)) {
		(void) unlink (fs->fs_name);
		DLOG (log_dsap,LLOG_DEBUG,("Removed photo file '%s'",fs->fs_name));
	}

	if (fs->fs_name)
		free (fs->fs_name);
		
	free ((char *) fs);
}
