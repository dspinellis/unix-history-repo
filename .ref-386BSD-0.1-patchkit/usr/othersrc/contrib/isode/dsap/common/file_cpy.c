#include "quipu/util.h"
#include "quipu/attrvalue.h"

struct file_syntax * fileattr_cpy (fs)
struct file_syntax * fs;
{
	fs->fs_ref++;
	return (fs);
}

file_cmp (a,b)
struct file_syntax *a, *b;
{
	if ((a->fs_attr != NULLAttrV) && (b->fs_attr != NULLAttrV))
		return (AttrV_cmp (a->fs_attr,b->fs_attr));

	/* just compare file name for now */
	if ((a->fs_name == NULLCP) || (b->fs_name == NULLCP)) {
		if (a->fs_mode & FS_DEFAULT)
			if (b->fs_mode & FS_DEFAULT)
				return 0;
		return (2);
	}

	return (pstrcmp (a->fs_name,b->fs_name));
}
