#!/bin/sh -
#
# Copyright (c) 1992 The Regents of the University of California.
# All rights reserved.
#
# %sccs.include.redist.sh%
#
#	@(#)vnode_if.sh	7.1 (Berkeley) %G%
#

# Script to produce VFS front-end sugar.

cat << END_OF_LEADING_COMMENT
/*
 * This file is produced by the script /sys/kern/vnode_if.sh.
 * Do not modify anything in here by hand.
 *
 *	@(#)vnode_if.sh	7.1 (Berkeley) %G%
 */
extern struct vnodeop_desc vop_default_desc;
END_OF_LEADING_COMMENT

# Awk script to take vnode_if.src and turn it into vnode_if.h.
#
# This script is not particularly well written, it figures out the
# same stuff repeatedly.  Feel free to fix it.  Note, it uses nawk
# extensions.

awk '
	NF == 0 || $0 ~ "^#" {
		next;
	}
	{
		# get the function name
		name = $1;
		uname = toupper(name);

		# get the function arguments
		for (c1 = 0;; ++c1) {
			if (getline <= 0)
				exit
			if ($0 ~ "^};")
				break;
			a[c1] = $0;
		}

		# print out the vop_F_args structure
		printf("struct %s_args {\n\tstruct vnodeop_desc *a_desc;\n",
		    name);
		for (c2 = 0; c2 < c1; ++c2) {
			c3 = split(a[c2], t);
			printf("\t");
			for (c4 = 2; c4 < c3; ++c4)
				printf("%s ", t[c4]);
			beg = match(t[c3], "[^*]");
			printf("%sa_%s\n",
			    substr(t[c4], 0, beg - 1), substr(t[c4], beg));
		}
		printf("};\n");

		# print out extern declaration
		printf("extern struct vnodeop_desc %s_desc;\n", name);

		# print out inline struct
		printf("static inline int %s(", uname);
		sep = ", ";
		for (c2 = 0; c2 < c1; ++c2) {
			if (c2 == c1 - 1)
				sep = ")\n";
			c3 = split(a[c2], t);
			beg = match(t[c3], "[^*]");
			end = match(t[c3], ";");
			printf("%s%s", substr(t[c3], beg, end - beg), sep);
		}
		for (c2 = 0; c2 < c1; ++c2) {
			c3 = split(a[c2], t);
			printf("\t");
			for (c4 = 2; c4 < c3; ++c4)
				printf("%s ", t[c4]);
			beg = match(t[c3], "[^*]");
			printf("%s%s\n",
			    substr(t[c4], 0, beg - 1), substr(t[c4], beg));
		}
		printf("{\n\tstruct %s_args a;\n\n", name);
		printf("\ta.a_desc = VDESC(%s);\n", name);
		for (c2 = 0; c2 < c1; ++c2) {
			c3 = split(a[c2], t);
			printf("\t");
			beg = match(t[c3], "[^*]");
			end = match(t[c3], ";");
			printf("a.a_%s = %s\n",
			    substr(t[c3], beg, end - beg), substr(t[c3], beg));
		}
		c1 = split(a[0], t);
		beg = match(t[c1], "[^*]");
		end = match(t[c1], ";");
		printf("\treturn (VCALL(%s, VOFFSET(%s), &a));\n}\n",
		    substr(t[c1], beg, end - beg), name);
	}'

# THINGS THAT DON'T WORK RIGHT YET.
# 
# Two existing BSD vnodeops (bwrite and strategy) don't take any vnodes as
# arguments.  This means that these operations can't function successfully
# through a bypass routine.
#
# Bwrite and strategy will be replaced when the VM page/buffer cache
# integration happens.
#
# To get around this problem for now we handle these ops as special cases.

cat << END_OF_SPECIAL_CASES
#include <sys/buf.h>
struct vop_strategy_args {
	struct vnodeop_desc *a_desc;
	struct buf *a_bp;
};
extern struct vnodeop_desc vop_strategy_desc;
static inline int VOP_STRATEGY(bp)
	struct buf *bp;
{
	struct vop_strategy_args a;

	a.a_desc = VDESC(vop_strategy);
	a.a_bp = bp;
	return (VCALL((bp)->b_vp, VOFFSET(vop_strategy), &a));
}

struct vop_bwrite_args {
	struct vnodeop_desc *a_desc;
	struct buf *a_bp;
};
extern struct vnodeop_desc vop_bwrite_desc;
static inline int VOP_BWRITE(bp)
	struct buf *bp;
{
	struct vop_bwrite_args a;

	a.a_desc = VDESC(vop_bwrite);
	a.a_bp = bp;
	return (VCALL((bp)->b_vp, VOFFSET(vop_bwrite), &a));
}
END_OF_SPECIAL_CASES
