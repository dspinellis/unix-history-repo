#!/bin/sh -
#
# Copyright (c) 1992 The Regents of the University of California.
# All rights reserved.
#
# %sccs.include.redist.sh%
#
#	@(#)vnode_if.sh	7.2 (Berkeley) %G%
#

# Script to produce VFS front-end sugar.
#
# usage: vnode_if.sh srcfile
#	(where srcfile is currently /sys/kern/vnode_if.src)
#
# These awk scripts are not particularly well written, specifically they
# don't use arrays well and figure out the same information repeatedly.
# Please rewrite them if you actually understand how to use awk.  Note,
# they use nawk extensions.

if [ $# -ne 1 ] ; then
	printf 'usage: vnode_if.sh srcfile\n'
	exit 1
fi

# Name of the source file.
SRC=$1

# Names of the created files.
CFILE=vnode_if.c
HEADER=vnode_if.h

# Print out header information for vnode_if.h.
cat << END_OF_LEADING_COMMENT > $HEADER
/*
 * This file is produced by the script /sys/kern/vnode_if.sh.
 * Do not modify anything in here by hand.
 *
 *	@(#)vnode_if.sh	7.2 (Berkeley) %G%
 */

extern struct vnodeop_desc vop_default_desc;
END_OF_LEADING_COMMENT

# Awk script to take vnode_if.src and turn it into vnode_if.h.
awk '
	NF == 0 || $0 ~ "^#" {
		next;
	}
	{
		# Get the function name.
		name = $1;
		uname = toupper(name);

		# Get the function arguments.
		for (c1 = 0;; ++c1) {
			if (getline <= 0)
				exit
			if ($0 ~ "^};")
				break;
			a[c1] = $0;
		}

		# Print out the vop_F_args structure.
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

		# Print out extern declaration.
		printf("extern struct vnodeop_desc %s_desc;\n", name);

		# Print out inline struct.
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
	}' < $SRC >> $HEADER

# Print out header information for vnode_if.c.
cat << END_OF_LEADING_COMMENT > $CFILE
/*
 * This file is produced by the script /sys/kern/vnode_if.sh.
 * Do not modify anything in here by hand.
 *
 *	@(#)vnode_if.sh	7.2 (Berkeley) %G%
 */

#include <sys/param.h>
#include <sys/mount.h>
#include <sys/vnode.h>

struct vnodeop_desc vop_default_desc = {
	0,
	"default",
	0,
	NULL,
	VDESC_NO_OFFSET,
	VDESC_NO_OFFSET,
	VDESC_NO_OFFSET,
	NULL,
};

END_OF_LEADING_COMMENT

# Awk script to take vnode_if.src and turn it into vnode_if.c.
awk '
	NF == 0 || $0 ~ "^#" {
		next;
	}
	{
		# get the function name
		name = $1;

		# get the function arguments
		for (c1 = 0;; ++c1) {
			if (getline <= 0)
				exit
			if ($0 ~ "^};")
				break;
			a[c1] = $0;
		}

		# Print out the vop_F_vp_offsets structure.  This all depends
		# on naming conventions and nothing else.
		printf("int %s_vp_offsets[] = {\n", name);
		for (c2 = 0; c2 < c1; ++c2) {
			c3 = split(a[c2], t);
			if (c3 != 4 || t[1] !~ /^IN$/ || t[2] !~ /^struct$/ ||
			    t[3] !~ /^vnode$/ || t[4] !~ /.*vp;$/)
				continue;
			beg = match(t[c3], "[^*]");
			end = match(t[c3], ";");
			printf("\tVOPARG_OFFSETOF(struct %s_args, a_%s),\n",
			    name, substr(t[4], beg, end - beg));
		}
		printf("\tVDESC_NO_OFFSET\n};\n");

		# Print out the vnodeop_desc structure.
		printf("struct vnodeop_desc %s_desc = {\n", name);
		printf("\t0,\n\t\"%s\",\n\t0,\n\t%s_vp_offsets,\n", name, name);

		# Print out return vpp entry, if any.
		for (found = c2 = 0; c2 < c1; ++c2) {
			c3 = split(a[c2], t);
			if (c3 != 4 || t[1] !~ /^OUT$/ || t[2] !~ /^struct$/ ||
			    t[3] !~ /^vnode$/)
				continue;
			printf("\tVOPARG_OFFSETOF(struct %s_args, a_vpp),\n",
			    name);
			found = 1;
			break;
		}
		if (found == 0)
			printf("\tVDESC_NO_OFFSET,\n");

		# Print out cred entry, if any.
		for (found = c2 = 0; c2 < c1; ++c2) {
			c3 = split(a[c2], t);
			if (c3 != 4 || t[1] !~ /^IN$/ || t[2] !~ /^struct$/ ||
			    t[3] !~ /^ucred$/)
				continue;
			printf("\tVOPARG_OFFSETOF(struct %s_args, a_cred),\n",
			    name);
			found = 1;
			break;
		}
		if (found == 0)
			printf("\tVDESC_NO_OFFSET,\n");

		# Print out proc entry, if any.
		for (found = c2 = 0; c2 < c1; ++c2) {
			c3 = split(a[c2], t);
			if (c3 != 4 || t[1] !~ /^IN$/ || t[2] !~ /^struct$/ ||
			    t[3] !~ /^proc$/)
				continue;
			printf("\tVOPARG_OFFSETOF(struct %s_args, a_p),\n",
			    name);
			found = 1;
			break;
		}
		if (found == 0)
			printf("\tVDESC_NO_OFFSET,\n");
		printf("\tNULL,\n};\n");
	}' < $SRC >> $CFILE

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

cat << END_OF_SPECIAL_CASES >> $HEADER
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

cat << END_OF_SPECIAL_CASES >> $CFILE
int vop_strategy_vp_offsets[] = {
	VDESC_NO_OFFSET
};
struct vnodeop_desc vop_strategy_desc = {
	0,
	"vop_strategy",
	0,
	vop_strategy_vp_offsets,
	VDESC_NO_OFFSET,
	VDESC_NO_OFFSET,
	VDESC_NO_OFFSET,
	NULL,
};
int vop_bwrite_vp_offsets[] = {
	VDESC_NO_OFFSET
};
struct vnodeop_desc vop_bwrite_desc = {
	0,
	"vop_bwrite",
	0,
	vop_bwrite_vp_offsets,
	VDESC_NO_OFFSET,
	VDESC_NO_OFFSET,
	VDESC_NO_OFFSET,
	NULL,
};
END_OF_SPECIAL_CASES

# Add the vfs_op_descs array to the C file.
awk '
	BEGIN {
		printf("struct vnodeop_desc *vfs_op_descs[] = {\n");
		printf("\t&vop_default_desc,	/* MUST BE FIRST */\n");
		printf("\t&vop_strategy_desc,	/* XXX: SPECIAL CASE */\n");
		printf("\t&vop_bwrite_desc,	/* XXX: SPECIAL CASE */\n");
	}
	END {
		printf("\tNULL\n};\n");
	}
	NF == 0 || $0 ~ "^#" {
		next;
	}
	{
		# Get the function name.
		printf("\t&%s_desc,\n", $1);

		# Skip the function arguments.
		for (;;) {
			if (getline <= 0)
				exit
			if ($0 ~ "^};")
				break;
		}
	}' < $SRC >> $CFILE
