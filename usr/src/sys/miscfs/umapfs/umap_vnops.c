/*
 * Copyright (c) 1992 The Regents of the University of California
 * All rights reserved.
 *
 * This code is derived from the null layer of
 * John Heidemann from the UCLA Ficus project and
 * Jan-Simon Pendry's loopback file system.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)umap_vnops.c	1.1 (Berkeley) %G%
 *
 * Ancestors:
 *	@(#)lofs_vnops.c	1.2 (Berkeley) 6/18/92
 *	$Id: lofs_vnops.c,v 1.11 1992/05/30 10:05:43 jsp Exp jsp $
 *	...and...
 *	@(#)umap_vnodeops.c 1.20 92/07/07 UCLA Ficus project
 */

/*
 * Umap Layer
 *
 * The umap layer duplicates a portion of the file system
 * name space under a new name.  In this respect, it is
 * similar to the loopback file system.  It differs from
 * the loopback fs in two respects:  it is implemented using
 * a bypass operation, and it's "umap-node"s stack above
 * all lower-layer vnodes, not just over directory vnodes.
 *
 * The umap layer is the minimum file system layer,
 * simply bypassing all possible operations to the lower layer
 * for processing there.  All but vop_getattr, _inactive, _reclaim,
 * and _print are bypassed.
 *
 * Vop_getattr is not bypassed so that we can change the fsid being
 * returned.  Vop_{inactive,reclaim} are bypassed so that
 * they can handle freeing umap-layer specific data.
 * Vop_print is not bypassed for debugging.
 *
 *
 * INVOKING OPERATIONS ON LOWER LAYERS
 *
 * NEEDSWORK: Describe methods to invoke operations on the lower layer
 * (bypass vs. VOP).
 *
 *
 * CREATING NEW FILESYSTEM LAYERS
 *
 * One of the easiest ways to construct new file system layers is to make
 * a copy of the umap layer, rename all files and variables, and
 * then begin modifing the copy.  Sed can be used to easily rename
 * all variables.
 *
 */

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/user.h>
#include <sys/proc.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/vnode.h>
#include <sys/mount.h>
#include <sys/namei.h>
#include <sys/malloc.h>
#include <sys/buf.h>
#include <umapfs/umap.h>


int umap_bug_bypass = 0;   /* for debugging: enables bypass printf'ing */

/*
 * This is the 10-Apr-92 bypass routine.
 *    This version has been optimized for speed, throwing away some
 * safety checks.  It should still always work, but it's not as
 * robust to programmer errors.
 *    Define SAFETY to include some error checking code.
 *
 * In general, we map all vnodes going down and unmap them on the way back.
 * As an exception to this, vnodes can be marked "unmapped" by setting
 * the Nth bit in operation's vdesc_flags.
 *
 * Also, some BSD vnode operations have the side effect of vrele'ing
 * their arguments.  With stacking, the reference counts are held
 * by the upper node, not the lower one, so we must handle these
 * side-effects here.  This is not of concern in Sun-derived systems
 * since there are no such side-effects.
 *
 * This makes the following assumptions:
 * - only one returned vpp
 * - no INOUT vpp's (Sun's vop_open has one of these)
 * - the vnode operation vector of the first vnode should be used
 *   to determine what implementation of the op should be invoked
 * - all mapped vnodes are of our vnode-type (NEEDSWORK:
 *   problems on rmdir'ing mount points and renaming?)
 */ 
int
umap_bypass(ap)
	struct vop_generic_args *ap;
{
	extern int (**umap_vnodeop_p)();  /* not extern, really "forward" */
	int *mapdata, nentries ;
	int *gmapdata, gnentries ;
	struct ucred **credpp,*credp, *savecredp, *saveucredp ;
	register struct vnode **this_vp_p;
	int error;
	struct vnode *old_vps[VDESC_MAX_VPS];
	struct vnode **vps_p[VDESC_MAX_VPS];
	struct vnode ***vppp;
	struct vnodeop_desc *descp = ap->a_desc;
	int reles, i;

	if (umap_bug_bypass)
		printf ("umap_bypass: %s\n", descp->vdesc_name);

#ifdef SAFETY
	/*
	 * We require at least one vp.
	 */
	if (descp->vdesc_vp_offsets==UMAP ||
	    descp->vdesc_vp_offsets[0]==VDESC_NO_OFFSET)
		panic ("umap_bypass: no vp's in map.\n");
#endif

	/*
	 * Map the vnodes going in.
	 * Later, we'll invoke the operation based on
	 * the first mapped vnode's operation vector.
	 */
	reles = descp->vdesc_flags;
	for (i=0; i<VDESC_MAX_VPS; reles>>=1, i++) {
		if (descp->vdesc_vp_offsets[i]==VDESC_NO_OFFSET)
			break;   /* bail out at end of list */
		vps_p[i] = this_vp_p = 
			VOPARG_OFFSETTO(struct vnode**,descp->vdesc_vp_offsets[i],ap);

		if (i == 0)
		{
			vp1 = *vps_p[0];
		}

		/*
		 * We're not guaranteed that any but the first vnode
		 * are of our type.  Check for and don't map any
		 * that aren't.
		 */

		if ((*this_vp_p)->v_op != umap_vnodeop_p) {
			old_vps[i] = UMAP;
		} else {
			old_vps[i] = *this_vp_p;
			*(vps_p[i]) = UMAPVPTOLOWERVP(*this_vp_p);
			if (reles & 1)
				VREF(*this_vp_p);
		};
			
	};

	/* Doctor the credentials.  (That's the purpose of this layer.) */

	if (descp->vdesc_cred_offset != VDESC_NO_OFFSET) {

		credpp = VOPARG_OFFSETTO(struct ucred**, 
		    descp->vdesc_cred_offset, ap);

		/* Save old values */

		savecredp = (*credpp);
		saveucredp = u.u_cred;
		(*credpp) = u.u_cred = crdup(savecredp);
		credp = *credpp;

		if (umap_bug_bypass && credp->cr_uid != 0 )
			printf("umap_bypass: user was %d, group %d\n", 
			    credp->cr_uid,credp->cr_gid);

		nentries =  MOUNTTOUMAPMOUNT(vp1->v_vfsp)->info_nentries;
		mapdata =  &(MOUNTTOUMAPMOUNT(vp1->v_vfsp)->info_mapdata[0][0]);
		gnentries =  MOUNTTOUMAPMOUNT(vp1->v_vfsp)->info_gnentries;
		gmapdata =  &(MOUNTTOUMAPMOUNT(vp1->v_vfsp)->info_gmapdata[0][0]);

		if (umap_bug_bypass && credp->cr_uid != 0 )
			printf("nentries = %d, gnentries = %d\n", nentries, 
			    gnentries);

		/* Map all ids in the credential structure. */

		umap_mapids(credp,mapdata,nentries,gmapdata,gnentries);

		if (umap_bug_bypass && credp->cr_uid != 0 )
			printf("umap_bypass: user now %d, group %d\n", 
			    credp->cr_uid,credp->cr_gid);
	}

	/*
	 * Call the operation on the lower layer
	 * with the modified argument structure.
	 */
	error = VCALL(*(vps_p[0]), descp->vdesc_offset, ap);

	/*
	 * Maintain the illusion of call-by-value
	 * by restoring vnodes in the argument structure
	 * to their original value.
	 */
	reles = descp->vdesc_flags;
	for (i=0; i<VDESC_MAX_VPS; reles>>=1, i++) {
		if (descp->vdesc_vp_offsets[i]==VDESC_NO_OFFSET)
			break;   /* bail out at end of list */
		if (old_vps[i]) {
			*(vps_p[i]) = old_vps[i];
			if (reles & 1)
				vrele(*(vps_p[i]));
		};
	};

	/*
	 * Map the possible out-going vpp
	 * (Assumes that the lower layer always returns
	 * a VREF'ed vpp unless it gets an error.)
	 */
	if (descp->vdesc_vpp_offset != VDESC_NO_OFFSET &&
	    !(descp->vdesc_flags & VDESC_NOMAP_VPP) &&
	    !error) {
		/*
		 * XXX - even though symlink has a vpp arg,
		 * it doesn't return a VREF'ed vpp in that
		 * field.  The vpp arg should become a vp
		 * arg.
		 */
		if (descp == VDESC(vop_symlink)) {
#ifdef UMAPFS_DIAGNOSTIC
			printf("umap_bypass (symlink), lowervp->usecount = %d\n", (**vppp)->v_usecount);
#endif
			return (error);
		};
		vppp=VOPARG_OFFSETTO(struct vnode***,
				 descp->vdesc_vpp_offset,ap);
		error = umap_node_create(old_vps[0]->v_mount, **vppp, *vppp);
	};

	/* 
	 * Free duplicate cred structure and restore old one.
	 */
	if (descp->vdesc_cred_offset != VDESC_NO_OFFSET) {
		if (umap_bug_bypass && credp->cr_uid != 0 )
		printf("umap_bypass: returning-user was %d\n", credp->cr_uid);

		crfree(credp);
		(*credpp) = savecredp;
		u.u_cred = saveucredp;
		if (umap_bug_bypass && (*credpp)->cr_uid != 0 )
		 	printf("umap_bypass: returning-user now %d\n\n", 
			    (*credpp)->cr_uid);
	}

	return (error);
}


/*
 *  We handle getattr to change the fsid.
 */
int
umap_getattr(ap)
	struct vop_getattr_args *ap;
{
	short uid, gid;
	int error, tmpid, *mapdata, nentries, *gmapdata, gnentries;

	if (error=umap_bypass(ap))
		return error;
	/* Requires that arguments be restored. */
	ap->a_vap->va_fsid = ap->a_vp->v_mount->mnt_stat.f_fsid.val[0];

	/* umap needs to map the uid and gid returned by a stat
		into the proper values for this site.  This involves
		finding the returned uid in the mapping information,
		translating it into the uid on the other end,
		and filling in the proper field in the vattr
		structure pointed to by ap->a_vap.  The group
		is easier, since currently all groups will be
		translate to the NULLGROUP. */

	/* Find entry in map */

	uid = ap->a_vap->va_uid;
	gid = ap->a_vap->va_gid;
	if (umap_bug_bypass)
		printf("umap_getattr: mapped uid = %d, mapped gid = %d\n",uid, 
		    gid);

	vp1p = VOPARG_OFFSETTO(struct vnode**,descp->vdesc_vp_offsets[0],ap);
	nentries =  MOUNTTOUMAPMOUNT((*vp1p)->v_vfsp)->info_nentries;
	mapdata =  &(MOUNTTOUMAPMOUNT((*vp1p)->v_vfsp)->info_mapdata[0][0]);
	gnentries =  MOUNTTOUMAPMOUNT((*vp1p)->v_vfsp)->info_gnentries;
	gmapdata =  &(MOUNTTOUMAPMOUNT((*vp1p)->v_vfsp)->info_gmapdata[0][0]);

	/* Reverse map the uid for the vnode.  Since it's a reverse
		map, we can't use umap_mapids() to do it. */

	tmpid = umap_reverse_findid(uid,mapdata,nentries);

	if (tmpid != -1 ) {

		ap->a_vap->va_uid = (uid_t)tmpid;
		if (umap_bug_bypass)
			printf("umap_getattr: original uid = %d\n",uid);
	} else 
		ap->a_vap->va_uid = (uid_t)NOBODY;

	/* Reverse map the gid for the vnode. */

	tmpid = umap_reverse_findid(gid,gmapdata,gnentries);

	if (tmpid != -1) {

		ap->a_vap->va_gid = (gid_t)tmpid;
		if (umap_bug_bypass)
			printf("umap_getattr: original gid = %d\n",gid);
	} else
		ap->a_vap->va_gid = (gid_t)NULLGROUP;
	
	return 0;
}

int
umap_inactive (ap)
	struct vop_inactive_args *ap;
{
#ifdef UMAPFS_DIAGNOSTIC
	printf("umap_inactive(ap->a_vp = %x->%x)\n", ap->a_vp, UMAPVPTOLOWERVP(ap->a_vp));
#endif
	/*
	 * Do nothing (and _don't_ bypass).
	 * Wait to vrele lowervp until reclaim,
	 * so that until then our umap_node is in the
	 * cache and reusable.
	 *
	 * NEEDSWORK: Someday, consider inactive'ing
	 * the lowervp and then trying to reactivate it
	 * like they do in the name lookup cache code.
	 * That's too much work for now.
	 */
	return 0;
}

int
umap_reclaim (ap)
	struct vop_reclaim_args *ap;
{
	struct vnode *targetvp;

	/*
	 * Note: at this point, ap->a_vp->v_op == dead_vnodeop_p.
	 */
#ifdef UMAPFS_DIAGNOSTIC
	printf("umap_reclaim(ap->a_vp = %x->%x)\n", ap->a_vp, UMAPVPTOLOWERVP(ap->a_vp));
#endif
	remque(VTOUMAP(ap->a_vp));	     /* NEEDSWORK: What? */
	vrele (UMAPVPTOLOWERVP(ap->a_vp));   /* release lower layer */
	FREE(ap->a_vp->v_data, M_TEMP);
	ap->a_vp->v_data = 0;
	return (0);
}

int
umap_bmap (ap)
	struct vop_bmap_args *ap;
{
#ifdef UMAPFS_DIAGNOSTIC
	printf("umap_bmap(ap->a_vp = %x->%x)\n", ap->a_vp, UMAPVPTOLOWERVP(ap->a_vp));
#endif

	return VOP_BMAP(UMAPVPTOLOWERVP(ap->a_vp), ap->a_bn, ap->a_vpp, ap->a_bnp);
}

int
umap_strategy (ap)
	struct vop_strategy_args *ap;
{
	int error;
	struct vnode *savedvp;

#ifdef UMAPFS_DIAGNOSTIC
	printf("umap_strategy(vp = %x->%x)\n", ap->a_bp->b_vp, UMAPVPTOLOWERVP(ap->a_bp->b_vp));
#endif

	savedvp = ap->a_bp->b_vp;

	error = VOP_STRATEGY(ap->a_bp);

	ap->a_bp->b_vp = savedvp;

	return error;
}


int
umap_print (ap)
	struct vop_print_args *ap;
{
	register struct vnode *vp = ap->a_vp;
	printf ("\ttag VT_UMAPFS, vp=%x, lowervp=%x\n", vp, UMAPVPTOLOWERVP(vp));
	return 0;
}


/*
 * Global vfs data structures
 */
/*
 * NEEDSWORK: strategy,bmap are hand coded currently.  They should
 * go away with a merged buffer/block cache.
 *
 */
int (**umap_vnodeop_p)();
struct vnodeopv_entry_desc umap_vnodeop_entries[] = {
	{ &vop_default_desc, umap_bypass },

	{ &vop_getattr_desc, umap_getattr },
	{ &vop_inactive_desc, umap_inactive },
	{ &vop_reclaim_desc, umap_reclaim },
	{ &vop_print_desc, umap_print },

	{ &vop_bmap_desc, umap_bmap },
	{ &vop_strategy_desc, umap_strategy },

	{ (struct vnodeop_desc*)UMAP, (int(*)())UMAP }
};
struct vnodeopv_desc umap_vnodeop_opv_desc =
	{ &umap_vnodeop_p, umap_vnodeop_entries };
