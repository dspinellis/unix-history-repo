/*
 * @(#)icondata.h	1.2	%G%
 *
 * References to memory pixrects used primarily as menu icons in
 * the SUN Gremlin picture editor.  This file must be maintained
 * consistently with icondata.c where the pixrects are actually
 * defined.
 *
 * Mark Opperman (opcode@monet.BERKELEY)
 *
 */

extern struct pixrect align_pr,
		      arc_pr,
		      arrow_pr,
		      bigblack_pr,
		      box_pr,
		      boxinc_pr,
		      copy_pr,
		      curve_pr,
		      erase_pr,
		      filecabinet_pr,
		      get1_pr,
		      get2_pr,
		      get3_pr,
		      get4_pr,
		      gravity_pr,
		      grid_pr,
		      hmirror_pr,
		      horizontal_pr,
		      horvert_pr,
		      include_pr,
		      includeset_pr,
		      justify_pr,
		      littlepoint_pr,
		      move_pr,
		      movepoint_pr,
		      polygon_pr,
		      bpolygon_pr,
		      put1_pr,
		      put2_pr,
		      put3_pr,
		      put4_pr,
		      question_pr,
		      rotate_pr,
		      scale_pr,
		      text_pr,
		      undo_pr,
		      vector_pr,
		      vertical_pr,
		      vmirror_pr;
		      
/***************** stipple data ****************/

extern struct pixrect openbox_pr,
		      white_pr,
		      gray_pr,
		      _50_pr,
		      black_pr,
		      stipple5_pr,
		      stipple6_pr,
		      stipple7_pr,
		      stipple8_pr;

/********************  sizes  *********************/

extern struct pixrect size1_pr,
		      size2_pr,
		      size3_pr,
		      size4_pr;

/********************  fonts  *********************/

extern struct pixrect roman_pr,
		      bold_pr,
		      italics_pr,
		      special_pr;

/********************  lines  *********************/

extern struct pixrect broken_pr,
		      dashed_pr,
		      dotted_pr,
		      medium_pr,
		      narrow_pr,
		      thick_pr;

/********************  misc   *********************/

extern struct pixrect point_pr,
		      uparrow_pr,
		      kbdcursor_pr,
		      diamond_pr,
		      bigopenbox_pr,
		      replgrid32_pr,
		      misc_pr,
		      linestyle_pr,
		      pan_pr,
		      dot_pr;

/*************  gremlin tool icon   **************/

extern struct pixrect gremlin_icon_pr;
