/*
 *$Header: qfuncl.c,v 1.9 84/02/29 17:23:24 sklower Exp $
 *$Locker:  $
 *
 * Copyright (c) 1982, by the Regents, University of California
 *
 *			-[Tue Apr 10 08:32:07 1984 by jkf]-
 *
 * Interface to sun core graphics.
 *
 * 
 * 
 *
 */

int inquire_detectability();
int set_detectability();
int inquire_highlighting();
int set_highlighting();
int inquire_image_transformation_2();
int set_image_transformation_2();
int inquire_image_transformation_3();
int set_image_transformation_3();
int inquire_visibility();
int set_visibility();
int inquire_image_translate_2();
int set_image_translate_2();
int inquire_image_translate_3();
int set_image_translate_3();
int print_error();
int report_most_recent_error();
int initialize_core();
int terminate_core();
int set_pick_id();
int set_charup_2();
int set_charup_3();
int set_charjust();
int define_color_indices();
int set_primitive_attributes();
int set_marker_symbol();
int set_text_index();
int set_fill_index();
int set_rasterop();
int set_polygon_edge_style();
int set_linestyle();
int set_charspace();
int set_line_index();
int set_drag();
int set_polygon_interior_style();
int set_charsize();
int set_charprecision();
int set_charpath_2();
int set_linewidth();
int set_charpath_3();
int set_font();
int set_pen();
int inquire_pick_id();
int inquire_charup_2();
int inquire_charup_3();
int inquire_charjust();
int inquire_color_indices();
int inquire_primitive_attributes();
int inquire_marker_symbol();
int inquire_text_index();
int inquire_fill_index();
int inquire_rasterop();
int inquire_polygon_edge_style();
int inquire_linestyle();
int inquire_charspace();
int inquire_line_index();
int inquire_pen();
int inquire_polygon_interior_style();
int inquire_font();
int inquire_charsize();
int inquire_charprecision();
int inquire_charpath_2();
int inquire_linewidth();
int inquire_charpath_3();
int terminate_view_surface();
int deselect_view_surface();
int select_view_surface();
int initialize_view_surface();
int polymarker_abs_2();
int marker_abs_2();
int polymarker_abs_3();
int marker_abs_3();
int polymarker_rel_2();
int marker_rel_2();
int polymarker_rel_3();
int marker_rel_3();
int text();
int inquire_text_extent_2();
int inquire_text_extent_3();
int inquire_current_position_2();
int polyline_abs_2();
int move_abs_2();
int line_abs_2();
int polyline_rel_2();
int move_rel_2();
int line_rel_2();
int inquire_current_position_3();
int polyline_abs_3();
int move_abs_3();
int line_abs_3();
int polyline_rel_3();
int move_rel_3();
int line_rel_3();
int set_echo();
int await_pick();
int await_stroke_2();
int await_any_button();
int set_valuator();
int await_any_button_get_valuator();
int set_echo_surface();
int set_echo_position();
int set_stroke();
int set_locator_2();
int await_any_button_get_locator_2();
int set_echo_group();
int get_mouse_state();
int terminate_device();
int initialize_device();
int set_keyboard();
int await_keyboard();
int inquire_valuator();
int inquire_echo_surface();
int inquire_echo_position();
int inquire_locator_2();
int inquire_stroke();
int inquire_echo();
int inquire_keyboard();
int polygon_abs_2();
int polygon_rel_2();
int set_vertex_indices();
int polygon_abs_3();
int polygon_rel_3();
int set_shading_parameters();
int set_light_direction();
int set_vertex_normals();
int get_raster();
int put_raster();
int free_raster();
int allocate_raster();
int size_raster();
int raster_to_file();
int file_to_raster();
int set_zbuffer_cut();
int delete_all_retained_segments();
int rename_retained_segment();
int delete_retained_segment();
int set_image_transformation_type();
int close_retained_segment();
int close_temporary_segment();
int create_temporary_segment();
int create_retained_segment();
int inquire_segment_image_transformation_3();
int set_segment_image_transformation_3();
int inquire_segment_image_translate_3();
int set_segment_image_translate_3();
int set_segment_detectability();
int set_segment_highlighting();
int set_segment_image_transformation_2();
int set_segment_visibility();
int set_segment_image_translate_2();
int inquire_segment_detectability();
int inquire_segment_highlighting();
int inquire_segment_image_transformation_type();
int inquire_image_transformation_type();
int inquire_segment_image_transformation_2();
int inquire_segment_visibility();
int inquire_segment_image_translate_2();
int inquire_view_plane_normal();
int inquire_view_depth();
int inquire_retained_segment_names();
int inquire_viewing_control_parameters();
int inquire_viewing_parameters();
int inquire_projection();
int inquire_world_coordinate_matrix_2();
int inquire_world_coordinate_matrix_3();
int inquire_retained_segment_surfaces();
int inquire_ndc_space_2();
int inquire_ndc_space_3();
int inquire_view_reference_point();
int inquire_view_plane_distance();
int inquire_inverse_composite_matrix();
int inquire_viewport_2();
int inquire_viewport_3();
int inquire_window();
int inquire_view_up_2();
int inquire_view_up_3();
int inquire_open_temporary_segment();
int inquire_open_retained_segment();
int end_batch_of_updates();
int begin_batch_of_updates();
int new_frame();
int restore_segment();
int save_segment();
int set_view_plane_normal();
int set_view_depth();
int set_viewing_parameters();
int set_projection();
int set_window();
int set_ndc_space_2();
int set_ndc_space_3();
int set_view_reference_point();
int set_view_plane_distance();
int set_viewport_2();
int set_viewport_3();
int set_view_up_2();
int set_view_up_3();
int map_world_to_ndc_2();
int map_world_to_ndc_3();
int map_ndc_to_world_2();
int map_ndc_to_world_3();
int set_world_coordinate_matrix_2();
int set_world_coordinate_matrix_3();
int set_back_plane_clipping();
int set_front_plane_clipping();
int set_window_clipping();
int set_output_clipping();
int set_coordinate_system_type();

static char *disc = "c-function";
static struct cftab {
	char *name;
	int (*cfn)();
	char **discipline;
} corefuns[] = {
{"_inquire_detectability", inquire_detectability, &disc},
{"_set_detectability", set_detectability, &disc},
{"_inquire_highlighting", inquire_highlighting, &disc},
{"_set_highlighting", set_highlighting, &disc},
{"_inquire_image_transformation_2", inquire_image_transformation_2, &disc},
{"_set_image_transformation_2", set_image_transformation_2, &disc},
{"_inquire_image_transformation_3", inquire_image_transformation_3, &disc},
{"_set_image_transformation_3", set_image_transformation_3, &disc},
{"_inquire_visibility", inquire_visibility, &disc},
{"_set_visibility", set_visibility, &disc},
{"_inquire_image_translate_2", inquire_image_translate_2, &disc},
{"_set_image_translate_2", set_image_translate_2, &disc},
{"_inquire_image_translate_3", inquire_image_translate_3, &disc},
{"_set_image_translate_3", set_image_translate_3, &disc},
{"_print_error", print_error, &disc},
{"_report_most_recent_error", report_most_recent_error, &disc},
{"_initialize_core", initialize_core, &disc},
{"_terminate_core", terminate_core, &disc},
{"_set_pick_id", set_pick_id, &disc},
{"_set_charup_2", set_charup_2, &disc},
{"_set_charup_3", set_charup_3, &disc},
{"_set_charjust", set_charjust, &disc},
{"_define_color_indices", define_color_indices, &disc},
{"_set_primitive_attributes", set_primitive_attributes, &disc},
{"_set_marker_symbol", set_marker_symbol, &disc},
{"_set_text_index", set_text_index, &disc},
{"_set_fill_index", set_fill_index, &disc},
{"_set_rasterop", set_rasterop, &disc},
{"_set_polygon_edge_style", set_polygon_edge_style, &disc},
{"_set_linestyle", set_linestyle, &disc},
{"_set_charspace", set_charspace, &disc},
{"_set_line_index", set_line_index, &disc},
{"_set_drag", set_drag, &disc},
{"_set_polygon_interior_style", set_polygon_interior_style, &disc},
{"_set_charsize", set_charsize, &disc},
{"_set_charprecision", set_charprecision, &disc},
{"_set_charpath_2", set_charpath_2, &disc},
{"_set_linewidth", set_linewidth, &disc},
{"_set_charpath_3", set_charpath_3, &disc},
{"_set_font", set_font, &disc},
{"_set_pen", set_pen, &disc},
{"_inquire_pick_id", inquire_pick_id, &disc},
{"_inquire_charup_2", inquire_charup_2, &disc},
{"_inquire_charup_3", inquire_charup_3, &disc},
{"_inquire_charjust", inquire_charjust, &disc},
{"_inquire_color_indices", inquire_color_indices, &disc},
{"_inquire_primitive_attributes", inquire_primitive_attributes, &disc},
{"_inquire_marker_symbol", inquire_marker_symbol, &disc},
{"_inquire_text_index", inquire_text_index, &disc},
{"_inquire_fill_index", inquire_fill_index, &disc},
{"_inquire_rasterop", inquire_rasterop, &disc},
{"_inquire_polygon_edge_style", inquire_polygon_edge_style, &disc},
{"_inquire_linestyle", inquire_linestyle, &disc},
{"_inquire_charspace", inquire_charspace, &disc},
{"_inquire_line_index", inquire_line_index, &disc},
{"_inquire_pen", inquire_pen, &disc},
{"_inquire_polygon_interior_style", inquire_polygon_interior_style, &disc},
{"_inquire_font", inquire_font, &disc},
{"_inquire_charsize", inquire_charsize, &disc},
{"_inquire_charprecision", inquire_charprecision, &disc},
{"_inquire_charpath_2", inquire_charpath_2, &disc},
{"_inquire_linewidth", inquire_linewidth, &disc},
{"_inquire_charpath_3", inquire_charpath_3, &disc},
{"_terminate_view_surface", terminate_view_surface, &disc},
{"_deselect_view_surface", deselect_view_surface, &disc},
{"_select_view_surface", select_view_surface, &disc},
{"_initialize_view_surface", initialize_view_surface, &disc},
{"_polymarker_abs_2", polymarker_abs_2, &disc},
{"_marker_abs_2", marker_abs_2, &disc},
{"_polymarker_abs_3", polymarker_abs_3, &disc},
{"_marker_abs_3", marker_abs_3, &disc},
{"_polymarker_rel_2", polymarker_rel_2, &disc},
{"_marker_rel_2", marker_rel_2, &disc},
{"_polymarker_rel_3", polymarker_rel_3, &disc},
{"_marker_rel_3", marker_rel_3, &disc},
{"_text", text, &disc},
{"_inquire_text_extent_2", inquire_text_extent_2, &disc},
{"_inquire_text_extent_3", inquire_text_extent_3, &disc},
{"_inquire_current_position_2", inquire_current_position_2, &disc},
{"_polyline_abs_2", polyline_abs_2, &disc},
{"_move_abs_2", move_abs_2, &disc},
{"_line_abs_2", line_abs_2, &disc},
{"_polyline_rel_2", polyline_rel_2, &disc},
{"_move_rel_2", move_rel_2, &disc},
{"_line_rel_2", line_rel_2, &disc},
{"_inquire_current_position_3", inquire_current_position_3, &disc},
{"_polyline_abs_3", polyline_abs_3, &disc},
{"_move_abs_3", move_abs_3, &disc},
{"_line_abs_3", line_abs_3, &disc},
{"_polyline_rel_3", polyline_rel_3, &disc},
{"_move_rel_3", move_rel_3, &disc},
{"_line_rel_3", line_rel_3, &disc},
{"_set_echo", set_echo, &disc},
{"_await_pick", await_pick, &disc},
{"_await_stroke_2", await_stroke_2, &disc},
{"_await_any_button", await_any_button, &disc},
{"_set_valuator", set_valuator, &disc},
{"_await_any_button_get_valuator", await_any_button_get_valuator, &disc},
{"_set_echo_surface", set_echo_surface, &disc},
{"_set_echo_position", set_echo_position, &disc},
{"_set_stroke", set_stroke, &disc},
{"_set_locator_2", set_locator_2, &disc},
{"_await_any_button_get_locator_2", await_any_button_get_locator_2, &disc},
{"_set_echo_group", set_echo_group, &disc},
{"_get_mouse_state", get_mouse_state, &disc},
{"_terminate_device", terminate_device, &disc},
{"_initialize_device", initialize_device, &disc},
{"_set_keyboard", set_keyboard, &disc},
{"_await_keyboard", await_keyboard, &disc},
{"_inquire_valuator", inquire_valuator, &disc},
{"_inquire_echo_surface", inquire_echo_surface, &disc},
{"_inquire_echo_position", inquire_echo_position, &disc},
{"_inquire_locator_2", inquire_locator_2, &disc},
{"_inquire_stroke", inquire_stroke, &disc},
{"_inquire_echo", inquire_echo, &disc},
{"_inquire_keyboard", inquire_keyboard, &disc},
{"_polygon_abs_2", polygon_abs_2, &disc},
{"_polygon_rel_2", polygon_rel_2, &disc},
{"_set_vertex_indices", set_vertex_indices, &disc},
{"_polygon_abs_3", polygon_abs_3, &disc},
{"_polygon_rel_3", polygon_rel_3, &disc},
{"_set_shading_parameters", set_shading_parameters, &disc},
{"_set_light_direction", set_light_direction, &disc},
{"_set_vertex_normals", set_vertex_normals, &disc},
{"_get_raster", get_raster, &disc},
{"_put_raster", put_raster, &disc},
{"_free_raster", free_raster, &disc},
{"_allocate_raster", allocate_raster, &disc},
{"_size_raster", size_raster, &disc},
{"_raster_to_file", raster_to_file, &disc},
{"_file_to_raster", file_to_raster, &disc},
{"_set_zbuffer_cut", set_zbuffer_cut, &disc},
{"_delete_all_retained_segments", delete_all_retained_segments, &disc},
{"_rename_retained_segment", rename_retained_segment, &disc},
{"_delete_retained_segment", delete_retained_segment, &disc},
{"_set_image_transformation_type", set_image_transformation_type, &disc},
{"_close_retained_segment", close_retained_segment, &disc},
{"_close_temporary_segment", close_temporary_segment, &disc},
{"_create_temporary_segment", create_temporary_segment, &disc},
{"_create_retained_segment", create_retained_segment, &disc},
{"_inquire_segment_image_transformation_3", inquire_segment_image_transformation_3, &disc},
{"_set_segment_image_transformation_3", set_segment_image_transformation_3, &disc},
{"_inquire_segment_image_translate_3", inquire_segment_image_translate_3, &disc},
{"_set_segment_image_translate_3", set_segment_image_translate_3, &disc},
{"_set_segment_detectability", set_segment_detectability, &disc},
{"_set_segment_highlighting", set_segment_highlighting, &disc},
{"_set_segment_image_transformation_2", set_segment_image_transformation_2, &disc},
{"_set_segment_visibility", set_segment_visibility, &disc},
{"_set_segment_image_translate_2", set_segment_image_translate_2, &disc},
{"_inquire_segment_detectability", inquire_segment_detectability, &disc},
{"_inquire_segment_highlighting", inquire_segment_highlighting, &disc},
{"_inquire_segment_image_transformation_type", inquire_segment_image_transformation_type, &disc},
{"_inquire_image_transformation_type", inquire_image_transformation_type, &disc},
{"_inquire_segment_image_transformation_2", inquire_segment_image_transformation_2, &disc},
{"_inquire_segment_visibility", inquire_segment_visibility, &disc},
{"_inquire_segment_image_translate_2", inquire_segment_image_translate_2, &disc},
{"_inquire_view_plane_normal", inquire_view_plane_normal, &disc},
{"_inquire_view_depth", inquire_view_depth, &disc},
{"_inquire_retained_segment_names", inquire_retained_segment_names, &disc},
{"_inquire_viewing_control_parameters", inquire_viewing_control_parameters, &disc},
{"_inquire_viewing_parameters", inquire_viewing_parameters, &disc},
{"_inquire_projection", inquire_projection, &disc},
{"_inquire_world_coordinate_matrix_2", inquire_world_coordinate_matrix_2, &disc},
{"_inquire_world_coordinate_matrix_3", inquire_world_coordinate_matrix_3, &disc},
{"_inquire_retained_segment_surfaces", inquire_retained_segment_surfaces, &disc},
{"_inquire_ndc_space_2", inquire_ndc_space_2, &disc},
{"_inquire_ndc_space_3", inquire_ndc_space_3, &disc},
{"_inquire_view_reference_point", inquire_view_reference_point, &disc},
{"_inquire_view_plane_distance", inquire_view_plane_distance, &disc},
{"_inquire_inverse_composite_matrix", inquire_inverse_composite_matrix, &disc},
{"_inquire_viewport_2", inquire_viewport_2, &disc},
{"_inquire_viewport_3", inquire_viewport_3, &disc},
{"_inquire_window", inquire_window, &disc},
{"_inquire_view_up_2", inquire_view_up_2, &disc},
{"_inquire_view_up_3", inquire_view_up_3, &disc},
{"_inquire_open_temporary_segment", inquire_open_temporary_segment, &disc},
{"_inquire_open_retained_segment", inquire_open_retained_segment, &disc},
{"_end_batch_of_updates", end_batch_of_updates, &disc},
{"_begin_batch_of_updates", begin_batch_of_updates, &disc},
{"_new_frame", new_frame, &disc},
{"_restore_segment", restore_segment, &disc},
{"_save_segment", save_segment, &disc},
{"_set_view_plane_normal", set_view_plane_normal, &disc},
{"_set_view_depth", set_view_depth, &disc},
{"_set_viewing_parameters", set_viewing_parameters, &disc},
{"_set_projection", set_projection, &disc},
{"_set_window", set_window, &disc},
{"_set_ndc_space_2", set_ndc_space_2, &disc},
{"_set_ndc_space_3", set_ndc_space_3, &disc},
{"_set_view_reference_point", set_view_reference_point, &disc},
{"_set_view_plane_distance", set_view_plane_distance, &disc},
{"_set_viewport_2", set_viewport_2, &disc},
{"_set_viewport_3", set_viewport_3, &disc},
{"_set_view_up_2", set_view_up_2, &disc},
{"_set_view_up_3", set_view_up_3, &disc},
{"_map_world_to_ndc_2", map_world_to_ndc_2, &disc},
{"_map_world_to_ndc_3", map_world_to_ndc_3, &disc},
{"_map_ndc_to_world_2", map_ndc_to_world_2, &disc},
{"_map_ndc_to_world_3", map_ndc_to_world_3, &disc},
{"_set_world_coordinate_matrix_2", set_world_coordinate_matrix_2, &disc},
{"_set_world_coordinate_matrix_3", set_world_coordinate_matrix_3, &disc},
{"_set_back_plane_clipping", set_back_plane_clipping, &disc},
{"_set_front_plane_clipping", set_front_plane_clipping, &disc},
{"_set_window_clipping", set_window_clipping, &disc},
{"_set_output_clipping", set_output_clipping, &disc},
{"_set_coordinate_system_type", set_coordinate_system_type, &disc},
{0}
};
hookupcore() {mftab(corefuns);}
