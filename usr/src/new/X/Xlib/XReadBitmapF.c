#include <X/mit-copyright.h>

/* Copyright 	Massachusetts Institute of Technology  1985 */
/* $Header: XReadBitmapF.c,v 10.10 86/11/18 11:54:02 jg Rel $ */
#include "XlibInternal.h"
#include <stdio.h>
#include <errno.h>
#include <strings.h>

#define boolean int

extern int errno;

Status XReadBitmapFile(filename, width, height, data, x_hot, y_hot)
    char *filename;
    register int *width, *height;  /* RETURN; must be non-NULL */
    register short **data;   /* RETURN */
    int *x_hot, *y_hot;  /* RETURN; may be NULL */
    {
    char variable[81];
    int status, value, i, data_length;
    FILE *file = fopen (filename, "r");

    if (file == NULL)
    	return (0);

    *width = *height = -1;
    if (x_hot) *x_hot = -1;
    if (y_hot) *y_hot = -1;
    while ((status = fscanf (file, "#define %80s %2d\n", variable, &value))==2)
    	{
	if (StringEndsWith (variable, "width"))
	    *width = value;
	else if (StringEndsWith (variable, "height"))
    	    *height = value;
	else if (StringEndsWith (variable, "x_hot")) {
	      if (x_hot) *x_hot = value;
	    }
	else if (StringEndsWith (variable, "y_hot")) {
    	    if (y_hot) *y_hot = value;
	  }
	}

    if (*width <= 0) {
	fclose (file);
	errno = EINVAL;
	return (-1);
	}
	
    if (*height <= 0) {
	fclose (file);
	errno = EINVAL;
	return (-2);
	}

    data_length = BitmapSize (*width, *height);
    *data = (short *) malloc (data_length);
    data_length /= sizeof(short);
    if (*data == NULL) {
	fclose (file);
    	return (-3);
	}
    
    status = fscanf (file, "static short %80s = { 0x%4hx", variable,
	*data);  /* fills in 0th element of *data array */
    if ((status != 2) || !StringEndsWith (variable, "bits[]")) {
	free ((char *)*data);
	fclose (file);
    	errno = EINVAL;
	return (-4);
	}

    for (i=1;i<data_length;i++) {
	/* fill in i'th element of data array */
	status = fscanf (file, ", 0x%4hx", *data + i);
	if (status != 1) {
	    free ((char *)*data);
	    fclose (file);
	    errno = EINVAL;
	    return (-5);
	    }
    	}

    fclose (file);
    return (1);
    }

/* StringEndsWith returns TRUE if "s" ends with "suffix", else returns FALSE */
static boolean StringEndsWith (s, suffix)
  char *s, *suffix;
  {
  int s_len = strlen (s);
  int suffix_len = strlen (suffix);
  return (strcmp (s + s_len - suffix_len, suffix) == 0);
  }
