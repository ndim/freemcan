#ifndef FMAINWINDOW_H
#define FMAINWINDOW_H

#include <gtk/gtk.h>

#include "fhistogram.h"

G_BEGIN_DECLS

#define GTK_FMAINWINDOW(obj)         \
  GTK_CHECK_CAST(obj, gtk_fmainwindow_get_type (), GtkFMainWindow)
#define GTK_FMAINWINDOW_CLASS(klass) \
  GTK_CHECK_CLASS_CAST(klass, gtk_fmainwindow_get_type(), GtkFMainWindowClass)
#define GTK_IS_FMAINWINDOW(obj)      \
  GTK_CHECK_TYPE(obj, gtk_fmainwindow_get_type())

typedef struct _GtkFMainWindow GtkFMainWindow;
typedef struct _GtkFMainWindowClass GtkFMainWindowClass;

GtkWidget *gtk_fmainwindow_new();

GtkType gtk_fmainwindow_get_type(void);
void gtk_fmainwindow_set_sel(GtkFMainWindow *cpu, gint sel);
GtkWidget *gtk_fmainwindow_new();

void gtk_fmainwindow_histogram_update(GtkFMainWindow *self,
				      fhistogram_t *histogram);

/** Set status line text */
void gtk_fmainwindow_set_statusbar(GtkFMainWindow *self, const gchar *text);


void gtk_fmainwindow_init_device_name(GtkFMainWindow *self, const char *device_name);


G_END_DECLS

#endif /* !FMAINWINDOW_H */
