/** \file hostware/fmainwindow.c
 * \brief GtkFMainWindow class
 *
 */

#include <assert.h>
#include <math.h>
#include <stdint.h>
#include <string.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include <gtk/gtk.h>
#include <gconf/gconf-client.h>

#include "compiler.h"

#include "fmainwindow.h"
#include "freemcan-gui-device.h"
#include "freemcan-gui-diagram.h"

#include "freemcan-export.h"
#include "freemcan-log.h"


/**
 * \defgroup fmainwindow GtkFMainWindow
 * \ingroup freemcan_gui
 *
 * @{
 */


/** gconf namespace */
#define GCONF_KEY_NAMESPACE "/apps/" "freemcan"
#define GCONF_KEY_NAME(x) GCONF_KEY_NAMESPACE "/" x

#define GCONF_KEY_DEVICE_NAME          GCONF_KEY_NAME("device-name")
#define GCONF_KEY_MEASUREMENT_DURATION GCONF_KEY_NAME("measurement-duration")
#define GCONF_KEY_INTERVAL_DURATION    GCONF_KEY_NAME("interval-duration")


/** Log file */
FILE *stdlog = NULL;


/** GUI specific message logger for #fmlog() & Co. */
static void
gui_log_handler(void *UP(data),
		const char *message,
		const size_t UP(length))
{
  fprintf(stdout, "%s\n", message);
  fflush(stdout);
  if (stdlog) {
    /* We could print a timestamp in front of the message string here */
    fprintf(stdlog, "%s\n", message);
    fflush(stdlog);
  }
}


/** GUI state for GUI state machine */
typedef enum {
  /** Unitialized GUI. How the GUI starts up */
  ST_UNINITIALIZED,
  /** Some error occured while communicating with the device */
  ST_ERROR,
  /** Device is ready for command */
  ST_READY,
  /** Device is measuring */
  ST_MEASURING,
  /** Device has completed measurement */
  ST_DONE,
  /** Device is about to reset itself */
  ST_RESET
} gui_state_t;


gui_state_t string_to_state(const char *str)
{
  static const struct {
    const char *name;
    const gui_state_t state;
  } foo[] = {
    {"READY", ST_READY},
    {"MEASURING", ST_MEASURING},
    {"DONE", ST_DONE},
    {"RESET", ST_RESET},
    {NULL, 0}
  };
  for (int i=0; foo[i].name; ++i) {
    if (0 == strcmp(foo[i].name, str)) {
      return foo[i].state;
    }
  }
  return ST_ERROR;
}


struct _GtkFMainWindowClass {
  GtkWindowClass parent_class;
};

struct _GtkFMainWindow {
  GtkWindow parent_instance;

  gui_state_t state;
  fhistogram_t *current_histogram;

  GtkBuilder *builder;
  GConfClient *config;
  GtkWindow *main_window;

  GtkButton *abort_button;
  GtkButton *reset_button;
  GtkButton *intermediate_button;
  GtkButton *measure_button;
  GtkSpinButton *duration_spinbutton;
  GtkSpinButton *interval_spinbutton;
  GtkStatusbar *statusbar;

  GtkDrawingArea *histogram_chart;


  GConfEngine *conf_engine;

  GtkLabel *device_label;
  GtkLabel *state_label;

  unsigned int histogram_probe_counter;
};


/* Forward declarations */
void gtk_fmainwindow_set_statusbar(GtkFMainWindow *self,
				   const gchar *text)
  __attribute__((nonnull(1)));

void show_gui_as_measuring(gboolean b);


/** \defgroup freemcan_gui_interaction_events User Interaction Events
 * \ingroup freemcan_gui
 * @{
 */


/** \bug Set numeric value to N without focussing outside the spinbutton: New value is ignored. */

G_MODULE_EXPORT void
on_duration_spinbutton_value_changed(GtkSpinButton *spinbutton,
				     GtkFMainWindow *gui)
{
  const gint value = gtk_spin_button_get_value_as_int(spinbutton);
  gconf_client_set_int(GTK_FMAINWINDOW(gui)->config,
		       GCONF_KEY_MEASUREMENT_DURATION, value, NULL);
}


/** \bug Set numeric value to N without focussing outside the spinbutton: New value is ignored. */
G_MODULE_EXPORT void
on_interval_spinbutton_value_changed(GtkSpinButton *spinbutton,
				     GtkFMainWindow *gui)
{
  const gint value = gtk_spin_button_get_value_as_int(spinbutton);
  gconf_client_set_int(GTK_FMAINWINDOW(gui)->config,
		       GCONF_KEY_INTERVAL_DURATION, value, NULL);
}


static
gboolean gtkfilefilterfunc(const GtkFileFilterInfo *filter_info,
			   gpointer UP(data))
{
  struct stat sb;
  const int stat_ret = stat(filter_info->filename, &sb);
  if (stat_ret != 0) return FALSE;
  return (S_ISCHR(sb.st_mode) || S_ISSOCK(sb.st_mode));
}


G_MODULE_EXPORT void
on_open_button_clicked(GtkButton *UP(button), GtkFMainWindow *gui)
{
  GtkWidget *dialog =
    gtk_file_chooser_dialog_new("Open Device File",
				gui->main_window,
				GTK_FILE_CHOOSER_ACTION_OPEN,
				GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
				GTK_STOCK_OPEN, GTK_RESPONSE_ACCEPT,
				NULL);

  if (1) {
    const char *gconf_device_name =
      gconf_client_get_string(gui->config, GCONF_KEY_DEVICE_NAME, NULL);
    if (gconf_device_name) {
      gtk_file_chooser_set_filename(GTK_FILE_CHOOSER(dialog), gconf_device_name);
    }
  }

  /* Only show character device files and UNIX domain sockets */
  GtkFileFilter *filter = gtk_file_filter_new();
  gtk_file_filter_add_custom(filter, GTK_FILE_FILTER_FILENAME, gtkfilefilterfunc,
			     NULL, NULL);
  gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(dialog), filter);

  if (gtk_dialog_run(GTK_DIALOG(dialog)) == GTK_RESPONSE_ACCEPT) {
    const char *device_name =
      gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(dialog));
    assert(device_name);
    gconf_client_set_string(gui->config, GCONF_KEY_DEVICE_NAME, device_name, NULL);
  } else {
    gtk_fmainwindow_set_statusbar(gui, "Not Accept");
  }
  gtk_widget_destroy(dialog);
}


static
void on_change_to_measurement_duration(const gint value,
				       GtkFMainWindow *gui)
  __attribute__((nonnull(2)));

static
void on_change_to_measurement_duration(const gint value,
				       GtkFMainWindow *gui)
{
  gtk_widget_set_sensitive(GTK_WIDGET(gui->measure_button), value);
  /** \bug Clamp value to uint16_t range */
}


static
void on_change_to_device_name(const char *filename, GtkFMainWindow *gui)
  __attribute__((nonnull(1,2)));

/** \bug Fix "open" button and label */
static
void on_change_to_device_name(const char *filename, GtkFMainWindow *gui)
{
  fmlog("%s(\"%s\")", __PRETTY_FUNCTION__, filename);
  if (filename) {
    const char *lastslash = strrchr(filename, '/');
    const char *basename = (lastslash)?(lastslash+1):(filename);
    gtk_label_set_text(gui->device_label, basename);
    gtk_widget_set_tooltip_text(GTK_WIDGET(gui->device_label), filename);
    /** \bug Call device_close and device_open */
  } else {
    gtk_label_set_text(gui->device_label, "<error>");
    gtk_widget_set_tooltip_text(GTK_WIDGET(gui->device_label), "Error changing device name");
  }
}


void gconf_client_notify_func(GConfClient *UP(client),
			      guint UP(cnxn_id),
			      GConfEntry *entry,
			      gpointer user_data)
{
  GtkFMainWindow *gui = (GtkFMainWindow *) user_data;
  fmlog("%s(%p,%d,%p,%p): key=%s, value=%p",
	__PRETTY_FUNCTION__,
	(void *)_UP(client), _UP(cnxn_id), (void *)entry, (void *)user_data,
	entry->key, (void *)entry->value);
  if (g_strcmp0(entry->key, GCONF_KEY_MEASUREMENT_DURATION) == 0) {
    const gint value = gconf_value_get_int(entry->value);
    fmlog("         value=%d=0x%x", value, value);
    on_change_to_measurement_duration(value, gui);
  } else if (g_strcmp0(entry->key, GCONF_KEY_MEASUREMENT_DURATION) == 0) {
    const char *value = gconf_value_get_string(entry->value);
    fmlog("         value=\"%s\"", value);
    on_change_to_device_name(value, gui);
  }
}


gboolean device_histogram_probe(gpointer data)
{
  GtkFMainWindow *gui = GTK_FMAINWINDOW(data);
  if ((gui->state != ST_MEASURING) || (gui->histogram_probe_counter <= 0)) {
    /* remove the timer which regularly calls us */
    return FALSE;
  }
  gui->histogram_probe_counter--;
  if (gui->histogram_probe_counter <= 0) {
    return FALSE;
  }
  gui_device_command(FRAME_CMD_INTERMEDIATE, 0);
  return TRUE;
}


gboolean device_error_probe(gpointer data)
{
  /** \bug Detect the device disappearing, i.e. not answering to our
   *       commands within a reasonable time. */
  GtkFMainWindow *gui = GTK_FMAINWINDOW(data);
  if (gui->state != ST_ERROR) {
    /* remove the timer which regularly calls us */
    return FALSE;
  }
  gui_device_command(FRAME_CMD_STATE, 0);
  return TRUE;
}


static
void gtk_fmainwindow_state_label(GtkFMainWindow *self,
				     const char *state_label,
				     const char *state_tooltip)
{
  gtk_label_set_text(GTK_LABEL(self->state_label), state_label);
  gtk_widget_set_tooltip_text(GTK_WIDGET(self->state_label), state_tooltip);
}


static
void gtk_fmainwindow_set_state(GtkFMainWindow *gui, const gui_state_t state)
  __attribute__((nonnull(1)));

/** \bug Need changing intermediate update interval during ongoing measurement. */
/** \bug Need way to trigger download when GUI is started in DONE state.
 *       Via ST_UNINIT -> ST_DONE transition? */
static
void gtk_fmainwindow_set_state(GtkFMainWindow *self, const gui_state_t state)
{
  const gui_state_t old_state = self->state;
  self->state = state;
  gboolean
    en_duration_spinbutton = FALSE,
    en_interval_spinbutton = FALSE,
    en_measure_button = FALSE,
    en_reset_button = FALSE,
    en_abort_button = FALSE,
    en_intermediate_button = FALSE;

  const gint measurement_duration_value =
    gconf_client_get_int(self->config, GCONF_KEY_MEASUREMENT_DURATION, NULL);
  const gint interval_seconds =
    gconf_client_get_int(self->config, GCONF_KEY_INTERVAL_DURATION, NULL);

  switch (state) {
  case ST_UNINITIALIZED:
    /* keep everything disabled */
    gtk_fmainwindow_state_label(self, "UNINITIALIZED",
				"The GUI's device interface has not been initialized yet");
    break;
  case ST_ERROR:
    /* keep everything disabled */
    gtk_fmainwindow_state_label(self, "ERROR",
				"Some error occured while accessing device");
    if (old_state != ST_ERROR) {
      const guint UV(timer_id) =
	g_timeout_add_full(G_PRIORITY_DEFAULT, 1000,
			   device_error_probe, self, NULL);
    }
    break;
  case ST_DONE:
    en_reset_button = TRUE;
    gtk_fmainwindow_state_label(self, "DONE",
				"The measurement has completed");
    break;
  case ST_RESET:
    /* keep everything disabled */
    gtk_fmainwindow_state_label(self, "RESET",
				"Device is resetting");
    break;
  case ST_READY:
    en_reset_button = TRUE;
    en_measure_button = TRUE;
    en_duration_spinbutton = TRUE;
    en_interval_spinbutton = TRUE;
    if (self->current_histogram) {
      fhistogram_unref(self->current_histogram);
      self->current_histogram = NULL;
    }
    gtk_fmainwindow_state_label(self, "READY",
                                "Device is ready for starting a measurement");
    gtk_widget_queue_draw(GTK_WIDGET(self->histogram_chart));
    break;
  case ST_MEASURING:
    en_intermediate_button = (interval_seconds<1);
    en_abort_button = TRUE;
    gtk_fmainwindow_state_label(self, "MEASURING",
				"Measurement in progress");
    if (old_state != ST_MEASURING) {
      if (!en_intermediate_button) {
	self->histogram_probe_counter =
	  measurement_duration_value / interval_seconds;
	const guint UV(timer_id) =
	  g_timeout_add_full(G_PRIORITY_DEFAULT, interval_seconds*1000,
			     device_histogram_probe, self, NULL);
      }
    }
    break;
  }
  gtk_widget_set_sensitive(GTK_WIDGET(self->duration_spinbutton), en_duration_spinbutton);
  gtk_widget_set_sensitive(GTK_WIDGET(self->interval_spinbutton), en_interval_spinbutton);
  gtk_widget_set_sensitive(GTK_WIDGET(self->measure_button),
			   en_measure_button && measurement_duration_value);
  gtk_widget_set_sensitive(GTK_WIDGET(self->reset_button), en_reset_button);
  gtk_widget_set_sensitive(GTK_WIDGET(self->abort_button), en_abort_button);
  gtk_widget_set_sensitive(GTK_WIDGET(self->intermediate_button), en_intermediate_button);
}


G_MODULE_EXPORT void
on_measure_button_clicked(GtkButton *UP(button), GtkFMainWindow *gui)
{
  GError *error = NULL;
  GtkFMainWindow *UV(mw) = GTK_FMAINWINDOW(gui);
  fmlog("gui->config = %p", (void *)gui->config);
  const gint value =
    gconf_client_get_int(gui->config, GCONF_KEY_MEASUREMENT_DURATION, &error);
  if (error) {
    g_warning("%s(): %s", __PRETTY_FUNCTION__, error->message);
    exit(EXIT_FAILURE);
  }
  assert((value > 0) && (value < UINT16_MAX));
  gui_device_command(FRAME_CMD_MEASURE, value);
}


G_MODULE_EXPORT void
on_abort_button_clicked(GtkButton *UP(button), GtkFMainWindow *UP(gui))
{
  gui_device_command(FRAME_CMD_ABORT, 0);
}


G_MODULE_EXPORT void
on_reset_button_clicked(GtkButton *UP(button), GtkFMainWindow *UP(gui))
{
  gui_device_command(FRAME_CMD_RESET, 0);
}


G_MODULE_EXPORT void
on_intermediate_button_clicked(GtkButton *UP(button), GtkFMainWindow *UP(gui))
{
  /* This is the state anyway, no need to call it: show_gui_as_measuring(TRUE); */
  gui_device_command(FRAME_CMD_INTERMEDIATE, 0);
}


/** \bug Fix about dialog's close button */
G_MODULE_EXPORT void
on_about_button_clicked(GtkButton *UP(button), GtkFMainWindow *gui)
{
  GtkDialog *about_dialog = GTK_DIALOG(gtk_builder_get_object(gui->builder, "about_dialog"));
  gtk_dialog_run(about_dialog);
}


/** @} */


void gtk_fmainwindow_histogram_update(GtkFMainWindow *self, fhistogram_t *histogram)
{
  if (self->current_histogram) {
    fhistogram_unref(self->current_histogram);
  }
  self->current_histogram = histogram;
  fhistogram_ref(self->current_histogram);
  gtk_widget_queue_draw(GTK_WIDGET(self->histogram_chart));
  draw_diagram_to_eps_file(self->current_histogram);
  draw_diagram_to_pdf_file(self->current_histogram);
  draw_diagram_to_png_file(self->current_histogram);
  draw_diagram_to_svg_file(self->current_histogram);
}


G_MODULE_EXPORT gboolean
on_histogram_chart_expose_event(GtkWidget *widget, GdkEventExpose *event,
				GtkFMainWindow *gui)
{
  if (1) {
    /* paint background */
    cairo_t *cr = gdk_cairo_create(event->window);
    cairo_set_source_rgb(cr, 0.7, 0.5, 0.6);
    cairo_paint(cr);
    cairo_destroy(cr);
  }

  if (1) {
    gdk_draw_arc(widget->window,
		 widget->style->fg_gc[GTK_WIDGET_STATE(widget)],
		 TRUE,
		 0, 0, widget->allocation.width, widget->allocation.height,
		 0, 64 * 360);
  }

  if (1) {
    /* paint half circle, scaled v=h */
    cairo_t *cr = gdk_cairo_create(widget->window);
    double
      w = widget->allocation.width,
      h = widget->allocation.height;
    double s = 0.3*sqrt(w*w+h*h);
    cairo_translate(cr, 0.5*w, 0.5*h);
    cairo_scale(cr, s, s);
    cairo_set_line_width(cr, 0.2);

    cairo_arc(cr,
	      0.0, 0.0, 0.95,
	      0, M_PI);
    cairo_set_source_rgba(cr, 1.0, 0.0, 0.0, 0.5);
    cairo_fill_preserve(cr);
    cairo_set_source_rgba(cr, 0.0, 0.0, 1.0, 0.5);
    cairo_stroke(cr);

    cairo_destroy(cr);
  }

  if (1) {
    /* paint other half circle, scaled v!=h */
    cairo_t *cr = gdk_cairo_create(widget->window);
    double
      w = widget->allocation.width,
      h = widget->allocation.height;
    cairo_translate(cr, 0.5*w, 0.5*h);
    cairo_scale(cr, 0.5*w, 0.5*h);
    cairo_set_line_width(cr, 0.2);

    cairo_arc(cr,
	      0.0, 0.0, 0.95,
	      M_PI, 2*M_PI);
    cairo_set_source_rgba(cr, 0.0, 1.0, 0.0, 0.5);
    cairo_fill_preserve(cr);
    cairo_set_source_rgba(cr, 1.0, 1.0, 1.0, 0.5);
    cairo_stroke(cr);
    cairo_destroy(cr);
  }

  if (1) {
    /* paint text */
    cairo_t *cr = gdk_cairo_create(widget->window);
    double
      w = widget->allocation.width,
      h = widget->allocation.height;
    const char *text = "FreeMCAn";
    cairo_text_extents_t extents;
    cairo_set_font_size(cr, 0.2*h);
    cairo_text_extents(cr, text, &extents);
    cairo_move_to(cr, 0.5*(w-extents.width), 0.5*(0.3*h+extents.height));
    cairo_show_text(cr, text);
    cairo_destroy(cr);
  }

  if (1) {
    /* paint diagram, */
    cairo_t *cr = gdk_cairo_create(widget->window);
    double
      w = widget->allocation.width,
      h = widget->allocation.height;

    /* attenuate background */
    cairo_rectangle(cr, 0, 0, w, h);
    cairo_set_source_rgba(cr, 1.0, 1.0, 1.0, 0.8);
    cairo_fill(cr);

    /* done painting */
    cairo_destroy(cr);
  }

  if (1) {
    /* paint diagram */
    cairo_t *cr = gdk_cairo_create(widget->window);
    double
      w = widget->allocation.width,
      h = widget->allocation.height;

    /* draw the diagram */
    draw_diagram(cr, w, h, gui->current_histogram);

    /* done painting */
    cairo_destroy(cr);
  }

  /* we have handled this event, ergo TRUE */
  return TRUE;
}


/************************************************************************/
/** \defgroup freemcan_gui_data_handling Data Handling (Layer 4)
 * \ingroup freemcan_gui
 * @{
 */
/************************************************************************/


void on_packet_state(const char *state, void *data)
{
  GtkFMainWindow *gui = GTK_FMAINWINDOW(data);
  gtk_fmainwindow_set_statusbar(gui, state);
  gtk_fmainwindow_set_state(gui, string_to_state(state));
}


void on_packet_text(const char *text, void *data)
{
  GtkFMainWindow *gui = GTK_FMAINWINDOW(data);
  gtk_fmainwindow_set_statusbar(gui, text);
}


void on_packet_histogram(fhistogram_t *histogram, void *data)
{
  GtkFMainWindow *gui = GTK_FMAINWINDOW(data);
  gtk_fmainwindow_histogram_update(gui, histogram);
}


/** Status data packet handler (GUI specific)
 *
 * \bug Use received status packets to update GUI state
 */
static void packet_handler_state(const char *state, void *data)
{
  fmlog("STATE: %s", state);
  on_packet_state(state, data);
}


/** Text data packet handler (GUI specific) */
static void packet_handler_text(const char *text, void *data)
{
  fmlog("TEXT: %s", text);
  on_packet_text(text, data);
}


/** Histogram data packet handler (GUI specific) */
static void packet_handler_histogram(packet_histogram_t *histogram_packet,
				     void *data)
{
  const size_t element_count = histogram_packet->element_count;
  fmlog("Received '%c' type histogram data of %d elements:",
	histogram_packet->type, element_count);
  fmlog_hist(histogram_packet->elements, element_count);

  /* export current histogram to file(s) */
  export_histogram(histogram_packet);

  fhistogram_t *histogram = fhistogram_new_from_packet(histogram_packet);
  on_packet_histogram(histogram, data);
  fhistogram_unref(histogram);
}

/** @} */


void gtk_fmainwindow_set_statusbar(GtkFMainWindow *self, const gchar *text)
{
  if (self->statusbar) {
    gtk_statusbar_pop(self->statusbar, 0);
    gtk_statusbar_push(self->statusbar, 0, text);
  }
}


static void
gtk_fmainwindow_destroy(GtkObject *object)
{
  g_return_if_fail(object != NULL);
  g_return_if_fail(GTK_IS_FMAINWINDOW(object));

  GtkFMainWindow *mw = GTK_FMAINWINDOW(object);

  GtkFMainWindowClass *klass = gtk_type_class(gtk_widget_get_type());

  g_object_unref(G_OBJECT(mw->builder));

  if (GTK_OBJECT_CLASS(klass)->destroy) {
     (* GTK_OBJECT_CLASS(klass)->destroy) (object);
  }
}


static void
gtk_fmainwindow_class_init(GtkFMainWindowClass *klass)
{
  GtkWidgetClass *widget_class;
  GtkObjectClass *object_class;


  widget_class = (GtkWidgetClass *) klass;
  object_class = (GtkObjectClass *) klass;

  /*
    widget_class->realize = gtk_fmainwindow_realize;
    widget_class->size_request = gtk_cpu_size_request;
    widget_class->size_allocate = gtk_cpu_size_allocate;
    widget_class->expose_event = gtk_cpu_expose;
  */

  object_class->destroy = gtk_fmainwindow_destroy;
}


static void
gtk_fmainwindow_init(GtkFMainWindow *self)
{
  GError *error = NULL;

  GtkFMainWindow *UV(mw) = GTK_FMAINWINDOW(self);

  self->state = ST_UNINITIALIZED;
  self->current_histogram = NULL;
  self->histogram_probe_counter = 0;

  /** Must be called before gconf_* */
  g_type_init();
  /** Get ourselves a GConfClient */
  self->config = gconf_client_get_default();
  fmlog("self->config = %p", (void *)self->config);

  /* Create new GtkBuilder object */
  self->builder = gtk_builder_new();

  /* Load UI from file. If error occurs, report it and quit application.
   * Replace "tut.glade" with your saved project. */
  if (!gtk_builder_add_from_file(self->builder, "freemcan-gui.ui", &error)) {
    g_warning("%s", error->message);
    // g_free(error);
    exit(EXIT_FAILURE);
  }

  /* Connect signals */
  gtk_builder_connect_signals(self->builder, self);

  /* Start gconf stuff */
  gconf_client_notify_add(self->config, GCONF_KEY_NAMESPACE,
			  gconf_client_notify_func, NULL,
			  NULL,
			  NULL);
  /** \bug Add gconf schema (however that is supposed to work) */

  /* Get main window pointer from UI */
  self->main_window =
    GTK_WINDOW(gtk_builder_get_object(self->builder, "freemcan_mainwindow"));

  /* Get a few parts from UI */
  self->duration_spinbutton =
    GTK_SPIN_BUTTON(gtk_builder_get_object(self->builder,
					   "duration_spinbutton"));
  self->interval_spinbutton =
    GTK_SPIN_BUTTON(gtk_builder_get_object(self->builder,
					   "interval_spinbutton"));
  self->abort_button =
    GTK_BUTTON(gtk_builder_get_object(self->builder, "abort_button"));
  self->intermediate_button =
    GTK_BUTTON(gtk_builder_get_object(self->builder, "intermediate_button"));
  self->measure_button =
    GTK_BUTTON(gtk_builder_get_object(self->builder, "measure_button"));
  self->reset_button =
    GTK_BUTTON(gtk_builder_get_object(self->builder, "reset_button"));
  self->statusbar =
    GTK_STATUSBAR(gtk_builder_get_object(self->builder, "statusbar"));

  self->histogram_chart =
    GTK_DRAWING_AREA(gtk_builder_get_object(self->builder, "histogram_chart"));

  self->device_label =
    GTK_LABEL(gtk_builder_get_object(self->builder, "device_label"));
  self->state_label =
    GTK_LABEL(gtk_builder_get_object(self->builder, "state_label"));

  /* init measurement_value */
  if (1) {
    const gint value =
      gconf_client_get_int(self->config, GCONF_KEY_MEASUREMENT_DURATION, &error);
    g_prefix_error(&error, "error getting m-d");
    fmlog("got m-d value: %d", value);
    const gint new_value = (value > 0)?value:60;
    fmlog("new m-d value: %d", new_value);
    gconf_client_set_int(self->config, GCONF_KEY_MEASUREMENT_DURATION, new_value, &error);
    g_prefix_error(&error, "error setting m-d");
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(self->duration_spinbutton), new_value);
    /** \bug Update our values when gconf stuff changes */
  }

  /* init interval_value */
  if (1) {
    const gint value =
      gconf_client_get_int(self->config, GCONF_KEY_INTERVAL_DURATION, &error);
    g_prefix_error(&error, "error getting i-d");
    fmlog("got i-d value: %d", value);
    const gint new_value = (value > 0)?value:20;
    fmlog("new i-d value: %d", new_value);
    gconf_client_set_int(self->config, GCONF_KEY_INTERVAL_DURATION, new_value, &error);
    g_prefix_error(&error, "error setting i-d");
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(self->interval_spinbutton), new_value);
    /** \bug Update our values when gconf stuff changes */
  }

  /* Start up freemcan infrastructure */
  stdlog = fopen("freemcan-gui.log", "w");
  fmlog_set_handler(gui_log_handler, NULL);
  packet_set_handlers(packet_handler_histogram,
		      packet_handler_state,
		      packet_handler_text,
		      self);
  fmlog("Graphical User Interface (GUI) set up");

  /* This will cause the device to send us data */
  gtk_fmainwindow_set_state(self, ST_ERROR);
}


void gtk_fmainwindow_init_device_name(GtkFMainWindow *self, const char *init_device_fname)
{
  if (init_device_fname) {
    /** Set via --device, so note it down */
    gconf_client_set_string(self->config, GCONF_KEY_DEVICE_NAME,
			    init_device_fname, NULL);
  } else {
    /** Not set via --device, so look for the last value */
    gchar *gconf_devname = gconf_client_get_string(self->config, GCONF_KEY_DEVICE_NAME, NULL);
    if (gconf_devname) {
      init_device_fname = gconf_devname;
    }
  }
}



GtkType gtk_fmainwindow_get_type(void)
{
  static GtkType gtk_fmainwindow_type = 0;
  if (!gtk_fmainwindow_type) {
      static const GtkTypeInfo gtk_fmainwindow_info = {
          "GtkFMainWindow",
          sizeof(GtkFMainWindow),
          sizeof(GtkFMainWindowClass),
          (GtkClassInitFunc) gtk_fmainwindow_class_init,
          (GtkObjectInitFunc) gtk_fmainwindow_init,
          NULL,
          NULL,
          (GtkClassInitFunc) NULL
      };
      gtk_fmainwindow_type = gtk_type_unique(GTK_TYPE_WIDGET, &gtk_fmainwindow_info);
  }
  return gtk_fmainwindow_type;
}


GtkWidget *gtk_fmainwindow_new()
{
   return GTK_WIDGET(gtk_type_new(gtk_fmainwindow_get_type()));
}


/** @} */
