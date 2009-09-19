/* settings */
static const float  ZOOM_STEP         = 0.1;
static const float  SCROLL_STEP       = 40;
static const float  HL_TRANSPARENCY   = 0.4;
static const int    SHOW_NOTIFICATION = 5;
static const int    DEFAULT_WIDTH     = 1280;
static const int    DEFAULT_HEIGHT    = 800;
static const char   BROWSER[]         = "opera %s";
static const char  *PRINTER[]         = {};

/* look */
static const char font[]                  = "onuava normal 10";
static const char default_bgcolor[]       = "#1C1C1C";
static const char default_fgcolor[]       = "#DDEEDD";

static const char notification_fgcolor[]         = "#A3BABF";
static const char notification_bgcolor[]         = "#5E7175";
static const char notification_warning_fgcolor[] = "#FFE863";
static const char notification_warning_bgcolor[] = "#B7416E";
static const char notification_error_fgcolor[]   = "#F00060";
static const char notification_error_bgcolor[]   = "#FFE863";

static const char inputbar_bgcolor[]      = "#1C1C1C";
static const char inputbar_fgcolor[]      = "#DDEEDD";

static const char completion_fgcolor[]    = "#DDEEDD";
static const char completion_bgcolor[]    = "#1C1C1C";
static const char completion_hl_fgcolor[] = "#A3BABF";
static const char completion_hl_bgcolor[] = "#5E7175";

static const char search_highlight[] = "#FEA63C";

/* additional settings */
#define SHOW_SCROLLBARS    0
#define INCREMENTAL_SEARCH 0

/* shortcuts */
Shortcut shortcuts[] = {
  // mask,               key,           function,            argument

  // Navigation
  {0,                    GDK_f,         sc_navigate,         { NEXT } },
  {0,                    GDK_b,         sc_navigate,         { PREVIOUS } },
  {0,                    GDK_h,         sc_scroll,           { LEFT } },
  {0,                    GDK_j,         sc_scroll,           { DOWN } },
  {0,                    GDK_k,         sc_scroll,           { UP } },
  {0,                    GDK_l,         sc_scroll,           { RIGHT } },
  {0,                    GDK_Page_Up,   sc_scroll,           { TOP } },
  {0,                    GDK_Page_Down, sc_scroll,           { BOTTOM } },

  // Searching
  {0,                    GDK_n,         sc_search,           { FORWARD } },
  {0,                    GDK_N,         sc_search,           { BACKWARD } },

  // Display
  {0,                    GDK_plus,      sc_zoom,             { ZOOM_IN } },
  {0,                    GDK_minus,     sc_zoom,             { ZOOM_OUT } },
  {0,                    GDK_0,         sc_zoom,             { ZOOM_ORIGINAL } },
  {0,                    GDK_i,         sc_adjust_window,    { ADJUST_BESTFIT } },
  {0,                    GDK_u,         sc_adjust_window,    { ADJUST_WIDTH } },
  {0,                    GDK_r,         sc_rotate,           { RIGHT } },
  {0,                    GDK_e,         sc_rotate,           { LEFT } },

  // Inputbar
  {0,                    GDK_colon,     sc_focus_inputbar,   { .data = ":" } },
  {0,                    GDK_o,         sc_focus_inputbar,   { .data = ":open " } },
  {0,                    GDK_g,         sc_focus_inputbar,   { .data = ":goto " } },
  {0,                    GDK_slash,     sc_focus_inputbar,   { .data = "/" } },

  // Misc
  {0,                    GDK_m,         sc_toggle_inputbar,  {0} },
  {0,                    GDK_q,         sc_quit,             {0} },
};

/* commands */
Command commands[] = {
  // command,         function
  {"export",          cmd_export},
  {"form",            cmd_form},
  {"goto",            cmd_goto},
  {"info",            cmd_info},
  {"links",           cmd_links},
  {"open",            cmd_open},
  {"rotate",          cmd_rotate},
  {"save",            cmd_save},
  {"quit",            cmd_quit},
  {"zoom",            cmd_zoom},

  {"e",               cmd_export},
  {"f",               cmd_form},
  {"g",               cmd_goto},
  {"i",               cmd_info},
  {"l",               cmd_links},
  {"o",               cmd_open},
  {"r",               cmd_rotate},
  {"s",               cmd_save},
  {"q",               cmd_quit},
  {"z",               cmd_zoom},
};
