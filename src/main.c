#include <stdio.h>
#include <libguile.h>
#include <curses.h>
#include <string.h>
#include <errno.h>

#include "error_handling.h"
#include "config.h"
#include "log.h"

void guile_debug_value(SCM val) {
    char *representation = scm_to_locale_string(scm_object_to_string(val, SCM_UNDEFINED));
    log_debug("scheme object is %s\n", representation);
    free(representation);
}

void wrapup() {
    // do all closing up shop in here
    endwin();
}

void register_functions() {
    SCM result = get_config_value("launch-debug-server");
    guile_debug_value(result);
}

void init_curses() {
    initscr();
    cbreak();
    echo();
    intrflush(stdscr, false);
    keypad(stdscr, true);
}

void setup_logging() {
    SCM log_path = get_config_value_and_eval("log-path");
    FILE *logfile;
    if (scm_is_true(log_path)) {
        char *str = scm_to_locale_string(log_path);
        log_debug("Log file is %s", str);
        logfile = fopen(str, "w+");
        free(str);
        if (!logfile) {
            log_error("Error opening file: %s", strerror(errno));
            return;
        }
        log_add_fp(logfile, LOG_DEBUG);
    }
}

void guile_main(void *unused, int argc, char **argv) {
    guile_debug_value(scm_current_module());
    load_config();
    setup_logging();
    register_functions();
    init_curses();

    while (true) {
        int ch = getch();
        if (ch == 'q') {
            break;
        }
    }

    wrapup();
}

int main(int argc, char **argv) {
    scm_boot_guile(argc, argv, guile_main, NULL);
    //this never returns
}
