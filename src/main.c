#include <stdio.h>
#include <libguile.h>
#include <curses.h>
#include <string.h>
#include <errno.h>

#include "error_handling.h"
#include "config.h"
#include "log.h"
#include "primitives.h"

void guile_debug_value(SCM val) {
    char *representation = scm_to_locale_string(scm_object_to_string(val, SCM_UNDEFINED));
    log_debug("scheme object is %s\n", representation);
    free(representation);
}

void wrapup() {
    // do all closing up shop in here
    endwin();
}

void load_prelude() {
    log_debug("Loading prelude...");
    scm_c_primitive_load_path("lib/prelude.scm");
    //scm_c_use_module("lib prelude");
    log_debug("Done.");
    scm_eval(scm_list_1(scm_from_utf8_symbol("init-debug-server")),
             scm_current_module());
}

void load_main() {
    log_debug("Loading main...");
    //scm_c_primitive_load_path("lib/main.scm");
    log_debug("Done.");

    while (1) {
        int input = getch();
        if (input == 'q') {
            break;
        }
    }
//    log_debug("Starting main...");
//    scm_c_eval_string("(call-main-protected main-loop)");
//    log_debug("Finished main.");
}

void init_curses() {
    log_set_quiet(true);
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
    load_config();
    setup_logging();
    scm_c_define_module("c-bindings", register_functions, NULL);
    //register_functions();
    load_prelude();

    //init_curses();
    load_main();

    wrapup();
}

int main(int argc, char **argv) {
    scm_boot_guile(argc, argv, guile_main, NULL);
    //this never returns
}
