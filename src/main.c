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

scm_t_port_type *logging_type;

size_t log_read(SCM port, SCM dst, size_t something, size_t something_else) {
}

size_t log_write(SCM port, SCM src, size_t start, size_t count) {
    scm_t_bits level = SCM_STREAM(port);
    log_log((int)level, __FILE__, __LINE__, "%s", SCM_BYTEVECTOR_CONTENTS(src));
}

int log_print(SCM port, SCM dest, scm_print_state *state) {
    scm_t_bits level = SCM_STREAM(port);
    scm_simple_format(dest,
                      scm_from_utf8_string("#<logging: level ~S>   "),
                      scm_list_1(scm_from_int((int)level)));
    return 1;
}

void setup_logging_ports() {
    // Create a new port type that uses our logger
    logging_type = scm_make_port_type("logging-type",
                                      log_read,
                                      log_write);
    scm_set_port_print(logging_type, log_print);
    SCM error_port = scm_c_make_port(logging_type, SCM_WRTNG, LOG_ERROR);
    scm_set_current_error_port(error_port);
    SCM warning_port = scm_c_make_port(logging_type, SCM_WRTNG, LOG_WARN);
    scm_set_current_warning_port(warning_port);
    SCM output_port = scm_c_make_port(logging_type, SCM_WRTNG, LOG_INFO);
    scm_set_current_output_port(output_port);
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
    setup_logging_ports();
    load_config();
    setup_logging();
    register_functions();
    load_prelude();

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
