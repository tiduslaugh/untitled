//
// Created by alex on 5/24/22.
//

#include "primitives.h"
#include <libguile.h>
#include <curses.h>
#include "log.h"

static struct { const char *symbol; int value; } lookup[] = {
    {"trace", LOG_TRACE},
    {"debug", LOG_DEBUG},
    {"info", LOG_INFO},
    {"warn", LOG_WARN},
    {"error", LOG_ERROR},
    {"fatal", LOG_FATAL}
};

static int log_level_symbol_to_int(SCM symbol) {
    for (int i = 0; i < (sizeof(lookup) / sizeof(lookup[0])); i++) {
        if (scm_is_true(scm_eq_p(
            scm_from_utf8_symbol(lookup[i].symbol),
            symbol))) {
            return lookup[i].value;
        }
    }
    return -1;
}

static SCM guile_log_level(SCM s_level, SCM s_file, SCM s_line, SCM s_formatted) {
    //int level = scm_to_int(s_level);
    int level = log_level_symbol_to_int(s_level);
    char *format_str = scm_to_locale_string(s_formatted);
    const char *file = "nowhere";
    if (scm_is_true(s_file)) {
        file = scm_to_locale_string(s_file);
    }
    int line = -1;
    if (scm_is_true(s_line)) {
        line = scm_to_int(s_line);
    }
    log_log(level, file, line, format_str);
    free(format_str);
    if (scm_is_true(s_file)) {
        free((char *)file);
    }
    return SCM_UNSPECIFIED;
}

static SCM guile_getch() {
    // Procedure (getch)
    // Returns: Multivalues, first is "was it a ctrl character" and second is either a char if in
    // ascii range or integer in high page range
    int input = getch();
    bool ctrl = false;
    SCM output = scm_from_int(input);
    if (input >= 1 && input < 32) {
        // One of the ASCII control characters, but we care about it as a control sequence, eg
        // 0x01 = start of record = ^A = ctrl+a
        // 0x02 = record separator = ^B = ctrl+b and so on
        ctrl = true;
        output = scm_integer_to_char(scm_from_int('a' + (input - 1)));
    } else if (input < 128) {
        // ASCII, non-control. Covers 0 and [32, 128)
        ctrl = false;
        output = scm_integer_to_char(scm_from_int(input));
    }
    return scm_values(scm_list_2(SCM_BOOL(ctrl), output));
}

static SCM guile_mvaddch(SCM y, SCM x, SCM chr) {
    int raw_y = scm_to_int(y), raw_x = scm_to_int(x);
    char raw_chr = scm_to_char(scm_char_to_integer(chr));
    int result = mvaddch(raw_y, raw_x, raw_chr);
    if(result == ERR) {
        return scm_from_utf8_symbol("curses-error");
    }
    return SCM_UNDEFINED;
}

void register_functions(void *unused) {
    scm_c_define_gsubr("log-level", 4, 0, 0, guile_log_level);
    scm_c_define_gsubr("getch", 0, 0, 0, guile_getch);
    scm_c_define_gsubr("mvaddch", 3, 0, 0, guile_mvaddch);
    scm_c_export("log-level", "getch", "mvaddch", NULL);
}