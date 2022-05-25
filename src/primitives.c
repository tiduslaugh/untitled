//
// Created by alex on 5/24/22.
//

#include "primitives.h"
#include <libguile.h>
#include "log.h"

static SCM guile_log_level(SCM s_level, SCM s_file, SCM s_line, SCM s_formatted) {
    int level = scm_to_int(s_level);
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

void register_functions() {
    scm_c_define_gsubr("log-level", 4, 0, 0, guile_log_level);
}