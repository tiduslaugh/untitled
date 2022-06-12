//
// Created by alex on 6/5/22.
//

#include <libguile/strports.h>
#include "util.h"
#include "log.h"

void guile_debug_value(SCM val) {
    char *representation = scm_to_locale_string(scm_object_to_string(val, SCM_UNDEFINED));
    log_debug("scheme object is %s\n", representation);
    free(representation);
}
