//
// Created by alex on 5/23/22.
//

#include "config.h"
#include "error_handling.h"
#include "log.h"
#include <libguile.h>
#include <sys/stat.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>

//static SCM config_module = SCM_UNDEFINED;

error_t find_root_dir(char **out_buf) {
    char buf[1024];
    char filebuf[1200]; // some extra to add to the path
    struct stat stats;

    while (1) {
        char *cwd = getcwd(buf, 1024);
        if (cwd == NULL) {
            log_error("Unable to get cwd: %s", strerror(errno));
            return make_error(errno);
        }
        log_debug("Checking path %s...", cwd);
        if (strcmp(cwd, "/") == 0) {
            // we've reached the root, better stop searching
            log_error("Could not find config.scm in all parents up through root");
            return make_error(ENOENT);
        }
        strncpy(filebuf, cwd, sizeof(filebuf));
        strncat(filebuf, "/config.scm", sizeof(filebuf) - strlen(filebuf) - 1);
        int status = stat(filebuf, &stats);
        if (status == 0) {
            // file exists, off we go
            log_debug("Directory found: %s", cwd);
            *out_buf = strdup(cwd);
            return SUCCESS;
        } else if (errno == ENOENT) {
            log_debug("Not found at path %s", filebuf);
        } else {
            log_error("Some other bad thing occurred when trying to stat: %s", strerror(errno));
            return make_error(errno);
        }
        chdir("..");
    }
}

error_t load_config(SCM *module) {
    char *root_dir;
    ERRT(find_root_dir(&root_dir));
    char *file_path = calloc(strlen(root_dir) + sizeof("/config.scm"), sizeof(char)); // sizeof string includes \0
    strcpy(file_path, root_dir);
    strcat(file_path, "/config.scm");
    SCM root_dir_scm = scm_c_define("root-dir", scm_from_utf8_string(root_dir));
    scm_eval(scm_list_2(
        scm_from_utf8_symbol("add-to-load-path"),
        scm_variable_ref(root_dir_scm)
    ), scm_current_module());
    scm_c_define("file-path", scm_from_utf8_string(file_path));

    //TODO come up with better ways to catch guile exceptions...
    scm_c_use_module("config");

    free(file_path);
    free(root_dir);
    return SUCCESS;
}

SCM get_config_value(const char *key) {
    return scm_variable_ref(scm_c_public_variable("config", key));
}

SCM get_config_value_and_eval(const char *key) {
    SCM val = get_config_value(key);
    if (scm_is_true(scm_list_p(val))) {
        log_debug("Evaluating %s", key);
        val = scm_eval(val, scm_current_module());
    }
    return val;
}
