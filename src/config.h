//
// Created by alex on 5/23/22.
//

#ifndef UNTITLED_SRC_CONFIG_H_
#define UNTITLED_SRC_CONFIG_H_
#include <libguile/scm.h>
#include "error_handling.h"

err_t find_root_dir(char **out_buf);
err_t load_config();

SCM get_config_value(const char *key);
SCM get_config_value_and_eval(const char *key);

#endif //UNTITLED_SRC_CONFIG_H_
