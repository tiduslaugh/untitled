//
// Created by alex on 5/23/22.
//
#include "error_handling.h"

const err_t SUCCESS = {0, NULL, -1};

err_t make_error_(int code, const char *file, int line) {
    err_t ret = {code, file, line};
    return ret;
}