//
// Created by alex on 5/23/22.
//
#include "error_handling.h"

const error_t SUCCESS = {0, NULL, -1};

error_t make_error_(int code, const char *file, int line) {
    error_t ret = {code, file, line};
    return ret;
}