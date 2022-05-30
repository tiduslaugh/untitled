//
// Created by alex on 5/23/22.
//

#ifndef UNTITLED_SRC_ERROR_HANDLING_H_
#define UNTITLED_SRC_ERROR_HANDLING_H_
#include <stddef.h>
#include <stdbool.h>

typedef struct {
  int code;
  const char *file;
  int line;
} err_t;

err_t make_error_(int code, const char *file, int line);
#define make_error(code) make_error_(code, __FILE__, __LINE__)

extern const err_t SUCCESS;

// Error trap macro.
#define ERRT(x) { err_t err = (x); if (err.code != 0) { return err; } }

#endif //UNTITLED_SRC_ERROR_HANDLING_H_
