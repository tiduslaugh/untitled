//
// Created by alex on 5/23/22.
//

#ifndef UNTITLED_SRC_ERROR_HANDLING_H_
#define UNTITLED_SRC_ERROR_HANDLING_H_

typedef struct {
  int code;
  const char *file;
  int line;
} error_t;

error_t make_error_(int code, const char *file, int line) {
    error_t ret = {code, file, line};
    return ret;
}
#define make_error(code) make_error_(code, __FILE__, __LINE__)

const error_t SUCCESS = {0, NULL, -1};

// Error trap macro.
#define ERRT(x) { error_t err = (x); if (err.code != 0) { return err; } }

#endif //UNTITLED_SRC_ERROR_HANDLING_H_
