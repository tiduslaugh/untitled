cmake_minimum_required(VERSION 3.21)
project(untitled C)

set(CMAKE_C_STANDARD 17)

set(CURSES_NEED_NCURSES TRUE)
find_package(Curses)
if (NOT CURSES_FOUND)
    message(FATAL_ERROR "Could not find curses...")
endif ()

find_library(GUILE_LIB guile-3.0)
if (NOT GUILE_LIB)
    message(FATAL_ERROR "guile_lib not set")
endif ()
find_path(GUILE_INCLUDE libguile.h HINTS /usr/include/guile/3.0 /usr/include/guile/3.0/guile/3.0)
if (NOT GUILE_INCLUDE)
    message(FATAL_ERROR "guile_include not set")
endif ()

add_executable(untitled src/main.c src/error_handling.h src/log.c src/log.h src/config.c src/config.h src/error_handling.c src/primitives.c src/primitives.h src/util.c src/util.h)
target_link_libraries(untitled "${GUILE_LIB}" "${CURSES_LIBRARIES}")
target_include_directories(untitled PUBLIC "${GUILE_INCLUDE}" "${CURSES_INCLUDE_DIRS}")