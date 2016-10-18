cmake_minimum_required(VERSION 2.8)

project(SWAT Fortran)

set(CPACK_PACKAGE_VERSION_MAJOR 2012)
set(CPACK_PACKAGE_VERSION_MINOR 637)
set(CPACK_PACKAGE_VERSION_PATCH "1")

set(CMAKE_Fortran_FLAGS_COVERAGE "-fprofile-arcs -ftest-coverage")
set(CMAKE_Fortran_FLAGS_LLVM "-fplugin=dragonegg -O4")

add_subdirectory(src)

set(CPACK_GENERATOR "ZIP")

set(CPACK_INSTALL_CMAKE_PROJECTS
  ${PROJECT_BINARY_DIR}/Realse; ${PROJECT_BINARY_DIR}/Debug
  )
include(CPack)
