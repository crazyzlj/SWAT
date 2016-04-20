cmake_minimum_required(VERSION 2.8)
## Project name and language
project(SWAT Fortran)
## SWAT version
set(CPACK_PACKAGE_VERSION_MAJOR 2012)
set(CPACK_PACKAGE_VERSION_MINOR 637)
set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} /machine:x64" )
set(CMAKE_Fortran_FLAGS_COVERAGE "-fprofile-arcs -ftest-coverage")
set(CMAKE_Fortran_FLAGS_LLVM "-fplugin=dragonegg -O4")

## FFLAGS depend on the compiler
## reference: https://cmake.org/Wiki/CMakeForFortranExample
get_filename_component (Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)

if (Fortran_COMPILER_NAME MATCHES "gfortran.*")
  # gfortran
  set (CMAKE_Fortran_FLAGS_RELEASE "-funroll-all-loops -fno-f2c -O3")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-fno-f2c -O0 -g")
elseif (Fortran_COMPILER_NAME MATCHES "ifort.*")
  # ifort
  set (CMAKE_Fortran_FLAGS_RELEASE "-f77rtl -O3")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-f77rtl -O0")
elseif (Fortran_COMPILER_NAME MATCHES "g77")
  # g77
  set (CMAKE_Fortran_FLAGS_RELEASE "-funroll-all-loops -fno-f2c -O3 -m32")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-fno-f2c -O0 -g -m32")
else (Fortran_COMPILER_NAME MATCHES "gfortran.*")
  message ("CMAKE_Fortran_COMPILER full path: " ${CMAKE_Fortran_COMPILER})
  message ("Fortran compiler: " ${Fortran_COMPILER_NAME})
  message ("No optimized Fortran compiler flags are known, we just try -O2...")
  set (CMAKE_Fortran_FLAGS_RELEASE "-O2")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-O0 -g")
endif (Fortran_COMPILER_NAME MATCHES "gfortran.*")


add_subdirectory(src)

set(CPACK_GENERATOR "ZIP")

set(CPACK_INSTALL_CMAKE_PROJECTS
  ${PROJECT_BINARY_DIR}/Realse; ${PROJECT_BINARY_DIR}/Debug)
include(CPack)
