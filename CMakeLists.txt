cmake_minimum_required(VERSION 2.8)
## SWAT version
set(SWAT_VER_MAJOR 2012)
set(SWAT_VER_MINOR 664)
set(CMAKE_Fortran_FLAGS_COVERAGE "-fprofile-arcs -ftest-coverage")
set(CMAKE_Fortran_FLAGS_LLVM "-fplugin=dragonegg -O4")
## Project name and language
set(prjname SWAT${SWAT_VER_MAJOR}rev${SWAT_VER_MINOR})
project(${prjname} Fortran)
################# Install directory ##################
IF (INSTALL_PREFIX)
    SET(CMAKE_INSTALL_PREFIX ${INSTALL_PREFIX})
    SET(INSTALL_DIR ${INSTALL_PREFIX})
ELSE ()
    SET(CMAKE_INSTALL_PREFIX ${CMAKE_SOURCE_DIR})
    SET(INSTALL_DIR ${CMAKE_SOURCE_DIR}/bin)
ENDIF ()
## FFLAGS depend on the compiler
## reference: https://cmake.org/Wiki/CMakeForFortranExample
get_filename_component (Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)

if (Fortran_COMPILER_NAME MATCHES "gfortran.*")
  # gfortran
  message(STATUS "Compiling with gfortran...")
  set (CMAKE_Fortran_FLAGS_RELEASE "-funroll-all-loops -fno-f2c -O3 -Ofast -fimplicit-none -Wall -Wextra -Wtabs -Wuninitialized -Wno-unused -freal-4-real-8")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-fno-f2c -O0 -g -ggdb -fimplicit-none -Wall -Wextra -Wtabs -ffpe-trap=invalid -finit-real=snan -Wuninitialized -Wno-unused -freal-4-real-8")
elseif (Fortran_COMPILER_NAME MATCHES "ifort.*")
  # ifort
  message(STATUS "Compiling with intel fortran (ifor)...")
  set (CMAKE_Fortran_FLAGS_RELEASE "-f77rtl -O3 -warn=all -warn=notruncated_source -warn=nounused -diag-disable=8291 -diag-disable=8290 -diag-disable=5415")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-f77rtl -O3 -check=all,nopointers -warn=all -warn=notruncated_source -warn=nounused -diag-disable=8291 -diag-disable=8290 -diag-disable=5415")
elseif (Fortran_COMPILER_NAME MATCHES "g77")
  # g77
  message(STATUS "Compiling with g77...")
  set (CMAKE_Fortran_FLAGS_RELEASE "-funroll-all-loops -fno-f2c -O3 -m32")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-fno-f2c -O0 -g -m32")
else ()
  message ("CMAKE_Fortran_COMPILER full path: " ${CMAKE_Fortran_COMPILER})
  message ("Fortran compiler: " ${Fortran_COMPILER_NAME})
  message ("No optimized Fortran compiler flags are known, we just try -O2...")
  set (CMAKE_Fortran_FLAGS_RELEASE "-O2")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-O0 -g")
endif ()

add_subdirectory(swat_src/src)
