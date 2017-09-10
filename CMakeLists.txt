cmake_minimum_required(VERSION 2.8)
## SWAT version
set(SWAT_VER_MAJOR 2012)
set(SWAT_VER_MINOR 664)
set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} /machine:x64" )
set(CMAKE_Fortran_FLAGS_COVERAGE "-fprofile-arcs -ftest-coverage")
set(CMAKE_Fortran_FLAGS_LLVM "-fplugin=dragonegg -O4")

## Project name and language
set(prjname SWAT${SWAT_VER_MAJOR}rev${SWAT_VER_MINOR})
project({prjname} Fortran)
## FFLAGS depend on the compiler
## reference: https://cmake.org/Wiki/CMakeForFortranExample
get_filename_component (Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)

if (Fortran_COMPILER_NAME MATCHES "gfortran.*")
  # gfortran
  set (CMAKE_Fortran_FLAGS_RELEASE "-funroll-all-loops -fno-f2c -O3 -Ofast -fimplicit-none -Wall -Wextra -Wtabs -Wuninitialized -Wno-unused -freal-4-real-8")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-fno-f2c -O0 -g -ggdb -fimplicit-none -Wall -Wextra -Wtabs -ffpe-trap=invalid -finit-real=snan -Wuninitialized -Wno-unused -freal-4-real-8")
elseif (Fortran_COMPILER_NAME MATCHES "ifort.*")
  # ifort
  set (CMAKE_Fortran_FLAGS_RELEASE "-f77rtl -O3 -fast -fp-model fast=2 -opt-multi-version-aggressive -warn all -warn notruncated_source -warn nounused -diag-disable 8291 -diag-disable 8290 -diag-disable 5415")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-f77rtl -O0 -gdwarf-3 -check all,nopointers -fp-stack-check -fstack-protector-all -ftrapuv -opt-multi-version-aggressive -warn all -warn notruncated_source -warn nounused -diag-disable 8291 -diag-disable 8290 -diag-disable 5415")
elseif (Fortran_COMPILER_NAME MATCHES "g77")
  # g77
  set (CMAKE_Fortran_FLAGS_RELEASE "-funroll-all-loops -fno-f2c -O3 -m32")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-fno-f2c -O0 -g -m32")
else ()
  message ("CMAKE_Fortran_COMPILER full path: " ${CMAKE_Fortran_COMPILER})
  message ("Fortran compiler: " ${Fortran_COMPILER_NAME})
  message ("No optimized Fortran compiler flags are known, we just try -O2...")
  set (CMAKE_Fortran_FLAGS_RELEASE "-O2")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-O0 -g")
endif ()


add_subdirectory(swat_src)

set(CPACK_GENERATOR "ZIP")

set(CPACK_INSTALL_CMAKE_PROJECTS
  ${PROJECT_BINARY_DIR}/Realse; ${PROJECT_BINARY_DIR}/Debug)
include(CPack)
