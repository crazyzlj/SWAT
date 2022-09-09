# ----------------------------------------------------------------------------------------------
#  Root CMake file for origin official SWAT and its variants developed by community.
#     Soil and Water Assessment Tool,
#       a public domain model jointly developed by USDA Agricultural Research Service (USDA-ARS)
#       and Texas A&M AgriLife Research, part of The Texas A&M University System.
#     http://swat.tamu.edu/
#
#     Please from the off-tree build directory, invoke:
#       $ cmake <path/to/SWAT>
#
#     This is an unofficial repository maintained by Dr. Liang-Jun Zhu (zlj@lreis.ac.cn)
#       for non-commercial, educational, and scientific purpose.
#
#  Routine testing platforms and compilers include:
#     1. Windows 10 with Visual Studio 2013, Intel Compiler 17.0 Update 4 (ifort 17.0.4.210)
#     2. Windows 10 with mingw64 (GCC-9.1.0), gfortran-9.1.0
#     3. Ubuntu 16.04 with GCC-5.4.0, gfortran-5.4.0
#
#  This CMake build script was maintained by Liang-Jun Zhu based on cmake_fortran_template
#    https://github.com/SethMMorton/cmake_fortran_template @SethMMorton (https://github.com/SethMMorton)
#  
#  Contact: zlj@lreis.ac.cn
#  Latest updated: Jun. 16 2022
#  Copyright (C) 2017-2022 LREIS, IGSNRR, CAS
# ----------------------------------------------------------------------------------------------
cmake_minimum_required(VERSION 2.8.5)

### Read SWAT versions and set project name
file(READ "VERSIONS" ver)
string(REGEX MATCH "VERSION_MAJOR ([0-9]*)" _ ${ver})  # major version, e.g., 2009 and 2012
set(SWAT_VER_MAJOR ${CMAKE_MATCH_1})
string(REGEX MATCH "VERSION_MINOR ([0-9]*)" _ ${ver})
set(SWAT_VER_MINOR ${CMAKE_MATCH_1})

set(SWAT_VER_STR ${SWAT_VER_MAJOR}.${SWAT_VER_MINOR})
set(prjname SWAT${SWAT_VER_MAJOR}Rev${SWAT_VER_MINOR})

if(${ver} MATCHES "VERSION_PATCH ([a-zA-Z0-9_.-]*)")
    set(SWAT_VER_PATCH ${CMAKE_MATCH_1})
    set(SWAT_VER_STR ${SWAT_VER_MAJOR}.${SWAT_VER_MINOR}.${SWAT_VER_PATCH})
    set(prjname SWAT${SWAT_VER_MAJOR}Rev${SWAT_VER_MINOR}-${SWAT_VER_PATCH})
endif()

message("Compiling SWAT version: ${SWAT_VER_STR}")

project(${prjname} Fortran)
set(CMAKE_VERBOSE_MAKEFILE ON)

### Set cmake find path
set(CMAKE_MODULE_PATH
        ${CMAKE_CURRENT_SOURCE_DIR}/cmake
        ${CMAKE_MODULE_PATH}
        )
INCLUDE(Utils)

### Set default or specified installation directory.
IF (INSTALL_PREFIX)
    SET(INSTALL_DIR ${INSTALL_PREFIX} CACHE PATH "Set specified install path" FORCE)
ELSE()
    IF (CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
        SET(INSTALL_DIR ${PROJECT_BINARY_DIR} CACHE PATH "Set specified install path" FORCE)
    ELSE ()
        SET(INSTALL_DIR ${CMAKE_INSTALL_PREFIX} CACHE PATH "Set default install path" FORCE)
    ENDIF ()
ENDIF ()

### In case of Makefiles if the user does not setup CMAKE_BUILD_TYPE, assume it's Release.
IF(CMAKE_GENERATOR MATCHES "Makefiles|Ninja" AND "${CMAKE_BUILD_TYPE}" STREQUAL "")
    SET(CMAKE_BUILD_TYPE "Release")
ENDIF()
IF(MSVC)
    IF(CMAKE_GENERATOR MATCHES "Win64")
        SET(CMAKE_BUILD_BIT "x64")
    ELSE()
        SET(CMAKE_BUILD_BIT "x86")
    ENDIF()
ENDIF()

IF(MINGW)
    SET(TOOL_CHAIN "mingw64")
ELSEIF(MSVC)
    SET(TOOL_CHAIN "msvc.ifort")
ELSEIF(${CMAKE_Fortran_COMPILER_ID} MATCHES "GNU")
    SET(TOOL_CHAIN "gfort")
ELSEIF(${CMAKE_Fortran_COMPILER_ID} MATCHES "Intel")
    SET(TOOL_CHAIN "ifort")
ELSEIF(${CMAKE_Fortran_COMPILER_ID} MATCHES "Flang")
    SET(TOOL_CHAIN "flang")
ENDIF()

### Break in case of popular CMake configuration mistakes.
IF(NOT CMAKE_SIZEOF_VOID_P GREATER 0)
    MESSAGE(FATAL_ERROR "CMake fails to determine the bitness of the target platform.
  Please check your CMake and compiler installation.")
ENDIF()

### Required the support of Fortran 90
IF(NOT CMAKE_Fortran_COMPILER_SUPPORTS_F90)
    MESSAGE(FATAL_ERROR "Fortran compiler does not support F90")
ENDIF(NOT CMAKE_Fortran_COMPILER_SUPPORTS_F90)

### Set the compile flags for DEBUG, RELEASE, and TESTING.
INCLUDE(SetFortranFlags)

### One of MPI and OpenMP is allowed to set for parallel computation
OPTION(USE_MPI "Use the MPI library for parallelization" OFF)
OPTION(USE_OPENMP "Use OpenMP for parallelization" OFF)
INCLUDE(SetParallelizationLibrary)

### Use statically or dynamically linkage?
### Global flag to cause add_library to create shared libraries if on, otherwise static library.
OPTION(BUILD_SHARED_LIBS "Build shared libraries (DLLs)." OFF)
IF(MSVC)
    ### libraries built with hunter link against the dynamic CRT on windows
    OPTION(BUILD_WITH_STATIC_CRT "Build against dynamic CRT on windows." ON)
    INCLUDE(CRTLinkage)
ELSE()
    IF(APPLE)
        OPTION(ENABLE_STATIC_LINKING "Enable static libraries linking" OFF)
    ELSE()
        OPTION(ENABLE_STATIC_LINKING "Enable static libraries linking" ON)
        INCLUDE(StaticLinking)
    ENDIF()
ENDIF()

### There is an error in CMAKE with this flag for pgf90.  Unset it
GET_FILENAME_COMPONENT(FCNAME ${CMAKE_Fortran_COMPILER} NAME)
IF(FCNAME STREQUAL "pgf90")
    UNSET(CMAKE_SHARED_LIBRARY_LINK_Fortran_FLAGS)
ENDIF(FCNAME STREQUAL "pgf90")

############################################################
# Define the actual files and folders that make up the build
############################################################

### Add subdirectories.
ADD_SUBDIRECTORY(src)

### Add a distclean target to the Makefile
ADD_CUSTOM_TARGET(distclean 
    COMMAND ${CMAKE_COMMAND} -P ${CMAKE_CURRENT_SOURCE_DIR}/cmake/distclean.cmake
)

############################################################
# Build information
############################################################

STATUS("")
STATUS("  Platform:")
IF(NOT CMAKE_VERSION VERSION_LESS 2.8.11 AND NOT BUILD_INFO_SKIP_TIMESTAMP)
    STRING(TIMESTAMP TIMESTAMP "" UTC)
    IF(TIMESTAMP)
        STATUS("    Timestamp:"    ${TIMESTAMP})
    ENDIF()
ENDIF()
STATUS("    Host:"             ${CMAKE_HOST_SYSTEM_NAME} ${CMAKE_HOST_SYSTEM_VERSION} ${CMAKE_HOST_SYSTEM_PROCESSOR})
IF(CMAKE_CROSSCOMPILING)
    STATUS("    Target:"         ${CMAKE_SYSTEM_NAME} ${CMAKE_SYSTEM_VERSION} ${CMAKE_SYSTEM_PROCESSOR})
ENDIF()
STATUS("    CMake:"            ${CMAKE_VERSION})
STATUS("    CMake generator:"  ${CMAKE_GENERATOR})
STATUS("    CMake build tool:" ${CMAKE_BUILD_TOOL})
IF(MSVC)
    STATUS("    MSVC:"           ${MSVC_VERSION})
ENDIF()
IF(CMAKE_GENERATOR MATCHES Xcode)
    STATUS("    Xcode:"          ${XCODE_VERSION})
ENDIF()
IF(NOT CMAKE_GENERATOR MATCHES "Xcode|Visual Studio")
    STATUS("    Configuration:"  ${CMAKE_BUILD_TYPE})
ENDIF()

### C/C++ options.
IF(CMAKE_CXX_COMPILER_VERSION)
    SET(GEO_COMPILER_STR "${CMAKE_Fortran_COMPILER} ${CMAKE_Fortran_COMPILER_ARG1} (ver ${CMAKE_Fortran_COMPILER_VERSION})")
ELSE()
    SET(GEO_COMPILER_STR "${CMAKE_Fortran_COMPILER} ${CMAKE_Fortran_COMPILER_ARG1}")
ENDIF()
STRING(STRIP "${GEO_COMPILER_STR}" GEO_COMPILER_STR)

STATUS("")
STATUS("  Fortran:")
STATUS("    Fortran Compiler:"           ${GEO_COMPILER_STR})
STATUS("    Fortran flags (Release):"    ${CMAKE_Fortran_FLAGS} ${CMAKE_Fortran_FLAGS_RELEASE})
STATUS("    Fortran flags (Debug):"      ${CMAKE_Fortran_FLAGS} ${CMAKE_Fortran_FLAGS_DEBUG})
IF(WIN32)
    STATUS("    Linker flags (Release):" ${CMAKE_EXE_LINKER_FLAGS} ${CMAKE_EXE_LINKER_FLAGS_RELEASE})
    STATUS("    Linker flags (Debug):"   ${CMAKE_EXE_LINKER_FLAGS} ${CMAKE_EXE_LINKER_FLAGS_DEBUG})
ELSE()
    STATUS("    Linker flags (Release):" ${CMAKE_SHARED_LINKER_FLAGS} ${CMAKE_SHARED_LINKER_FLAGS_RELEASE})
    STATUS("    Linker flags (Debug):"   ${CMAKE_SHARED_LINKER_FLAGS} ${CMAKE_SHARED_LINKER_FLAGS_DEBUG})
ENDIF()
### Auxiliary.
STATUS("")
STATUS("  Install path:" ${INSTALL_DIR})
STATUS("")
