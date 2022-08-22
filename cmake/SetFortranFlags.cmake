######################################################
# Determine and set the Fortran compiler flags we want 
######################################################

####################################################################
# Make sure that the default build type is RELEASE if not specified.
####################################################################
INCLUDE(${CMAKE_MODULE_PATH}/SetCompileFlag.cmake)

# Make sure the build type is uppercase
STRING(TOUPPER "${CMAKE_BUILD_TYPE}" BT)

IF(BT STREQUAL "RELEASE")
    SET(CMAKE_BUILD_TYPE RELEASE CACHE STRING
      "Choose the type of build, options are DEBUG, RELEASE, or TESTING."
      FORCE)
ELSEIF(BT STREQUAL "DEBUG")
    SET (CMAKE_BUILD_TYPE DEBUG CACHE STRING
      "Choose the type of build, options are DEBUG, RELEASE, or TESTING."
      FORCE)
ELSEIF(BT STREQUAL "TESTING")
    SET (CMAKE_BUILD_TYPE TESTING CACHE STRING
      "Choose the type of build, options are DEBUG, RELEASE, or TESTING."
      FORCE)
ELSEIF(NOT BT)
    SET(CMAKE_BUILD_TYPE RELEASE CACHE STRING
      "Choose the type of build, options are DEBUG, RELEASE, or TESTING."
      FORCE)
    MESSAGE(STATUS "CMAKE_BUILD_TYPE not given, defaulting to RELEASE")
ELSE()
    MESSAGE(FATAL_ERROR "CMAKE_BUILD_TYPE not valid, choices are DEBUG, RELEASE, or TESTING")
ENDIF(BT STREQUAL "RELEASE")

#########################################################
# If the compiler flags have already been set, return now
#########################################################

IF(CMAKE_Fortran_FLAGS_RELEASE AND CMAKE_Fortran_FLAGS_TESTING AND CMAKE_Fortran_FLAGS_DEBUG)
    RETURN ()
ENDIF(CMAKE_Fortran_FLAGS_RELEASE AND CMAKE_Fortran_FLAGS_TESTING AND CMAKE_Fortran_FLAGS_DEBUG)

########################################################################
# Determine the appropriate flags for this compiler for each build type.
# For each option type, a list of possible flags is given that work
# for various compilers.  The first flag that works is chosen.
# If none of the flags work, nothing is added (unless the REQUIRED 
# flag is given in the call).  This way unknown compilers are supported.
#######################################################################

#####################
### GENERAL FLAGS ###
#####################

# Don't add underscores in symbols for C-compatability
SET_COMPILE_FLAG(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS}"
                 Fortran "-fno-underscoring")

# There is some bug where -march=native doesn't work on Mac
IF(APPLE)
    SET(GNUNATIVE "-mtune=native")
ELSE()
    SET(GNUNATIVE "-march=native")
ENDIF()
# Optimize for the host's architecture
# SET_COMPILE_FLAG(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS}"
#                  Fortran "-xHost"        # Intel
#                          "/QxHost"       # Intel Windows
#                          ${GNUNATIVE}    # GNU
#                          "-ta=host"      # Portland Group
#                 )

###################
### DEBUG FLAGS ###
###################

# NOTE: debugging symbols (-g or /debug:full) are already on by default

# Disable optimizations
SET_COMPILE_FLAG(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG}"
                 Fortran REQUIRED "-O0" # All compilers not on Windows
                                  "/Od" # Intel Windows
                )

# Turn on all warnings 
SET_COMPILE_FLAG(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG}"
                 Fortran "-warn all" # Intel
                         "-Wall"     # GNU
                         "-h error_on_warning" # Cray
                                     # Portland Group (on by default)
                         "/warn:all" # Intel Windows
                )

# Traceback
SET_COMPILE_FLAG(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG}"
                 Fortran "-traceback"   # Intel/Portland Group
                         "-fbacktrace"  # GNU (gfortran)
                         "-ftrace=full" # GNU (g95)
                         "-G0"          # Cray
                         "/traceback"   # Intel Windows
                )

# Check array bounds
SET_COMPILE_FLAG(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG}"
                 Fortran "-check bounds"  # Intel
                         "-fcheck=bounds" # GNU (New style)
                         "-fbounds-check" # GNU (Old style)
                         "-Mbounds"       # Portland Group
                         "-h bounds"      # Cray
                         "/check:bounds"  # Intel Windows
                )

#####################
### TESTING FLAGS ###
#####################

# Optimizations
SET_COMPILE_FLAG(CMAKE_Fortran_FLAGS_TESTING "${CMAKE_Fortran_FLAGS_TESTING}"
                 Fortran REQUIRED "-O2" # All compilers not on Windows
                                  "/O2" # Intel Windows
                )

#####################
### RELEASE FLAGS ###
#####################

# NOTE: agressive optimizations (-O3) are already turned on by default

# Unroll loops
SET_COMPILE_FLAG(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE}"
                 Fortran "-funroll-loops" # gfortran, flang,... 
                         "-unroll"        # Intel
                         "-Munroll"       # Portland Group
                         "-h unroll2"     # Cray
                         "/unroll"        # Intel Windows
                )

# Inline functions
SET_COMPILE_FLAG(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE}"
                 Fortran "-inline"            # Intel
                         "-finline-functions" # GNU, flang
                         "-Minline"           # Portland Group
                         "-h fusion1"         # Cray max is 2
                         "/Qinline"           # Intel Windows
                )

# Interprocedural (link-time) optimizations
SET_COMPILE_FLAG(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE}"
                 Fortran "-ipo"     # Intel
                         "-flto"    # GNU, flang
                         "-Mipa"    # Portland Group
                         "-h ipa2"  # Cray max is 5
                         "/Qipo"    # Intel Windows
                )

# Single-file optimizations
# SET_COMPILE_FLAG(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE}"
#                  Fortran "-ip"  # Intel
#                          "/Qip" # Intel Windows
#                 )

# Vectorize code
#SET_COMPILE_FLAG(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE}"
#                 Fortran "-vec-report0"  # Intel
#                         "/Qvec-report0" # Intel Windows
#                         "-Mvect"        # Portland Group
#                )
