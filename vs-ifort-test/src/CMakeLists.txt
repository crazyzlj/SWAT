cmake_minimum_required(VERSION 2.8)

set( SWAT_SRCS
  HELLO.F90
)
add_executable(swat ${SWAT_SRCS})
set(PATCH ${CPACK_PACKAGE_VERSION_PATCH})
if(CPACK_PACKAGE_VERSION_PATCH MATCHES "^[0-9]")
  set(PATCH ".${PATCH}")
endif()

set_target_properties(swat PROPERTIES
  RELEASE_POSTFIX "${CPACK_PACKAGE_VERSION_MAJOR}Rev${CPACK_PACKAGE_VERSION_MINOR}${PATCH}Rel"
  DEBUG_POSTFIX "${CPACK_PACKAGE_VERSION_MAJOR}Rev${CPACK_PACKAGE_VERSION_MINOR}${PATCH}Dbg"
  PGO_POSTFIX "${CPACK_PACKAGE_VERSION_MAJOR}Rev${CPACK_PACKAGE_VERSION_MINOR}${PATCH}RelPGO"
  IPGO_POSTFIX "${CPACK_PACKAGE_VERSION_MAJOR}Rev${CPACK_PACKAGE_VERSION_MINOR}${PATCH}RelPGO"
)

install(TARGETS swat RUNTIME DESTINATION ".")

# Profile guided optimization

if(CMAKE_BUILD_TYPE MATCHES "PGO")

  if(CMAKE_BUILD_TYPE MATCHES "I")
    set(PROF_DIR "-prof-dir")
    set(PROF_GEN "-prof-gen")
    set(PROF_USE "-prof-use")
  else()			# GCC
    set(PROF_DIR "-fprofile-dir")
    set(PROF_GEN "-fprofile-generate")
    set(PROF_USE "-fprofile-use")
  endif()

  set(EXTRA_LINK_FLAGS "${PROF_DIR}=${CMAKE_BINARY_DIR}/profile")
  set(EXTRA_COMPILE_FLAGS "${EXTRA_LINK_FLAGS}")

  add_executable(swat0 ${SWAT_SRCS})

endif()
