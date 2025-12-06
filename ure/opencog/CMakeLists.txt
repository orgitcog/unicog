DECLARE_GUILE_CONFIG_TARGET(SCM_CONFIG_URE "opencog ure-config" "URE_TEST")

ADD_SUBDIRECTORY (ure)
ADD_SUBDIRECTORY (scm)

# Extension language support
IF (HAVE_CYTHON)
	ADD_SUBDIRECTORY (cython)
ENDIF (HAVE_CYTHON)

WRITE_GUILE_CONFIG(${GUILE_BIN_DIR}/opencog/ure-config.scm SCM_CONFIG_URE TRUE)
WRITE_GUILE_CONFIG(${GUILE_BIN_DIR}/opencog/ure-config-installable.scm SCM_CONFIG_URE FALSE)
INSTALL(FILES ${GUILE_BIN_DIR}/opencog/ure-config-installable.scm DESTINATION ${GUILE_SITE_DIR}/opencog RENAME ure-config.scm)
