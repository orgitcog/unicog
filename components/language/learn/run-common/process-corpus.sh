#!/bin/bash
#
# process-corpus.sh <config>
#
# Batch process a collection of corpora files.
#
# Loop over all of the corpora files (all the files in $CORPORA_DIR),
# and then (optionally) pre-process them and submit them for counting
# by the cogserver. Pre-processing usually means sentence-splitting.
#
# As files are processed, them will be moved from $CORP to the directory
# `submitted` in the current working dir.
#
# Assorted environment variables will be fetched from the <config> file.
# An example config file is in `~/run-config/2-pair-conf.sh`
#
# Configuration can be provided either via:
# 1. Config file (for backward compatibility)
# 2. Environment variables (preferred method)
# ---------

# If config file is provided, use it; otherwise rely on environment variables
if [ -n "$1" ]; then
	CONF_FILE=$1
	if [ -r "$CONF_FILE" ]; then
		echo "Loading configuration from file: $CONF_FILE"
		. "$CONF_FILE"
	else
		echo "Error: Cannot read configuration file: $CONF_FILE"
		exit 1
	fi
else
	# Check if essential environment variables are set
	if [ -z "$CORPUS_DIR" ] || [ -z "$STORAGE_NODE" ]; then
		echo "Error: Required environment variables not set."
		echo "Either provide a config file or set CORPUS_DIR and STORAGE_NODE environment variables."
		exit 1
	fi
	echo "Using configuration from environment variables"
fi

export HOSTNAME
export PORT
export WEBPORT
export OBSERVE
export IN_PROCESS_DIR
export COMPLETED_DIR
export MSG

cwd=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )

# Always do block processing by default. All of the other modes
# are deprecated, and will be removed eventually.
if $BLOCK_SUBMIT; then
	time find $CORPORA_DIR -type f \
		-exec $cwd/file-block-process.sh {} $CORPORA_DIR \;
elif $SENTENCE_SPLIT; then
	time find $CORPORA_DIR -type f \
		-exec $cwd/file-split-process.sh $SPLIT_LANG {} $CORPORA_DIR \;
elif $XFORM_SPLIT; then
	time find $CORPORA_DIR -type f \
		-exec $cwd/file-xform-process.sh {} $CORPORA_DIR $XFORM_CMD \;
elif $LINE_SPLIT; then
	time find $CORPORA_DIR -type f \
		-exec $cwd/file-line-process.sh {} $CORPORA_DIR \;
else
	time find $CORPORA_DIR -type f \
		-exec $cwd/file-block-process.sh {} $CORPORA_DIR \;
fi
