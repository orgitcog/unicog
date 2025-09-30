/*
 * LoggerSCM.h
 *
 * Copyright (C) 2015, 2017 OpenCog Foundation
 *
 * Author: Nil Geisweiller
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Free Software Foundation and including the exceptions
 * at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program; if not, write to:
 * Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#ifndef _OPENCOG_LOGGER_SCM_H
#define _OPENCOG_LOGGER_SCM_H

#include <set>
#include <mutex>
#include <string>
#include <libguile.h>

#include <opencog/util/Logger.h>
#include <opencog/guile/SchemeModule.h>

namespace opencog {

/**
 * Expose the Logger singleton to Scheme
 */
class LoggerSCM : public ModuleWrap
{
private:
	// Logger management functions - moved from SchemeSmob
	static std::mutex lgr_mtx;
	static std::set<Logger*> deleteable_lgr;
	static scm_t_bits cog_logger_tag;
	static bool logger_smob_inited;

protected:
	virtual void init();

	Logger* do_default_logger();
	Logger* do_new_logger();
	std::string do_logger_set_level(Logger*, const std::string& level);
	std::string do_logger_get_level(const Logger*);
	std::string do_logger_set_filename(Logger*, const std::string& filename);
	std::string do_logger_get_filename(const Logger*);
	std::string do_logger_set_component(Logger*, const std::string& component);
	std::string do_logger_get_component(const Logger*);
	bool do_logger_set_stdout(Logger*, bool);
	bool do_logger_set_sync(Logger*, bool);
	bool do_logger_set_timestamp(Logger*, bool);
	bool do_logger_is_error_enabled(Logger*);
	bool do_logger_is_warn_enabled(Logger*);
	bool do_logger_is_info_enabled(Logger*);
	bool do_logger_is_debug_enabled(Logger*);
	bool do_logger_is_fine_enabled(Logger*);
	void do_logger_error(Logger*, const std::string& msg);
	void do_logger_warn(Logger*, const std::string& msg);
	void do_logger_info(Logger*, const std::string& msg);
	void do_logger_debug(Logger*, const std::string& msg);
	void do_logger_fine(Logger*, const std::string& msg);
	void do_flush(Logger*);

	bool is_logger(SCM);

public:
	LoggerSCM();

	// Static utility functions - moved from SchemeSmob
	static void init_logger_smob_type(void);
	static SCM logger_to_scm(Logger*);
	static Logger* ss_to_logger(SCM);
	static std::string logger_to_string(const Logger*);
	static void release_logger(Logger*);
	static Logger* new_logger();
	static Logger* verify_logger(SCM, const char*, int pos = 1);
	static int print_logger(SCM, SCM, scm_print_state*);
	static size_t free_logger(SCM);
};

} // namespace opencog

#endif // _OPENCOG_LOGGER_SCM_H