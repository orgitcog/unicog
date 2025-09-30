/*
 * Dispatcher.cc
 * Dispatcher for a command interpreter for basic AtomSpace commands.
 *
 * Copyright (C) 2020, 2022 Linas Vepstas
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

#include <time.h>

#include <functional>
#include <iomanip>
#include <string>

#include "Dispatcher.h"
#include "Commands.h"

using namespace opencog;

/// The cogserver provides a network API to send/receive Atoms over
/// the internet. The actual API is that of the StorageNode (see the
/// wiki page https://wiki.opencog.org/w/StorageNode for details.)
/// The cogserver supports the full `StorageNode` API, and it uses
/// the code in this directory in order to make it fast.
///
/// To aid in performance, a very special set of about 15 scheme
/// functions have been hard-coded in C++, in the static function
/// `Dispatcher::interpret_command()` below.  The goal is to avoid the
/// overhead of entry/exit into guile. This works because the cogserver
/// is guaranteed to send only these commands, and no others.
//

Dispatcher::Dispatcher(void)
{
	// Optimized dispatch using direct method pointers
	// This avoids the overhead of std::bind and std::function
#define REGISTER_HANDLER(HSH,STR,CB) \
   static const size_t HSH = std::hash<std::string>{}(STR); \
   _handler_map[HSH] = std::make_unique<CommandHandlerImpl<Commands>>(&_default, &Commands::CB);

	REGISTER_HANDLER(space, "cog-atomspace)",         cog_atomspace);
	REGISTER_HANDLER(clear, "cog-atomspace-clear)",   cog_atomspace_clear);
	REGISTER_HANDLER(proxy, "cog-set-proxy!",         cog_set_proxy);
	REGISTER_HANDLER(popen, "cog-proxy-open)",        cog_proxy_open);
	REGISTER_HANDLER(pclos, "cog-proxy-close)",       cog_proxy_close);
	REGISTER_HANDLER(cache, "cog-execute-cache!",     cog_execute_cache);

	REGISTER_HANDLER(gtatm, "cog-get-atoms",          cog_get_atoms);
	REGISTER_HANDLER(incty, "cog-incoming-by-type",   cog_incoming_by_type);
	REGISTER_HANDLER(incom, "cog-incoming-set",       cog_incoming_set);
	REGISTER_HANDLER(keys,  "cog-keys->alist",        cog_keys_alist);
	REGISTER_HANDLER(link,  "cog-link",               cog_link);
	REGISTER_HANDLER(node,  "cog-node",               cog_node);
	REGISTER_HANDLER(value, "cog-value",              cog_value);

	REGISTER_HANDLER(extra, "cog-extract!",           cog_extract);
	REGISTER_HANDLER(recur, "cog-extract-recursive!", cog_extract_recursive);
	REGISTER_HANDLER(stval, "cog-set-value!",         cog_set_value);
	REGISTER_HANDLER(svals, "cog-set-values!",        cog_set_values);
	REGISTER_HANDLER(settv, "cog-set-tv!",            cog_set_tv);
	REGISTER_HANDLER(updval, "cog-update-value!",     cog_update_value);

	REGISTER_HANDLER(dfine, "define",                 cog_define);
	REGISTER_HANDLER(ping,  "ping)",                  cog_ping);
	REGISTER_HANDLER(versn, "cog-version)",           cog_version);
}

Dispatcher::~Dispatcher()
{
}

void Dispatcher::install_handler(const std::string& idstr, Meth handler)
{
	size_t idhash = std::hash<std::string>{}(idstr);
	_dispatch_map.insert_or_assign(idhash, handler);
}

// -----------------------------------------------

std::string Dispatcher::interpret_command(const std::string& cmd)
{
	// Find the command and dispatch
	size_t pos = cmd.find_first_not_of(" \n\t");
	if (std::string::npos == pos) return "";

	// Ignore comments
	if (';' == cmd[pos]) return "";

	if ('(' != cmd[pos])
		throw SyntaxException(TRACE_INFO, "Badly formed command: %s",
			cmd.c_str());

	pos ++; // Skip over the open-paren

	size_t epos = cmd.find_first_of(" \n\t", pos);
	if (std::string::npos == epos)
		throw SyntaxException(TRACE_INFO, "Not a command: %s",
			cmd.c_str());

	// Look up the method to call, based on the hash of the command string.
	size_t action = std::hash<std::string>{}(cmd.substr(pos, epos-pos));
	
	// First try the optimized handler map
	const auto& handler = _handler_map.find(action);
	if (_handler_map.end() != handler)
	{
		pos = cmd.find_first_not_of(" \n\t", epos);
		if (cmd.npos != pos)
			return handler->second->execute(cmd.substr(pos));
		return handler->second->execute(""); // no arguments available.
	}
	
	// Fall back to legacy dispatch map for custom handlers
	const auto& disp = _dispatch_map.find(action);
	if (_dispatch_map.end() != disp)
	{
		Meth f = disp->second;
		pos = cmd.find_first_not_of(" \n\t", epos);
		if (cmd.npos != pos)
			return f(cmd.substr(pos));
		return f(""); // no arguments available.
	}

	throw SyntaxException(TRACE_INFO, "Command not supported: >>%s<<",
		cmd.substr(pos, epos-pos).c_str());
}

// ===================================================================
