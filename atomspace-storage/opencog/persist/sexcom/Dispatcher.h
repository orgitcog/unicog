/*
 * Dispatcher.h
 * Dispatcher for command interpreter.
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

#ifndef _DISPATCHER_H
#define _DISPATCHER_H

#include <functional>
#include <memory>
#include <string>

#include <opencog/persist/sexcom/Commands.h>

namespace opencog
{
/** \addtogroup grp_persist
 *  @{
 */

class AtomSpace;

// Command handler interface for performance optimization
class CommandHandler
{
public:
	virtual ~CommandHandler() {}
	virtual std::string execute(const std::string& args) = 0;
};

// Template for command handler implementations
template<typename T>
class CommandHandlerImpl : public CommandHandler
{
private:
	T* _target;
	std::string (T::*_method)(const std::string&);
	
public:
	CommandHandlerImpl(T* target, std::string (T::*method)(const std::string&))
		: _target(target), _method(method) {}
	
	std::string execute(const std::string& args) override
	{
		return (_target->*_method)(args);
	}
};

class Dispatcher
{
public:
	// Legacy typedef for compatibility
	typedef std::function<std::string (const std::string&)> Meth;

protected:
	Commands _default;

	/// Optimized dispatch table using direct command handlers
	std::unordered_map<size_t, std::unique_ptr<CommandHandler>> _handler_map;
	
	/// Legacy dispatch map for custom handlers
	std::unordered_map<size_t, Meth> _dispatch_map;

public:
	Dispatcher(void);
	~Dispatcher();

	// Indicate which AtomSpace to use
	void set_base_space(const AtomSpacePtr& asp) {
		_default.set_base_space(asp); }

	/// Interpret a very small subset of singular scheme commands.
	/// This is an ultra-minimalistic command interpreter. It only
	/// supports those commands needed for network I/O of AtomSpace
	/// contents (The cogserver uses this to provide peer AtomSpace
	/// network services). The goal is to provide much higher
	/// performance than what is possible through the guile interfaces.
	///
	/// The supported commands are:
	///    cog-atomspace
	///    cog-atomspace-clear
	///    cog-set-proxy!
	///    cog-proxy-open
	///    cog-proxy-close
	///    cog-execute-cache!
	///
	///    cog-get-atoms
	///    cog-incoming-by-type
	///    cog-incoming-set
	///    cog-keys->alist
	///    cog-link
	///    cog-node
	///    cog-value
	///
	///    cog-extract!
	///    cog-extract-recursive!
	///    cog-set-value!
	///    cog-set-values!
	///    cog-set-tv!
	///    cog-update-value!
	///
	///    ping
	///    cog-version
	///
	/// They MUST appear only once in the string, at the very beginning,
	/// and they MUST be followed by valid Atomese s-expressions, and
	/// nothing else.
	///
	std::string interpret_command(const std::string&);

	/// Install a callback handler, over-riding the default behavior for
	/// the command interpreter. This allows proxy agents to over-ride the
	/// default interpretation of any message that is received, so as to do
	/// ... something different. Anything different.
	void install_handler(const std::string&, Meth);
};

/** @}*/
} // namespace opencog

#endif // _DISPATCHER_H
