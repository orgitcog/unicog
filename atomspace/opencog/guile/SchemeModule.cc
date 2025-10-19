/*
 * SchemeModule.cc
 *
 * Simplified API for creating guile modules.
 * Copyright (c) 2008, 2014, 2015 Linas Vepstas <linas@linas.org>
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

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/guile/SchemeModule.h>
#include <opencog/guile/SchemePrimitive.h>
#include <opencog/guile/SchemeSmob.h>

using namespace opencog;

FunctionWrap::FunctionWrap(Handle (f)(AtomSpace*, const Handle&),
                           const char* funcname, const char* modname)
	: _func_h_ah(f), _func_h_ahz(nullptr), _func_h_ah_seq(nullptr),
	  _pred_ah(nullptr), _pred_ah_seq(nullptr), 
	  _proto_ah(nullptr), _proto_ah_seq(nullptr),
	  _name(funcname)
{
	define_scheme_primitive(_name, &FunctionWrap::as_wrapper_h_h, this, modname);
}

FunctionWrap::FunctionWrap(Handle (f)(AtomSpace*, const Handle&, size_t),
                           const char* funcname, const char* modname)
	: _func_h_ah(nullptr), _func_h_ahz(f), _func_h_ah_seq(nullptr),
	  _pred_ah(nullptr), _pred_ah_seq(nullptr),
	  _proto_ah(nullptr), _proto_ah_seq(nullptr),
	  _name(funcname)
{
	define_scheme_primitive(_name, &FunctionWrap::as_wrapper_h_hz, this, modname);
}

FunctionWrap::FunctionWrap(TruthValuePtr (p)(AtomSpace*, const Handle&),
                           const char* funcname, const char* modname)
	: _func_h_ah(nullptr), _func_h_ahz(nullptr), _func_h_ah_seq(nullptr),
	  _pred_ah(p), _pred_ah_seq(nullptr),
	  _proto_ah(nullptr), _proto_ah_seq(nullptr),
	  _name(funcname)
{
	define_scheme_primitive(_name, &FunctionWrap::as_wrapper_p_h, this, modname);
}

FunctionWrap::FunctionWrap(ValuePtr (p)(AtomSpace*, const Handle&),
                           const char* funcname, const char* modname)
	: _func_h_ah(nullptr), _func_h_ahz(nullptr), _func_h_ah_seq(nullptr),
	  _pred_ah(nullptr), _pred_ah_seq(nullptr),
	  _proto_ah(p), _proto_ah_seq(nullptr),
	  _name(funcname)
{
	define_scheme_primitive(_name, &FunctionWrap::as_wrapper_v_h, this, modname);
}

//XXX New constructors supporting optional arguments as list of handles
FunctionWrap::FunctionWrap(Handle (f)(AtomSpace*, const Handle&, const HandleSeq&),
                           const char* funcname, const char* modname)
	: _func_h_ah(nullptr), _func_h_ahz(nullptr), _func_h_ah_seq(f),
	  _pred_ah(nullptr), _pred_ah_seq(nullptr),
	  _proto_ah(nullptr), _proto_ah_seq(nullptr),
	  _name(funcname)
{
	define_scheme_primitive(_name, &FunctionWrap::as_wrapper_h_h_seq, this, modname);
}

FunctionWrap::FunctionWrap(TruthValuePtr (p)(AtomSpace*, const Handle&, const HandleSeq&),
                           const char* funcname, const char* modname)
	: _func_h_ah(nullptr), _func_h_ahz(nullptr), _func_h_ah_seq(nullptr),
	  _pred_ah(nullptr), _pred_ah_seq(p),
	  _proto_ah(nullptr), _proto_ah_seq(nullptr),
	  _name(funcname)
{
	define_scheme_primitive(_name, &FunctionWrap::as_wrapper_p_h_seq, this, modname);
}

FunctionWrap::FunctionWrap(ValuePtr (p)(AtomSpace*, const Handle&, const HandleSeq&),
                           const char* funcname, const char* modname)
	: _func_h_ah(nullptr), _func_h_ahz(nullptr), _func_h_ah_seq(nullptr),
	  _pred_ah(nullptr), _pred_ah_seq(nullptr),
	  _proto_ah(nullptr), _proto_ah_seq(p),
	  _name(funcname)
{
	define_scheme_primitive(_name, &FunctionWrap::as_wrapper_v_h_seq, this, modname);
}

Handle FunctionWrap::as_wrapper_h_h(Handle h)
{
	// Note: For optional arguments as list of handles, use as_wrapper_h_h_seq
	const AtomSpacePtr& asp = SchemeSmob::ss_get_env_as(_name);
	AtomSpace* as = asp.get();
	return _func_h_ah(as, h);
}

Handle FunctionWrap::as_wrapper_h_hz(Handle h, size_t sz)
{
	// Note: For optional arguments as list of handles, use as_wrapper_h_h_seq
	const AtomSpacePtr& asp = SchemeSmob::ss_get_env_as(_name);
	AtomSpace* as = asp.get();
	return _func_h_ahz(as, h, sz);
}

TruthValuePtr FunctionWrap::as_wrapper_p_h(Handle h)
{
	// Note: For optional arguments as list of handles, use as_wrapper_p_h_seq
	const AtomSpacePtr& asp = SchemeSmob::ss_get_env_as(_name);
	AtomSpace* as = asp.get();
	return _pred_ah(as, h);
}

ValuePtr FunctionWrap::as_wrapper_v_h(Handle h)
{
	// Note: For optional arguments as list of handles, use as_wrapper_v_h_seq
	const AtomSpacePtr& asp = SchemeSmob::ss_get_env_as(_name);
	AtomSpace* as = asp.get();
	return _proto_ah(as, h);
}

// XXX New wrapper methods supporting optional arguments as list of handles
Handle FunctionWrap::as_wrapper_h_h_seq(Handle h, const HandleSeq& opt_args)
{
	const AtomSpacePtr& asp = SchemeSmob::ss_get_env_as(_name);
	AtomSpace* as = asp.get();
	return _func_h_ah_seq(as, h, opt_args);
}

TruthValuePtr FunctionWrap::as_wrapper_p_h_seq(Handle h, const HandleSeq& opt_args)
{
	const AtomSpacePtr& asp = SchemeSmob::ss_get_env_as(_name);
	AtomSpace* as = asp.get();
	return _pred_ah_seq(as, h, opt_args);
}

ValuePtr FunctionWrap::as_wrapper_v_h_seq(Handle h, const HandleSeq& opt_args)
{
	const AtomSpacePtr& asp = SchemeSmob::ss_get_env_as(_name);
	AtomSpace* as = asp.get();
	return _proto_ah_seq(as, h, opt_args);
}

// ========================================================

ModuleWrap::ModuleWrap(const char* m) :
	_modname(m)
{}

void ModuleWrap::module_init(void)
{
	scm_with_guile(init_in_guile, this);
}

void* ModuleWrap::init_in_guile(void* data)
{
	// init_in_module(NULL);
	ModuleWrap* self = (ModuleWrap*) data;

	scm_c_define_module(self->_modname, init_in_module, data);
	scm_c_use_module(self->_modname);

	return NULL;
}

/// This is called while _modname is the current module.
/// Thus, all the definitions below happen in that module.
void ModuleWrap::init_in_module(void* data)
{
	ModuleWrap* self = (ModuleWrap*) data;
	self->init();
}

ModuleWrap::~ModuleWrap()
{
}
