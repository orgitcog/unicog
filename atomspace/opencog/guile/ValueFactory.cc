/*
 * ValueFactory.cc
 *
 * Copyright (C) 2025 OpenCog Foundation
 * All Rights Reserved
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

#include "ValueFactory.h"
#include "SchemeSmob.h"

#include <opencog/atoms/value/ValueServer.h>
#include <opencog/atoms/atom_types/NameServer.h>
#include <opencog/atomspace/AtomSpace.h>

using namespace opencog;

// Initialize singleton instance
ValueFactory* ValueFactory::_instance = nullptr;

ValueFactory& ValueFactory::getInstance()
{
	if (!_instance) {
		_instance = new ValueFactory();
	}
	return *_instance;
}

ValueFactory::ValueFactory()
{
	initializeCreators();
}

void ValueFactory::initializeCreators()
{
	// Register creators for specific types
	registerCreator(RANDOM_STREAM, createRandomStream);
	registerCreator(FUTURE_STREAM, createFormulaStream);
	registerCreator(FORMULA_STREAM, createFormulaStream);
	
	// Register creators for type hierarchies
	// These will be used as fallbacks for derived types
	registerCreator(FLOAT_VALUE, createFloatValue);
	registerCreator(BOOL_VALUE, createBoolValue);
	registerCreator(LINK_VALUE, createLinkValue);
	registerCreator(STRING_VALUE, createStringValue);
	registerCreator(VOID_VALUE, createVoidValue);
	registerCreator(NODE, createNode);
	registerCreator(LINK, createLink);
}

void ValueFactory::registerCreator(Type t, CreatorFunc creator)
{
	_creators[t] = creator;
}

bool ValueFactory::canCreate(Type t) const
{
	// Check if we have a direct creator
	if (_creators.find(t) != _creators.end()) {
		return true;
	}
	
	// Check if any parent type has a creator
	NameServer& ns = nameserver();
	for (const auto& [type, creator] : _creators) {
		if (ns.isA(t, type)) {
			return true;
		}
	}
	
	return false;
}

ValuePtr ValueFactory::createValue(Type t, SCM svalue_list, const AtomSpacePtr& asp)
{
	// Look for exact type match first
	auto it = _creators.find(t);
	if (it != _creators.end()) {
		return it->second(t, svalue_list, asp);
	}
	
	// Look for parent type match
	NameServer& ns = nameserver();
	for (const auto& [type, creator] : _creators) {
		if (ns.isA(t, type)) {
			return creator(t, svalue_list, asp);
		}
	}
	
	// Type not supported
	scm_wrong_type_arg_msg("cog-new-value", 1, 
		scm_from_utf8_symbol(ns.getTypeName(t).c_str()), 
		"unsupported value type");
	return nullptr;
}

// Creator implementations

ValuePtr ValueFactory::createRandomStream(Type t, SCM svalue_list, const AtomSpacePtr& asp)
{
	if (!scm_is_pair(svalue_list) and !scm_is_null(svalue_list))
		scm_wrong_type_arg_msg("cog-new-value", 1,
			svalue_list, "an optional dimension");
	int dim = 1;

	if (!scm_is_null(svalue_list))
	{
		SCM svalue = SCM_CAR(svalue_list);
		dim = SchemeSmob::verify_int(svalue, "cog-new-value", 2);
	}
	return valueserver().create(t, dim);
}

ValuePtr ValueFactory::createFormulaStream(Type t, SCM svalue_list, const AtomSpacePtr& asp)
{
	HandleSeq oset(SchemeSmob::verify_handle_list(svalue_list, "cog-new-value", 2));
	return valueserver().create(t, std::move(oset));
}

ValuePtr ValueFactory::createFloatValue(Type t, SCM svalue_list, const AtomSpacePtr& asp)
{
	std::vector<double> valist;
	valist = SchemeSmob::verify_float_list(svalue_list, "cog-new-value", 2);
	return valueserver().create(t, valist);
}

ValuePtr ValueFactory::createBoolValue(Type t, SCM svalue_list, const AtomSpacePtr& asp)
{
	// Special case -- if it is a single integer, then its a mask.
	SCM sval = SCM_CAR(svalue_list);
	SCM srest = SCM_CDR(svalue_list);
	if (scm_is_null(srest) and scm_is_integer(sval))
	{
		size_t mask = SchemeSmob::verify_size_t(sval, "cog-new-value", 2);
		return valueserver().create(t, mask);
	}
	std::vector<bool> valist;
	valist = SchemeSmob::verify_bool_list(svalue_list, "cog-new-value", 2);
	return valueserver().create(t, valist);
}

ValuePtr ValueFactory::createLinkValue(Type t, SCM svalue_list, const AtomSpacePtr& asp)
{
	std::vector<ValuePtr> valist;
	valist = SchemeSmob::verify_protom_list(svalue_list, "cog-new-value", 2);
	return valueserver().create(t, valist);
}

ValuePtr ValueFactory::createStringValue(Type t, SCM svalue_list, const AtomSpacePtr& asp)
{
	std::vector<std::string> valist;
	valist = SchemeSmob::verify_string_list(svalue_list, "cog-new-value", 2);
	return valueserver().create(t, valist);
}

ValuePtr ValueFactory::createVoidValue(Type t, SCM svalue_list, const AtomSpacePtr& asp)
{
	return valueserver().create(t);
}

ValuePtr ValueFactory::createNode(Type t, SCM svalue_list, const AtomSpacePtr& asp)
{
	// Note: At this time, nodes have a single name.
	// This may change in the future to support multi-valued nodes
	SCM sname = SCM_CAR(svalue_list);
	std::string name = SchemeSmob::verify_string(sname, "cog-new-value", 2);
	return asp->add_node(t, std::move(name));
}

ValuePtr ValueFactory::createLink(Type t, SCM svalue_list, const AtomSpacePtr& asp)
{
	HandleSeq oset = SchemeSmob::verify_handle_list(svalue_list, "cog-new-value", 2);
	return asp->add_link(t, std::move(oset));
}