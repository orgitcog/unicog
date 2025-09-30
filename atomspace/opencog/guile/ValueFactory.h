/*
 * ValueFactory.h
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

#ifndef _OPENCOG_VALUE_FACTORY_H
#define _OPENCOG_VALUE_FACTORY_H

#include <functional>
#include <map>
#include <memory>
#include <libguile.h>

#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/value/Value.h>
#include <opencog/atoms/atom_types/types.h>

namespace opencog {

class AtomSpace;
typedef std::shared_ptr<AtomSpace> AtomSpacePtr;

/**
 * ValueFactory - Factory pattern for creating Values from Scheme
 *
 * This class implements a factory pattern for creating different types
 * of Values based on their Type. It provides a cleaner, more extensible
 * alternative to a large switch statement.
 */
class ValueFactory
{
public:
	typedef std::function<ValuePtr(Type, SCM, const AtomSpacePtr&)> CreatorFunc;

private:
	// Map from Type to creator function
	std::map<Type, CreatorFunc> _creators;
	
	// Singleton instance
	static ValueFactory* _instance;
	
	// Private constructor for singleton
	ValueFactory();
	
	// Initialize standard creators
	void initializeCreators();
	
	// Creator functions for different value types
	static ValuePtr createRandomStream(Type t, SCM svalue_list, const AtomSpacePtr& asp);
	static ValuePtr createFormulaStream(Type t, SCM svalue_list, const AtomSpacePtr& asp);
	static ValuePtr createFloatValue(Type t, SCM svalue_list, const AtomSpacePtr& asp);
	static ValuePtr createBoolValue(Type t, SCM svalue_list, const AtomSpacePtr& asp);
	static ValuePtr createLinkValue(Type t, SCM svalue_list, const AtomSpacePtr& asp);
	static ValuePtr createStringValue(Type t, SCM svalue_list, const AtomSpacePtr& asp);
	static ValuePtr createVoidValue(Type t, SCM svalue_list, const AtomSpacePtr& asp);
	static ValuePtr createNode(Type t, SCM svalue_list, const AtomSpacePtr& asp);
	static ValuePtr createLink(Type t, SCM svalue_list, const AtomSpacePtr& asp);

public:
	// Get singleton instance
	static ValueFactory& getInstance();
	
	// Register a creator function for a type hierarchy
	void registerCreator(Type t, CreatorFunc creator);
	
	// Create a value of the given type
	ValuePtr createValue(Type t, SCM svalue_list, const AtomSpacePtr& asp);
	
	// Check if we can create values of this type
	bool canCreate(Type t) const;
};

} // namespace opencog

#endif // _OPENCOG_VALUE_FACTORY_H