/*
 * ProxyNode.cc
 *
 * Copyright (C) 2022 Linas Vepstas
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

#include <opencog/persist/proxy/ProxyNode.h>

using namespace opencog;

ProxyNode::ProxyNode(const std::string&& name)
	: StorageNode(PROXY_NODE, std::move(name))
{
	init();
}

ProxyNode::ProxyNode(Type t, const std::string&& name)
	: StorageNode(t, std::move(name))
{
	init();
}

ProxyNode::~ProxyNode()
{
}

void ProxyNode::init(void)
{
	have_getAtom = false;
	have_fetchIncomingSet = false;
	have_fetchIncomingByType = false;
	have_storeAtom = false;
	have_removeAtom = false;
	have_storeValue = false;
	have_updateValue = false;
	have_loadValue = false;
	have_loadType = false;
	have_loadAtomSpace = false;
	have_storeAtomSpace = false;
}

void ProxyNode::proxy_open(void)
{
	throw RuntimeException(TRACE_INFO,
		"If you want to open the proxy, just say `cog-open`");
}

void ProxyNode::proxy_close(void)
{
	throw RuntimeException(TRACE_INFO,
		"If you want to close the proxy, just say `cog-close`");
}

void ProxyNode::set_proxy(const Handle&)
{
	throw RuntimeException(TRACE_INFO,
		"Error: `cog-set-proxy!` is not appropriate, here.");
}

std::string ProxyNode::monitor(void)
{
	std::string rpt;
	rpt += "ProxyNode Monitor Report:\n";
	rpt += "=======================\n";
	rpt += "Node: " + to_short_string() + "\n";
	rpt += "State: " + std::string(is_open() ? "OPEN" : "CLOSED") + "\n\n";
	
	// Report on which methods are available
	rpt += "Available Methods:\n";
	if (have_getAtom) rpt += "  [✓] getAtom\n";
	else rpt += "  [✗] getAtom\n";
	
	if (have_fetchIncomingSet) rpt += "  [✓] fetchIncomingSet\n";
	else rpt += "  [✗] fetchIncomingSet\n";
	
	if (have_fetchIncomingByType) rpt += "  [✓] fetchIncomingByType\n";
	else rpt += "  [✗] fetchIncomingByType\n";
	
	if (have_storeAtom) rpt += "  [✓] storeAtom\n";
	else rpt += "  [✗] storeAtom\n";
	
	if (have_removeAtom) rpt += "  [✓] removeAtom\n";
	else rpt += "  [✗] removeAtom\n";
	
	if (have_storeValue) rpt += "  [✓] storeValue\n";
	else rpt += "  [✗] storeValue\n";
	
	if (have_updateValue) rpt += "  [✓] updateValue\n";
	else rpt += "  [✗] updateValue\n";
	
	if (have_loadValue) rpt += "  [✓] loadValue\n";
	else rpt += "  [✗] loadValue\n";
	
	if (have_loadType) rpt += "  [✓] loadType\n";
	else rpt += "  [✗] loadType\n";
	
	if (have_loadAtomSpace) rpt += "  [✓] loadAtomSpace\n";
	else rpt += "  [✗] loadAtomSpace\n";
	
	if (have_storeAtomSpace) rpt += "  [✓] storeAtomSpace\n";
	else rpt += "  [✗] storeAtomSpace\n";
	
	// Get backend storage nodes if any
	StorageNodeSeq backends = setup();
	if (!backends.empty())
	{
		rpt += "\nBackend Storage Nodes:\n";
		for (size_t i = 0; i < backends.size(); ++i)
		{
			rpt += "  [" + std::to_string(i) + "] " + backends[i]->to_short_string() + "\n";
			rpt += "      State: " + std::string(backends[i]->is_open() ? "OPEN" : "CLOSED") + "\n";
		}
	}
	else
	{
		rpt += "\nNo backend storage nodes configured.\n";
	}
	
	return rpt;
}

// Get our configuration from the DefineLink we live in.
// Hmm, perhaps this should be a StateLink?
//
// XXX FIXME. Using this ProxyParametersLink thing is a kind of
// cheesy hack, to pass parameters to the ProxyNode. It vaguely
// resembles the structure of an ExecutionLink, but instead of
// writing (Execution (Predicate "foo") (List (args...)))
// we write (ProxyParameters (Proxy "foo") (List (params...)))
// Except that we don't have a C++ class for ProxyParameters
// and it is not executable. So ... I dunno. I'm not happy with
// this design.
//
// More generally there is the work in sensory to create a DTD/IDL
// to describe parameters. The design work there is not done, but
// when it is, this should convert to that.
StorageNodeSeq ProxyNode::setup(void)
{
	StorageNodeSeq stolist;

	IncomingSet dli(getIncomingSetByType(PROXY_PARAMETERS_LINK));

	// We could throw an error here ... or we can just no-op.
	if (0 == dli.size()) return stolist;

	// Don't know what to do if there are two or more parameter sets.
	if (1 != dli.size())
	{
		throw SyntaxException(TRACE_INFO,
			"Expecting only one set of parameters for a ProxyNode. Got %lu of them:\n%s\n",
			dli.size(), oc_to_string(dli).c_str());
	}

	// If there is no ListLink, then just grab that.
	const Handle& params = dli[0]->getOutgoingAtom(1);
	if (params->is_type(STORAGE_NODE))
	{
		stolist.emplace_back(StorageNodeCast(params));
		return stolist;
	}

	// Expect the parameters to be wrapped in a ListLink
	if (not params->is_type(LIST_LINK))
		throw SyntaxException(TRACE_INFO,
			"Expecting parameters in a ListLink! Got\n%s\n",
			dli[0]->to_short_string().c_str());

	for (const Handle& h : params->getOutgoingSet())
	{
		StorageNodePtr stnp = StorageNodeCast(h);
		if (nullptr == stnp)
		{
			// If its a StorageNode but the cast failed, that
			// means the type definition was not loaded. Print a
			// user-friendly error message for this case.
			if (nameserver().isA(h->get_type(), STORAGE_NODE))
			{
				throw SyntaxException(TRACE_INFO,
					"There is no definition for %s.\n"
					"Did you forget to load the module that deffines this?\n"
					"For example: `(use-modules (opencog persist-rocks))`\n"
					"Config was %s\n",
					h->to_short_string().c_str(),
					dli[0]->to_short_string().c_str());
			}
			throw SyntaxException(TRACE_INFO,
				"Expecting a list of Storage or ProxyNodes! Got\n%s\n",
				dli[0]->to_short_string().c_str());
		}

		stolist.emplace_back(stnp);
	}

	return stolist;
}

void ProxyNode::destroy(void) 
{
	// Close all backend connections before destroying
	if (is_open())
	{
		proxy_close();
	}
	
	// Clear all backend storage nodes
	StorageNodeSeq backends = setup();
	for (auto& backend : backends)
	{
		if (backend && backend->is_open())
		{
			backend->close();
		}
	}
}

void ProxyNode::erase(void) 
{
	// Erase data from all backend storage nodes
	StorageNodeSeq backends = setup();
	for (auto& backend : backends)
	{
		if (backend && backend->is_open())
		{
			backend->erase();
		}
	}
}

HandleSeq ProxyNode::loadFrameDAG(void)
{
	HandleSeq result;
	
	// Load frame DAG from all backend storage nodes
	StorageNodeSeq backends = setup();
	for (auto& backend : backends)
	{
		if (backend && backend->is_open())
		{
			HandleSeq backend_frames = backend->loadFrameDAG();
			result.insert(result.end(), backend_frames.begin(), backend_frames.end());
		}
	}
	
	// Remove duplicates if any
	std::sort(result.begin(), result.end());
	result.erase(std::unique(result.begin(), result.end()), result.end());
	
	return result;
}

void ProxyNode::storeFrameDAG(AtomSpace* frame)
{
	// Store frame DAG to all backend storage nodes
	StorageNodeSeq backends = setup();
	for (auto& backend : backends)
	{
		if (backend && backend->is_open())
		{
			backend->storeFrameDAG(frame);
		}
	}
}

void ProxyNode::deleteFrame(AtomSpace* frame)
{
	// Delete frame from all backend storage nodes
	StorageNodeSeq backends = setup();
	for (auto& backend : backends)
	{
		if (backend && backend->is_open())
		{
			backend->deleteFrame(frame);
		}
	}
}

Handle ProxyNode::getLink(Type t, const HandleSeq& hseq)
{
	// Ugh Copy
	HandleSeq hsc(hseq);
	return _atom_space->get_link(t, std::move(hsc));
}

void opencog_persist_proxy_init(void)
{
   // Force shared lib ctors to run
};

/* ===================== END OF FILE ===================== */
