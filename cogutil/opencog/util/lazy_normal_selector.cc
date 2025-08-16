/*
 * opencog/util/lazy_normal_selector.cc
 *
 * Copyright (C) 2002-2007 Novamente LLC
 * All Rights Reserved
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Free Software Foundation and including the exceptions
 * at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR ANY PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program; if not, write to:
 * Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include "lazy_normal_selector.h"
#include <opencog/util/oc_assert.h>

namespace opencog
{

unsigned int lazy_normal_selector::select()
{
    // Generate a normally distributed random number
    std::normal_distribution<double> dist(_mean, _stddev);
    
    // Generate numbers until we get one in the valid range [0, n)
    int result;
    do {
        result = static_cast<int>(std::round(dist(_rng)));
    } while (result < 0 || result >= static_cast<int>(_u));
    
    return static_cast<unsigned int>(result);
}

} // namespace opencog
