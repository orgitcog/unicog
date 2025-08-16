/*
 * opencog/util/lazy_normal_selector.h
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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program; if not, write to:
 * Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#ifndef _OPENCOG_LAZY_NORMAL_SELECTOR_H
#define _OPENCOG_LAZY_NORMAL_SELECTOR_H

#include <opencog/util/lazy_selector.h>
#include <opencog/util/oc_assert.h>
#include <random>

namespace opencog
{
/** \addtogroup grp_cogutil
 *  @{
 */

//! apply lazy_selector but the select method returns numbers following a normal distribution
//! around the given mean with the given standard deviation
struct lazy_normal_selector : public lazy_selector {
    lazy_normal_selector(unsigned int n, unsigned int mean = 0, unsigned int stddev = 1) :
        lazy_selector(n), _mean(mean), _stddev(stddev), _rng(std::random_device{}()) {
        OC_ASSERT(mean < n, "Mean must be less than n");
        OC_ASSERT(stddev > 0, "Standard deviation must be positive");
    }
protected:
    unsigned int select() override;
private:
    unsigned int _mean;
    unsigned int _stddev;
    mutable std::mt19937 _rng;
};

/** @}*/
} //~namespace opencog

#endif
