/* $Id: script_accounting.cpp 23355 2011-11-29 23:15:35Z truebrain $ */

/*
 * This file is part of OpenTTD.
 * OpenTTD is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, version 2.
 * OpenTTD is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with OpenTTD. If not, see <http://www.gnu.org/licenses/>.
 */

/** @file script_accounting.cpp Implementation of ScriptAccounting. */

#include "../../stdafx.h"
#include "script_accounting.hpp"

Money ScriptAccounting::GetCosts()
{
	return this->GetDoCommandCosts();
}

void ScriptAccounting::ResetCosts()
{
	this->SetDoCommandCosts(0);
}

ScriptAccounting::ScriptAccounting()
{
	this->last_costs = this->GetDoCommandCosts();
	this->SetDoCommandCosts(0);
}

ScriptAccounting::~ScriptAccounting()
{
	this->SetDoCommandCosts(this->last_costs);
}
