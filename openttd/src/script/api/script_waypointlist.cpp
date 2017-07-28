/* $Id: script_waypointlist.cpp 23632 2011-12-19 21:05:25Z truebrain $ */

/*
 * This file is part of OpenTTD.
 * OpenTTD is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, version 2.
 * OpenTTD is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with OpenTTD. If not, see <http://www.gnu.org/licenses/>.
 */

/** @file script_waypointlist.cpp Implementation of ScriptWaypointList and friends. */

#include "../../stdafx.h"
#include "script_waypointlist.hpp"
#include "script_vehicle.hpp"
#include "../../vehicle_base.h"
#include "../../waypoint_base.h"

ScriptWaypointList::ScriptWaypointList(ScriptWaypoint::WaypointType waypoint_type)
{
	const Waypoint *wp;
	FOR_ALL_WAYPOINTS(wp) {
		if ((wp->facilities & waypoint_type) &&
				(wp->owner == ScriptObject::GetCompany() || ScriptObject::GetCompany() == OWNER_DEITY || wp->owner == OWNER_NONE)) this->AddItem(wp->index);
	}
}

ScriptWaypointList_Vehicle::ScriptWaypointList_Vehicle(VehicleID vehicle_id)
{
	if (!ScriptVehicle::IsValidVehicle(vehicle_id)) return;

	const Vehicle *v = ::Vehicle::Get(vehicle_id);

	for (const Order *o = v->GetFirstOrder(); o != NULL; o = o->next) {
		if (o->IsType(OT_GOTO_WAYPOINT)) this->AddItem(o->GetDestination());
	}
}
