/* $Id: cargopacket.h 24900 2013-01-08 22:46:42Z planetmaker $ */

/*
 * This file is part of OpenTTD.
 * OpenTTD is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, version 2.
 * OpenTTD is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with OpenTTD. If not, see <http://www.gnu.org/licenses/>.
 */

/** @file cargopacket.h Base class for cargo packets. */

#ifndef CARGOPACKET_H
#define CARGOPACKET_H

#include "core/pool_type.hpp"
#include "economy_type.h"
#include "station_type.h"
#include "cargo_type.h"
#include "vehicle_type.h"
#include "order_type.h"
#include "cargotype.h"
#include "core/multimap.hpp"
#include <list>
#include <map>

/** Unique identifier for a single cargo packet. */
typedef uint32 CargoPacketID;
struct CargoPacket;

/** Type of the pool for cargo packets for a little over 16 million packets. */
typedef Pool<CargoPacket, CargoPacketID, 1024, 0xFFF000, PT_NORMAL, true, false> CargoPacketPool;
/** The actual pool with cargo packets. */
extern CargoPacketPool _cargopacket_pool;

template <class Tinst, class Tcont> class CargoList;
class StationCargoList; // forward-declare, so we can use it in VehicleCargoList.
extern const struct SaveLoad *GetCargoPacketDesc();

typedef uint32 TileOrStationID;

/**
 * Container for cargo from the same location and time.
 */
struct CargoPacket : CargoPacketPool::PoolItem<&_cargopacket_pool> {
private:
	Money feeder_share;         ///< Value of feeder pickup to be paid for on delivery of cargo.
	uint16 count;               ///< The amount of cargo in this packet.
	byte days_in_transit;       ///< Amount of days this packet has been in transit.
	SourceTypeByte source_type; ///< Type of \c source_id.
	SourceID source_id;         ///< Index of source, INVALID_SOURCE if unknown/invalid.
	StationID source;           ///< The station where the cargo came from first.
	TileIndex source_xy;        ///< The origin of the cargo (first station in feeder chain).
	TileIndex loaded_at_xy;     ///< Location where this cargo has been loaded into the vehicle.
	TileIndex dest_xy;          ///< Destination tile or INVALID_TILE if no specific destination
	SourceID dest_id;           ///< Index of the destination.
	SourceTypeByte dest_type;   ///< Type of #dest_id.
	byte flags;                 ///< Flags influencing the routing decision of this packet, see #RouteFlags.
	OrderID next_order;         ///< Next desired hop.
	StationID next_station;     ///< Unload at this station next.

	/** The CargoList caches, thus needs to know about it. */
	template <class Tinst, class Tcont> friend class CargoList;
	friend class VehicleCargoList;
	friend class StationCargoList;
	/** We want this to be saved, right? */
	friend const struct SaveLoad *GetCargoPacketDesc();
	friend bool CargodestModeChanged(int32 p1);
public:
	/** Maximum number of items in a single cargo packet. */
	static const uint16 MAX_COUNT = UINT16_MAX;

	CargoPacket();
	CargoPacket(StationID source, TileIndex source_xy, uint16 count, SourceType source_type, SourceID source_id, TileIndex dest_xy = INVALID_TILE, SourceType dest_type = ST_INDUSTRY, SourceID dest_id = INVALID_SOURCE, OrderID next_order = INVALID_ORDER, StationID next_station = INVALID_STATION, byte flags = 0);
	CargoPacket(uint16 count, byte days_in_transit, StationID source, TileIndex source_xy, TileIndex loaded_at_xy, Money feeder_share = 0, SourceType source_type = ST_INDUSTRY, SourceID source_id = INVALID_SOURCE, TileIndex dest_xy = INVALID_TILE, SourceType dest_type = ST_INDUSTRY, SourceID dest_id = INVALID_SOURCE, OrderID next_order = INVALID_ORDER, StationID next_station = INVALID_STATION, byte flags = 0);

	/** Destroy the packet. */
	~CargoPacket() { }

	CargoPacket *Split(uint new_size);
	void Merge(CargoPacket *cp);
	void Reduce(uint count);

	/**
	 * Sets the tile where the packet was loaded last.
	 * @param load_place Tile where the packet was loaded last.
	 */
	void SetLoadPlace(TileIndex load_place) { this->loaded_at_xy = load_place; }

	/**
	 * Adds some feeder share to the packet.
	 * @param new_share Feeder share to be added.
	 */
	void AddFeederShare(Money new_share) { this->feeder_share += new_share; }

	/**
	 * Gets the number of 'items' in this packet.
	 * @return Item count.
	 */
	inline uint16 Count() const
	{
		return this->count;
	}

	/**
	 * Gets the amount of money already paid to earlier vehicles in
	 * the feeder chain.
	 * @return Feeder share.
	 */
	inline Money FeederShare() const
	{
		return this->feeder_share;
	}

	/**
	 * Gets part of the amount of money already paid to earlier vehicles in
	 * the feeder chain.
	 * @param part Amount of cargo to get the share for.
	 * @return Feeder share for the given amount of cargo.
	 */
	inline Money FeederShare(uint part) const
	{
		return this->feeder_share * part / static_cast<uint>(this->count);
	}

	/**
	 * Gets the number of days this cargo has been in transit.
	 * This number isn't really in days, but in 2.5 days (CARGO_AGING_TICKS = 185 ticks) and
	 * it is capped at 255.
	 * @return Length this cargo has been in transit.
	 */
	inline byte DaysInTransit() const
	{
		return this->days_in_transit;
	}

	/**
	 * Gets the type of the cargo's source. industry, town or head quarter.
	 * @return Source type.
	 */
	inline SourceType SourceSubsidyType() const
	{
		return this->source_type;
	}

	/**
	 * Gets the ID of the cargo's source. An IndustryID, TownID or CompanyID.
	 * @return Source ID.
	 */
	inline SourceID SourceSubsidyID() const
	{
		return this->source_id;
	}

	/**
	 * Gets the ID of the station where the cargo was loaded for the first time.
	 * @return StationID.
	 */
	inline SourceID SourceStation() const
	{
		return this->source;
	}

	/**
	 * Gets the coordinates of the cargo's source station.
	 * @return Source station's coordinates.
	 */
	inline TileIndex SourceStationXY() const
	{
		return this->source_xy;
	}

	/**
	 * Gets the coordinates of the cargo's last loading station.
	 * @return Last loading station's coordinates.
	 */
	inline TileIndex LoadedAtXY() const
	{
		return this->loaded_at_xy;
	}

	/**
	 * Gets the coordinates of the cargo's destination.
	 * @return The destination tile.
	 */
	inline TileIndex DestinationXY() const
	{
		return this->dest_xy;
	}

	/**
	 * Gets the ID of the destination of the cargo.
	 * @return The destination ID.
	 */
	inline SourceID DestinationID() const
	{
		return this->dest_id;
	}

	/**
	 * Gets the type of the destination of the cargo.
	 * @return The destination type.
	 */
	inline SourceType DestinationType() const
	{
		return this->dest_type;
	}

	/**
	 * Gets the routing behaviour flags of this packet.
	 * @return The routing flags.
	 */
	inline byte Flags() const
	{
		return this->flags;
	}

	/**
	 * Gets the order ID of the next desired hop.
	 * @return The order ID of the next desired hop.
	 */
	inline OrderID NextHop() const
	{
		return this->next_order;
	}

	/**
	 * Gets the station ID where the packet should be unloaded next.
	 * @return The station ID where the packet should be unloaded.
	 */
	inline StationID NextStation() const
	{
		return this->next_station;
	}

	static void InvalidateAllFrom(SourceType src_type, SourceID src);
	static void InvalidateAllFrom(StationID sid);
	static void AfterLoad();
};

/**
 * Iterate over all _valid_ cargo packets from the given start.
 * @param var   Variable used as "iterator".
 * @param start Cargo packet ID of the first packet to iterate over.
 */
#define FOR_ALL_CARGOPACKETS_FROM(var, start) FOR_ALL_ITEMS_FROM(CargoPacket, cargopacket_index, var, start)

/**
 * Iterate over all _valid_ cargo packets from the begin of the pool.
 * @param var   Variable used as "iterator".
 */
#define FOR_ALL_CARGOPACKETS(var) FOR_ALL_CARGOPACKETS_FROM(var, 0)

/**
 * Simple collection class for a list of cargo packets.
 * @tparam Tinst Actual instantiation of this cargo list.
 */
template <class Tinst, class Tcont>
class CargoList {
public:
	/** The iterator for our container. */
	typedef typename Tcont::iterator Iterator;
	/** The reverse iterator for our container. */
	typedef typename Tcont::reverse_iterator ReverseIterator;
	/** The const iterator for our container. */
	typedef typename Tcont::const_iterator ConstIterator;
	/** The const reverse iterator for our container. */
	typedef typename Tcont::const_reverse_iterator ConstReverseIterator;

	/** Kind of actions that could be done with packets on move. */
	enum MoveToAction {
		MTA_BEGIN = 0,
		MTA_TRANSFER = 0, ///< Transfer the cargo to the station.
		MTA_DELIVER,      ///< Deliver the cargo to some town or industry.
		MTA_KEEP,         ///< Keep the cargo in the vehicle.
		MTA_LOAD,         ///< Load the cargo from the station.
		MTA_END,
		NUM_MOVE_TO_ACTION = MTA_END
	};

	friend bool CargodestModeChanged(int32 p1);

protected:
	uint count;                 ///< Cache for the number of cargo entities.
	uint cargo_days_in_transit; ///< Cache for the sum of number of days in transit of each entity; comparable to man-hours.

	Tcont packets;              ///< The cargo packets in this list.

	void AddToCache(const CargoPacket *cp);

	void RemoveFromCache(const CargoPacket *cp, uint count);

	static bool TryMerge(CargoPacket *cp, CargoPacket *icp);

	void RemoveFromCacheLocal(const CargoPacket *cp, uint amount) {}

	virtual bool UpdateCargoNextHop(CargoPacket *cp, Station *st, CargoID cid)
	{
		return true;
	}

public:
	/** Create the cargo list. */
	CargoList() {}

	virtual ~CargoList();

	void OnCleanPool();

	/**
	 * Returns a pointer to the cargo packet list (so you can iterate over it etc).
	 * @return Pointer to the packet list.
	 */
	inline const Tcont *Packets() const
	{
		return &this->packets;
	}

	/**
	 * Returns average number of days in transit for a cargo entity.
	 * @return The before mentioned number.
	 */
	inline uint DaysInTransit() const
	{
		return this->count == 0 ? 0 : this->cargo_days_in_transit / this->count;
	}

	void InvalidateCache();
};

typedef std::list<CargoPacket *> CargoPacketList;

/**
 * CargoList that is used for vehicles.
 */
class VehicleCargoList : public CargoList<VehicleCargoList, CargoPacketList> {
protected:
	/** The (direct) parent of this class. */
	typedef CargoList<VehicleCargoList, CargoPacketList> Parent;

	Money feeder_share;                     ///< Cache for the feeder share.
	uint action_counts[NUM_MOVE_TO_ACTION]; ///< Counts of cargo to be transfered, delivered, kept and loaded.

	template<class Taction>
	void ShiftCargo(Taction action);
	
	template<class Taction>
	void PopCargo(Taction action);
	
	/**
	 * Assert that the designation counts add up.
	 */
	inline void AssertCountConsistency() const
	{
		assert(this->action_counts[MTA_KEEP] +
				this->action_counts[MTA_DELIVER] +
				this->action_counts[MTA_TRANSFER] +
				this->action_counts[MTA_LOAD] == this->count);
	}

	void AddToCache(const CargoPacket *cp);
	void RemoveFromCache(const CargoPacket *cp, uint count);

	void AddToMeta(const CargoPacket *cp, MoveToAction action);
	void RemoveFromMeta(const CargoPacket *cp, MoveToAction action, uint count);

public:
	/** The station cargo list needs to control the unloading. */
	friend class StationCargoList;
	/** The super class ought to know what it's doing. */
	friend class CargoList<VehicleCargoList, CargoPacketList>;
	/** The vehicles have a cargo list (and we want that saved). */
	friend const struct SaveLoad *GetVehicleDescription(VehicleType vt);

	friend class CargoShift;
	friend class CargoTransfer;
	friend class CargoDelivery;
	template<class Tsource>
	friend class CargoRemoval;
	friend class CargoReturn;

	/**
	 * Returns source of the first cargo packet in this list.
	 * @return The before mentioned source.
	 */
	inline StationID Source() const
	{
		return this->count == 0 ? INVALID_STATION : this->packets.front()->source;
	}
	
	
	/**
	 * Returns total sum of the feeder share for all packets.
	 * @return The before mentioned number.
	 */
	inline Money FeederShare() const
	{
		return this->feeder_share;
	}

	/**
	 * Returns the amount of cargo designated for a given purpose.
	 * @param action Action the cargo is designated for.
	 * @return Amount of cargo designated for the given action.
	 */
	inline uint ActionCount(MoveToAction action) const
	{
		return this->action_counts[action];
	}

	/**
	 * Returns sum of cargo on board the vehicle (ie not only
	 * reserved).
	 * @return Cargo on board the vehicle.
	 */
	inline uint StoredCount() const
	{
		return this->count - this->action_counts[MTA_LOAD];
	}

	/**
	 * Returns sum of cargo, including reserved cargo.
	 * @return Sum of cargo.
	 */
	inline uint TotalCount() const
	{
		return this->count;
	}

	/**
	 * Returns sum of reserved cargo.
	 * @return Sum of reserved cargo.
	 */
	inline uint ReservedCount() const
	{
		return this->action_counts[MTA_LOAD];
	}

	/**
	 * Returns sum of cargo to be moved out of the vehicle at the current station.
	 * @return Cargo to be moved.
	 */
	inline uint UnloadCount() const
	{
		return this->action_counts[MTA_TRANSFER] + this->action_counts[MTA_DELIVER];
	}

	/**
	 * Returns the sum of cargo to be kept in the vehicle at the current station.
	 * @return Cargo to be kept or loaded.
	 */
	inline uint RemainingCount() const
	{
		return this->action_counts[MTA_KEEP] + this->action_counts[MTA_LOAD];
	}

	void Append(CargoPacket *cp, MoveToAction action = MTA_KEEP);

	void AgeCargo();

	void InvalidateCache();
	void SetTransferLoadPlace(TileIndex xy);
	
	bool Stage(bool accepted, StationID current_station, OrderID cur_order, uint8 order_flags, CargoID cid, CargoPayment *payment);

	/**
	 * Marks all cargo in the vehicle as to be kept. This is mostly useful for
	 * loading old savegames. When loading is aborted the reserved cargo has
	 * to be returned first.
	 */
	inline void KeepAll()
	{
		this->action_counts[MTA_DELIVER] = this->action_counts[MTA_TRANSFER] = this->action_counts[MTA_LOAD] = 0;
		this->action_counts[MTA_KEEP] = this->count;
	}

	/* Methods for moving cargo around. First parameter is always maximum
	 * amount of cargo to be moved. Second parameter is destination (if
	 * applicable), return value is amount of cargo actually moved. */

	uint Reassign(uint max_move, MoveToAction from, MoveToAction to);
	uint Return(uint max_move, StationCargoList *dest, StationID next_station);
	uint Unload(uint max_move, StationCargoList *dest, CargoPayment *payment);
	uint Shift(uint max_move, VehicleCargoList *dest);
	uint Truncate(uint max_move = UINT_MAX);


	void InvalidateNextStation();

	/**
	 * Are two the two CargoPackets mergeable in the context of
	 * a list of CargoPackets for a Vehicle?
	 * @param cp1 First CargoPacket.
	 * @param cp2 Second CargoPacket.
	 * @return True if they are mergeable.
	 */
	static bool AreMergable(const CargoPacket *cp1, const CargoPacket *cp2)
	{
		return cp1->source_xy    == cp2->source_xy &&
				cp1->days_in_transit == cp2->days_in_transit &&
				cp1->source_type     == cp2->source_type &&
				cp1->source_id       == cp2->source_id &&
				cp1->loaded_at_xy    == cp2->loaded_at_xy &&
				cp1->dest_xy         == cp2->dest_xy &&
				cp1->dest_type       == cp2->dest_type &&
				cp1->dest_id         == cp2->dest_id &&
				cp1->next_order      == cp2->next_order &&
				cp1->next_station    == cp2->next_station &&
				cp1->flags           == cp2->flags;
	}
};

typedef MultiMap<StationID, CargoPacket *> StationCargoPacketMap;

/**
 * CargoList that is used for stations.
 */
class StationCargoList : public CargoList<StationCargoList, StationCargoPacketMap> {
public:
	typedef std::map<OrderID, int> OrderMap;

protected:
	/** The (direct) parent of this class. */
	typedef CargoList<StationCargoList, StationCargoPacketMap> Parent;

	uint reserved_count; ///< Amount of cargo being reserved for loading.
	
	OrderMap order_cache;
	uint32 next_start;        ///< Packet number to start the next hop update loop from.

	void AddToCache(const CargoPacket *cp);
	void RemoveFromCache(const CargoPacket *cp, uint count);
	void RemoveFromCacheLocal(const CargoPacket *cp, uint amount);

	/* virtual */ bool UpdateCargoNextHop(CargoPacket *cp, Station *st, CargoID cid);

public:
	/** The super class ought to know what it's doing. */
	friend class CargoList<StationCargoList, StationCargoPacketMap>;
	/** The stations, via GoodsEntry, have a CargoList. */
	friend const struct SaveLoad *GetGoodsDesc();
	
	friend class CargoLoad;
	friend class CargoTransfer;
	template<class Tsource>
	friend class CargoRemoval;
	friend class CargoReservation;
	friend class CargoReturn;
	
	static void InvalidateAllFrom(SourceType src_type, SourceID src);
	
	template<class Taction>
	bool ShiftCargo(Taction &action, StationID next);

	template<class Taction>
	uint ShiftCargo(Taction action, StationID next, bool include_invalid);
	
	void Append(CargoPacket *cp, StationID next);

	/**
	 * Check for cargo headed for a specific station.
	 * @param next Station the cargo is headed for.
	 * @return If there is any cargo for that station.
	 */
	inline bool HasCargoFor(StationID next) const
	{
		return this->packets.find(next) != this->packets.end();
	}

	/**
	 * Returns source of the first cargo packet in this list.
	 * @return The before mentioned source.
	 */
	inline StationID Source() const
	{
		return this->count == 0 ? INVALID_STATION : this->packets.begin()->second.front()->source;
	}
	
	/**
	 * Returns sum of cargo still available for loading at the sation.
	 * (i.e. not counting cargo which is already reserved for loading)
	 * @return Cargo on board the vehicle.
	 */
	inline uint AvailableCount() const
	{
		return this->count;
	}

	/**
	 * Returns sum of cargo reserved for loading onto vehicles.
	 * @return Cargo reserved for loading.
	 */
	inline uint ReservedCount() const
	{
		return this->reserved_count;
	}

	/**
	 * Returns total count of cargo at the station, including
	 * cargo which is already reserved for loading.
	 * @return Total cargo count.
	 */
	inline uint TotalCount() const
	{
		return this->count + this->reserved_count;
	}

	/* Methods for moving cargo around. First parameter is always maximum
	 * amount of cargo to be moved. Second parameter is destination (if
	 * applicable), return value is amount of cargo actually moved. */

	uint Reserve(uint max_move, VehicleCargoList *dest, TileIndex load_place, StationID next);
	uint Load(uint max_move, VehicleCargoList *dest, TileIndex load_place, StationID next);
	uint Truncate(uint max_move = UINT_MAX);

	void InvalidateCache();

	void UpdateCargoNextHop(Station *st, CargoID cid);

	/**
	 * Gets the cargo counts per next hop.
	 * @return Cargo counts.
	 */
	const OrderMap& CountForNextHop() const
	{
		return this->order_cache;
	}

	/**
	 * Gets the cargo count for a next hop.
	 * @param order The next hop.
	 * @return The cargo count for the specified next hop.
	 */
	int CountForNextHop(OrderID order) const
	{
		OrderMap::const_iterator i = this->order_cache.find(order);
		return i != this->order_cache.end() ? i->second : 0;
	}
	
	/**
	 * Are two the two CargoPackets mergeable in the context of
	 * a list of CargoPackets for a Vehicle?
	 * @param cp1 First CargoPacket.
	 * @param cp2 Second CargoPacket.
	 * @return True if they are mergeable.
	 */
	static bool AreMergable(const CargoPacket *cp1, const CargoPacket *cp2)
	{
		return cp1->source_xy    == cp2->source_xy &&
				cp1->days_in_transit == cp2->days_in_transit &&
				cp1->source_type     == cp2->source_type &&
				cp1->source_id       == cp2->source_id &&
				cp1->dest_xy         == cp2->dest_xy &&
				cp1->dest_type       == cp2->dest_type &&
				cp1->dest_id         == cp2->dest_id &&
				cp1->next_order      == cp2->next_order &&
				cp1->next_station    == cp2->next_station &&
				cp1->flags           == cp2->flags;
	}

	static void InvalidateAllTo(OrderID order, StationID st_unload);
	static void InvalidateAllTo(SourceType type, SourceID dest);
};

#endif /* CARGOPACKET_H */
