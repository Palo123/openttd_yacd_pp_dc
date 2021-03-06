/* $Id: station_gui.cpp 25984 2013-11-13 21:43:16Z rubidium $ */

/*
 * This file is part of OpenTTD.
 * OpenTTD is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, version 2.
 * OpenTTD is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with OpenTTD. If not, see <http://www.gnu.org/licenses/>.
 */

/** @file station_gui.cpp The GUI for stations. */

#include "stdafx.h"
#include "debug.h"
#include "gui.h"
#include "textbuf_gui.h"
#include "company_func.h"
#include "command_func.h"
#include "vehicle_gui.h"
#include "cargotype.h"
#include "station_gui.h"
#include "strings_func.h"
#include "window_func.h"
#include "viewport_func.h"
#include "widgets/dropdown_func.h"
#include "station_base.h"
#include "waypoint_base.h"
#include "tilehighlight_func.h"
#include "company_base.h"
#include "sortlist_type.h"
#include "core/geometry_func.hpp"
#include "vehiclelist.h"
#include "core/math_func.hpp"
#include "overlay_cmd.h"
#include "town.h"
#include "industry.h"
#include "cargodest_base.h"
#include "departures_gui.h"

#include "widgets/station_widget.h"

#include "table/strings.h"
#include "table/control_codes.h"

/**
 * Calculates and draws the accepted or supplied cargo around the selected tile(s)
 * @param left x position where the string is to be drawn
 * @param right the right most position to draw on
 * @param top y position where the string is to be drawn
 * @param sct which type of cargo is to be displayed (passengers/non-passengers)
 * @param rad radius around selected tile(s) to be searched
 * @param supplies if supplied cargoes should be drawn, else accepted cargoes
 * @return Returns the y value below the string that was drawn
 */
int DrawStationCoverageAreaText(int left, int right, int top, StationCoverageType sct, int rad, bool supplies)
{
	TileIndex tile = TileVirtXY(_thd.pos.x, _thd.pos.y);
	uint32 cargo_mask = 0;
	if (_thd.drawstyle == HT_RECT && tile < MapSize()) {
		CargoArray cargoes;
		CargoArray rates;
		if (supplies) {
			cargoes = GetProductionAroundTiles(tile, _thd.size.x / TILE_SIZE, _thd.size.y / TILE_SIZE, rad);
			rates = GetProductionRateAroundTiles(tile, _thd.size.x / TILE_SIZE, _thd.size.y / TILE_SIZE, rad);
		} else {
			cargoes = GetAcceptanceAroundTiles(tile, _thd.size.x / TILE_SIZE, _thd.size.y / TILE_SIZE, rad);
			rates = GetAcceptanceRateAroundTiles(tile, _thd.size.x / TILE_SIZE, _thd.size.y / TILE_SIZE, rad);
		}

		/* Convert cargo counts to a set of cargo bits, and draw the result. */
		for (CargoID i = 0; i < NUM_CARGO; i++) {
			switch (sct) {
				case SCT_PASSENGERS_ONLY: if (!IsCargoInClass(i, CC_PASSENGERS)) continue; break;
				case SCT_NON_PASSENGERS_ONLY: if (IsCargoInClass(i, CC_PASSENGERS)) continue; break;
				case SCT_ALL: break;
				default: NOT_REACHED();
			}
			if (cargoes[i] >= (supplies ? 1U : 8U)) SetBit(cargo_mask, i);

			if (i == CT_PASSENGERS) {
				SetDParam(2, rates[i]);
			} else if (i == CT_MAIL) {
				SetDParam(3, rates[i]);
			}
		}
	}
	SetDParam(0, cargo_mask);

	/* SCC_CARGO_LIST works as a magic number to let FormatString() know it's being called from here. */
	SetDParam(1, SCC_CARGO_LIST);
	return DrawStringMultiLine(left, right, top, INT32_MAX, supplies ? STR_STATION_BUILD_SUPPLIES_CARGO : STR_STATION_BUILD_ACCEPTS_CARGO);
}

/**
 * Check whether we need to redraw the station coverage text.
 * If it is needed actually make the window for redrawing.
 * @param w the window to check.
 */
void CheckRedrawStationCoverage(const Window *w)
{
	if (_thd.dirty & 1) {
		_thd.dirty &= ~1;
		w->SetDirty();
	}
}

/**
 * Draw small boxes of cargo amount and ratings data at the given
 * coordinates. If amount exceeds 576 units, it is shown 'full', same
 * goes for the rating: at above 90% orso (224) it is also 'full'
 *
 * @param left   left most coordinate to draw the box at
 * @param right  right most coordinate to draw the box at
 * @param y      coordinate to draw the box at
 * @param type   Cargo type
 * @param amount Cargo amount
 * @param rating ratings data for that particular cargo
 *
 * @note Each cargo-bar is 16 pixels wide and 6 pixels high
 * @note Each rating 14 pixels wide and 1 pixel high and is 1 pixel below the cargo-bar
 */
static void StationsWndShowStationRating(int left, int right, int y, CargoID type, uint amount, byte rating)
{
	static const uint units_full  = 576; ///< number of units to show station as 'full'
	static const uint rating_full = 224; ///< rating needed so it is shown as 'full'

	const CargoSpec *cs = CargoSpec::Get(type);
	if (!cs->IsValid()) return;

	int colour = cs->rating_colour;
	TextColour tc = GetContrastColour(colour);
	uint w = (minu(amount, units_full) + 5) / 36;

	int height = GetCharacterHeight(FS_SMALL);

	/* Draw total cargo (limited) on station (fits into 16 pixels) */
	if (w != 0) GfxFillRect(left, y, left + w - 1, y + height, colour);

	/* Draw a one pixel-wide bar of additional cargo meter, useful
	 * for stations with only a small amount (<=30) */
	if (w == 0) {
		uint rest = amount / 5;
		if (rest != 0) {
			w += left;
			GfxFillRect(w, y + height - rest, w, y + height, colour);
		}
	}

	DrawString(left + 1, right, y, cs->abbrev, tc);

	/* Draw green/red ratings bar (fits into 14 pixels) */
	y += height + 2;
	GfxFillRect(left + 1, y, left + 14, y, PC_RED);
	rating = minu(rating, rating_full) / 16;
	if (rating != 0) GfxFillRect(left + 1, y, left + rating, y, PC_GREEN);
}

typedef GUIList<const Station*> GUIStationList;

/**
 * The list of stations per company.
 */
class CompanyStationsWindow : public Window
{
protected:
	/* Runtime saved values */
	static Listing last_sorting;
	static byte facilities;               // types of stations of interest
	static bool include_empty;            // whether we should include stations without waiting cargo
	static const uint32 cargo_filter_max;
	static uint32 cargo_filter;           // bitmap of cargo types to include
	static const Station *last_station;

	/* Constants for sorting stations */
	static const StringID sorter_names[];
	static GUIStationList::SortFunction * const sorter_funcs[];

	GUIStationList stations;
	Scrollbar *vscroll;

	/**
	 * (Re)Build station list
	 *
	 * @param owner company whose stations are to be in list
	 */
	void BuildStationsList(const Owner owner)
	{
		if (!this->stations.NeedRebuild()) return;

		DEBUG(misc, 3, "Building station list for company %d", owner);

		this->stations.Clear();

		const Station *st;
		FOR_ALL_STATIONS(st) {
			if (st->owner == owner || (st->owner == OWNER_NONE && HasStationInUse(st->index, true, owner))) {
				if (this->facilities & st->facilities) { // only stations with selected facilities
					int num_waiting_cargo = 0;
					for (CargoID j = 0; j < NUM_CARGO; j++) {
						if (st->goods[j].HasRating()) {
							num_waiting_cargo++; // count number of waiting cargo
							if (HasBit(this->cargo_filter, j)) {
								*this->stations.Append() = st;
								break;
							}
						}
					}
					/* stations without waiting cargo */
					if (num_waiting_cargo == 0 && this->include_empty) {
						*this->stations.Append() = st;
					}
				}
			}
		}

		this->stations.Compact();
		this->stations.RebuildDone();

		this->vscroll->SetCount(this->stations.Length()); // Update the scrollbar
	}

	/** Sort stations by their name */
	static int CDECL StationNameSorter(const Station * const *a, const Station * const *b)
	{
		static char buf_cache[64];
		char buf[64];

		SetDParam(0, (*a)->index);
		GetString(buf, STR_STATION_NAME, lastof(buf));

		if (*b != last_station) {
			last_station = *b;
			SetDParam(0, (*b)->index);
			GetString(buf_cache, STR_STATION_NAME, lastof(buf_cache));
		}

		return strcmp(buf, buf_cache);
	}

	/** Sort stations by their type */
	static int CDECL StationTypeSorter(const Station * const *a, const Station * const *b)
	{
		return (*a)->facilities - (*b)->facilities;
	}

	/** Sort stations by their waiting cargo */
	static int CDECL StationWaitingSorter(const Station * const *a, const Station * const *b)
	{
		Money diff = 0;

		CargoID j;
		FOR_EACH_SET_CARGO_ID(j, cargo_filter) {
			if ((*a)->goods[j].cargo.TotalCount() > 0) diff += GetTransportedGoodsIncome((*a)->goods[j].cargo.TotalCount(), 20, 50, j);
			if ((*b)->goods[j].cargo.TotalCount() > 0) diff -= GetTransportedGoodsIncome((*b)->goods[j].cargo.TotalCount(), 20, 50, j);
		}

		return ClampToI32(diff);
	}

	/** Sort stations by their rating */
	static int CDECL StationRatingMaxSorter(const Station * const *a, const Station * const *b)
	{
		byte maxr1 = 0;
		byte maxr2 = 0;

		CargoID j;
		FOR_EACH_SET_CARGO_ID(j, cargo_filter) {
			if ((*a)->goods[j].HasRating()) maxr1 = max(maxr1, (*a)->goods[j].rating);
			if ((*b)->goods[j].HasRating()) maxr2 = max(maxr2, (*b)->goods[j].rating);
		}

		return maxr1 - maxr2;
	}

	/** Sort stations by their rating */
	static int CDECL StationRatingMinSorter(const Station * const *a, const Station * const *b)
	{
		byte minr1 = 255;
		byte minr2 = 255;

		for (CargoID j = 0; j < NUM_CARGO; j++) {
			if (!HasBit(cargo_filter, j)) continue;
			if ((*a)->goods[j].HasRating()) minr1 = min(minr1, (*a)->goods[j].rating);
			if ((*b)->goods[j].HasRating()) minr2 = min(minr2, (*b)->goods[j].rating);
		}

		return -(minr1 - minr2);
	}

	/** Sort the stations list */
	void SortStationsList()
	{
		if (!this->stations.Sort()) return;

		/* Reset name sorter sort cache */
		this->last_station = NULL;

		/* Set the modified widget dirty */
		this->SetWidgetDirty(WID_STL_LIST);
	}

public:
	CompanyStationsWindow(WindowDesc *desc, WindowNumber window_number) : Window(desc)
	{
		this->stations.SetListing(this->last_sorting);
		this->stations.SetSortFuncs(this->sorter_funcs);
		this->stations.ForceRebuild();
		this->stations.NeedResort();
		this->SortStationsList();

		this->CreateNestedTree();
		this->vscroll = this->GetScrollbar(WID_STL_SCROLLBAR);
		this->FinishInitNested(window_number);
		this->owner = (Owner)this->window_number;

		const CargoSpec *cs;
		FOR_ALL_SORTED_STANDARD_CARGOSPECS(cs) {
			if (!HasBit(this->cargo_filter, cs->Index())) continue;
			this->LowerWidget(WID_STL_CARGOSTART + index);
		}

		if (this->cargo_filter == this->cargo_filter_max) this->cargo_filter = _cargo_mask;

		for (uint i = 0; i < 5; i++) {
			if (HasBit(this->facilities, i)) this->LowerWidget(i + WID_STL_TRAIN);
		}
		this->SetWidgetLoweredState(WID_STL_NOCARGOWAITING, this->include_empty);

		this->GetWidget<NWidgetCore>(WID_STL_SORTDROPBTN)->widget_data = this->sorter_names[this->stations.SortType()];
	}

	~CompanyStationsWindow()
	{
		this->last_sorting = this->stations.GetListing();
	}

	virtual void UpdateWidgetSize(int widget, Dimension *size, const Dimension &padding, Dimension *fill, Dimension *resize)
	{
		switch (widget) {
			case WID_STL_SORTBY: {
				Dimension d = GetStringBoundingBox(this->GetWidget<NWidgetCore>(widget)->widget_data);
				d.width += padding.width + WD_SORTBUTTON_ARROW_WIDTH * 2; // Doubled since the string is centred and it also looks better.
				d.height += padding.height;
				*size = maxdim(*size, d);
				break;
			}

			case WID_STL_SORTDROPBTN: {
				Dimension d = {0, 0};
				for (int i = 0; this->sorter_names[i] != INVALID_STRING_ID; i++) {
					d = maxdim(d, GetStringBoundingBox(this->sorter_names[i]));
				}
				d.width += padding.width;
				d.height += padding.height;
				*size = maxdim(*size, d);
				break;
			}

			case WID_STL_LIST:
				resize->height = FONT_HEIGHT_NORMAL;
				size->height = WD_FRAMERECT_TOP + 5 * resize->height + WD_FRAMERECT_BOTTOM;
				break;

			case WID_STL_TRAIN:
			case WID_STL_TRUCK:
			case WID_STL_BUS:
			case WID_STL_AIRPLANE:
			case WID_STL_SHIP:
				size->height = max<uint>(FONT_HEIGHT_SMALL, 10) + padding.height;
				break;

			case WID_STL_CARGOALL:
			case WID_STL_FACILALL:
			case WID_STL_NOCARGOWAITING: {
				Dimension d = GetStringBoundingBox(widget == WID_STL_NOCARGOWAITING ? STR_ABBREV_NONE : STR_ABBREV_ALL);
				d.width  += padding.width + 2;
				d.height += padding.height;
				*size = maxdim(*size, d);
				break;
			}

			default:
				if (widget >= WID_STL_CARGOSTART) {
					Dimension d = GetStringBoundingBox(_sorted_cargo_specs[widget - WID_STL_CARGOSTART]->abbrev);
					d.width  += padding.width + 2;
					d.height += padding.height;
					*size = maxdim(*size, d);
				}
				break;
		}
	}

	virtual void OnPaint()
	{
		this->BuildStationsList((Owner)this->window_number);
		this->SortStationsList();

		this->DrawWidgets();
	}

	virtual void DrawWidget(const Rect &r, int widget) const
	{
		switch (widget) {
			case WID_STL_SORTBY:
				/* draw arrow pointing up/down for ascending/descending sorting */
				this->DrawSortButtonState(WID_STL_SORTBY, this->stations.IsDescSortOrder() ? SBS_DOWN : SBS_UP);
				break;

			case WID_STL_LIST: {
				bool rtl = _current_text_dir == TD_RTL;
				int max = min(this->vscroll->GetPosition() + this->vscroll->GetCapacity(), this->stations.Length());
				int y = r.top + WD_FRAMERECT_TOP;
				for (int i = this->vscroll->GetPosition(); i < max; ++i) { // do until max number of stations of owner
					const Station *st = this->stations[i];
					assert(st->xy != INVALID_TILE);

					/* Do not do the complex check HasStationInUse here, it may be even false
					 * when the order had been removed and the station list hasn't been removed yet */
					assert(st->owner == owner || st->owner == OWNER_NONE);

					SetDParam(0, st->index);
					SetDParam(1, st->facilities);
					int x = DrawString(r.left + WD_FRAMERECT_LEFT, r.right - WD_FRAMERECT_RIGHT, y, STR_STATION_LIST_STATION);
					x += rtl ? -5 : 5;

					/* show cargo waiting and station ratings */
					for (uint j = 0; j < _sorted_standard_cargo_specs_size; j++) {
						CargoID cid = _sorted_cargo_specs[j]->Index();
						if (st->goods[cid].cargo.TotalCount() > 0) {
							/* For RTL we work in exactly the opposite direction. So
							 * decrement the space needed first, then draw to the left
							 * instead of drawing to the left and then incrementing
							 * the space. */
							if (rtl) {
								x -= 20;
								if (x < r.left + WD_FRAMERECT_LEFT) break;
							}
							StationsWndShowStationRating(x, x + 16, y, cid, st->goods[cid].cargo.TotalCount(), st->goods[cid].rating);
							if (!rtl) {
								x += 20;
								if (x > r.right - WD_FRAMERECT_RIGHT) break;
							}
						}
					}
					y += FONT_HEIGHT_NORMAL;
				}

				if (this->vscroll->GetCount() == 0) { // company has no stations
					DrawString(r.left + WD_FRAMERECT_LEFT, r.right - WD_FRAMERECT_RIGHT, y, STR_STATION_LIST_NONE);
					return;
				}
				break;
			}

			case WID_STL_NOCARGOWAITING: {
				int cg_ofst = this->IsWidgetLowered(widget) ? 2 : 1;
				DrawString(r.left + cg_ofst, r.right + cg_ofst, r.top + cg_ofst, STR_ABBREV_NONE, TC_BLACK, SA_HOR_CENTER);
				break;
			}

			case WID_STL_CARGOALL: {
				int cg_ofst = this->IsWidgetLowered(widget) ? 2 : 1;
				DrawString(r.left + cg_ofst, r.right + cg_ofst, r.top + cg_ofst, STR_ABBREV_ALL, TC_BLACK, SA_HOR_CENTER);
				break;
			}

			case WID_STL_FACILALL: {
				int cg_ofst = this->IsWidgetLowered(widget) ? 2 : 1;
				DrawString(r.left + cg_ofst, r.right + cg_ofst, r.top + cg_ofst, STR_ABBREV_ALL, TC_BLACK, SA_HOR_CENTER);
				break;
			}

			default:
				if (widget >= WID_STL_CARGOSTART) {
					const CargoSpec *cs = _sorted_cargo_specs[widget - WID_STL_CARGOSTART];
					int cg_ofst = HasBit(this->cargo_filter, cs->Index()) ? 2 : 1;
					GfxFillRect(r.left + cg_ofst, r.top + cg_ofst, r.right - 2 + cg_ofst, r.bottom - 2 + cg_ofst, cs->rating_colour);
					TextColour tc = GetContrastColour(cs->rating_colour);
					DrawString(r.left + cg_ofst, r.right + cg_ofst, r.top + cg_ofst, cs->abbrev, tc, SA_HOR_CENTER);
				}
				break;
		}
	}

	virtual void SetStringParameters(int widget) const
	{
		if (widget == WID_STL_CAPTION) {
			SetDParam(0, this->window_number);
			SetDParam(1, this->vscroll->GetCount());
		}
	}

	virtual void OnClick(Point pt, int widget, int click_count)
	{
		switch (widget) {
			case WID_STL_LIST: {
				uint id_v = this->vscroll->GetScrolledRowFromWidget(pt.y, this, WID_STL_LIST, 0, FONT_HEIGHT_NORMAL);
				if (id_v >= this->stations.Length()) return; // click out of list bound

				const Station *st = this->stations[id_v];
				/* do not check HasStationInUse - it is slow and may be invalid */
				assert(st->owner == (Owner)this->window_number || st->owner == OWNER_NONE);

				if (_ctrl_pressed) {
					ShowExtraViewPortWindow(st->xy);
				} else {
					ScrollMainWindowToTile(st->xy);
				}
				break;
			}

			case WID_STL_TRAIN:
			case WID_STL_TRUCK:
			case WID_STL_BUS:
			case WID_STL_AIRPLANE:
			case WID_STL_SHIP:
				if (_ctrl_pressed) {
					ToggleBit(this->facilities, widget - WID_STL_TRAIN);
					this->ToggleWidgetLoweredState(widget);
				} else {
					uint i;
					FOR_EACH_SET_BIT(i, this->facilities) {
						this->RaiseWidget(i + WID_STL_TRAIN);
					}
					this->facilities = 1 << (widget - WID_STL_TRAIN);
					this->LowerWidget(widget);
				}
				this->stations.ForceRebuild();
				this->SetDirty();
				break;

			case WID_STL_FACILALL:
				for (uint i = WID_STL_TRAIN; i <= WID_STL_SHIP; i++) {
					this->LowerWidget(i);
				}

				this->facilities = FACIL_TRAIN | FACIL_TRUCK_STOP | FACIL_BUS_STOP | FACIL_AIRPORT | FACIL_DOCK;
				this->stations.ForceRebuild();
				this->SetDirty();
				break;

			case WID_STL_CARGOALL: {
				for (uint i = 0; i < _sorted_standard_cargo_specs_size; i++) {
					this->LowerWidget(WID_STL_CARGOSTART + i);
				}
				this->LowerWidget(WID_STL_NOCARGOWAITING);

				this->cargo_filter = _cargo_mask;
				this->include_empty = true;
				this->stations.ForceRebuild();
				this->SetDirty();
				break;
			}

			case WID_STL_SORTBY: // flip sorting method asc/desc
				this->stations.ToggleSortOrder();
				this->SetDirty();
				break;

			case WID_STL_SORTDROPBTN: // select sorting criteria dropdown menu
				ShowDropDownMenu(this, this->sorter_names, this->stations.SortType(), WID_STL_SORTDROPBTN, 0, 0);
				break;

			case WID_STL_NOCARGOWAITING:
				if (_ctrl_pressed) {
					this->include_empty = !this->include_empty;
					this->ToggleWidgetLoweredState(WID_STL_NOCARGOWAITING);
				} else {
					for (uint i = 0; i < _sorted_standard_cargo_specs_size; i++) {
						this->RaiseWidget(WID_STL_CARGOSTART + i);
					}

					this->cargo_filter = 0;
					this->include_empty = true;

					this->LowerWidget(WID_STL_NOCARGOWAITING);
				}
				this->stations.ForceRebuild();
				this->SetDirty();
				break;

			default:
				if (widget >= WID_STL_CARGOSTART) { // change cargo_filter
					/* Determine the selected cargo type */
					const CargoSpec *cs = _sorted_cargo_specs[widget - WID_STL_CARGOSTART];

					if (_ctrl_pressed) {
						ToggleBit(this->cargo_filter, cs->Index());
						this->ToggleWidgetLoweredState(widget);
					} else {
						for (uint i = 0; i < _sorted_standard_cargo_specs_size; i++) {
							this->RaiseWidget(WID_STL_CARGOSTART + i);
						}
						this->RaiseWidget(WID_STL_NOCARGOWAITING);

						this->cargo_filter = 0;
						this->include_empty = false;

						SetBit(this->cargo_filter, cs->Index());
						this->LowerWidget(widget);
					}
					this->stations.ForceRebuild();
					this->SetDirty();
				}
				break;
		}
	}

	virtual void OnDropdownSelect(int widget, int index)
	{
		if (this->stations.SortType() != index) {
			this->stations.SetSortType(index);

			/* Display the current sort variant */
			this->GetWidget<NWidgetCore>(WID_STL_SORTDROPBTN)->widget_data = this->sorter_names[this->stations.SortType()];

			this->SetDirty();
		}
	}

	virtual void OnTick()
	{
		if (_pause_mode != PM_UNPAUSED) return;
		if (this->stations.NeedResort()) {
			DEBUG(misc, 3, "Periodic rebuild station list company %d", this->window_number);
			this->SetDirty();
		}
	}

	virtual void OnResize()
	{
		this->vscroll->SetCapacityFromWidget(this, WID_STL_LIST, WD_FRAMERECT_TOP + WD_FRAMERECT_BOTTOM);
	}

	/**
	 * Some data on this window has become invalid.
	 * @param data Information about the changed data.
	 * @param gui_scope Whether the call is done from GUI scope. You may not do everything when not in GUI scope. See #InvalidateWindowData() for details.
	 */
	virtual void OnInvalidateData(int data = 0, bool gui_scope = true)
	{
		if (data == 0) {
			/* This needs to be done in command-scope to enforce rebuilding before resorting invalid data */
			this->stations.ForceRebuild();
		} else {
			this->stations.ForceResort();
		}
	}
};

Listing CompanyStationsWindow::last_sorting = {false, 0};
byte CompanyStationsWindow::facilities = FACIL_TRAIN | FACIL_TRUCK_STOP | FACIL_BUS_STOP | FACIL_AIRPORT | FACIL_DOCK;
bool CompanyStationsWindow::include_empty = true;
const uint32 CompanyStationsWindow::cargo_filter_max = UINT32_MAX;
uint32 CompanyStationsWindow::cargo_filter = UINT32_MAX;
const Station *CompanyStationsWindow::last_station = NULL;

/* Availible station sorting functions */
GUIStationList::SortFunction * const CompanyStationsWindow::sorter_funcs[] = {
	&StationNameSorter,
	&StationTypeSorter,
	&StationWaitingSorter,
	&StationRatingMaxSorter,
	&StationRatingMinSorter
};

/* Names of the sorting functions */
const StringID CompanyStationsWindow::sorter_names[] = {
	STR_SORT_BY_NAME,
	STR_SORT_BY_FACILITY,
	STR_SORT_BY_WAITING,
	STR_SORT_BY_RATING_MAX,
	STR_SORT_BY_RATING_MIN,
	INVALID_STRING_ID
};

/**
 * Make a horizontal row of cargo buttons, starting at widget #WID_STL_CARGOSTART.
 * @param biggest_index Pointer to store biggest used widget number of the buttons.
 * @return Horizontal row.
 */
static NWidgetBase *CargoWidgets(int *biggest_index)
{
	NWidgetHorizontal *container = new NWidgetHorizontal();

	for (uint i = 0; i < _sorted_standard_cargo_specs_size; i++) {
		NWidgetBackground *panel = new NWidgetBackground(WWT_PANEL, COLOUR_GREY, WID_STL_CARGOSTART + i);
		panel->SetMinimalSize(14, 11);
		panel->SetResize(0, 0);
		panel->SetFill(0, 1);
		panel->SetDataTip(0, STR_STATION_LIST_USE_CTRL_TO_SELECT_MORE);
		container->Add(panel);
	}
	*biggest_index = WID_STL_CARGOSTART + _sorted_standard_cargo_specs_size;
	return container;
}

static const NWidgetPart _nested_company_stations_widgets[] = {
	NWidget(NWID_HORIZONTAL),
		NWidget(WWT_CLOSEBOX, COLOUR_GREY),
		NWidget(WWT_CAPTION, COLOUR_GREY, WID_STL_CAPTION), SetDataTip(STR_STATION_LIST_CAPTION, STR_TOOLTIP_WINDOW_TITLE_DRAG_THIS),
		NWidget(WWT_SHADEBOX, COLOUR_GREY),
		NWidget(WWT_DEFSIZEBOX, COLOUR_GREY),
		NWidget(WWT_STICKYBOX, COLOUR_GREY),
	EndContainer(),
	NWidget(NWID_HORIZONTAL),
		NWidget(WWT_TEXTBTN, COLOUR_GREY, WID_STL_TRAIN), SetMinimalSize(14, 11), SetDataTip(STR_TRAIN, STR_STATION_LIST_USE_CTRL_TO_SELECT_MORE), SetFill(0, 1),
		NWidget(WWT_TEXTBTN, COLOUR_GREY, WID_STL_TRUCK), SetMinimalSize(14, 11), SetDataTip(STR_LORRY, STR_STATION_LIST_USE_CTRL_TO_SELECT_MORE), SetFill(0, 1),
		NWidget(WWT_TEXTBTN, COLOUR_GREY, WID_STL_BUS), SetMinimalSize(14, 11), SetDataTip(STR_BUS, STR_STATION_LIST_USE_CTRL_TO_SELECT_MORE), SetFill(0, 1),
		NWidget(WWT_TEXTBTN, COLOUR_GREY, WID_STL_SHIP), SetMinimalSize(14, 11), SetDataTip(STR_SHIP, STR_STATION_LIST_USE_CTRL_TO_SELECT_MORE), SetFill(0, 1),
		NWidget(WWT_TEXTBTN, COLOUR_GREY, WID_STL_AIRPLANE), SetMinimalSize(14, 11), SetDataTip(STR_PLANE, STR_STATION_LIST_USE_CTRL_TO_SELECT_MORE), SetFill(0, 1),
		NWidget(WWT_PUSHBTN, COLOUR_GREY, WID_STL_FACILALL), SetMinimalSize(14, 11), SetDataTip(0x0, STR_STATION_LIST_SELECT_ALL_FACILITIES), SetFill(0, 1),
		NWidget(WWT_PANEL, COLOUR_GREY), SetMinimalSize(5, 11), SetFill(0, 1), EndContainer(),
		NWidgetFunction(CargoWidgets),
		NWidget(WWT_PANEL, COLOUR_GREY, WID_STL_NOCARGOWAITING), SetMinimalSize(14, 11), SetDataTip(0x0, STR_STATION_LIST_NO_WAITING_CARGO), SetFill(0, 1), EndContainer(),
		NWidget(WWT_PUSHBTN, COLOUR_GREY, WID_STL_CARGOALL), SetMinimalSize(14, 11), SetDataTip(0x0, STR_STATION_LIST_SELECT_ALL_TYPES), SetFill(0, 1),
		NWidget(WWT_PANEL, COLOUR_GREY), SetDataTip(0x0, STR_NULL), SetResize(1, 0), SetFill(1, 1), EndContainer(),
	EndContainer(),
	NWidget(NWID_HORIZONTAL),
		NWidget(WWT_PUSHTXTBTN, COLOUR_GREY, WID_STL_SORTBY), SetMinimalSize(81, 12), SetDataTip(STR_BUTTON_SORT_BY, STR_TOOLTIP_SORT_ORDER),
		NWidget(WWT_DROPDOWN, COLOUR_GREY, WID_STL_SORTDROPBTN), SetMinimalSize(163, 12), SetDataTip(STR_SORT_BY_NAME, STR_TOOLTIP_SORT_CRITERIA), // widget_data gets overwritten.
		NWidget(WWT_PANEL, COLOUR_GREY), SetDataTip(0x0, STR_NULL), SetResize(1, 0), SetFill(1, 1), EndContainer(),
	EndContainer(),
	NWidget(NWID_HORIZONTAL),
		NWidget(WWT_PANEL, COLOUR_GREY, WID_STL_LIST), SetMinimalSize(346, 125), SetResize(1, 10), SetDataTip(0x0, STR_STATION_LIST_TOOLTIP), SetScrollbar(WID_STL_SCROLLBAR), EndContainer(),
		NWidget(NWID_VERTICAL),
			NWidget(NWID_VSCROLLBAR, COLOUR_GREY, WID_STL_SCROLLBAR),
			NWidget(WWT_RESIZEBOX, COLOUR_GREY),
		EndContainer(),
	EndContainer(),
};

static WindowDesc _company_stations_desc(
	WDP_AUTO, "list_stations", 358, 162,
	WC_STATION_LIST, WC_NONE,
	0,
	_nested_company_stations_widgets, lengthof(_nested_company_stations_widgets)
);

/**
 * Opens window with list of company's stations
 *
 * @param company whose stations' list show
 */
void ShowCompanyStations(CompanyID company)
{
	if (!Company::IsValidID(company)) return;

	AllocateWindowDescFront<CompanyStationsWindow>(&_company_stations_desc, company);
}

static const NWidgetPart _nested_station_view_widgets[] = {
	NWidget(NWID_HORIZONTAL),
		NWidget(WWT_CLOSEBOX, COLOUR_GREY),
		NWidget(WWT_CAPTION, COLOUR_GREY, WID_SV_CAPTION), SetDataTip(STR_STATION_VIEW_CAPTION, STR_TOOLTIP_WINDOW_TITLE_DRAG_THIS),
                NWidget(WWT_PUSHTXTBTN, COLOUR_GREY, WID_SV_DEPARTURES), SetMinimalSize(50, 12),
                    SetDataTip(STR_STATION_VIEW_DEPARTURES_BUTTON, STR_STATION_VIEW_DEPARTURES_TOOLTIP),
		NWidget(WWT_SHADEBOX, COLOUR_GREY),
		NWidget(WWT_DEFSIZEBOX, COLOUR_GREY),
		NWidget(WWT_STICKYBOX, COLOUR_GREY),
	EndContainer(),
	NWidget(NWID_HORIZONTAL),
		NWidget(WWT_PANEL, COLOUR_GREY, WID_SV_WAITING), SetMinimalSize(237, 52), SetResize(1, 10), SetScrollbar(WID_SV_SCROLLBAR), EndContainer(),
		NWidget(NWID_VSCROLLBAR, COLOUR_GREY, WID_SV_SCROLLBAR),
	EndContainer(),
	NWidget(WWT_PANEL, COLOUR_GREY, WID_SV_ACCEPT_RATING_LIST), SetMinimalSize(249, 32), SetResize(1, 0), EndContainer(),
	NWidget(NWID_HORIZONTAL),
		NWidget(NWID_HORIZONTAL, NC_EQUALSIZE),
//I don't like the cover button
//                       NWidget(WWT_PUSHTXTBTN, COLOUR_GREY, WID_SV_COVERAGE), SetMinimalSize(60, 12), SetResize(1, 0), SetFill(1, 1),
//                                       SetDataTip(STR_BUTTON_COVERAGE, STR_STATION_VIEW_COVERAGE_TIP),
			NWidget(WWT_PUSHTXTBTN, COLOUR_GREY, WID_SV_LOCATION), SetMinimalSize(36, 12), SetResize(1, 0), SetFill(1, 1),
					SetDataTip(STR_BUTTON_LOCATION, STR_STATION_VIEW_CENTER_TOOLTIP),
			NWidget(WWT_PUSHTXTBTN, COLOUR_GREY, WID_SV_ACCEPTS_RATINGS), SetMinimalSize(37, 12), SetResize(1, 0), SetFill(1, 1),
					SetDataTip(STR_STATION_VIEW_RATINGS_BUTTON, STR_STATION_VIEW_RATINGS_TOOLTIP),
			NWidget(WWT_PUSHTXTBTN, COLOUR_GREY, WID_SV_CARGO_FROM_TO_VIA), SetMinimalSize(36, 12), SetResize(1, 0), SetFill(1, 1),
					SetDataTip(STR_STATION_VIEW_WAITING_VIA_BUTTON, STR_STATION_VIEW_WAITING_VIA_TOOLTIP),
			NWidget(WWT_PUSHTXTBTN, COLOUR_GREY, WID_SV_RENAME), SetMinimalSize(36, 12), SetResize(1, 0), SetFill(1, 1),
					SetDataTip(STR_BUTTON_RENAME, STR_STATION_VIEW_RENAME_TOOLTIP),
		NWidget(WWT_TEXTBTN, COLOUR_GREY, WID_SV_CLOSE_AIRPORT), SetMinimalSize(36, 12), SetResize(1, 0), SetFill(1, 1),
				SetDataTip(STR_STATION_VIEW_CLOSE_AIRPORT, STR_STATION_VIEW_CLOSE_AIRPORT_TOOLTIP),
		NWidget(WWT_PUSHTXTBTN, COLOUR_GREY, WID_SV_TRAINS), SetMinimalSize(14, 12), SetFill(0, 1), SetDataTip(STR_TRAIN, STR_STATION_VIEW_SCHEDULED_TRAINS_TOOLTIP),
		NWidget(WWT_PUSHTXTBTN, COLOUR_GREY, WID_SV_ROADVEHS), SetMinimalSize(14, 12), SetFill(0, 1), SetDataTip(STR_LORRY, STR_STATION_VIEW_SCHEDULED_ROAD_VEHICLES_TOOLTIP),
		NWidget(WWT_PUSHTXTBTN, COLOUR_GREY, WID_SV_SHIPS), SetMinimalSize(14, 12), SetFill(0, 1), SetDataTip(STR_SHIP, STR_STATION_VIEW_SCHEDULED_SHIPS_TOOLTIP),
		NWidget(WWT_PUSHTXTBTN, COLOUR_GREY, WID_SV_PLANES),  SetMinimalSize(14, 12), SetFill(0, 1), SetDataTip(STR_PLANE, STR_STATION_VIEW_SCHEDULED_AIRCRAFT_TOOLTIP),
		NWidget(WWT_RESIZEBOX, COLOUR_GREY),
	EndContainer(),
};

/**
 * Draws icons of waiting cargo in the StationView window
 *
 * @param i type of cargo
 * @param waiting number of waiting units
 * @param left  left most coordinate to draw on
 * @param right right most coordinate to draw on
 * @param y y coordinate
 * @param width the width of the view
 */
static void DrawCargoIcons(CargoID i, uint waiting, int left, int right, int y)
{
	uint num = min((waiting + 5) / 10, (right - left) / 10); // maximum is width / 10 icons so it won't overflow
	if (num == 0) return;

	SpriteID sprite = CargoSpec::Get(i)->GetCargoIcon();

	int x = _current_text_dir == TD_RTL ? right - num * 10 : left;
	do {
		DrawSprite(sprite, PAL_NONE, x, y);
		x += 10;
	} while (--num);
}

struct CargoData {
	CargoID cargo;
	union {
		StationID station;
		SourceID  css;
	};
	uint count;
	SourceType type;

	CargoData(CargoID cargo, StationID station, uint count, SourceType type = ST_INDUSTRY) :
		cargo(cargo),
		station(station),
		count(count),
		type(type)
	{ }
};

typedef std::list<CargoData> CargoDataList;

/** List of cargo for either one next hop or one destination. */
struct CargoDestEntry {
	typedef std::list<CargoDestEntry> List;

	/** Enum for type of stored data. */
	enum Type {
		FINAL_DEST,   ///< Data is the final destination.
		NEXT_HOP,     ///< Data is the next hop.
		TRANSFER_HOP  ///< Data is the transfer station.
	};

	List          children;       ///< Child entries of this entry.
	CargoData     data;           ///< Stores the info for the current item.
	Type          type;           ///< Type of the data stored in #entry.
	uint16        start_row;      ///< Row number of the header line.
	bool          expanded;       ///< Is this entry expanded?

	CargoDestEntry(Type type, StationID station, uint count, SourceType st = ST_INDUSTRY) :
		data(INVALID_CARGO, station, count, st),
		type(type),
		start_row(0),
		expanded(false)
	{ }

	/** Zero out this entry and all child entries. */
	void Zero()
	{
		for (List::iterator i = this->children.begin(); i != this->children.end(); ++i ) {
			i->Zero();
		}
		this->data.count = 0;
		this->start_row = 0;
	}

	/** Remove all empty child entries. */
	void RemoveEmpty()
	{
		for (List::iterator i = this->children.begin(); i != this->children.end(); ) {
			if (i->data.count > 0) {
				i->RemoveEmpty();
				++i;
			} else {
				i = this->children.erase(i);
			}
		}
	}

	/** Update header row number. */
	int UpdateRowCount(int row)
	{
		this->start_row = ++row;
		if (this->expanded) {
			for (List::iterator i = this->children.begin(); i != this->children.end(); ++i) {
				row = i->UpdateRowCount(row);
			}
		}
		return row;
	}
};

/**
 * Get the next hop of a cargo packet.
 * @param ge Station cargo info for the matching cargo type.
 * @param cp The cargo packet.
 * @return Station ID of the next hop or INVALID_STATION if not possible.
 */
static StationID GetNextHopStation(const GoodsEntry &ge, const CargoPacket *cp)
{
	StationID next = INVALID_STATION;
	for (RouteLinkList::const_iterator i = ge.routes.begin(); i != ge.routes.end(); ++i) {
		if ((*i)->GetOriginOrderId() == cp->NextHop()) {
			next = (*i)->GetDestination();
			break;
		}
	}
	return next;
}

/**
 * Add a cargo packet to a #CargoDestEntry list.
 * @param list The list to add the packet to.
 * @param type Which value to select as the entry info.
 * @param cp The cargo packet.
 * @param ge Where this cargo packets belongs to.
 * @return Pointer to the added entry or NULL if the packet had no valid destination of the specified type.
 */
static CargoDestEntry *AddCargoPacketToList(CargoDestEntry::List &list, CargoDestEntry::Type type, const CargoPacket *cp, const GoodsEntry &ge)
{
	assert_compile(INVALID_STATION == INVALID_SOURCE);

	/* Extract the wanted sort type from the cargo packet. */
	uint16 sort_val;
	switch (type) {
		case CargoDestEntry::FINAL_DEST:
			sort_val = cp->DestinationID();
			break;
		case CargoDestEntry::NEXT_HOP:
			sort_val = GetNextHopStation(ge, cp);
			break;
		case CargoDestEntry::TRANSFER_HOP:
			sort_val = cp->NextStation();
			break;
		default:
			NOT_REACHED();
	}

	if (sort_val == INVALID_STATION) return NULL;

	/* Search for a matching child. */
	for (CargoDestEntry::List::iterator i = list.begin(); i != list.end(); ++i) {
		if (type == CargoDestEntry::FINAL_DEST ? i->data.css == sort_val && i->data.type == cp->DestinationType() : i->data.station == sort_val) {
			i->data.count += cp->Count();
			return &*i;
		}
	}

	/* No entry found, add new. */
	list.push_back(CargoDestEntry(type, sort_val, cp->Count(), cp->DestinationType()));
	return &list.back();
}


/**
 * The StationView window
 */
struct StationViewWindow : public Window {
	uint32 cargo;                 ///< Bitmask of cargo types to expand
	uint16 cargo_rows[NUM_CARGO]; ///< Header row for each cargo type
	bool stdacceptratings;        ///< Standart or extended type of ACCEPTRATINGS list.
	CargoArray cargo_around;      ///< Array of extra cargo that not visible in standart view.
	uint expand_shrink_width;     ///< The width allocated to the expand/shrink 'button'
	int rating_lines;             ///< Number of lines in the cargo ratings view.
	int accepts_lines;            ///< Number of lines in the accepted cargo view.
	Scrollbar *vscroll;
	CargoDestEntry::List cargodest_list[NUM_CARGO]; ///< List of cargoes sorted by destination.

	static StringID last_cargo_from_str;
	static StringID last_cargo_from_tooltip;

	/** Height of the #WID_SV_ACCEPT_RATING_LIST widget for different views. */
	enum AcceptListHeight {
		ALH_RATING  = 13, ///< Height of the cargo ratings view.
		ALH_ACCEPTS = 3,  ///< Height of the accepted cargo view.
	};

	StationViewWindow(WindowDesc *desc, WindowNumber window_number) : Window(desc)
	{
		this->rating_lines  = ALH_RATING;
		this->accepts_lines = ALH_ACCEPTS;
		this->stdacceptratings = true;

		this->CreateNestedTree();
		this->vscroll = this->GetScrollbar(WID_SV_SCROLLBAR);
		this->GetWidget<NWidgetCore>(WID_SV_CARGO_FROM_TO_VIA)->SetDataTip(StationViewWindow::last_cargo_from_str, StationViewWindow::last_cargo_from_tooltip);
		/* Nested widget tree creation is done in two steps to ensure that this->GetWidget<NWidgetCore>(WID_SV_ACCEPTS_RATINGS) exists in UpdateWidgetSize(). */
		this->FinishInitNested(window_number);

		Owner owner = Station::Get(window_number)->owner;
		if (owner != OWNER_NONE) this->owner = owner;
	}

	~StationViewWindow()
	{
		Overlays::Instance()->RemoveStation(Station::Get(this->window_number));
		MarkWholeScreenDirty();
		Owner owner = Station::Get(this->window_number)->owner;
		DeleteWindowById(WC_TRAINS_LIST,   VehicleListIdentifier(VL_STATION_LIST, VEH_TRAIN,    owner, this->window_number).Pack(), false);
		DeleteWindowById(WC_ROADVEH_LIST,  VehicleListIdentifier(VL_STATION_LIST, VEH_ROAD,     owner, this->window_number).Pack(), false);
		DeleteWindowById(WC_SHIPS_LIST,    VehicleListIdentifier(VL_STATION_LIST, VEH_SHIP,     owner, this->window_number).Pack(), false);
		DeleteWindowById(WC_AIRCRAFT_LIST, VehicleListIdentifier(VL_STATION_LIST, VEH_AIRCRAFT, owner, this->window_number).Pack(), false);
	}

	virtual void UpdateWidgetSize(int widget, Dimension *size, const Dimension &padding, Dimension *fill, Dimension *resize)
	{
		switch (widget) {
			case WID_SV_WAITING:
				resize->height = FONT_HEIGHT_NORMAL;
				size->height = WD_FRAMERECT_TOP + 5 * resize->height + WD_FRAMERECT_BOTTOM;
				this->expand_shrink_width = max(GetStringBoundingBox("-").width, GetStringBoundingBox("+").width) + WD_FRAMERECT_LEFT + WD_FRAMERECT_RIGHT;
				break;

			case WID_SV_ACCEPT_RATING_LIST:
				size->height = WD_FRAMERECT_TOP + ((this->GetWidget<NWidgetCore>(WID_SV_ACCEPTS_RATINGS)->widget_data == STR_STATION_VIEW_RATINGS_BUTTON) ? this->accepts_lines : this->rating_lines) * FONT_HEIGHT_NORMAL + WD_FRAMERECT_BOTTOM;
				break;

			case WID_SV_CLOSE_AIRPORT:
				if (!(Station::Get(this->window_number)->facilities & FACIL_AIRPORT)) {
					/* Hide 'Close Airport' button if no airport present. */
					size->width = 0;
					resize->width = 0;
					fill->width = 0;
				}
				break;
		}
	}

	virtual void OnPaint()
	{
		CargoDataList cargolist;
		uint32 transfers = 0;

		NWidgetCore *cargo_btn = this->GetWidget<NWidgetCore>(WID_SV_CARGO_FROM_TO_VIA);
		if (cargo_btn->widget_data == STR_STATION_VIEW_WAITING_TO_BUTTON) {
			this->OrderWaitingCargo(&cargolist, &transfers);
			this->vscroll->SetCount((int)cargolist.size() + 1); // update scrollbar
		} else {
			/* Determine the current view. */
			CargoDestEntry::Type dest_type;
			switch (cargo_btn->widget_data) {
				case STR_STATION_VIEW_WAITING_VIA_BUTTON:
					dest_type = CargoDestEntry::FINAL_DEST;
					break;
				case STR_STATION_VIEW_WAITING_TRANSFER_BUTTON:
					dest_type = CargoDestEntry::NEXT_HOP;
					break;
				case STR_STATION_VIEW_WAITING_BUTTON:
					dest_type = CargoDestEntry::TRANSFER_HOP;
					break;
				default:
					NOT_REACHED();
			}
			int num = this->FillCargodestList(dest_type, this->cargodest_list);
			this->vscroll->SetCount(num + 1); // update scrollbar
		}

		/* disable some buttons */
		const Station *st = Station::Get(this->window_number);
		this->SetWidgetDisabledState(WID_SV_RENAME,   st->owner != _local_company);
		this->SetWidgetDisabledState(WID_SV_TRAINS,   !(st->facilities & FACIL_TRAIN));
		this->SetWidgetDisabledState(WID_SV_ROADVEHS, !(st->facilities & FACIL_TRUCK_STOP) && !(st->facilities & FACIL_BUS_STOP));
		this->SetWidgetDisabledState(WID_SV_SHIPS,    !(st->facilities & FACIL_DOCK));
		this->SetWidgetDisabledState(WID_SV_PLANES,   !(st->facilities & FACIL_AIRPORT));
		this->SetWidgetDisabledState(WID_SV_CLOSE_AIRPORT, !(st->facilities & FACIL_AIRPORT) || st->owner != _local_company || st->owner == OWNER_NONE); // Also consider SE, where _local_company == OWNER_NONE
		this->SetWidgetLoweredState(WID_SV_CLOSE_AIRPORT, (st->facilities & FACIL_AIRPORT) && (st->airport.flags & AIRPORT_CLOSED_block) != 0);

//I don't like the cover button
		/* check lowered stated for some buttons */
//		this->SetWidgetLoweredState(WID_SV_COVERAGE, Overlays::Instance()->HasStation(st));

		this->DrawWidgets();

		if (!this->IsShaded()) {
			/* Draw 'accepted cargo' or 'cargo ratings'. */
			const NWidgetBase *wid = this->GetWidget<NWidgetBase>(WID_SV_ACCEPT_RATING_LIST);
			const Rect r = {wid->pos_x, wid->pos_y, wid->pos_x + wid->current_x - 1, wid->pos_y + wid->current_y - 1};
			if (this->GetWidget<NWidgetCore>(WID_SV_ACCEPTS_RATINGS)->widget_data == STR_STATION_VIEW_RATINGS_BUTTON) {
				int lines = this->DrawAcceptedCargo(r);
				if (lines > this->accepts_lines) { // Resize the widget, and perform re-initialization of the window.
					this->accepts_lines = lines;
					this->ReInit();
					return;
				}
			} else {
				int lines = this->stdacceptratings ? this->DrawCargoRatings(r) : this->DrawExtraCargoRatings(r);
				if (lines > this->rating_lines) { // Resize the widget, and perform re-initialization of the window.
					this->rating_lines = lines;
					this->ReInit();
					return;
				}
			}

			/* Draw waiting cargo. */
			NWidgetBase *nwi = this->GetWidget<NWidgetBase>(WID_SV_WAITING);
			Rect waiting_rect = {nwi->pos_x, nwi->pos_y, nwi->pos_x + nwi->current_x - 1, nwi->pos_y + nwi->current_y - 1};
			if (cargo_btn->widget_data == STR_STATION_VIEW_WAITING_TO_BUTTON) {
				this->DrawWaitingCargo(waiting_rect, cargolist, transfers);
			} else {
				this->DrawWaitingCargoByDest(waiting_rect, this->cargodest_list);
			}
		}
	}

	virtual void SetStringParameters(int widget) const
	{
		if (widget == WID_SV_CAPTION) {
			const Station *st = Station::Get(this->window_number);
			SetDParam(0, st->index);
			SetDParam(1, st->facilities);
		}
	}

	/**
	 * Order waiting cargo by type and destination.
	 * @param cargolist [out] Ordered cargo.
	 * @param transfers [out] Bitmask for cargoes being transfered.
	 * @pre \c *cargolist must be empty.
	 */
	void OrderWaitingCargo(CargoDataList *cargolist, uint32 *transfers)
	{
		assert(cargolist->size() == 0);
		*transfers = 0;

		StationID station_id = this->window_number;
		const Station *st = Station::Get(station_id);

		/* count types of cargoes waiting in station */
		for (CargoID i = 0; i < NUM_CARGO; i++) {
			if (st->goods[i].cargo.TotalCount() == 0) {
				this->cargo_rows[i] = 0;
			} else {
				/* Add an entry for total amount of cargo of this type waiting. */
				cargolist->push_back(CargoData(i, INVALID_STATION, st->goods[i].cargo.TotalCount()));

				/* Set the row for this cargo entry for the expand/hide button */
				this->cargo_rows[i] = (uint16)cargolist->size();

				/* Add an entry for each distinct cargo source. */
				const StationCargoPacketMap *packets = st->goods[i].cargo.Packets();
				for (StationCargoList::ConstIterator it(packets->begin()); it != packets->end(); it++) {
					const CargoPacket *cp = *it;
					if (cp->SourceStation() != station_id) {
						bool added = false;

						/* Enable the expand/hide button for this cargo type */
						SetBit(*transfers, i);

						/* Don't add cargo lines if not expanded */
						if (!HasBit(this->cargo, i)) break;

						/* Check if we already have this source in the list */
						for (CargoDataList::iterator jt(cargolist->begin()); jt != cargolist->end(); jt++) {
							CargoData *cd = &(*jt);
							if (cd->cargo == i && cd->station == cp->SourceStation()) {
								cd->count += cp->Count();
								added = true;
								break;
							}
						}
						if (!added) cargolist->push_back(CargoData(i, cp->SourceStation(), cp->Count()));
					}
				}
				if (st->goods[i].cargo.ReservedCount() > 0) {
					SetBit(*transfers, i);
					if (HasBit(this->cargo, i)) {
						cargolist->push_back(CargoData(i, NEW_STATION, st->goods[i].cargo.ReservedCount()));
					}
				}
			}
		}
	}

	/**
	 * Fill cargo list sorted by type and destination/next hop.
	 * @param sort_via Set to true to sort by next hop, false to sort by final destination.
	 * @param list Cargo list to fill.
	 * @return Number of visible lines.
	 */
	int FillCargodestList(CargoDestEntry::Type sort_by, CargoDestEntry::List *list)
	{
		StationID station_id = this->window_number;
		const Station *st = Station::Get(station_id);

		int lines = 0;

		/* Fill the list for each cargo type. */
		for (CargoID cid = 0; cid < NUM_CARGO; cid++) {
			/* Zero out all existing items. */
			for (CargoDestEntry::List::iterator i = list[cid].begin(); i != list[cid].end(); ++i) {
				i->Zero();
			}

			/* Remove all entries if no cargo of this type is present. */
			if (st->goods[cid].cargo.TotalCount() == 0) {
				this->cargo_rows[cid] = 0;
				list[cid].clear();
				continue;
			}

			/* Store line number of the header line. */
			this->cargo_rows[cid] = ++lines;

			/* Add each cargo packet to the list. */
			const StationCargoPacketMap *packets = st->goods[cid].cargo.Packets();
			for (StationCargoList::ConstIterator it = packets->begin(); it != packets->end(); ++it) {
				const CargoPacket *cp = *it;

				/* Add entry and sub-entries according to the chosen sort type. */
				static const CargoDestEntry::Type sort_types[][3] = {
					{CargoDestEntry::FINAL_DEST, CargoDestEntry::NEXT_HOP, CargoDestEntry::TRANSFER_HOP},
					{CargoDestEntry::NEXT_HOP, CargoDestEntry::TRANSFER_HOP, CargoDestEntry::FINAL_DEST},
					{CargoDestEntry::TRANSFER_HOP, CargoDestEntry::NEXT_HOP, CargoDestEntry::FINAL_DEST}
				};

				CargoDestEntry *entry = AddCargoPacketToList(list[cid], sort_types[sort_by][0], cp, st->goods[cid]);
				if (entry != NULL) {
					entry = AddCargoPacketToList(entry->children, sort_types[sort_by][1], cp, st->goods[cid]);
					if (entry != NULL) AddCargoPacketToList(entry->children, sort_types[sort_by][2], cp, st->goods[cid]);
				}
			}

			/* Remove all empty list items and update visible row numbers. */
			for (CargoDestEntry::List::iterator i = list[cid].begin(); i != list[cid].end(); )  {
				if (i->data.count > 0) {
					i->RemoveEmpty();
					if (HasBit(this->cargo, cid)) lines = i->UpdateRowCount(lines);
					++i;
				} else {
					i = list[cid].erase(i);
				}
			}
		}

		return lines;
	}

	/**
	 * Draw waiting cargo.
	 * @param r Rectangle of the widget.
	 * @param cargolist Cargo, ordered by type and destination.
	 * @param transfers Bitmask for cargoes that are transfered.
	 */
	void DrawWaitingCargo(const Rect &r, const CargoDataList &cargolist, uint32 transfers) const
	{
		int y = r.top + WD_FRAMERECT_TOP;
		int pos = this->vscroll->GetPosition();

		const Station *st = Station::Get(this->window_number);
		if (--pos < 0) {
			StringID str = STR_JUST_NOTHING;
			for (CargoID i = 0; i < NUM_CARGO; i++) {
				if (st->goods[i].cargo.TotalCount() > 0) str = STR_EMPTY;
			}
			SetDParam(0, str);
			DrawString(r.left + WD_FRAMERECT_LEFT, r.right - WD_FRAMERECT_RIGHT, y, STR_STATION_VIEW_WAITING_TITLE);
			y += FONT_HEIGHT_NORMAL;
		}

		bool rtl = _current_text_dir == TD_RTL;
		int text_left    = rtl ? r.left + this->expand_shrink_width : r.left + WD_FRAMERECT_LEFT;
		int text_right   = rtl ? r.right - WD_FRAMERECT_LEFT : r.right - this->expand_shrink_width;
		int shrink_left  = rtl ? r.left + WD_FRAMERECT_LEFT : r.right - this->expand_shrink_width + WD_FRAMERECT_LEFT;
		int shrink_right = rtl ? r.left + this->expand_shrink_width - WD_FRAMERECT_RIGHT : r.right - WD_FRAMERECT_RIGHT;


		int maxrows = this->vscroll->GetCapacity();
		for (CargoDataList::const_iterator it = cargolist.begin(); it != cargolist.end() && pos > -maxrows; ++it) {
			if (--pos < 0) {
				const CargoData *cd = &(*it);
				if (cd->station == INVALID_STATION) {
					/* Heading */
					DrawCargoIcons(cd->cargo, cd->count, r.left + WD_FRAMERECT_LEFT, r.right - WD_FRAMERECT_RIGHT, y);
					SetDParam(0, cd->cargo);
					SetDParam(1, cd->count);
					if (HasBit(transfers, cd->cargo)) {
						/* This cargo has transfers waiting so show the expand or shrink 'button' */
						const char *sym = HasBit(this->cargo, cd->cargo) ? "-" : "+";
						DrawString(text_left, text_right, y, STR_STATION_VIEW_WAITING_CARGO, TC_FROMSTRING, SA_RIGHT);
						DrawString(shrink_left, shrink_right, y, sym, TC_YELLOW, SA_RIGHT);
					} else {
						DrawString(r.left + WD_FRAMERECT_LEFT, r.right - WD_FRAMERECT_RIGHT, y, STR_STATION_VIEW_WAITING_CARGO, TC_FROMSTRING, SA_RIGHT);
					}
				} else if (cd->station == NEW_STATION) {
					SetDParam(0, cd->cargo);
					SetDParam(1, cd->count);
					DrawString(r.left + WD_FRAMERECT_LEFT, r.right - WD_FRAMERECT_RIGHT, y, STR_STATION_VIEW_RESERVED, TC_FROMSTRING, SA_RIGHT);
				} else {
					SetDParam(0, cd->cargo);
					SetDParam(1, cd->count);
					SetDParam(2, cd->station);
					DrawString(r.left + WD_FRAMERECT_LEFT, r.right - WD_FRAMERECT_RIGHT, y, STR_STATION_VIEW_EN_ROUTE_FROM, TC_FROMSTRING, SA_RIGHT);
				}
				y += FONT_HEIGHT_NORMAL;
			}
		}
	}

	/**
	 * Draw a dest entry and its children.
	 * @param cid Current cargo type.
	 * @param pos Scroll position
	 * @param maxrows Number of visible rows.
	 * @param left Left string bound.
	 * @param right Right string bound.
	 * @param shrink_left Left bound of the expand marker.
	 * @param shrink_right Right bound of the expand marker.
	 * @param offs_left Child offset of the left bound.
	 * @param offs_right Child offset of the right bound.
	 * @param y Top of the current line.
	 * @param entry The entry to draw.
	 * @return The new y value.
	 */
	int DrawSingleDestEntry(CargoID cid, int *pos, int maxrows, int left, int right, int shrink_left, int shrink_right, int offs_left, int offs_right, int y, const CargoDestEntry &entry) const
	{
		if (--(*pos) < 0) {
			/* Draw current line. */
			StringID str;

			SetDParam(0, cid);
			SetDParam(1, entry.data.count);
			if (entry.type == CargoDestEntry::FINAL_DEST) {
				SetDParam(2, entry.data.type == ST_INDUSTRY ? STR_INDUSTRY_NAME : (entry.data.type == ST_TOWN ? STR_TOWN_NAME : STR_COMPANY_NAME));
				SetDParam(3, entry.data.css);
				str = STR_STATION_VIEW_WAITING_TO;
			} else {
				SetDParam(2, entry.data.station);
				str = (entry.type == CargoDestEntry::NEXT_HOP) ? STR_STATION_VIEW_WAITING_VIA : STR_STATION_VIEW_WAITING_TRANSFER;
			}
			DrawString(left, right, y, str);
			y += FONT_HEIGHT_NORMAL;

			if (!entry.children.empty()) {
				/* Draw expand/collapse marker. */
				DrawString(shrink_left, shrink_right, y - FONT_HEIGHT_NORMAL, entry.expanded ? "-" : "+", TC_YELLOW, SA_RIGHT);

				if (entry.expanded) {
					/* Draw visible children. */
					for (CargoDestEntry::List::const_iterator i = entry.children.begin(); i != entry.children.end() && *pos > -maxrows; ++i) {
						y = this->DrawSingleDestEntry(cid, pos, maxrows, left + offs_left, right + offs_right, shrink_left, shrink_right, offs_left, offs_right, y, *i);
					}
				}
			}
		}

		return y;
	}

	/**
	 * Draw waiting cargo ordered by destination/next hop.
	 * @param r Rectangle of the widget.
	 * @param list List to draw.
	 */
	void DrawWaitingCargoByDest(const Rect &r, const CargoDestEntry::List *list) const
	{
		int y = r.top + WD_FRAMERECT_TOP;
		int pos = this->vscroll->GetPosition();

		const Station *st = Station::Get(this->window_number);
		if (--pos < 0) {
			StringID str = STR_JUST_NOTHING;
			for (CargoID i = 0; i < NUM_CARGO; i++) {
				if (st->goods[i].cargo.TotalCount() > 0) str = STR_EMPTY;
			}
			SetDParam(0, str);
			DrawString(r.left + WD_FRAMERECT_LEFT, r.right - WD_FRAMERECT_RIGHT, y, STR_STATION_VIEW_WAITING_TITLE);
			y += FONT_HEIGHT_NORMAL;
		}

		bool rtl = _current_text_dir == TD_RTL;
		int text_left    = rtl ? r.left + this->expand_shrink_width : r.left + WD_FRAMERECT_LEFT;
		int text_right   = rtl ? r.right - WD_FRAMERECT_LEFT : r.right - this->expand_shrink_width;
		int shrink_left  = rtl ? r.left + WD_FRAMERECT_LEFT : r.right - this->expand_shrink_width + WD_FRAMERECT_LEFT;
		int shrink_right = rtl ? r.left + this->expand_shrink_width - WD_FRAMERECT_RIGHT : r.right - WD_FRAMERECT_RIGHT;

		int offs_left   = rtl ? 0 : this->expand_shrink_width;
		int offs_right  = rtl ? this->expand_shrink_width : 0;

		int maxrows = this->vscroll->GetCapacity();
		for (CargoID cid = 0; cid < NUM_CARGO && pos > -maxrows; cid++) {
			if (st->goods[cid].cargo.TotalCount() == 0) continue;

			if (--pos < 0) {
				/* Draw heading. */
				DrawCargoIcons(cid, st->goods[cid].cargo.TotalCount(), r.left + WD_FRAMETEXT_LEFT, r.right - WD_FRAMERECT_RIGHT, y);
				SetDParam(0, cid);
				SetDParam(1, st->goods[cid].cargo.TotalCount());
				DrawString(text_left, text_right, y, STR_STATION_VIEW_WAITING_CARGO, TC_FROMSTRING, SA_RIGHT);
				if (!list[cid].empty()) {
					DrawString(shrink_left, shrink_right, y, HasBit(this->cargo, cid) ? "-" : "+", TC_YELLOW, SA_RIGHT);
				}
				y += FONT_HEIGHT_NORMAL;
			}

			/* Draw sub-entries. */
			if (HasBit(this->cargo, cid)) {
				for (CargoDestEntry::List::const_iterator i = list[cid].begin(); i != list[cid].end() && pos > -maxrows; ++i) {
					y = this->DrawSingleDestEntry(cid, &pos, maxrows, text_left + offs_left, text_right + offs_right, shrink_left, shrink_right, offs_left, offs_right, y, *i);
				}
			}
		}
	}

	/**
	 * Draw accepted cargo in the #WID_SV_ACCEPT_RATING_LIST widget.
	 * @param r Rectangle of the widget.
	 * @return Number of lines needed for drawing the accepted cargo.
	 */
	int DrawAcceptedCargo(const Rect &r) const
	{
		const Station *st = Station::Get(this->window_number);

		uint32 cargo_mask = 0;
		for (CargoID i = 0; i < NUM_CARGO; i++) {
			if (HasBit(st->goods[i].acceptance_pickup, GoodsEntry::GES_ACCEPTANCE)) SetBit(cargo_mask, i);
		}
		SetDParam(0, cargo_mask);
		int bottom = DrawStringMultiLine(r.left + WD_FRAMERECT_LEFT, r.right - WD_FRAMERECT_RIGHT, r.top + WD_FRAMERECT_TOP, INT32_MAX, STR_STATION_VIEW_ACCEPTS_CARGO);
		return CeilDiv(bottom - r.top - WD_FRAMERECT_TOP, FONT_HEIGHT_NORMAL);
	}

	/**
	 * Draw cargo ratings in the #WID_SV_ACCEPT_RATING_LIST widget.
	 * @param r Rectangle of the widget.
	 * @return Number of lines needed for drawing the cargo ratings.
	 */
	int DrawCargoRatings(const Rect &r) const
	{
		const Station *st = Station::Get(this->window_number);
		int y = r.top + WD_FRAMERECT_TOP;

		if (st->town->exclusive_counter > 0) {
			SetDParam(0, st->town->exclusivity);
			y = DrawStringMultiLine(r.left + WD_FRAMERECT_LEFT, r.right - WD_FRAMERECT_RIGHT, y, r.bottom, st->town->exclusivity == st->owner ? STR_STATIOV_VIEW_EXCLUSIVE_RIGHTS_SELF : STR_STATIOV_VIEW_EXCLUSIVE_RIGHTS_COMPANY);
			y += WD_PAR_VSEP_WIDE;
		}

		DrawString(r.left + WD_FRAMERECT_LEFT, r.right - WD_FRAMERECT_RIGHT, y, STR_STATION_VIEW_CARGO_RATINGS_TITLE);
		y += FONT_HEIGHT_NORMAL;

		const CargoSpec *cs;
		FOR_ALL_SORTED_STANDARD_CARGOSPECS(cs) {
			const GoodsEntry *ge = &st->goods[cs->Index()];
			if (!ge->HasRating()) continue;
			SetDParam(0, cs->name);
			SetDParam(2, ToPercent8(ge->rating));
			SetDParam(1, STR_CARGO_RATING_APPALLING + (ge->rating >> 5));
			DrawString(r.left + WD_FRAMERECT_LEFT + 6, r.right - WD_FRAMERECT_RIGHT - 6, y, STR_STATION_VIEW_CARGO_RATING);
			y += FONT_HEIGHT_NORMAL;
		}
		return CeilDiv(y - r.top - WD_FRAMERECT_TOP, FONT_HEIGHT_NORMAL);
	}
	
	/**
	 * Draw cargo ratings that is not in the standart #WID_SV_ACCEPT_RATING_LIST widget.
	 * @param r Rectangle of the widget.
	 * @return Number of lines needed for drawing the cargo ratings.
	 */
	int DrawExtraCargoRatings(const Rect &r)
	{
		const Station *st = Station::Get(this->window_number);
		Rect sr = st->GetCatchmentRect();
		cargo_around = GetProductionAroundTiles(TileXY(sr.left, sr.top), sr.right - sr.left + 1, sr.bottom - sr.top + 1, 0);

		int y = r.top + WD_FRAMERECT_TOP;
		DrawString(r.left + WD_FRAMERECT_LEFT, r.right - WD_FRAMERECT_RIGHT, y, STR_STATION_VIEW_CARGO_RATINGS_TITLE);
		y += FONT_HEIGHT_NORMAL;
		
		/* Stop drawing if there no any production around */	
		if (cargo_around.GetCount() == 0) return CeilDiv(y - r.top - WD_FRAMERECT_TOP, FONT_HEIGHT_NORMAL);		
		
		CargoID counter = 0; /* Count rows of finded industries */
		const CargoSpec *cs;
		FOR_ALL_SORTED_STANDARD_CARGOSPECS(cs) {
			const GoodsEntry *ge = &st->goods[cs->Index()];
			if (ge->HasRating() || cargo_around[cs->Index()] == 0) continue;
			cargo_around[cs->Index()] = ++counter;
			SetDParam(0, cs->name);
			SetDParam(2, ToPercent8(ge->rating));
			SetDParam(1, STR_CARGO_RATING_APPALLING + (ge->rating >> 5));
			DrawString(r.left + WD_FRAMERECT_LEFT + 6, r.right - WD_FRAMERECT_RIGHT - 6, y, STR_STATION_VIEW_CARGO_RATING);
			y += FONT_HEIGHT_NORMAL;
		}
		return CeilDiv(y - r.top - WD_FRAMERECT_TOP, FONT_HEIGHT_NORMAL);
	}

	/**
	 * Test and handle a possible mouse click on a dest entry and its children.
	 * @param entry The entry to test for a hit.
	 * @param row The number of the clicked row.
	 * @return True if further entries need to be processed.
	 */
	bool HandleCargoDestEntryClick(CargoDestEntry &entry, int row)
	{
		if (entry.start_row == row) {
			if (_ctrl_pressed) {
				/* Scroll viewport to destination tile .*/
				TileIndex dest_tile = 0;
				switch (entry.type) {
					case CargoDestEntry::FINAL_DEST:
						switch (entry.data.type) {
							case ST_INDUSTRY:
								dest_tile = Industry::Get(entry.data.css)->location.tile;
								break;
							case ST_TOWN:
								dest_tile = Town::Get(entry.data.css)->xy;
								break;
							case ST_HEADQUARTERS:
								dest_tile = Company::Get(entry.data.css)->location_of_HQ;
								break;

							default:
								NOT_REACHED();
						}
						break;

					case CargoDestEntry::NEXT_HOP:
					case CargoDestEntry::TRANSFER_HOP:
						dest_tile = Station::Get(entry.data.station)->xy;
						break;

					default:
						NOT_REACHED();
				}
				ScrollMainWindowToTile(dest_tile);
			} else if (!entry.children.empty()) {
				/* Expand/collapse entry. */
				entry.expanded = !entry.expanded;
				this->SetWidgetDirty(WID_SV_WAITING);
				this->SetWidgetDirty(WID_SV_SCROLLBAR);
			}
		}

		if (entry.start_row < row) {
			/* Test child entries. */
			for (CargoDestEntry::List::iterator i = entry.children.begin(); i != entry.children.end(); ++i) {
				if (!this->HandleCargoDestEntryClick(*i, row)) return false;
			}
			return true;
		}

		return false;
	}

       void HandleAcceptRatingClick(int row)
       {
               if (row == 0 || !_ctrl_pressed || !_settings_game.station.advanced_control) return;

               for (CargoID c = 0; c < NUM_CARGO; c++) {
                       if (this->cargo_around[c] == (uint16)row) {
                               DoCommandP(0, this->window_number , c, CMD_CHANGE_STATION_ACCEPTANCE);
                               break;
                       }
               }
       }

	void HandleCargoWaitingClick(int row)
	{
		if (row == 0) return;

		bool dest_view = this->GetWidget<NWidgetCore>(WID_SV_CARGO_FROM_TO_VIA)->widget_data != STR_STATION_VIEW_WAITING_TO_BUTTON;

		for (CargoID c = 0; c < NUM_CARGO; c++) {
			/* Test for cargo type line. */
			if (this->cargo_rows[c] == row) {
                               /* Check for ctrl-click. If not - do thing as when old good times. */
                               if (!_ctrl_pressed || !_settings_game.station.advanced_control) {
                                       ToggleBit(this->cargo, c);
                               }
                               else {
                                       /* If ctrl-click happen - stop accepting it/delete this cargo from list. */
                                       DoCommandP(0, this->window_number , c | 0x0100, CMD_CHANGE_STATION_ACCEPTANCE);
                               }
				break;
			}

			if (dest_view) {
				/* Test for dest view lines. */
				for (CargoDestEntry::List::iterator i = this->cargodest_list[c].begin(); i != this->cargodest_list[c].end(); ++i) {
					if (!this->HandleCargoDestEntryClick(*i, row)) break;
				}
			}
		}
	}

	/** Clear the 'cargo by destination' list. */
	void ClearCargodestList()
	{
		for (CargoID cid = 0; cid < NUM_CARGO; cid++) {
			this->cargodest_list[cid].clear();
		}
	}

	virtual void OnClick(Point pt, int widget, int click_count)
	{
		switch (widget) {
			case WID_SV_WAITING: {
				if (click_count == 1) {
					this->HandleCargoWaitingClick(this->vscroll->GetScrolledRowFromWidget(pt.y, this, WID_SV_WAITING, WD_FRAMERECT_TOP, FONT_HEIGHT_NORMAL));
				}
				else {
					/* Toggle draw list with std/extra oprions */
					this->stdacceptratings = this->stdacceptratings ? false : true;
				}
				this->SetWidgetDirty(WID_SV_WAITING);
				this->SetWidgetDirty(WID_SV_ACCEPT_RATING_LIST);
				break;
			}
			
			case WID_SV_ACCEPT_RATING_LIST: {
				if (click_count == 1) {
					this->HandleAcceptRatingClick(this->GetRowFromWidget(pt.y, WID_SV_ACCEPT_RATING_LIST, WD_FRAMERECT_TOP, FONT_HEIGHT_NORMAL));
				}
				else {
					this->stdacceptratings = this->stdacceptratings ? false : true;
				}
				this->SetWidgetDirty(WID_SV_ACCEPT_RATING_LIST);
				break;
			}

			case WID_SV_LOCATION:
				if (_ctrl_pressed) {
					ShowExtraViewPortWindow(Station::Get(this->window_number)->xy);
				} else {
					ScrollMainWindowToTile(Station::Get(this->window_number)->xy);
				}
				break;

//I don't like the cover button
//			case WID_SV_COVERAGE:
//				Overlays::Instance()->ToggleStation(Station::Get(this->window_number));
//				MarkWholeScreenDirty();
//				break;

			case WID_SV_ACCEPTS_RATINGS: {
				/* Swap between 'accepts' and 'ratings' view. */
				int height_change;
				NWidgetCore *nwi = this->GetWidget<NWidgetCore>(WID_SV_ACCEPTS_RATINGS);
				if (this->GetWidget<NWidgetCore>(WID_SV_ACCEPTS_RATINGS)->widget_data == STR_STATION_VIEW_RATINGS_BUTTON) {
					nwi->SetDataTip(STR_STATION_VIEW_ACCEPTS_BUTTON, STR_STATION_VIEW_ACCEPTS_TOOLTIP); // Switch to accepts view.
					height_change = this->rating_lines - this->accepts_lines;
				} else {
					nwi->SetDataTip(STR_STATION_VIEW_RATINGS_BUTTON, STR_STATION_VIEW_RATINGS_TOOLTIP); // Switch to ratings view.
					height_change = this->accepts_lines - this->rating_lines;
				}
				this->ReInit(0, height_change * FONT_HEIGHT_NORMAL);
				break;
			}

			case WID_SV_CARGO_FROM_TO_VIA: {
				/* Swap between 'Source', 'Destination', 'Next hop' and 'Transfer' view.
				 * Store the new view so the next opened station window shows the same view. */
				NWidgetCore *nwi = this->GetWidget<NWidgetCore>(WID_SV_CARGO_FROM_TO_VIA);
				switch (nwi->widget_data) {
					case STR_STATION_VIEW_WAITING_BUTTON:
						StationViewWindow::last_cargo_from_str     = STR_STATION_VIEW_WAITING_TO_BUTTON;
						StationViewWindow::last_cargo_from_tooltip = STR_STATION_VIEW_WAITING_TO_TOOLTIP;
						break;
					case STR_STATION_VIEW_WAITING_TO_BUTTON:
						StationViewWindow::last_cargo_from_str     = STR_STATION_VIEW_WAITING_VIA_BUTTON;
						StationViewWindow::last_cargo_from_tooltip = STR_STATION_VIEW_WAITING_VIA_TOOLTIP;
						break;
					case STR_STATION_VIEW_WAITING_VIA_BUTTON:
						StationViewWindow::last_cargo_from_str     = STR_STATION_VIEW_WAITING_TRANSFER_BUTTON;
						StationViewWindow::last_cargo_from_tooltip = STR_STATION_VIEW_WAITING_TRANSFER_TOOLTIP;
						break;
					case STR_STATION_VIEW_WAITING_TRANSFER_BUTTON:
						StationViewWindow::last_cargo_from_str     = STR_STATION_VIEW_WAITING_BUTTON;
						StationViewWindow::last_cargo_from_tooltip = STR_STATION_VIEW_WAITING_TOOLTIP;
						break;
					default:
						NOT_REACHED();
				}
				nwi->SetDataTip(StationViewWindow::last_cargo_from_str, StationViewWindow::last_cargo_from_tooltip);
				this->ClearCargodestList();
				this->SetWidgetDirty(WID_SV_CARGO_FROM_TO_VIA);
				this->SetWidgetDirty(WID_SV_WAITING);
				this->SetWidgetDirty(WID_SV_SCROLLBAR);
				break;
			}

			case WID_SV_RENAME:
				SetDParam(0, this->window_number);
				ShowQueryString(STR_STATION_NAME, STR_STATION_VIEW_RENAME_STATION_CAPTION, MAX_LENGTH_STATION_NAME_CHARS,
						this, CS_ALPHANUMERAL, QSF_ENABLE_DEFAULT | QSF_LEN_IN_CHARS);
				break;

			case WID_SV_CLOSE_AIRPORT:
				DoCommandP(0, this->window_number, 0, CMD_OPEN_CLOSE_AIRPORT);
				break;

			case WID_SV_TRAINS:   // Show list of scheduled trains to this station
			case WID_SV_ROADVEHS: // Show list of scheduled road-vehicles to this station
			case WID_SV_SHIPS:    // Show list of scheduled ships to this station
			case WID_SV_PLANES: { // Show list of scheduled aircraft to this station
				Owner owner = Station::Get(this->window_number)->owner;
				ShowVehicleListWindow(owner, (VehicleType)(widget - WID_SV_TRAINS), (StationID)this->window_number);
				break;
            }

			case WID_SV_DEPARTURES:
				ShowStationDepartures((StationID)this->window_number);
				break;
		}
	}

	virtual void OnQueryTextFinished(char *str)
	{
		if (str == NULL) return;

		DoCommandP(0, this->window_number, 0, CMD_RENAME_STATION | CMD_MSG(STR_ERROR_CAN_T_RENAME_STATION), NULL, str);
	}

	virtual void OnResize()
	{
		this->vscroll->SetCapacityFromWidget(this, WID_SV_WAITING, WD_FRAMERECT_TOP + WD_FRAMERECT_BOTTOM);
	}

	/**
	 * Some data on this window has become invalid.
	 * @param data Information about the changed data.
	 * @param gui_scope Whether the call is done from GUI scope. You may not do everything when not in GUI scope. See #InvalidateWindowData() for details.
	 */
	virtual void OnInvalidateData(int data = 0, bool gui_scope = true)
	{
		if (gui_scope) this->ReInit();
	}
};

StringID StationViewWindow::last_cargo_from_str     = STR_STATION_VIEW_WAITING_VIA_BUTTON;
StringID StationViewWindow::last_cargo_from_tooltip = STR_STATION_VIEW_WAITING_VIA_TOOLTIP;

static WindowDesc _station_view_desc(
	WDP_AUTO, "view_station", 249, 110,
	WC_STATION_VIEW, WC_NONE,
	0,
	_nested_station_view_widgets, lengthof(_nested_station_view_widgets)
);

/**
 * Opens StationViewWindow for given station
 *
 * @param station station which window should be opened
 */
void ShowStationViewWindow(StationID station)
{
       if (_ctrl_pressed) {
               Overlays::Instance()->ToggleStation(Station::Get(station));
               MarkWholeScreenDirty();
       } else {
               AllocateWindowDescFront<StationViewWindow>(&_station_view_desc, station);
       }
}

/** Struct containing TileIndex and StationID */
struct TileAndStation {
	TileIndex tile;    ///< TileIndex
	StationID station; ///< StationID
};

static SmallVector<TileAndStation, 8> _deleted_stations_nearby;
static SmallVector<StationID, 8> _stations_nearby_list;

/**
 * Add station on this tile to _stations_nearby_list if it's fully within the
 * station spread.
 * @param tile Tile just being checked
 * @param user_data Pointer to TileArea context
 * @tparam T the type of station to look for
 */
template <class T>
static bool AddNearbyStation(TileIndex tile, void *user_data)
{
	TileArea *ctx = (TileArea *)user_data;

	/* First check if there were deleted stations here */
	for (uint i = 0; i < _deleted_stations_nearby.Length(); i++) {
		TileAndStation *ts = _deleted_stations_nearby.Get(i);
		if (ts->tile == tile) {
			*_stations_nearby_list.Append() = _deleted_stations_nearby[i].station;
			_deleted_stations_nearby.Erase(ts);
			i--;
		}
	}

	/* Check if own station and if we stay within station spread */
	if (!IsTileType(tile, MP_STATION)) return false;

	StationID sid = GetStationIndex(tile);

	/* This station is (likely) a waypoint */
	if (!T::IsValidID(sid)) return false;

	T *st = T::Get(sid);
	if (st->owner != _local_company || _stations_nearby_list.Contains(sid)) return false;

	if (st->rect.BeforeAddRect(ctx->tile, ctx->w, ctx->h, StationRect::ADD_TEST).Succeeded()) {
		*_stations_nearby_list.Append() = sid;
	}

	return false; // We want to include *all* nearby stations
}

/**
 * Circulate around the to-be-built station to find stations we could join.
 * Make sure that only stations are returned where joining wouldn't exceed
 * station spread and are our own station.
 * @param ta Base tile area of the to-be-built station
 * @param distant_join Search for adjacent stations (false) or stations fully
 *                     within station spread
 * @tparam T the type of station to look for
 */
template <class T>
static const T *FindStationsNearby(TileArea ta, bool distant_join)
{
	TileArea ctx = ta;

	_stations_nearby_list.Clear();
	_deleted_stations_nearby.Clear();

	/* Check the inside, to return, if we sit on another station */
	TILE_AREA_LOOP(t, ta) {
		if (t < MapSize() && IsTileType(t, MP_STATION) && T::IsValidID(GetStationIndex(t))) return T::GetByTile(t);
	}

	/* Look for deleted stations */
	const BaseStation *st;
	FOR_ALL_BASE_STATIONS(st) {
		if (T::IsExpected(st) && !st->IsInUse() && st->owner == _local_company) {
			/* Include only within station spread (yes, it is strictly less than) */
			if (max(DistanceMax(ta.tile, st->xy), DistanceMax(TILE_ADDXY(ta.tile, ta.w - 1, ta.h - 1), st->xy)) < _settings_game.station.station_spread) {
				TileAndStation *ts = _deleted_stations_nearby.Append();
				ts->tile = st->xy;
				ts->station = st->index;

				/* Add the station when it's within where we're going to build */
				if (IsInsideBS(TileX(st->xy), TileX(ctx.tile), ctx.w) &&
						IsInsideBS(TileY(st->xy), TileY(ctx.tile), ctx.h)) {
					AddNearbyStation<T>(st->xy, &ctx);
				}
			}
		}
	}

	/* Only search tiles where we have a chance to stay within the station spread.
	 * The complete check needs to be done in the callback as we don't know the
	 * extent of the found station, yet. */
	if (distant_join && min(ta.w, ta.h) >= _settings_game.station.station_spread) return NULL;
	uint max_dist = distant_join ? _settings_game.station.station_spread - min(ta.w, ta.h) : 1;

	TileIndex tile = TILE_ADD(ctx.tile, TileOffsByDir(DIR_N));
	CircularTileSearch(&tile, max_dist, ta.w, ta.h, AddNearbyStation<T>, &ctx);

	return NULL;
}

static const NWidgetPart _nested_select_station_widgets[] = {
	NWidget(NWID_HORIZONTAL),
		NWidget(WWT_CLOSEBOX, COLOUR_DARK_GREEN),
		NWidget(WWT_CAPTION, COLOUR_DARK_GREEN, WID_JS_CAPTION), SetDataTip(STR_JOIN_STATION_CAPTION, STR_TOOLTIP_WINDOW_TITLE_DRAG_THIS),
		NWidget(WWT_DEFSIZEBOX, COLOUR_DARK_GREEN),
	EndContainer(),
	NWidget(NWID_HORIZONTAL),
		NWidget(WWT_PANEL, COLOUR_DARK_GREEN, WID_JS_PANEL), SetResize(1, 0), SetScrollbar(WID_JS_SCROLLBAR), EndContainer(),
		NWidget(NWID_VERTICAL),
			NWidget(NWID_VSCROLLBAR, COLOUR_DARK_GREEN, WID_JS_SCROLLBAR),
			NWidget(WWT_RESIZEBOX, COLOUR_DARK_GREEN),
		EndContainer(),
	EndContainer(),
};

/**
 * Window for selecting stations/waypoints to (distant) join to.
 * @tparam T The type of station to join with
 */
template <class T>
struct SelectStationWindow : Window {
	CommandContainer select_station_cmd; ///< Command to build new station
	TileArea area; ///< Location of new station
	Scrollbar *vscroll;

	SelectStationWindow(WindowDesc *desc, CommandContainer cmd, TileArea ta) :
		Window(desc),
		select_station_cmd(cmd),
		area(ta)
	{
		this->CreateNestedTree();
		this->vscroll = this->GetScrollbar(WID_JS_SCROLLBAR);
		this->GetWidget<NWidgetCore>(WID_JS_CAPTION)->widget_data = T::EXPECTED_FACIL == FACIL_WAYPOINT ? STR_JOIN_WAYPOINT_CAPTION : STR_JOIN_STATION_CAPTION;
		this->FinishInitNested(0);
		this->OnInvalidateData(0);
	}

	virtual void UpdateWidgetSize(int widget, Dimension *size, const Dimension &padding, Dimension *fill, Dimension *resize)
	{
		if (widget != WID_JS_PANEL) return;

		/* Determine the widest string */
		Dimension d = GetStringBoundingBox(T::EXPECTED_FACIL == FACIL_WAYPOINT ? STR_JOIN_WAYPOINT_CREATE_SPLITTED_WAYPOINT : STR_JOIN_STATION_CREATE_SPLITTED_STATION);
		for (uint i = 0; i < _stations_nearby_list.Length(); i++) {
			const T *st = T::Get(_stations_nearby_list[i]);
			SetDParam(0, st->index);
			SetDParam(1, st->facilities);
			d = maxdim(d, GetStringBoundingBox(T::EXPECTED_FACIL == FACIL_WAYPOINT ? STR_STATION_LIST_WAYPOINT : STR_STATION_LIST_STATION));
		}

		resize->height = d.height;
		d.height *= 5;
		d.width += WD_FRAMERECT_RIGHT + WD_FRAMERECT_LEFT;
		d.height += WD_FRAMERECT_TOP + WD_FRAMERECT_BOTTOM;
		*size = d;
	}

	virtual void DrawWidget(const Rect &r, int widget) const
	{
		if (widget != WID_JS_PANEL) return;

		uint y = r.top + WD_FRAMERECT_TOP;
		if (this->vscroll->GetPosition() == 0) {
			DrawString(r.left + WD_FRAMERECT_LEFT, r.right - WD_FRAMERECT_RIGHT, y, T::EXPECTED_FACIL == FACIL_WAYPOINT ? STR_JOIN_WAYPOINT_CREATE_SPLITTED_WAYPOINT : STR_JOIN_STATION_CREATE_SPLITTED_STATION);
			y += this->resize.step_height;
		}

		for (uint i = max<uint>(1, this->vscroll->GetPosition()); i <= _stations_nearby_list.Length(); ++i, y += this->resize.step_height) {
			/* Don't draw anything if it extends past the end of the window. */
			if (i - this->vscroll->GetPosition() >= this->vscroll->GetCapacity()) break;

			const T *st = T::Get(_stations_nearby_list[i - 1]);
			SetDParam(0, st->index);
			SetDParam(1, st->facilities);
			DrawString(r.left + WD_FRAMERECT_LEFT, r.right - WD_FRAMERECT_RIGHT, y, T::EXPECTED_FACIL == FACIL_WAYPOINT ? STR_STATION_LIST_WAYPOINT : STR_STATION_LIST_STATION);
		}
	}

	virtual void OnClick(Point pt, int widget, int click_count)
	{
		if (widget != WID_JS_PANEL) return;

		uint st_index = this->vscroll->GetScrolledRowFromWidget(pt.y, this, WID_JS_PANEL, WD_FRAMERECT_TOP);
		bool distant_join = (st_index > 0);
		if (distant_join) st_index--;

		if (distant_join && st_index >= _stations_nearby_list.Length()) return;

		/* Insert station to be joined into stored command */
		SB(this->select_station_cmd.p2, 16, 16,
		   (distant_join ? _stations_nearby_list[st_index] : NEW_STATION));

		/* Execute stored Command */
		DoCommandP(&this->select_station_cmd);

		/* Close Window; this might cause double frees! */
		DeleteWindowById(WC_SELECT_STATION, 0);
	}

	virtual void OnTick()
	{
		if (_thd.dirty & 2) {
			_thd.dirty &= ~2;
			this->SetDirty();
		}
	}

	virtual void OnResize()
	{
		this->vscroll->SetCapacityFromWidget(this, WID_JS_PANEL, WD_FRAMERECT_TOP + WD_FRAMERECT_BOTTOM);
	}

	/**
	 * Some data on this window has become invalid.
	 * @param data Information about the changed data.
	 * @param gui_scope Whether the call is done from GUI scope. You may not do everything when not in GUI scope. See #InvalidateWindowData() for details.
	 */
	virtual void OnInvalidateData(int data = 0, bool gui_scope = true)
	{
		if (!gui_scope) return;
		FindStationsNearby<T>(this->area, true);
		this->vscroll->SetCount(_stations_nearby_list.Length() + 1);
		this->SetDirty();
	}
protected:
    void Get(WindowNumber window_number);
};

static WindowDesc _select_station_desc(
	WDP_AUTO, "build_station_join", 200, 180,
	WC_SELECT_STATION, WC_NONE,
	WDF_CONSTRUCTION,
	_nested_select_station_widgets, lengthof(_nested_select_station_widgets)
);


/**
 * Check whether we need to show the station selection window.
 * @param cmd Command to build the station.
 * @param ta Tile area of the to-be-built station
 * @tparam T the type of station
 * @return whether we need to show the station selection window.
 */
template <class T>
static bool StationJoinerNeeded(CommandContainer cmd, TileArea ta)
{
	/* Only show selection if distant join is enabled in the settings */
	if (!_settings_game.station.distant_join_stations) return false;

	/* If a window is already opened and we didn't ctrl-click,
	 * return true (i.e. just flash the old window) */
	Window *selection_window = FindWindowById(WC_SELECT_STATION, 0);
	if (selection_window != NULL) {
		/* Abort current distant-join and start new one */
		delete selection_window;
		UpdateTileSelection();
	}

	/* only show the popup, if we press ctrl */
	if (!_ctrl_pressed) return false;

	/* Now check if we could build there */
	if (DoCommand(&cmd, CommandFlagsToDCFlags(GetCommandFlags(cmd.cmd))).Failed()) return false;

	/* Test for adjacent station or station below selection.
	 * If adjacent-stations is disabled and we are building next to a station, do not show the selection window.
	 * but join the other station immediately. */
	const T *st = FindStationsNearby<T>(ta, false);
	return st == NULL && (_settings_game.station.adjacent_stations || _stations_nearby_list.Length() == 0);
}

/**
 * Show the station selection window when needed. If not, build the station.
 * @param cmd Command to build the station.
 * @param ta Area to build the station in
 * @tparam the class to find stations for
 */
template <class T>
void ShowSelectBaseStationIfNeeded(CommandContainer cmd, TileArea ta)
{
	if (StationJoinerNeeded<T>(cmd, ta)) {
		if (!_settings_client.gui.persistent_buildingtools) ResetObjectToPlace();
		new SelectStationWindow<T>(&_select_station_desc, cmd, ta);
	} else {
		DoCommandP(&cmd);
	}
}

/**
 * Show the station selection window when needed. If not, build the station.
 * @param cmd Command to build the station.
 * @param ta Area to build the station in
 */
void ShowSelectStationIfNeeded(CommandContainer cmd, TileArea ta)
{
	ShowSelectBaseStationIfNeeded<Station>(cmd, ta);
}

/**
 * Show the waypoint selection window when needed. If not, build the waypoint.
 * @param cmd Command to build the waypoint.
 * @param ta Area to build the waypoint in
 */
void ShowSelectWaypointIfNeeded(CommandContainer cmd, TileArea ta)
{
	ShowSelectBaseStationIfNeeded<Waypoint>(cmd, ta);
}
