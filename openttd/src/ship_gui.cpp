/* $Id: ship_gui.cpp 25500 2013-06-28 19:29:08Z rubidium $ */

/*
 * This file is part of OpenTTD.
 * OpenTTD is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, version 2.
 * OpenTTD is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with OpenTTD. If not, see <http://www.gnu.org/licenses/>.
 */

/** @file ship_gui.cpp GUI for ships. */

#include "stdafx.h"
#include "vehicle_base.h"
#include "window_gui.h"
#include "gfx_func.h"
#include "vehicle_gui.h"
#include "strings_func.h"
#include "vehicle_func.h"
#include "spritecache.h"
#include "zoom_func.h"

#include "table/strings.h"

/**
 * Draws an image of a ship
 * @param v         Front vehicle
 * @param left      The minimum horizontal position
 * @param right     The maximum horizontal position
 * @param y         Vertical position to draw at
 * @param selection Selected vehicle to draw a frame around
 */
void DrawShipImage(const Vehicle *v, int left, int right, int y, VehicleID selection, EngineImageType image_type)
{
	bool rtl = _current_text_dir == TD_RTL;

	SpriteID sprite = v->GetImage(rtl ? DIR_E : DIR_W, image_type);
	const Sprite *real_sprite = GetSprite(sprite, ST_NORMAL);

	int width = UnScaleByZoom(real_sprite->width, ZOOM_LVL_GUI);
	int x_offs = UnScaleByZoom(real_sprite->x_offs, ZOOM_LVL_GUI);
	int x = rtl ? right - width - x_offs : left - x_offs;

	DrawSprite(sprite, GetVehiclePalette(v), x, y + 10);

	if (v->index == selection) {
		x += x_offs;
		y += UnScaleByZoom(real_sprite->y_offs, ZOOM_LVL_GUI) + 10;
		DrawFrameRect(x - 1, y - 1, x + width + 1, y + UnScaleByZoom(real_sprite->height, ZOOM_LVL_GUI) + 1, COLOUR_WHITE, FR_BORDERONLY);
	}
}

/**
 * Draw the details for the given vehicle at the given position
 *
 * @param v     current vehicle
 * @param left  The left most coordinate to draw
 * @param right The right most coordinate to draw
 * @param y     The y coordinate
 */
void DrawShipDetails(const Vehicle *v, int left, int right, int y)
{
        CargoArray act_cargo;
        CargoDestSummary dests[NUM_CARGO];
        CargoArray max_cargo;

	SetDParam(0, v->engine_type);
	SetDParam(1, v->build_year);
	SetDParam(2, v->value);
	DrawString(left, right, y, STR_VEHICLE_INFO_BUILT_VALUE);

	SetDParam(0, v->cargo_type);
	SetDParam(1, v->cargo_cap);
	SetDParam(4, GetCargoSubtypeText(v));
	DrawString(left, right, y + FONT_HEIGHT_NORMAL, STR_VEHICLE_INFO_CAPACITY);

	StringID str = STR_VEHICLE_DETAILS_CARGO_EMPTY;
	if (v->cargo.StoredCount() > 0) {
		SetDParam(0, v->cargo_type);
		SetDParam(1, v->cargo.StoredCount());
		SetDParam(2, v->cargo.Source());
		str = STR_VEHICLE_DETAILS_CARGO_FROM;
                act_cargo[v->cargo_type] += v->cargo.StoredCount();
                AddVehicleCargoDestSummary(v, &dests[v->cargo_type]);
	}
	DrawString(left, right, y + 2 * FONT_HEIGHT_NORMAL + 1, str);

	/* Draw Transfer credits text */
	SetDParam(0, v->cargo.FeederShare());
	DrawString(left, right, y + 3 * FONT_HEIGHT_NORMAL + 3, STR_VEHICLE_INFO_FEEDER_CARGO_VALUE);

        int y_offset = y - 2 * FONT_HEIGHT_NORMAL;
        max_cargo[v->cargo_type] += v->cargo_cap;
       DrawString(left, right, y + FONT_HEIGHT_NORMAL + y_offset, STR_STATION_VIEW_WAITING_TO_BUTTON);

                for (CargoID i = 0; i < NUM_CARGO; i++) {
                        if (max_cargo[i] > 0) {
                                SetDParam(0, i);            // {CARGO} #1
                                SetDParam(1, act_cargo[i]); // {CARGO} #2
                                SetDParam(2, i);            // {SHORTCARGO} #1
                                SetDParam(3, max_cargo[i]); // {SHORTCARGO} #2
//                                SetDParam(4, _settings_game.vehicle.freight_trains);
                                DrawString(left, right, y + 2 * FONT_HEIGHT_NORMAL + 1 + y_offset, STR_VEHICLE_DETAILS_TRAIN_TOTAL_CAPACITY);
                               y_offset += FONT_HEIGHT_NORMAL + 1;
                        }
                        for (CargoDestSummary::const_iterator row = dests[i].begin(); row != dests[i].end(); ++row) {
                                        SetDParam(0, i);          // {SHORTCARGO} #1
                                        SetDParam(1, row->count); // {SHORTCARGO} #2
                                        SetDParam(2, row->type == ST_INDUSTRY ? STR_INDUSTRY_NAME : (row->type == ST_TOWN ? STR_TOWN_NAME : STR_COMPANY_NAME)); // {STRING1}
                                        SetDParam(3, row->dest);  // Parameter of {STRING1}
                                        DrawString(left + 2 * WD_PAR_VSEP_WIDE, right, y + 2 * FONT_HEIGHT_NORMAL + 1 + y_offset, STR_VEHICLE_DETAILS_CARGO_TO);
                                       y_offset += FONT_HEIGHT_NORMAL + 1;
                        }
                }
}
