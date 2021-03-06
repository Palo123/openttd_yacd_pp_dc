/* $Id: main_gui.cpp 26025 2013-11-17 13:53:33Z rubidium $ */

/*
 * This file is part of OpenTTD.
 * OpenTTD is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, version 2.
 * OpenTTD is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with OpenTTD. If not, see <http://www.gnu.org/licenses/>.
 */

/** @file main_gui.cpp Handling of the main viewport. */

#include "stdafx.h"
#include "currency.h"
#include "spritecache.h"
#include "window_gui.h"
#include "window_func.h"
#include "textbuf_gui.h"
#include "viewport_func.h"
#include "command_func.h"
#include "console_gui.h"
#include "progress.h"
#include "transparency_gui.h"
#include "map_func.h"
#include "sound_func.h"
#include "transparency.h"
#include "strings_func.h"
#include "zoom_func.h"
#include "company_base.h"
#include "company_func.h"
#include "toolbar_gui.h"
#include "statusbar_gui.h"
#include "tilehighlight_func.h"
#include "hotkeys.h"
#include "industry.h"
#include "town_map.h"

#include "saveload/saveload.h"

#include "widgets/main_widget.h"

#include "network/network.h"
#include "network/network_func.h"
#include "network/network_gui.h"
#include "network/network_base.h"

#include "table/sprites.h"
#include "table/strings.h"

void CcGiveMoney(const CommandCost &result, TileIndex tile, uint32 p1, uint32 p2)
{
#ifdef ENABLE_NETWORK
	if (result.Failed() || !_settings_game.economy.give_money) return;

	/* Inform the company of the action of one of its clients (controllers). */
	char msg[64];
	SetDParam(0, p2);
	GetString(msg, STR_COMPANY_NAME, lastof(msg));

	if (!_network_server) {
		NetworkClientSendChat(NETWORK_ACTION_GIVE_MONEY, DESTTYPE_TEAM, p2, msg, p1);
	} else {
		NetworkServerSendChat(NETWORK_ACTION_GIVE_MONEY, DESTTYPE_TEAM, p2, msg, CLIENT_ID_SERVER, p1);
	}
#endif /* ENABLE_NETWORK */
}

void HandleOnEditText(const char *str)
{
    NOT_REACHED();
}

/**
 * This code is shared for the majority of the pushbuttons.
 * Handles e.g. the pressing of a button (to build things), playing of click sound and sets certain parameters
 *
 * @param w Window which called the function
 * @param widget ID of the widget (=button) that called this function
 * @param cursor How should the cursor image change? E.g. cursor with depot image in it
 * @param mode Tile highlighting mode, e.g. drawing a rectangle or a dot on the ground
 * @return true if the button is clicked, false if it's unclicked
 */
bool HandlePlacePushButton(Window *w, int widget, CursorID cursor, HighLightStyle mode)
{
	if (w->IsWidgetDisabled(widget)) return false;

	if (_settings_client.sound.click_beep) SndPlayFx(SND_15_BEEP);
	w->SetDirty();

	if (w->IsWidgetLowered(widget) && mode == _thd.place_mode) {
		ResetObjectToPlace();
		return false;
	}

	SetObjectToPlace(cursor, PAL_NONE, mode, w->window_class, w->window_number);
	w->LowerWidget(widget);
	return true;
}


void CcPlaySound10(const CommandCost &result, TileIndex tile, uint32 p1, uint32 p2)
{
	if (result.Succeeded() && _settings_client.sound.confirm) SndPlayTileFx(SND_12_EXPLOSION, tile);
}

/**
 * Zooms a viewport in a window in or out.
 * @param how Zooming direction.
 * @param w   Window owning the viewport.
 * @return Returns \c true if zooming step could be done, \c false if further zooming is not possible.
 * @note No button handling or what so ever is done.
 */
bool DoZoomInOutWindow(ZoomStateChange how, Window *w)
{
	ViewPort *vp;

	assert(w != NULL);
	vp = w->viewport;

	switch (how) {
		case ZOOM_NONE:
			/* On initialisation of the viewport we don't do anything. */
			break;

		case ZOOM_IN:
			if (vp->zoom <= _settings_client.gui.zoom_min) return false;
			vp->zoom = (ZoomLevel)((int)vp->zoom - 1);
			vp->virtual_width >>= 1;
			vp->virtual_height >>= 1;

			w->viewport->scrollpos_x += vp->virtual_width >> 1;
			w->viewport->scrollpos_y += vp->virtual_height >> 1;
			w->viewport->dest_scrollpos_x = w->viewport->scrollpos_x;
			w->viewport->dest_scrollpos_y = w->viewport->scrollpos_y;
			w->viewport->follow_vehicle = INVALID_VEHICLE;
			break;
		case ZOOM_OUT:
			if (vp->zoom >= _settings_client.gui.zoom_max) return false;
			vp->zoom = (ZoomLevel)((int)vp->zoom + 1);

			w->viewport->scrollpos_x -= vp->virtual_width >> 1;
			w->viewport->scrollpos_y -= vp->virtual_height >> 1;
			w->viewport->dest_scrollpos_x = w->viewport->scrollpos_x;
			w->viewport->dest_scrollpos_y = w->viewport->scrollpos_y;

			vp->virtual_width <<= 1;
			vp->virtual_height <<= 1;
			w->viewport->follow_vehicle = INVALID_VEHICLE;
			break;
	}
	if (vp != NULL) { // the vp can be null when how == ZOOM_NONE
		vp->virtual_left = w->viewport->scrollpos_x;
		vp->virtual_top = w->viewport->scrollpos_y;
	}
	/* Update the windows that have zoom-buttons to perhaps disable their buttons */
	w->InvalidateData();
	return true;
}

void ZoomInOrOutToCursorWindow(bool in, Window *w)
{
	assert(w != NULL);

	if (_game_mode != GM_MENU) {
		ViewPort *vp = w->viewport;
		if ((in && vp->zoom <= _settings_client.gui.zoom_min) || (!in && vp->zoom >= _settings_client.gui.zoom_max)) return;

		Point pt = GetTileZoomCenterWindow(in, w);
		if (pt.x != -1) {
			ScrollWindowTo(pt.x, pt.y, -1, w, true);

			DoZoomInOutWindow(in ? ZOOM_IN : ZOOM_OUT, w);
		}
	}
}

static const struct NWidgetPart _nested_main_window_widgets[] = {
	NWidget(NWID_VIEWPORT, INVALID_COLOUR, WID_M_VIEWPORT), SetResize(1, 1),
};

static WindowDesc _main_window_desc(
	WDP_MANUAL, NULL, 0, 0,
	WC_MAIN_WINDOW, WC_NONE,
	0,
	_nested_main_window_widgets, lengthof(_nested_main_window_widgets)
);

enum {
	GHK_QUIT,
	GHK_ABANDON,
	GHK_CONSOLE,
	GHK_BOUNDING_BOXES,
	GHK_DIRTY_BLOCKS,
	GHK_CENTER,
	GHK_CENTER_ZOOM,
	GHK_RESET_OBJECT_TO_PLACE,
	GHK_DELETE_WINDOWS,
	GHK_DELETE_NONVITAL_WINDOWS,
	GHK_REFRESH_SCREEN,
	GHK_CRASH,
	GHK_MONEY,
	GHK_UPDATE_COORDS,
	GHK_TOGGLE_TRANSPARENCY,
	GHK_TOGGLE_INVISIBILITY = GHK_TOGGLE_TRANSPARENCY + 9,
	GHK_TRANSPARENCY_TOOLBAR = GHK_TOGGLE_INVISIBILITY + 8,
	GHK_TRANSPARANCY,
	GHK_CHAT,
	GHK_CHAT_ALL,
	GHK_CHAT_COMPANY,
	GHK_CHAT_SERVER,
};

struct MainWindow : Window
{
	MainWindow() : Window(&_main_window_desc)
	{
		this->InitNested(0);
		CLRBITS(this->flags, WF_WHITE_BORDER);
		ResizeWindow(this, _screen.width, _screen.height);

		NWidgetViewport *nvp = this->GetWidget<NWidgetViewport>(WID_M_VIEWPORT);
		nvp->InitializeViewport(this, TileXY(32, 32), ZOOM_LVL_VIEWPORT);
	}

	virtual void OnPaint()
	{
		this->DrawWidgets();
		if (_game_mode == GM_MENU) {
			static const SpriteID title_sprites[] = {SPR_OTTD_O, SPR_OTTD_P, SPR_OTTD_E, SPR_OTTD_N, SPR_OTTD_T, SPR_OTTD_T, SPR_OTTD_D};
			static const uint LETTER_SPACING = 10;
			int name_width = (lengthof(title_sprites) - 1) * LETTER_SPACING;

			for (uint i = 0; i < lengthof(title_sprites); i++) {
				name_width += GetSpriteSize(title_sprites[i]).width;
			}
			int off_x = (this->width - name_width) / 2;

			for (uint i = 0; i < lengthof(title_sprites); i++) {
				DrawSprite(title_sprites[i], PAL_NONE, off_x, 50);
				off_x += GetSpriteSize(title_sprites[i]).width + LETTER_SPACING;
			}
		}
	}

	virtual EventState OnKeyPress(WChar key, uint16 keycode)
	{
		int num = CheckHotkeyMatch(global_hotkeys, keycode, this);
		if (num == GHK_QUIT) {
			HandleExitGameRequest();
			return ES_HANDLED;
		}

		/* Disable all key shortcuts, except quit shortcuts when
		 * generating the world, otherwise they create threading
		 * problem during the generating, resulting in random
		 * assertions that are hard to trigger and debug */
		if (HasModalProgress()) return ES_NOT_HANDLED;

		switch (num) {
			case GHK_ABANDON:
				/* No point returning from the main menu to itself */
				if (_game_mode == GM_MENU) return ES_HANDLED;
				if (_settings_client.gui.autosave_on_exit) {
					DoExitSave();
					_switch_mode = SM_MENU;
				} else {
					AskExitToGameMenu();
				}
				return ES_HANDLED;

			case GHK_CONSOLE:
				IConsoleSwitch();
				return ES_HANDLED;

			case GHK_BOUNDING_BOXES:
				ToggleBoundingBoxes();
				return ES_HANDLED;

			case GHK_DIRTY_BLOCKS:
				ToggleDirtyBlocks();
				return ES_HANDLED;
		}

		if (_game_mode == GM_MENU) return ES_NOT_HANDLED;

		switch (num) {
			case GHK_CENTER:
			case GHK_CENTER_ZOOM: {
				Point pt = GetTileBelowCursor();
				if (pt.x != -1) {
					bool instant = (num == GHK_CENTER_ZOOM && this->viewport->zoom != _settings_client.gui.zoom_min);
					if (num == GHK_CENTER_ZOOM) MaxZoomInOut(ZOOM_IN, this);
					ScrollMainWindowTo(pt.x, pt.y, -1, instant);
				}
				break;
			}

			case GHK_RESET_OBJECT_TO_PLACE: ResetObjectToPlace(); break;
			case GHK_DELETE_WINDOWS: DeleteNonVitalWindows(); break;
			case GHK_DELETE_NONVITAL_WINDOWS: DeleteAllNonVitalWindows(); break;
			case GHK_REFRESH_SCREEN: MarkWholeScreenDirty(); break;

			case GHK_CRASH: // Crash the game
				*(volatile byte *)0 = 0;
				break;

			case GHK_MONEY: // Gimme money
				/* You can only cheat for money in single player. */
				if (!_networking) DoCommandP(0, 10000000, 0, CMD_MONEY_CHEAT);
				break;

			case GHK_UPDATE_COORDS: // Update the coordinates of all station signs
				UpdateAllVirtCoords();
				break;

			case GHK_TOGGLE_TRANSPARENCY:
			case GHK_TOGGLE_TRANSPARENCY + 1:
			case GHK_TOGGLE_TRANSPARENCY + 2:
			case GHK_TOGGLE_TRANSPARENCY + 3:
			case GHK_TOGGLE_TRANSPARENCY + 4:
			case GHK_TOGGLE_TRANSPARENCY + 5:
			case GHK_TOGGLE_TRANSPARENCY + 6:
			case GHK_TOGGLE_TRANSPARENCY + 7:
			case GHK_TOGGLE_TRANSPARENCY + 8:
				/* Transparency toggle hot keys */
				ToggleTransparency((TransparencyOption)(num - GHK_TOGGLE_TRANSPARENCY));
				MarkWholeScreenDirty();
				break;

			case GHK_TOGGLE_INVISIBILITY:
			case GHK_TOGGLE_INVISIBILITY + 1:
			case GHK_TOGGLE_INVISIBILITY + 2:
			case GHK_TOGGLE_INVISIBILITY + 3:
			case GHK_TOGGLE_INVISIBILITY + 4:
			case GHK_TOGGLE_INVISIBILITY + 5:
			case GHK_TOGGLE_INVISIBILITY + 6:
			case GHK_TOGGLE_INVISIBILITY + 7:
				/* Invisibility toggle hot keys */
				ToggleInvisibilityWithTransparency((TransparencyOption)(num - GHK_TOGGLE_INVISIBILITY));
				MarkWholeScreenDirty();
				break;

			case GHK_TRANSPARENCY_TOOLBAR:
				ShowTransparencyToolbar();
				break;

			case GHK_TRANSPARANCY:
				ResetRestoreAllTransparency();
				break;

#ifdef ENABLE_NETWORK
			case GHK_CHAT: // smart chat; send to team if any, otherwise to all
				if (_networking) {
					const NetworkClientInfo *cio = NetworkClientInfo::GetByClientID(_network_own_client_id);
					if (cio == NULL) break;

					ShowNetworkChatQueryWindow(NetworkClientPreferTeamChat(cio) ? DESTTYPE_TEAM : DESTTYPE_BROADCAST, cio->client_playas);
				}
				break;

			case GHK_CHAT_ALL: // send text message to all clients
				if (_networking) ShowNetworkChatQueryWindow(DESTTYPE_BROADCAST, 0);
				break;

			case GHK_CHAT_COMPANY: // send text to all team mates
				if (_networking) {
					const NetworkClientInfo *cio = NetworkClientInfo::GetByClientID(_network_own_client_id);
					if (cio == NULL) break;

					ShowNetworkChatQueryWindow(DESTTYPE_TEAM, cio->client_playas);
				}
				break;

			case GHK_CHAT_SERVER: // send text to the server
				if (_networking && !_network_server) {
					ShowNetworkChatQueryWindow(DESTTYPE_CLIENT, CLIENT_ID_SERVER);
				}
				break;
#endif

			default: return ES_NOT_HANDLED;
		}
		return ES_HANDLED;
	}

	virtual void OnScroll(Point delta)
	{
		this->viewport->scrollpos_x += ScaleByZoom(delta.x, this->viewport->zoom);
		this->viewport->scrollpos_y += ScaleByZoom(delta.y, this->viewport->zoom);
		this->viewport->dest_scrollpos_x = this->viewport->scrollpos_x;
		this->viewport->dest_scrollpos_y = this->viewport->scrollpos_y;
	}

	virtual void OnMouseWheel(int wheel)
	{
		if (_settings_client.gui.scrollwheel_scrolling == 0) {
			ZoomInOrOutToCursorWindow(wheel < 0, this);
		}
	}

	virtual void OnResize()
	{
		if (this->viewport != NULL) {
			NWidgetViewport *nvp = this->GetWidget<NWidgetViewport>(WID_M_VIEWPORT);
			nvp->UpdateViewportCoordinates(this);
		}
	}

	/**
	 * Some data on this window has become invalid.
	 * @param data Information about the changed data.
	 * @param gui_scope Whether the call is done from GUI scope. You may not do everything when not in GUI scope. See #InvalidateWindowData() for details.
	 */
	virtual void OnInvalidateData(int data = 0, bool gui_scope = true)
	{
		if (!gui_scope) return;
		/* Forward the message to the appropriate toolbar (ingame or scenario editor) */
		InvalidateWindowData(WC_MAIN_TOOLBAR, 0, data, true);
	}
       virtual void OnMouseOver(Point pt, int widget)
       {
               /* Show tooltip with last month production or town name */
               if (pt.x != -1 && _game_mode != GM_MENU) {
                       TileIndex tile;
                       const bool viewport_is_in_map_mode = (this->viewport->zoom >= ZOOM_LVL_MAX);
                       if (viewport_is_in_map_mode) {
                               const int a = ((ScaleByZoom(pt.x, this->viewport->zoom) + this->viewport->virtual_left) >> 2) / ZOOM_LVL_BASE;
                               const int b = ((ScaleByZoom(pt.y, this->viewport->zoom) + this->viewport->virtual_top) >> 1) / ZOOM_LVL_BASE;
                               tile = TileVirtXY(b - a, b + a);
                       } else {
                               const Point p = GetTileBelowCursor();
                               tile = TileVirtXY(p.x, p.y);
                       }
                       if (tile >= MapSize()) return;

                       switch (GetTileType(tile)) {
                               case MP_ROAD:
                                       if (IsRoadDepot(tile)) return;
                                       /* FALL THROUGH */
                               case MP_HOUSE: {
                                       if (HasBit(_display_opt, DO_SHOW_TOWN_NAMES)) return; // No need for a town name tooltip when it is already displayed
                                       if (!viewport_is_in_map_mode) return;
                                       const TownID tid = GetTownIndex(tile);
                                       if (!tid) return;
                                       SetDParam(0, tid);
                                       GuiShowTooltips(this, STR_TOWN_NAME_TOOLTIP, 0, NULL, TCC_HOVER);
                                       break;
                               }
                               case MP_INDUSTRY: {
                                       const Industry *ind = Industry::GetByTile(tile);
                                       const IndustrySpec *indsp = GetIndustrySpec(ind->type);

                                       StringID str = STR_INDUSTRY_VIEW_TRANSPORTED_TOOLTIP;
                                       uint prm_count = 0;
                                       SetDParam(prm_count++, indsp->name);
                                       for (byte i = 0; i < lengthof(ind->produced_cargo); i++) {
                                               if (ind->produced_cargo[i] != CT_INVALID) {
                                                       SetDParam(prm_count++, ind->produced_cargo[i]);
                                                       SetDParam(prm_count++, ind->last_month_production[i]);
                                                       SetDParam(prm_count++, ToPercent8(ind->last_month_pct_transported[i]));
                                                       str++;
                                               }
                                       }
                                       GuiShowTooltips(this, str, 0, NULL, TCC_HOVER);
                                       break;
                               }
                               default:
                                       return;
                       }
               }
       }

	static Hotkey<MainWindow> global_hotkeys[];
};

const uint16 _ghk_quit_keys[] = {'Q' | WKC_CTRL, 'Q' | WKC_META, 0};
const uint16 _ghk_abandon_keys[] = {'W' | WKC_CTRL, 'W' | WKC_META, 0};
const uint16 _ghk_chat_keys[] = {WKC_RETURN, 'T', 0};
const uint16 _ghk_chat_all_keys[] = {WKC_SHIFT | WKC_RETURN, WKC_SHIFT | 'T', 0};
const uint16 _ghk_chat_company_keys[] = {WKC_CTRL | WKC_RETURN, WKC_CTRL | 'T', 0};
const uint16 _ghk_chat_server_keys[] = {WKC_CTRL | WKC_SHIFT | WKC_RETURN, WKC_CTRL | WKC_SHIFT | 'T', 0};

Hotkey<MainWindow> MainWindow::global_hotkeys[] = {
	Hotkey<MainWindow>(_ghk_quit_keys, "quit", GHK_QUIT),
	Hotkey<MainWindow>(_ghk_abandon_keys, "abandon", GHK_ABANDON),
	Hotkey<MainWindow>(WKC_BACKQUOTE, "console", GHK_CONSOLE),
	Hotkey<MainWindow>('B' | WKC_CTRL, "bounding_boxes", GHK_BOUNDING_BOXES),
	Hotkey<MainWindow>('I' | WKC_CTRL, "dirty_blocks", GHK_DIRTY_BLOCKS),
	Hotkey<MainWindow>('C', "center", GHK_CENTER),
	Hotkey<MainWindow>('Z', "center_zoom", GHK_CENTER_ZOOM),
	Hotkey<MainWindow>(WKC_ESC, "reset_object_to_place", GHK_RESET_OBJECT_TO_PLACE),
	Hotkey<MainWindow>(WKC_DELETE, "delete_windows", GHK_DELETE_WINDOWS),
	Hotkey<MainWindow>(WKC_DELETE | WKC_SHIFT, "delete_all_windows", GHK_DELETE_NONVITAL_WINDOWS),
	Hotkey<MainWindow>('R' | WKC_CTRL, "refresh_screen", GHK_REFRESH_SCREEN),
#if defined(_DEBUG)
	Hotkey<MainWindow>('0' | WKC_ALT, "crash_game", GHK_CRASH),
	Hotkey<MainWindow>('1' | WKC_ALT, "money", GHK_MONEY),
	Hotkey<MainWindow>('2' | WKC_ALT, "update_coordinates", GHK_UPDATE_COORDS),
#endif
	Hotkey<MainWindow>('1' | WKC_CTRL, "transparency_signs", GHK_TOGGLE_TRANSPARENCY),
	Hotkey<MainWindow>('2' | WKC_CTRL, "transparency_trees", GHK_TOGGLE_TRANSPARENCY + 1),
	Hotkey<MainWindow>('3' | WKC_CTRL, "transparency_houses", GHK_TOGGLE_TRANSPARENCY + 2),
	Hotkey<MainWindow>('4' | WKC_CTRL, "transparency_industries", GHK_TOGGLE_TRANSPARENCY + 3),
	Hotkey<MainWindow>('5' | WKC_CTRL, "transparency_buildings", GHK_TOGGLE_TRANSPARENCY + 4),
	Hotkey<MainWindow>('6' | WKC_CTRL, "transparency_bridges", GHK_TOGGLE_TRANSPARENCY + 5),
	Hotkey<MainWindow>('7' | WKC_CTRL, "transparency_structures", GHK_TOGGLE_TRANSPARENCY + 6),
	Hotkey<MainWindow>('8' | WKC_CTRL, "transparency_catenary", GHK_TOGGLE_TRANSPARENCY + 7),
	Hotkey<MainWindow>('9' | WKC_CTRL, "transparency_loading", GHK_TOGGLE_TRANSPARENCY + 8),
	Hotkey<MainWindow>('1' | WKC_CTRL | WKC_SHIFT, "invisibility_signs", GHK_TOGGLE_INVISIBILITY),
	Hotkey<MainWindow>('2' | WKC_CTRL | WKC_SHIFT, "invisibility_trees", GHK_TOGGLE_INVISIBILITY + 1),
	Hotkey<MainWindow>('3' | WKC_CTRL | WKC_SHIFT, "invisibility_houses", GHK_TOGGLE_INVISIBILITY + 2),
	Hotkey<MainWindow>('4' | WKC_CTRL | WKC_SHIFT, "invisibility_industries", GHK_TOGGLE_INVISIBILITY + 3),
	Hotkey<MainWindow>('5' | WKC_CTRL | WKC_SHIFT, "invisibility_buildings", GHK_TOGGLE_INVISIBILITY + 4),
	Hotkey<MainWindow>('6' | WKC_CTRL | WKC_SHIFT, "invisibility_bridges", GHK_TOGGLE_INVISIBILITY + 5),
	Hotkey<MainWindow>('7' | WKC_CTRL | WKC_SHIFT, "invisibility_structures", GHK_TOGGLE_INVISIBILITY + 6),
	Hotkey<MainWindow>('8' | WKC_CTRL | WKC_SHIFT, "invisibility_catenary", GHK_TOGGLE_INVISIBILITY + 7),
	Hotkey<MainWindow>('X' | WKC_CTRL, "transparency_toolbar", GHK_TRANSPARENCY_TOOLBAR),
	Hotkey<MainWindow>('X', "toggle_transparency", GHK_TRANSPARANCY),
#ifdef ENABLE_NETWORK
	Hotkey<MainWindow>(_ghk_chat_keys, "chat", GHK_CHAT),
	Hotkey<MainWindow>(_ghk_chat_all_keys, "chat_all", GHK_CHAT_ALL),
	Hotkey<MainWindow>(_ghk_chat_company_keys, "chat_company", GHK_CHAT_COMPANY),
	Hotkey<MainWindow>(_ghk_chat_server_keys, "chat_server", GHK_CHAT_SERVER),
#endif
	HOTKEY_LIST_END(MainWindow)
};
Hotkey<MainWindow> *_global_hotkeys = MainWindow::global_hotkeys;

/**
 * Does the given keycode match one of the keycodes bound to 'quit game'?
 * @param keycode The keycode that was pressed by the user.
 * @return True iff the keycode matches one of the hotkeys for 'quit'.
 */
bool IsQuitKey(uint16 keycode)
{
	int num = CheckHotkeyMatch<MainWindow>(_global_hotkeys, keycode, NULL);
	return num == GHK_QUIT;
}


void ShowSelectGameWindow();

/**
 * Initialise the default colours (remaps and the likes), and load the main windows.
 */
void SetupColoursAndInitialWindow()
{
	for (uint i = 0; i != 16; i++) {
		const byte *b = GetNonSprite(PALETTE_RECOLOUR_START + i, ST_RECOLOUR);

		assert(b);
		memcpy(_colour_gradient[i], b + 0xC6, sizeof(_colour_gradient[i]));
	}

	new MainWindow;

	/* XXX: these are not done */
	switch (_game_mode) {
		default: NOT_REACHED();
		case GM_MENU:
			ShowSelectGameWindow();
			break;

		case GM_NORMAL:
		case GM_EDITOR:
			ShowVitalWindows();
			break;
	}
}

/**
 * Show the vital in-game windows.
 */
void ShowVitalWindows()
{
	AllocateToolbar();

	/* Status bad only for normal games */
	if (_game_mode == GM_EDITOR) return;

	ShowStatusBar();
}

/**
 * Size of the application screen changed.
 * Adapt the game screen-size, re-allocate the open windows, and repaint everything
 */
void GameSizeChanged()
{
	_cur_resolution.width  = _screen.width;
	_cur_resolution.height = _screen.height;
	ScreenSizeChanged();
	RelocateAllWindows(_screen.width, _screen.height);
	MarkWholeScreenDirty();
}
