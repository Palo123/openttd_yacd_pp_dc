/* $Id$ */

/*
 * This file is part of OpenTTD.
 * OpenTTD is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, version 2.
 * OpenTTD is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with OpenTTD. If not, see <http://www.gnu.org/licenses/>.
 */

/** @file goal_gui.cpp GUI for goals. */

#include "stdafx.h"
#include "window_gui.h"
#include "strings_func.h"
#include "date_func.h"
#include "gui.h"
#include "story_base.h"
#include "core/geometry_func.hpp"
#include "company_func.h"
#include "command_func.h"
#include "widgets/dropdown_type.h"
#include "widgets/dropdown_func.h"
#include "sortlist_type.h"
#include "goal_base.h"
#include "viewport_func.h"

#include "widgets/story_widget.h"

#include "table/strings.h"
#include "table/sprites.h"

typedef GUIList<const StoryPage*> GUIStoryPageList;
typedef GUIList<const StoryPageElement*> GUIStoryPageElementList;

struct StoryBookWindow : Window {
protected:
	Scrollbar *vscroll;                ///< Scrollbar of the page text.

	GUIStoryPageList story_pages;      ///< Sorted list of pages.
	GUIStoryPageElementList story_page_elements; ///< Sorted list of page elements that belong to the current page.
	StoryPageID selected_page_id;      ///< Pool index of selected page.
	char selected_generic_title[255];  ///< If the selected page doesn't have a custom title, this buffer is used to store a generic page title.

	static GUIStoryPageList::SortFunction * const page_sorter_funcs[];
	static GUIStoryPageElementList::SortFunction * const page_element_sorter_funcs[];

	/** (Re)Build story page list. */
	void BuildStoryPageList()
	{
		if (this->story_pages.NeedRebuild()) {
			this->story_pages.Clear();

			const StoryPage *p;
			FOR_ALL_STORY_PAGES(p) {
				if (this->IsPageAvailable(p)) {
					*this->story_pages.Append() = p;
				}
			}

			this->story_pages.Compact();
			this->story_pages.RebuildDone();
		}

		this->story_pages.Sort();
	}

	/** Sort story pages by order value. */
	static int CDECL PageOrderSorter(const StoryPage * const *a, const StoryPage * const *b)
	{
		return (*a)->sort_value - (*b)->sort_value;
	}

	/** (Re)Build story page element list. */
	void BuildStoryPageElementList()
	{
		if (this->story_page_elements.NeedRebuild()) {
			this->story_page_elements.Clear();

			const StoryPage *p = GetSelPage();
			if (p != NULL) {
				const StoryPageElement *pe;
				FOR_ALL_STORY_PAGE_ELEMENTS(pe) {
					if (pe->page == p->index) {
						*this->story_page_elements.Append() = pe;
					}
				}
			}

			this->story_page_elements.Compact();
			this->story_page_elements.RebuildDone();
		}

		this->story_page_elements.Sort();
	}

	/** Sort story page elements by order value. */
	static int CDECL PageElementOrderSorter(const StoryPageElement * const *a, const StoryPageElement * const *b)
	{
		return (*a)->sort_value - (*b)->sort_value;
	}

	/*
	 * Checks if a given page should be visible in the story book.
	 * @param page The page to check.
	 * @return True if the page should be visible, otherwise false.
	 */
	bool IsPageAvailable(const StoryPage *page) const
	{
		return page->company == INVALID_COMPANY || page->company == _local_company;
	}

	/**
	 * Get instance of selected page.
	 * @return Instance of selected page or NULL if no page is selected.
	 */
	StoryPage *GetSelPage() const
	{
		if (!_story_page_pool.IsValidID(selected_page_id)) return NULL;
		return _story_page_pool.Get(selected_page_id);
	}

	/**
	 * Get the page number of selected page.
	 * @return Number of available pages before to the selected one, or -1 if no page is selected.
	 */
	int GetSelPageNum() const
	{
		int page_number = 0;
		for (const StoryPage *const*iter = this->story_pages.Begin(); iter != this->story_pages.End(); iter++) {
			const StoryPage *p = *iter;
			if (p->index == this->selected_page_id) {
				return page_number;
			}
			page_number++;
		}
		return -1;
	}

	/**
	 * Check if the selected page is also the first available page.
	 */
	bool IsFirstPageSelected()
	{
		/* Verify that the selected page exist. */
		if (!_story_page_pool.IsValidID(this->selected_page_id)) return false;

		return (*this->story_pages.Begin())->index == this->selected_page_id;
	}

	/**
	 * Check if the selected page is also the last available page.
	 */
	bool IsLastPageSelected()
	{
		/* Verify that the selected page exist. */
		if (!_story_page_pool.IsValidID(this->selected_page_id)) return false;

		if (this->story_pages.Length() <= 1) return true;
		const StoryPage *last = *(this->story_pages.End() - 1);
		return last->index == this->selected_page_id;
	}

	/**
	 * Updates the content of selected page.
	 */
	void RefreshSelectedPage()
	{
		/* Generate generic title if selected page have no custom title. */
		StoryPage *page = this->GetSelPage();
		if (page != NULL && page->title == NULL) {
			SetDParam(0, GetSelPageNum() + 1);
			GetString(selected_generic_title, STR_STORY_BOOK_GENERIC_PAGE_ITEM, lastof(selected_generic_title));
		}

		this->story_page_elements.ForceRebuild();
		this->BuildStoryPageElementList();

		this->vscroll->SetCount(this->CountLines());
		this->SetWidgetDirty(WID_SB_SCROLLBAR);
		this->SetWidgetDirty(WID_SB_SEL_PAGE);
		this->SetWidgetDirty(WID_SB_PAGE_PANEL);
	}

	/**
	 * Selects the previous available page before the currently selected page.
	 */
	void SelectPrevPage()
	{
		if (!_story_page_pool.IsValidID(this->selected_page_id)) return;

		/* Find the last available page which is previous to the current selected page. */
		const StoryPage *last_available;
		last_available = NULL;
		for (const StoryPage *const*iter = this->story_pages.Begin(); iter != this->story_pages.End(); iter++) {
			const StoryPage *p = *iter;
			if (p->index == this->selected_page_id) {
				if (last_available == NULL) return; // No previous page available.
				this->SetSelectedPage(last_available->index);
				return;
			}
			last_available = p;
		}
	}

	/**
	 * Selects the next available page after the currently selected page.
	 */
	void SelectNextPage()
	{
		if (!_story_page_pool.IsValidID(this->selected_page_id)) return;

		/* Find selected page. */
		for (const StoryPage *const*iter = this->story_pages.Begin(); iter != this->story_pages.End(); iter++) {
			const StoryPage *p = *iter;
			if (p->index == this->selected_page_id) {
				/* Select the page after selected page. */
				iter++;
				if (iter != this->story_pages.End()) {
					this->SetSelectedPage((*iter)->index);
				}
				return;
			}
		}
	}

	/**
	 * Builds the page selector drop down list.
	 */
	DropDownList *BuildDropDownList() const
	{
		DropDownList *list = new DropDownList();
		uint16 page_num = 1;
		for (const StoryPage *const*iter = this->story_pages.Begin(); iter != this->story_pages.End(); iter++) {
			const StoryPage *p = *iter;
			char *title = p->title;
			bool current_page = p->index == this->selected_page_id;
			DropDownListStringItem *item = NULL;
			if (p->title != NULL) {
				item = new DropDownListCharStringItem(p->title, p->index, current_page);
			} else {
				/* No custom title => use a generic page title with page number. */
				DropDownListParamStringItem *str_item =
						new DropDownListParamStringItem(STR_STORY_BOOK_GENERIC_PAGE_ITEM, p->index, current_page);
				str_item->SetParam(0, page_num);
				item = str_item;
			}

			list->push_back(item);
			page_num++;
		}

		/* Check if list is empty. */
		if (list->size() == 0) {
			free(list);
			list = NULL;
		}

		return list;
	}

	/**
	 * Get the width available for displaying content on the page panel.
	 */
	uint GetAvailablePageContentWidth()
	{
		return this->GetWidget<NWidgetCore>(WID_SB_PAGE_PANEL)->current_x - WD_FRAMETEXT_LEFT - WD_FRAMERECT_RIGHT;
	}

	/**
	 * Counts how many lines that are used by Date and Title
	 * (excluding marginal after Title, as each body element has
	 * an empty row before the elment).
	 * @param max_width Available width to display content.
	 * @return the number of lines.
	 */
	uint CountHeadLines(int max_width)
	{
		StoryPage *page = this->GetSelPage();
		if (page == NULL) return 0;
		int num_lines = 0;

		/* Title lines */
		num_lines += 1; // Date always use exactly one line.
		SetDParamStr(0, page->title != NULL ? page->title : this->selected_generic_title);
		num_lines += GetStringLineCount(STR_STORY_BOOK_TITLE, max_width);

		return num_lines;
	}

	/**
	 * Decides which sprite to display for a given page element.
	 * @param pe The page element.
	 * @return The SpriteID of the sprite to display.
	 * @pre pe.type must be SPET_GOAL or SPET_LOCATION.
	 */
	SpriteID GetPageElementSprite(const StoryPageElement &pe) const
	{
		switch (pe.type) {
			case SPET_GOAL: {
				Goal *g = Goal::Get((GoalID) pe.referenced_id);
				if (g == NULL) return SPR_IMG_GOAL_BROKEN_REF;
				return g->completed ? SPR_IMG_GOAL_COMPLETED : SPR_IMG_GOAL;
			}
			case SPET_LOCATION:
				return SPR_IMG_VIEW_LOCATION;
			default:
				NOT_REACHED();
		}
	}

	/**
	 * Count the number of lines used by a given page element.
	 * @param pe The story page element.
	 * @param max_width Available width to display content.
	 * @return the number of lines.
	 */
	uint CountPageElementLines(const StoryPageElement &pe, int max_width)
	{
		switch (pe.type) {
			case SPET_TEXT:
				SetDParamStr(0, pe.text);
				return GetStringLineCount(STR_BLACK_RAW_STRING, max_width);
				break;

			case SPET_GOAL:
			case SPET_LOCATION: {
				Dimension sprite_dim = GetSpriteSize(GetPageElementSprite(pe));
				int line_height = GetStringHeight(STR_JUST_NOTHING, INT_MAX);
				if (line_height == 0) return 1;
				return max((uint)1, sprite_dim.height / (uint)line_height);
				break;
			}
			default:
				NOT_REACHED();
		}
	}

	/**
	 * Count the number of lines in this window.
	 * @return the number of lines.
	 */
	uint CountLines()
	{
		StoryPage *page = this->GetSelPage();
		if (page == NULL) return 0;
		int max_width = GetAvailablePageContentWidth();

		/* Head lines */
		int num_lines = CountHeadLines(max_width);

		/* Body lines */
		for (const StoryPageElement **iter = this->story_page_elements.Begin(); iter != this->story_page_elements.End(); iter++) {
			const StoryPageElement *pe = *iter;
			num_lines += 1; // For the space between previous element and current element.

			num_lines += CountPageElementLines(*pe, max_width);
		}

		return num_lines;
	}

	/**
	 * Draws a page element that is composed of a sprite to the left and a single line of
	 * text after that. These page elements are generally clickable and are thus called
	 * action elements.
	 * @param y_offset Current y_offset which will get updated when this method has completed its drawing.
	 * @param width Width of the region available for drawing.
	 * @param line_height Height of one line of text.
	 * @param action_sprite The sprite to draw.
	 * @return the number of lines.
	 */
	void DrawActionElement(int &y_offset, int width, int line_height, SpriteID action_sprite) const
	{
		Dimension sprite_dim = GetSpriteSize(action_sprite);
		uint element_height = max((uint)1, sprite_dim.height / (uint)line_height) * line_height;

		uint sprite_top = y_offset + (element_height - sprite_dim.height) / 2;
		uint text_top = y_offset + (element_height - line_height) / 2;

		DrawSprite(action_sprite, PAL_NONE, 0, y_offset);
		DrawString(sprite_dim.width + WD_FRAMETEXT_LEFT, width, text_top, STR_JUST_RAW_STRING, TC_BLACK);

		y_offset += element_height;
	}

	/**
	 * Internal event handler for when a page element is clicked.
	 * @param pe The clicked page element.
	 */
	void OnPageElementClick(const StoryPageElement& pe)
	{
		switch (pe.type) {
			case SPET_TEXT:
				/* Do nothing. */
				break;

			case SPET_LOCATION:
				if (_ctrl_pressed) {
					ShowExtraViewPortWindow((TileIndex)pe.referenced_id);
				} else {
					ScrollMainWindowToTile((TileIndex)pe.referenced_id);
				}
				break;

			case SPET_GOAL:
				ShowGoalsList();
				break;

			default:
				NOT_REACHED();
		}
	}

public:
	StoryBookWindow(WindowDesc *desc, WindowNumber window_number) : Window(desc)
	{
		this->CreateNestedTree();
		this->vscroll = this->GetScrollbar(WID_SB_SCROLLBAR);

		/* Initalize page sort. */
		this->story_pages.SetSortFuncs(StoryBookWindow::page_sorter_funcs);
		this->story_pages.ForceRebuild();
		this->BuildStoryPageList();
		this->story_page_elements.SetSortFuncs(StoryBookWindow::page_element_sorter_funcs);
		/* story_page_elements will get built by SetSelectedPage */

		this->FinishInitNested(window_number);

		/* Initialize selected vars. */
		this->selected_generic_title[0] = '\0';
		this->selected_page_id = INVALID_STORY_PAGE;

		this->OnInvalidateData(-1);
	}

	/**
	 * Updates the disabled state of the prev/next buttons.
	 */
	void UpdatePrevNextDisabledState()
	{
		this->SetWidgetDisabledState(WID_SB_PREV_PAGE, story_pages.Length() == 0 || this->IsFirstPageSelected());
		this->SetWidgetDisabledState(WID_SB_NEXT_PAGE, story_pages.Length() == 0 || this->IsLastPageSelected());
		this->SetWidgetDirty(WID_SB_PREV_PAGE);
		this->SetWidgetDirty(WID_SB_NEXT_PAGE);
	}

	/**
	 * Sets the selected page.
	 * @param page_index pool index of the page to select.
	 */
	void SetSelectedPage(uint16 page_index)
	{
		if (this->selected_page_id != page_index) {
			this->selected_page_id = page_index;
			this->RefreshSelectedPage();
			this->UpdatePrevNextDisabledState();
		}
	}

	virtual void SetStringParameters(int widget) const
	{
		if (widget != WID_SB_SEL_PAGE) return;

		StoryPage *page = this->GetSelPage();
		SetDParamStr(0, page != NULL && page->title != NULL ? page->title : this->selected_generic_title);
	}

	virtual void DrawWidget(const Rect &r, int widget) const
	{
		if (widget != WID_SB_PAGE_PANEL) return;

		StoryPage *page = this->GetSelPage();
		if (page == NULL) return;

		const int x = r.left + WD_FRAMETEXT_LEFT;
		const int y = r.top + WD_FRAMETEXT_TOP;
		const int right = r.right - WD_FRAMETEXT_RIGHT;
		const int bottom = r.bottom - WD_FRAMETEXT_BOTTOM;

		/* Set up a clipping region for the panel. */
		DrawPixelInfo tmp_dpi;
		if (!FillDrawPixelInfo(&tmp_dpi, x, y, right - x + 1, r.bottom - y + 1)) return;

		DrawPixelInfo *old_dpi = _cur_dpi;
		_cur_dpi = &tmp_dpi;

		/* Draw content (now coordinates given to Draw** are local to the new clipping region). */
		int line_height = GetStringHeight(STR_JUST_NOTHING, INT_MAX);
		int y_offset = - this->vscroll->GetPosition() * line_height;

		/* Date */
		SetDParam(0, page->date);
		DrawString(0, right - x, y_offset, STR_JUST_DATE_LONG, TC_BLACK);
		y_offset += line_height;

		/* Title */
		SetDParamStr(0, page->title != NULL ? page->title : this->selected_generic_title);
		y_offset = DrawStringMultiLine(0, right - x, y_offset, bottom - y, STR_STORY_BOOK_TITLE, TC_BLACK, SA_TOP | SA_HOR_CENTER);

		/* Page elements */
		for (const StoryPageElement *const*iter = this->story_page_elements.Begin(); iter != this->story_page_elements.End(); iter++) {
			const StoryPageElement *const pe = *iter;
			y_offset += line_height; // margin to previous element

			switch (pe->type) {
				case SPET_TEXT:
					SetDParamStr(0, pe->text);
					y_offset = DrawStringMultiLine(0, right - x, y_offset, bottom - y, STR_JUST_RAW_STRING, TC_BLACK, SA_TOP | SA_LEFT);
					break;

				case SPET_GOAL: {
					Goal *g = Goal::Get((GoalID) pe->referenced_id);
					if (g != NULL) {
						SetDParamStr(0, g->text);
						DrawActionElement(y_offset, right - x, line_height, GetPageElementSprite(*pe));
					} else {
						y_offset += line_height;
					}
					break;
				}

				case SPET_LOCATION:
					SetDParamStr(0, pe->text);
					DrawActionElement(y_offset, right - x, line_height, GetPageElementSprite(*pe));
					break;
			}
		}

		/* Restore clipping region. */
		_cur_dpi = old_dpi;
	}

	virtual void UpdateWidgetSize(int widget, Dimension *size, const Dimension &padding, Dimension *fill, Dimension *resize)
	{
		if (widget != WID_SB_SEL_PAGE && widget != WID_SB_PAGE_PANEL) return;

		Dimension d = GetStringBoundingBox(STR_JUST_NOTHING);
		d.width = 0;

		switch(widget) {
			case WID_SB_SEL_PAGE: {

				/* Get max title width. */
				for (uint16 i = 0; i < this->story_pages.Length(); i++) {
					const StoryPage *s = this->story_pages[i];

					if (s->title != NULL) {
						SetDParamStr(0, s->title);
					} else {
						SetDParamStr(0, this->selected_generic_title);
					}
					Dimension title_d = GetStringBoundingBox(STR_BLACK_RAW_STRING);

					if (title_d.width > d.width) {
						d.width = title_d.width;
					}
				}

				d.width += padding.width + WD_DROPDOWNTEXT_LEFT + WD_DROPDOWNTEXT_RIGHT;
				d.height += padding.height + WD_DROPDOWNTEXT_TOP + WD_DROPDOWNTEXT_BOTTOM;
				*size = maxdim(*size, d);
				break;
			}

			case WID_SB_PAGE_PANEL: {
				resize->height = d.height;

				d.height *= 5;
				d.height += padding.height + WD_FRAMETEXT_TOP + WD_FRAMETEXT_BOTTOM;
				*size = maxdim(*size, d);
				break;
			}
		}

	}

	virtual void OnResize()
	{
		this->vscroll->SetCapacityFromWidget(this, WID_SB_PAGE_PANEL, WD_FRAMETEXT_TOP + WD_FRAMETEXT_BOTTOM);
		this->vscroll->SetCount(this->CountLines());
	}

	virtual void OnClick(Point pt, int widget, int click_count)
	{
		switch (widget) {
			case WID_SB_SEL_PAGE: {
				DropDownList *list = this->BuildDropDownList();
				if (list != NULL) {
					/* Get the index of selected page. */
					int selected = 0;
					for (uint16 i = 0; i < this->story_pages.Length(); i++) {
						const StoryPage *p = this->story_pages[i];
						if (p->index == this->selected_page_id) break;
						selected++;
					}

					ShowDropDownList(this, list, selected, widget);
				}
				break;
			}

			case WID_SB_PREV_PAGE:
				this->SelectPrevPage();
				break;

			case WID_SB_NEXT_PAGE:
				this->SelectNextPage();
				break;

			case WID_SB_PAGE_PANEL: {
				uint clicked_row = this->vscroll->GetScrolledRowFromWidget(pt.y, this, WID_SB_PAGE_PANEL, WD_FRAMETEXT_TOP);
				uint max_width = GetAvailablePageContentWidth();

				/* Skip head rows. */
				uint n_head_rows = this->CountHeadLines(max_width);
				if (clicked_row < n_head_rows) return;

				/* Detect if a page element was clicked. */
				uint row = n_head_rows;
				for (const StoryPageElement *const*iter = this->story_page_elements.Begin(); iter != this->story_page_elements.End(); iter++) {
					const StoryPageElement *const pe = *iter;

					row += 1; // margin row

					uint content_rows = CountPageElementLines(*pe, max_width);
					if (clicked_row >= row && clicked_row < row + content_rows) {
						this->OnPageElementClick(*pe);
						return;
					}

					row += content_rows;
				}
			}
		}
	}

	virtual void OnDropdownSelect(int widget, int index)
	{
		if (widget != WID_SB_SEL_PAGE) return;

		/* index (which is set in BuildDropDownList) is the page id. */
		this->SetSelectedPage(index);
	}

	/**
	 * Some data on this window has become invalid.
	 * @param data Information about the changed data.
	 *   -1     Rebuild page list and refresh current page;
	 *   >= 0   Id of the page that needs to be refreshed. If it is not the current page, nothing happens.
	 * @param gui_scope Whether the call is done from GUI scope. You may not do everything when not in GUI scope. See #InvalidateWindowData() for details.
	 */
	virtual void OnInvalidateData(int data = 0, bool gui_scope = true)
	{
		if (!gui_scope) return;

		/* If added/removed page, force rebuild. Sort order never change so just a
		 * re-sort is never needed.
		 */
		if (data == -1) {
			this->story_pages.ForceRebuild();
			this->BuildStoryPageList();

			/* Was the last page removed? */
			if (this->story_pages.Length() == 0) {
				this->selected_generic_title[0] = '\0';
			}

			/* Verify page selection. */
			if (!_story_page_pool.IsValidID(this->selected_page_id)) {
				this->selected_page_id = INVALID_STORY_PAGE;
			}
			if (this->selected_page_id == INVALID_STORY_PAGE && this->story_pages.Length() > 0) {
				/* No page is selected, but there exist at least one available.
				 * => Select first page.
				 */
				this->SetSelectedPage(this->story_pages[0]->index);
			}

			this->SetWidgetDisabledState(WID_SB_SEL_PAGE, this->story_pages.Length() == 0);
			this->SetWidgetDirty(WID_SB_SEL_PAGE);
			this->UpdatePrevNextDisabledState();
		} else if (data >= 0 && this->selected_page_id == data) {
			this->RefreshSelectedPage();
		}
	}
};

GUIStoryPageList::SortFunction * const StoryBookWindow::page_sorter_funcs[] = {
	&PageOrderSorter,
};

GUIStoryPageElementList::SortFunction * const StoryBookWindow::page_element_sorter_funcs[] = {
	&PageElementOrderSorter,
};

static const NWidgetPart _nested_story_book_widgets[] = {
	NWidget(NWID_HORIZONTAL),
		NWidget(WWT_CLOSEBOX, COLOUR_BROWN),
		NWidget(WWT_CAPTION, COLOUR_BROWN, WID_SB_CAPTION), SetDataTip(STR_STORY_BOOK_CAPTION, STR_TOOLTIP_WINDOW_TITLE_DRAG_THIS),
		NWidget(WWT_SHADEBOX, COLOUR_BROWN),
		NWidget(WWT_DEFSIZEBOX, COLOUR_BROWN),
		NWidget(WWT_STICKYBOX, COLOUR_BROWN),
	EndContainer(),
	NWidget(NWID_HORIZONTAL), SetFill(1, 1),
		NWidget(NWID_VERTICAL), SetFill(1, 1),
			NWidget(WWT_PANEL, COLOUR_BROWN, WID_SB_PAGE_PANEL), SetResize(1, 1), SetScrollbar(WID_SB_SCROLLBAR), EndContainer(),
			NWidget(NWID_HORIZONTAL),
				NWidget(WWT_TEXTBTN, COLOUR_BROWN, WID_SB_PREV_PAGE), SetMinimalSize(100, 0), SetFill(0, 0), SetDataTip(STR_STORY_BOOK_PREV_PAGE, STR_STORY_BOOK_PREV_PAGE_TOOLTIP),
				NWidget(NWID_BUTTON_DROPDOWN, COLOUR_BROWN, WID_SB_SEL_PAGE), SetMinimalSize(93, 12), SetFill(1, 0),
														SetDataTip(STR_BLACK_RAW_STRING, STR_STORY_BOOK_SEL_PAGE_TOOLTIP), SetResize(1, 0),
				NWidget(WWT_TEXTBTN, COLOUR_BROWN, WID_SB_NEXT_PAGE), SetMinimalSize(100, 0), SetFill(0, 0), SetDataTip(STR_STORY_BOOK_NEXT_PAGE, STR_STORY_BOOK_NEXT_PAGE_TOOLTIP),
			EndContainer(),
		EndContainer(),
		NWidget(NWID_VERTICAL), SetFill(0, 1),
			NWidget(NWID_VSCROLLBAR, COLOUR_BROWN, WID_SB_SCROLLBAR),
			NWidget(WWT_RESIZEBOX, COLOUR_BROWN),
		EndContainer(),
	EndContainer(),
};

static WindowDesc _story_book_desc(
	WDP_CENTER, "view_story", 400, 300,
	WC_STORY_BOOK, WC_NONE,
	0,
	_nested_story_book_widgets, lengthof(_nested_story_book_widgets)
);

void ShowStoryBook()
{
	AllocateWindowDescFront<StoryBookWindow>(&_story_book_desc, 0);
}
