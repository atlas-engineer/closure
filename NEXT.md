# Closure renderer

Goal: a modern web renderer that can display any page. Parsing, style, layout,
paint decisions, and script stay in Common Lisp. Cairo only rasterizes pixels
and, later, an OS window. Do not move those layers into C, C++, Rust, Node, or
an existing browser engine. Page scripts run in the Lisp interpreter.
Do not add Chromium, WebKit, Gecko, or a Node replay path.

Each pass:

1. Read this file.
2. Implement the single highest item under Next that is not done.
3. Add or extend a test in `tests/engine.lisp`.
4. Run `(asdf:test-system :closure/engine)` and do not leave it failing.
5. Render `screenshots/page.html` to a new timestamped PNG in `screenshots/`
   and keep the older shots.
6. Move the finished item to Done with one line on what landed.
7. Leave the next unfinished item at the top of Next.
8. Commit the pass.

Stop the loop only when the done condition below is met. A finished pass is
not a reason to stop.

Done when a real document — HTML, CSS, images, and script — paints correctly
from the Lisp engine without calling Node or another browser engine.

## Done

- Local PNG `<img>` elements: the Lisp layout sizes them from CSS, HTML
  width/height, then the PNG's intrinsic pixels, and Cairo paints the pixels
  into the content box. `file://` and absolute paths work.
- Local GIF and baseline JPEG `<img>` elements. GIF uses the same Lisp reader
  as `src/imagelib/gif.lisp`. JPEG is decoded in Lisp (`src/engine/image.lisp`);
  the imagelib JPEG path shells out to `djpeg` and is not used. Cairo only
  receives the finished pixels.
- Relative `img` `src` resolves against the document URL or `<base href>`.
  `http` and `https` images are fetched in Lisp with Drakma and then decoded
  by the same PNG, GIF, and JPEG path.
- CSS `background-image` tiles a URL across the padding box. `object-fit`
  (`fill`, `contain`, `cover`, `none`, `scale-down`) sizes replaced images
  inside the content box.
- Inline SVG is parsed in Lisp. `rect`, `circle`, `ellipse`, `line`,
  `polyline`, `polygon`, and `path` (move, line, horizontal, vertical, cubic,
  quadratic, close) are painted with Cairo. `viewBox` maps user units onto
  the content box.
- Inline text wraps on spaces, keeps newlines for `pre` and `pre-line`, and
  stays on one line for `nowrap`. Each run uses its own font. `vertical-align`
  (`baseline`, `top`, `middle`, `bottom`, `sub`, `super`, `text-top`,
  `text-bottom`) shifts glyphs against the line's baseline.
- Form controls paint and are scriptable. `input`, `select`, and `textarea`
  draw their value; `button` sizes to its label. The Lisp interpreter reads
  and writes `value`, including the selected option and a checkbox's checked
  state.
- External stylesheets load in Lisp. A `link rel=stylesheet` is fetched from
  a file or from `http`/`https`, and `@import` resolves against that sheet's
  own URL.
- Page scripts run in the Lisp interpreter inside `layout-html`. Node replay
  is gone. External `script src` files load through the same Lisp fetch as
  stylesheets.
- Scrolling and hit testing share one paint path. `paint-cr` draws a viewport
  for both PNG output and an X11 window. `hit-test` takes the scroll offset.
  With no display, `paint-window` returns nil instead of opening a window.
- `run-window` keeps an X11 window open. The wheel and arrow keys scroll, and
  each frame is painted with `paint-cr`. q or Escape closes it. With no
  display the function returns nil.
- A click on `<a href>` hit-tests the anchor, fetches the target in Lisp, and
  lays it out against the new base. The window repaints that page with
  `paint-cr`.
- A submit button sends the form with GET. Named fields are encoded in the
  action URL, unchecked boxes are left out, and the response is laid out and
  painted with `paint-cr`.
- Page history records each visit and its scroll offset. Backspace goes back
  and repaints the previous page with `paint-cr`. At the start of history,
  back does nothing.
- Forward history is kept after going back. Shift+Backspace or Alt+Right
  moves forward and repaints with `paint-cr`. The window title shows the
  current URL.
- A form with `method=post` sends the encoded fields in the body, not the
  URL. The response is laid out and painted with `paint-cr`.
- The window title uses the document `<title>` when that element has text,
  and falls back to the URL. Painting still goes through `paint-cr`.
- A script can read and assign `document.title`. The assignment updates the
  `<title>` element, so the window title follows. Painting still goes through
  `paint-cr`.
- Assigning `location.href` loads that URL in Lisp and lays out the target,
  the same path a link click uses. The window paints the result with
  `paint-cr`.
- A `location.href` navigation keeps the page that assigned it. Backspace
  returns to that page and repaints it with `paint-cr`, without following
  the redirect again.
- An inline image sits on the same baseline as the surrounding words, with
  the space before it preserved. Painting is unchanged.
- `vertical-align: middle` on an inline image centers the image slightly
  above the surrounding text baseline, about half an x-height.
- A missing image paints its alt text. With no width or height, the box is
  the size of that text.
- Alt text on a sized missing image is clipped to the content box, so a long
  alt does not paint into the following content.
- A `<br>` ends the current line. The text after it starts on the next line
  at the same left edge.
- Text inside a nested inline element keeps that element's font. A bold word
  inside a span is laid out and painted in a bold face.
- An `<a href>` is underlined in its link color. Text that is not a link is
  left plain.
- A link's underline continues under nested text. A red bold word inside the
  link keeps its own color, and the line under it stays the link color.
- `<sup>` sits above the baseline and `<sub>` sits below it, at a smaller size.
- `<s>` and `<del>` draw a line through the text. A plain letter is left open.
- A `<mark>` paints a yellow background behind its text, including text nested
  inside it. Plain text is left unhighlighted.
- An ordered list paints each item's number. A bullet list still paints a bullet.
- An `<hr>` paints a one-pixel horizontal rule across the content width.
- A `<q>` wraps its text in quotation marks. The words around the quote stay put.
- A `<blockquote>` is indented 40px from the surrounding text.
- A nested `<q>` uses single quotation marks inside the outer double quotes.
- `<small>` is drawn smaller than the surrounding text.
- `<big>` is drawn larger than the surrounding text.
- `<u>` and `<ins>` underline their text. Plain text is left without a line.
- A `<tt>` element is drawn in a monospace font. Wide and narrow letters take
  the same advance.
- A `<nobr>` stays on one line inside a box that would otherwise wrap the
  same words.
- A `<wbr>` is a line-break opportunity. A long word stays on one line, and
  the same word with a `<wbr>` splits.
- An `<address>` is drawn in italics. A paragraph beside it stays roman.
- `text-indent` shifts the first line of a block. The following line stays at
  the content edge.
- Positive `letter-spacing` puts extra space between characters, so the word
  is wider by that amount for each gap.
- Positive `word-spacing` adds space between words. The letters inside each
  word stay the same width.
- `text-transform: uppercase` draws capitals and `lowercase` draws small
  letters. The uppercase word is measured as capitals.
- `visibility: hidden` keeps the element's box and the place of what follows
  it, and paints none of that box.
- An `outline` is drawn outside the border. The box keeps its size, and the
  next box is not pushed down.
- `outline-offset` moves the outline farther from the border and leaves a gap.
  The box size stays the same.
- A `box-shadow` with no blur paints a solid shadow offset from the box. The
  box covers the part of the shadow underneath it, and its size stays the same.
- An inset `box-shadow` paints inside the box edge. The center stays the
  background color, and the box size stays the same.
- Two comma-separated `box-shadow`s both paint. One can sit to the right of
  the box and the other below it.
- A left-floated image sits at the left, and the following text starts to
  its right on the same line.
- Text that continues past a left float returns to the content edge on the
  next line below the float.
- A right-floated image sits at the right, and the following text stays to
  its left on the same line.
- A block with `clear: both` starts below a float, at the content edge.
- A `position: relative` box is shifted by `top` and `left`. The next box
  stays in the static place.
- Text inside a relatively positioned box moves with that box.
- A fixed-width block with `margin-left: auto` and `margin-right: auto` is
  centered in its parent.
- A fixed-width block with only `margin-left: auto` sits against the right
  edge of its parent.
- Adjacent block margins collapse. Two 20px margins leave 20px of space, and
  a 10px margin beside a 30px margin leaves 30px.
- A parent with no padding or border collapses its top margin with its first
  child's top margin. The child's box starts at the parent's top, and the
  collapsed margin sits outside the parent.
- A parent with no padding or border collapses its bottom margin with its
  last child's bottom margin. The parent's height stays the child's height,
  and the larger of the two margins sits outside the parent.
- A parent with padding-top or border-top keeps its first child's top margin
  inside. The parent stays at the origin, and its height includes that margin.
- A parent with padding-bottom or border-bottom keeps its last child's
  bottom margin inside. The parent's height includes that margin, and the
  next sibling starts at the parent's bottom edge.

- `flex-wrap: wrap` moves flex items that do not fit onto the next line.
  A gap separates the lines, and the row grows to fit them.
- `flex-wrap: wrap-reverse` places the first flex line at the cross end.
  In a taller row the first line sits on the bottom, and the next line sits
  above it.
- `align-content: center` centers wrapped flex lines in the extra cross
  space. The row keeps its height, and the lines stay at the content edge.
- `align-content: flex-end` packs wrapped flex lines against the cross end.
  The last line sits on the bottom of a taller row, and the line above it
  stays just above that.
- `align-content: space-between` puts the first wrapped flex line at the
  start and the last at the cross end. The extra space is split evenly
  between the lines.
- `align-content: space-around` puts equal space around each wrapped flex
  line. The gap between lines is twice the gap at each edge.
- `align-content: space-evenly` puts the same gap before, between, and after
  wrapped flex lines.

- `text-overflow: ellipsis` clips a single nowrap line that does not fit
  and paints an ellipsis at the end of the visible text. A short word is
  left unchanged.
- `overflow-wrap: break-word` wraps a long word that does not fit onto the
  next line inside the box. The pieces still spell the word, and a long word
  stays one line when the property is left off.
- `word-break: break-all` breaks a line between any two characters. The first
  line of a narrow box takes letters from the next word, and each piece stays
  inside the box.
- `text-align: justify` spreads the extra space between words on a wrapped
  line. The last word on that line reaches the right edge, and the last line
  stays packed at the start.
- `border-collapse: collapse` makes adjacent table cells share one border.
  The next cell overlaps the previous cell by that border's width, across
  and down.
- `border-spacing` separates table cells when borders are not collapsed.
  A spacing of `12px 8px` leaves 12px between columns and 8px between rows.
- A table cell with `colspan` spans that many columns. The cell beside it
  starts after the span, and the column under that cell lines up with it.
- A table cell with `rowspan` spans that many rows. The cell in the next row
  starts beside the span, and the span's height covers both rows.
- `vertical-align: bottom` in a table cell puts that cell's content at the
  bottom of the row when the row is taller. The other cell's text stays at
  the top.
- `vertical-align: middle` in a table cell centers that cell's content in
  the row when the row is taller. The other cell's text stays at the top.
- A `caption` is placed above its table, and the first row starts below the
  caption.
- `caption-side: bottom` places the caption below the table. The first row
  stays at the top of the table.
- `empty-cells: hide` leaves an empty table cell without a border or
  background. The cell keeps its place, and a cell with text still paints.
- `list-style-position: inside` puts the marker inside the list item. The
  item text starts to the right of that marker, and nothing hangs outside
  the item.
- `list-style-type: square` paints a square marker. Its corner is filled,
  while a disc leaves that corner empty.
- `list-style-type: circle` paints a hollow circle. The center stays empty,
  and the ring is drawn around it.
- `text-decoration: overline` draws a line above the text. Plain text at
  the same place stays empty.
- `background-repeat: no-repeat` paints a background image once. The default
  still tiles that image across the box.
- `background-position: right` places a background image on the right. The
  left edge of that box stays empty.
- `background-position: center` places a background image in the middle. Both
  side edges of that box stay empty.
- `background-position: bottom` places a background image along the bottom.
  The top edge of that box stays empty.
- `background-position: right bottom` places a background image in the
  bottom-right corner. The other three corners stay empty.
- `background-size: contain` scales a background image so the whole image
  fits inside the box. The extra side of the box stays empty.
- `background-size: cover` scales a background image until it covers the
  box. The part that does not fit is cropped.
- `background-size: 8px 4px` paints a background image at that size. The
  image stops at those edges instead of its intrinsic size.

## Next

1. `background-size: 50%` paints a background image at half the box size.
   The done condition is not met; pages still need more of the web than this
   viewport.
