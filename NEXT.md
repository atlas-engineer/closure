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
- `background-size: 50%` paints a background image at half the box size. The
  image stops halfway across the box.
- `background-position: 50%` places a background image halfway across its
  box. Both side edges of that box stay empty.
- `text-shadow: 2px 2px #cc0000` paints a shadow offset from the text. The
  glyph stays dark, and the shadow sits down and to the right.
- `text-shadow: 2px 2px #cc0000, 6px 6px #0000cc` paints both shadows. The
  second offset sits further down and to the right.
- `background: linear-gradient(#cc0000, #0000cc)` paints a vertical blend.
  The top of the box is red, the bottom is blue, and the middle mixes them.
- `linear-gradient(to right, #cc0000, #0000cc)` paints a horizontal blend.
  The left edge is red from top to bottom, and the right edge is blue.
- `linear-gradient(45deg, #cc0000, #0000cc)` paints the blend on a diagonal.
  The bottom-left is red, the top-right is blue, and the other corner mixes
  them.
- `linear-gradient(#cc0000 20%, #0000cc)` keeps the first color across the
  first fifth of the box, then blends to the last color.
- `radial-gradient(#cc0000, #0000cc)` paints a red center fading to blue.
  The center is red, and the corners are blue.
- `radial-gradient(circle closest-side, #cc0000, #0000cc)` ends the blend at
  the nearest edge. That edge is the last color, while the center stays the
  first.
- `radial-gradient(circle at 0 0, #cc0000, #0000cc)` puts the first color at
  the top-left corner. The opposite corner is the last color.
- `repeating-linear-gradient(#cc0000, #0000cc 50%)` paints the blend twice
  down the box. The second half starts red again and ends blue.
- `text-decoration: underline overline` draws both a line under the text and
  a line above it. Plain text has neither line.
- `text-decoration: underline line-through` draws both a line under the text
  and a line through it. Plain text has neither line.
- `text-decoration-color: #cc0000` paints the decoration red. The glyphs stay
  black, and the red line sits below them.
- `text-underline-offset: 6px` moves the underline further below the
  baseline. The default line stays near the text.
- `text-decoration-thickness: 4px` paints a thicker decoration line. A 4px
  line covers more rows than the default hairline.
- `text-decoration-style: dashed` draws a dashed underline. The solid line
  stays continuous, and the dashed line leaves gaps.
- `text-decoration-style: wavy` draws a wavy underline. The wave leaves gaps
  on the solid line's row and dips below it.
- `text-decoration-style: double` draws two parallel underlines. A gap
  separates the lines, and a solid underline stays a single line.
- `text-decoration-style: dotted` draws a dotted underline. The solid line
  stays continuous, and the dotted line leaves gaps.
- `outline-style: dashed` draws a dashed outline. The solid outline stays
  continuous, and the dashed outline leaves gaps. The box width is unchanged.
- `outline-style: dotted` draws a dotted outline. The solid outline stays
  continuous, and the dotted outline leaves gaps. The box width is unchanged.
- `outline-style: double` draws two parallel outlines. A gap separates the
  lines, and a solid outline of the same width fills the band. The box width
  is unchanged.
- `border-style: double` draws two parallel border edges. A gap separates
  them, and a solid border of the same width fills the band. Both boxes keep
  the same width.
- `border-style: groove` paints the top and left edges darker than the
  bottom and right. A gray groove is dark on the top and left and light on
  the bottom and right.
- `border-style: ridge` paints the top and left edges lighter than the
  bottom and right. A gray ridge is light on the top and left and dark on
  the bottom and right.
- `border-style: inset` paints the top and left edges darker than the
  bottom and right. A gray inset is dark on the top and left and light on
  the bottom and right.
- `border-style: outset` paints the top and left edges lighter than the
  bottom and right. A gray outset is light on the top and left and dark on
  the bottom and right.
- `list-style-image` paints that image as the list marker. The marker is
  the image color, and it covers a spot a disc leaves empty.
- `list-style-image: none` keeps the disc. An inherited image still paints
  on the other item, and the item set to none is a black disc.
- `background-clip: content-box` paints the background inside the content
  box. The padding stays empty, and the content area stays filled. The
  default still fills the padding.
- `background-clip: padding-box` paints the padding and leaves the border
  area empty. The default still shows the background in the border area.
- A `background-image` with `background-clip: content-box` stays inside
  the content box. The padding stays empty, and the content area still
  shows the image. The default image still paints in the padding.
- `background-origin: content-box` positions a background image from the
  content edge. The padding stays empty, and the image starts where the
  content starts. The default still starts in the padding.
- `background-origin: border-box` positions a background image from the
  outer border edge. The corner is the image, and the default still starts
  inside the border.
- A `background-image` with `background-clip: padding-box` does not paint
  under the border. The border area stays empty, and the image still paints
  inside it. The default image still paints under the border.
- `background-repeat: space` spreads the image with even gaps. A flush tile
  is continuous, and space leaves a gap before the next image.
- `background-repeat: round` scales the image so a whole number of tiles
  fill the box. The second color of a split image arrives sooner than it
  does in an unscaled tile, and the tile still starts with the first color.
- `object-position: right` places a replaced image against the right side
  of its box. The default stays in the middle, and the right-aligned image
  leaves that middle empty.
- `object-position: bottom` places a replaced image against the bottom of
  its box. The default stays in the middle, and the bottom-aligned image
  leaves that middle empty.
- `object-position: left bottom` places a replaced image in the bottom-left
  corner. The middle stays empty, and the corner is the image.
- `object-position: 100% 0` places a replaced image in the top-right
  corner. The middle stays empty, and that corner is the image.
- `aspect-ratio: 2 / 1` sets an auto height from the width. An empty 40px
  box stays flat, and the ratio box is 40 by 20.
- `aspect-ratio: 2 / 1` with `height: 20px` and `width: auto` makes the box
  40px wide. A height alone still stretches to the container.
- An `aspect-ratio: 2 / 1` box grows taller than the ratio when its content
  is taller. An empty 40px box stays 20px tall, and a 40px child makes it 40.
- An image with `width: 40px` and `aspect-ratio: 2 / 1` is 20px tall even
  when the file is square. The same square file with only the width stays 40.
- A table with `width: auto` is as wide as its cells, and a caption wraps
  inside that width. A long caption word widens the table. A specified
  table width still fills that width. A space that does not fit at the end
  of a line is dropped, so it does not become a blank line.
- The initial font is a Times-like serif. A 220px paragraph wraps onto
  three lines with the same breaks a browser uses. The sample page also
  has a bullet list; the items are indented and stacked.
- Text is measured without hinting, so a caption above the cells "One"
  and "Two" wraps onto two lines. An `<hr>` is a two-pixel inset gray rule
  across the content width.
- Normal line height adds the font's ascent, descent, and line gap after
  each is rounded to a pixel. A paragraph line is 18px, a heading is 37px,
  and a 13px preformatted line is 17px. The sample page also has a
  blockquote indented 40px on each side.
- Kerning tucks pairs such as "Tw" together, and a collapsed table does
  not count the shared border twice. The sample page shows H<sub>2</sub>O
  and a superscript.
- `vertical-align: sub` lowers the baseline by 0.3em. A subscript sits
  about 10px below the line top. `<code>` is 13px monospace.
- An image with `height: 20px` and `aspect-ratio: 2 / 1` is 40px wide even
  when the file is square. The same square file with only the height stays
  20px wide.

- A collapsed table cell's border box is the content, the padding, and
  one border. The cell starts half a pixel inside the table, and the next
  cell starts where the previous one ends. The sample page also has an
  italic word.
- Inline boxes use a pixel ascent and descent. A word is 17px tall and
  `<small>` is 15px. A subscript and a superscript are 15px.

- A line with a subscript includes the subscript's line gap, so the
  line is 26.4px and the next line starts with Chromium. An image on the
  baseline makes its line 24px. `<big>` is 21px tall.

- Kerning uses the unfitted pair adjustment, including between a word
  and the space after it. The link in the sample page starts at the same
  place as in a browser. A `<q>` wraps its words in quotes.
- A `<q>` uses the English quotation marks, so the quoted word is the
  same width as in a browser. A definition list indents the definition
  40px under the term.
- A subscript drops by the parent size divided by five, plus one pixel,
  and a superscript rises by the size divided by three, plus one pixel.
  The subscript line is 26.52px. A figure is indented 40px on each side.
- A word's advance rounds to the nearest 1/64px after kerning in font
  units. The sample tables are 73.875px and 48.016px wide. An address is
  an italic line across the content width.
- h4, h5, and h6 use the browser's size and margin. A space between two
  elements is kept. The sample page also has the smaller headings, the
  common phrasing elements, center, and the sectioning elements.
- A fieldset has a 2px groove border and its legend sits on the top edge.
  A closed details shows only the summary. A header cell is centered, and
  a separate table keeps 2px of border spacing.
- A video and a canvas are 300 by 150. Progress is 160 by 16 and meter is
  80 by 16, both 0.2em below the baseline. Audio is not shown.
- A ruby annotation is half size and sits above its base. A longer
  annotation overhangs by half its font size on each side, and the line
  grows to 26px.
- A button is 35 by 21, a text field is 177 by 21, a select fits its
  label at 49 by 19, and a textarea is 181 by 40.
- An iframe is 300 by 150 inside a 2px border, and an object is 300 by
  150 on the same baseline. bdi, bdo, and data are ordinary inline words.
- A closed dialog is hidden. An open dialog is a centered white box with
  a 3px border and 1em of padding, 13px from the top. search is a block
  and output is an inline word.
- A picture shows its image at the given size, and the line grows to fit
  it. A map and its areas take no space between the words around them.
- A col covers its cell and a colgroup covers the row. A slot's fallback
  is ordinary text. A datalist is not shown.
- A footer row is painted after the body even when it comes first in the
  source. A menu indents its item by 40px. A heading group holds its
  heading.
- A directory list indents like a menu. A select grows to its longest
  option or group label. wbr wraps a narrow line, and noscript is not shown.
- An optgroup label is bold, so a select is as wide as that label. The
  sample control is 65px wide and 19px tall.
- An open dialog stays where it would have sat in the flow, centered on
  that line. The block after it starts in the same place.
- A video line is 154px. Progress and meter sit on it without adding their
  font's line gap.
- A font element uses the size, color, and face attributes. strike is
  struck through. A marquee fills its line, and that line is 22px.
- listing and xmp are preformatted monospace blocks. An abbr or acronym
  with a title is underlined with dots. nobr does not wrap.
- tt is monospace, blink is an ordinary word, and image is an img.
  isindex adds no search form. plaintext is preformatted.
- An embed with a resource uses its image or its width and height. Those
  attributes also size an iframe and an object, unless author CSS sets them.
- A frameset fills the viewport. Columns and rows use pixels, percentages,
  or stars, and the default border leaves a 6px gap. noframes is hidden.
- An rb element is the ruby base. For WWW with the annotation note, that
  box is 45.31 by 17 at the same x as the ruby.
- An rt inside rtc stays full size and follows the base on the line. The
  ruby is 72.86 by 17, and the container starts where the base ends.
- An open details shows its body and the summary marker points down. The
  box is 52px tall, and the paragraph starts 34px down.
- A checkbox and a radio are 13px. Submit and reset use the button border
  and shrink to the label instead of the text-field width.
- A file input is 253 by 21, a range is 129 by 16, and a color input is
  50 by 27.
- A time input is 103 by 24: 98px of content, 1px of left padding, and a
  2px inset border.
- Progress paints a blue bar and meter a green bar, each across half the
  track when the value is half of the range.
- A range draws a blue track up to its thumb. A color input fills with
  its value.
- A file input paints a Choose File button and the label No file chosen.
- A select paints a downward triangle in the gutter at its right edge.
- A checked checkbox and radio are blue. An empty checkbox is a gray box.
- An open dialog's text moves with the centered box instead of staying at the static x.
- A fieldset border crosses the middle of the legend and leaves a gap behind it.
- That border is not painted again across the fieldset's text line.
- File, range, color, and time share Chromium's baselines. The line is 28.5px.

## Next

1. The done condition is not met. The tags that paint are on the sample
   page, but a real document still needs more of the web than this viewport.
