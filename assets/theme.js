/* Colour theme.  Parchment unless the reader picks otherwise: the choice is
   kept in localStorage as "light", "dark" or "auto" (follow the system), and
   the .theme-toggle button in the navbar cycles through them.

   The preference goes on <html> as data-theme-pref, which picks the button's
   glyph, and the theme in force as data-theme, which picks the palette in
   global.css.  Without this script neither attribute is set, so the page is
   parchment and the button is hidden.

   Loaded synchronously from <head> by the Org pages, so the palette is right
   before the first paint, and by `with-page-chrome' before it builds a CLOG
   page.  The button may not exist yet when this runs -- CLOG adds it later --
   hence one delegated listener rather than a handler bound to it. */
(function () {
  var KEY = 'theme';
  var ORDER = ['light', 'dark', 'auto'];
  var NAMES = { light: 'parchment', dark: 'dark', auto: 'automatic (as the system)' };
  var root = document.documentElement;
  var media = window.matchMedia('(prefers-color-scheme: dark)');

  function stored() {
    var value = null;
    try { value = localStorage.getItem(KEY); } catch (e) {}
    return ORDER.indexOf(value) >= 0 ? value : 'light';
  }

  function apply(pref) {
    var dark = pref === 'dark' || (pref === 'auto' && media.matches);
    root.setAttribute('data-theme-pref', pref);
    root.setAttribute('data-theme', dark ? 'dark' : 'light');
  }

  function next(pref) {
    return ORDER[(ORDER.indexOf(pref) + 1) % ORDER.length];
  }

  function label(button) {
    var pref = root.getAttribute('data-theme-pref');
    var text = 'Theme: ' + NAMES[pref] + '. Switch to ' + NAMES[next(pref)] + '.';
    button.title = text;
    button.setAttribute('aria-label', text);
  }

  function toggleAt(event) {
    return event.target.closest ? event.target.closest('.theme-toggle') : null;
  }

  apply(stored());

  // Only "auto" depends on the system setting.
  var follow = function () { apply(stored()); };
  if (media.addEventListener) media.addEventListener('change', follow);
  else if (media.addListener) media.addListener(follow);
  // Another tab changed the preference.
  window.addEventListener('storage', function (event) {
    if (event.key === KEY) apply(stored());
  });

  document.addEventListener('click', function (event) {
    var button = toggleAt(event);
    if (!button) return;
    var pref = next(root.getAttribute('data-theme-pref'));
    try { localStorage.setItem(KEY, pref); } catch (e) {}
    apply(pref);
    label(button);
  });
  // The label describes the state when it is read, not when the page loaded.
  ['mouseover', 'focusin'].forEach(function (type) {
    document.addEventListener(type, function (event) {
      var button = toggleAt(event);
      if (button) label(button);
    });
  });
})();

/* Sidenotes, for global.css's margins: a copy of each of an Org page's
   footnotes beside its reference.  Here rather than in a file of its own
   because every Org page already loads this one, so the exported pages and
   the export settings need no change.

   Built once, at DOMContentLoaded.  Which of the copies and the list at the
   end is shown is global.css's media query, so resizing needs nothing from
   here.  A page without #footnotes -- every CLOG page -- is left alone. */
(function () {
  // A note floats from the line of its reference, as wide as the margin past
  // the reference's container.  Inside one of these that is the wrong
  // place or the wrong edge, so the note goes before the outermost instead.
  var HOIST = 'h1, h2, h3, h4, h5, h6, table, pre, figure, blockquote, '
            + '.abstract, .org-src-container';

  function showing(note) { return note.getClientRects().length > 0; }

  function noteFor(num, body) {
    var note = document.createElement('span');
    note.className = 'sidenote';
    note.setAttribute('role', 'note');
    var mark = document.createElement('sup');
    mark.className = 'sidenote-number';
    mark.textContent = num.textContent;
    note.appendChild(mark);
    note.appendChild(document.createTextNode(' '));
    var copy = body.cloneNode(true);
    copy.removeAttribute('role');
    copy.removeAttribute('id');
    Array.prototype.forEach.call(copy.querySelectorAll('[id]'), function (el) {
      el.removeAttribute('id');
    });
    note.appendChild(copy);
    return note;
  }

  function place(note, ref, content) {
    var beside = ref.parentElement.tagName === 'SUP' ? ref.parentElement : ref;
    var before = null;
    for (var el = beside.parentElement; el && el !== content; el = el.parentElement)
      if (el.matches(HOIST)) before = el;
    if (before) before.parentNode.insertBefore(note, before);
    else beside.parentNode.insertBefore(note, beside.nextSibling);
  }

  // A note lights up with its number while either is under the pointer or
  // the number has focus.
  function pair(ref, note) {
    var hovered = 0, focused = false;
    function sync() {
      var on = hovered > 0 || focused;
      ref.classList.toggle('is-current', on);
      note.classList.toggle('is-current', on);
    }
    [ref, note].forEach(function (el) {
      el.addEventListener('mouseenter', function () { hovered++; sync(); });
      el.addEventListener('mouseleave', function () { hovered--; sync(); });
    });
    ref.addEventListener('focus', function () { focused = true; sync(); });
    ref.addEventListener('blur', function () { focused = false; sync(); });
    // The list the number links to is hidden while the notes show.  A
    // second reference to one note is not beside it, hence the scroll.
    ref.addEventListener('click', function (event) {
      if (!showing(note)) return;
      event.preventDefault();
      note.scrollIntoView({ block: 'nearest' });
    });
  }

  function build() {
    var content = document.getElementById('content');
    var list = document.getElementById('footnotes');
    if (!content || !list || !Element.prototype.closest) return;
    var notes = {}, made = 0;
    Array.prototype.forEach.call(content.querySelectorAll('a.footref'), function (ref) {
      var href = ref.getAttribute('href') || '';
      if (href.charAt(0) !== '#' || list.contains(ref)) return;
      var note = notes[href];
      if (!note) {
        var num = document.getElementById(href.slice(1));
        var def = num && num.closest('.footdef');
        var body = def && def.querySelector('.footpara');
        if (!body) return;
        note = notes[href] = noteFor(num, body);
        place(note, ref, content);
        made++;
      }
      pair(ref, note);
    });
    if (made > 0 && made === list.querySelectorAll('.footdef').length)
      content.classList.add('has-sidenotes');
  }

  if (document.readyState === 'loading')
    document.addEventListener('DOMContentLoaded', build);
  else build();
})();
