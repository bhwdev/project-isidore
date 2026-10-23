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
