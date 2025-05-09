// www/js/geolocation.js ----------------------------------------------------
(function () {

  function sendStatus(lat, lon, ok) {
    Shiny.setInputValue("lat",  lat, {priority: "event"});
    Shiny.setInputValue("lon",  lon, {priority: "event"});
    Shiny.setInputValue("geo_allowed", ok, {priority: "event"});
  }

  function getLocation() {
    if (!navigator.geolocation) {
      sendStatus(null, null, false);
      return;
    }
    navigator.geolocation.getCurrentPosition(
      function (pos) {  // success
        sendStatus(pos.coords.latitude, pos.coords.longitude, true);
      },
      function () {     // denied or error
        sendStatus(null, null, false);
      },
      {enableHighAccuracy: true, timeout: 10000}
    );
  }

  // Shiny v1.8 introduced 'shiny:input-invalidated' that fires *after* the
  // value has been set. We watch both to stay future‑proof.
  function attach () {
    $(document).on("shiny:inputchanged shiny:inputinvalidated", function (e) {
      if (e.name === "getloc" && e.value > 0) getLocation();
    });
  }

  // In case the script loads before Shiny initialises:
  if (window.Shiny) attach();          // already ready
  else $(document).on("shiny:connected", attach);

})();
