javascript:
if (
  !("is_debugging" in window)
) {
  is_debugging = false;
  var debug_el = document.createElement("style");
  debug_el.append(
    document.createTextNode(
      `html * {
        background: rgba(255, 0, 0, .1);
        box-shadow: 0 0 0 1px red;
      }`
    )
  );
}
function enable_debugger() {
  if (!is_debugging) {
    document.head.appendChild(debug_el);
    is_debugging = true;
  }
}
function disable_debugger() {
  if (is_debugging) {
    document.head.removeChild(debug_el);
    is_debugging = false;
  }
}
!is_debugging ? enable_debugger() : disable_debugger();
