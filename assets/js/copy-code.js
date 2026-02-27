var COPY_ICON = '<svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><rect x="9" y="9" width="13" height="13" rx="2" ry="2"></rect><path d="M5 15H4a2 2 0 0 1-2-2V4a2 2 0 0 1 2-2h9a2 2 0 0 1 2 2v1"></path></svg>';
var CHECK_ICON = '<svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><polyline points="20 6 9 17 4 12"></polyline></svg>';

document.addEventListener("DOMContentLoaded", function () {
  var highlights = document.querySelectorAll("div.highlight");

  highlights.forEach(function (highlight) {
    var button = document.createElement("button");
    button.className = "copy-code-btn";
    button.innerHTML = COPY_ICON;
    button.setAttribute("aria-label", "Copy code");

    button.addEventListener("click", function () {
      var code = highlight.querySelector("code");
      if (!code) return;

      navigator.clipboard.writeText(code.textContent).then(function () {
        button.innerHTML = CHECK_ICON;
        button.classList.add("copied");

        setTimeout(function () {
          button.innerHTML = COPY_ICON;
          button.classList.remove("copied");
        }, 2000);
      });
    });

    highlight.appendChild(button);
  });
});
