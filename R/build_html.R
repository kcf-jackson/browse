html_tags <- htmltools::tags

# HTML
build_html <- function(data) {
  html_tags$html(
    make_head(),
    make_body(data)
  )
}


# Head
make_head <- function() {
  dir0 <- system.file("assets", package = "browse")
  full_path <- function(x) { file.path(dir0, x) }
  html_tags$head(
    html_tags$title("Finder.js"),
    html_css(full_path("./site.css")),
    html_css(full_path("./finderjs.css")),
    html_tags$link(
      rel="stylesheet",
      href="https://maxcdn.bootstrapcdn.com/font-awesome/4.4.0/css/font-awesome.min.css"
    ),
    # html_css(full_path("./font-awesome.min.css")),  # fix later
    html_js(full_path("./finder.min.js")),
    html_js(full_path("./util.js"))
  )
}

html_js <- function(file) {
  src <- base64enc::dataURI(mime = "application/javascript", file = file)
  html_tags$script(src = src)
}

html_css <- function(file) {
  src <- base64enc::dataURI(mime = "text/css", file = file)
  html_tags$link(rel = "stylesheet", type = "text/css", href = src)
}


# Body
make_body <- function(data) {
  script0 <- glue::glue('
    var ws = new WebSocket("ws://localhost:9454");

    var container = document.getElementById("container");
    var data = <data>;
    var options = { createItemContent: createItemContent };
    function createItemContent(cfg, item) {
      var data = item.children || cfg.data;
      var frag = document.createDocumentFragment();
      var label = el("span");
      var iconPrepend = el("i");
      var iconAppend = el("i");
      var prependClasses = ["fa"];
      var appendClasses = ["fa"];

      // prepended icon
      if (data) {
        prependClasses.push("fa-folder");
      } else {
        prependClasses.push("fa-file-o");
      }
      addClass(iconPrepend, prependClasses);

      // text label
      append(label, [iconPrepend, text(item.label)]);
      frag.appendChild(label);

      // appended icon
      if (data) {
        appendClasses.push("fa-caret-right");
      } else if ("url" in item) {
        appendClasses.push("fa-external-link");
      }
      addClass(iconAppend, appendClasses);
      frag.appendChild(iconAppend);

      return frag;
    }

    var f = finder(container, data, options);
    var oldItem = null;
    f.on("interior-selected", function(item) {
      console.log("Leaf selected", item);
      oldItem = item;
    });
    f.on("leaf-selected", function(item) {
      console.log("Leaf selected", item);
      if (item === oldItem) {
        ws.send(JSON.stringify(item));
      } else {
        oldItem = item;
      }
    });
    window.addEventListener(
      "keypress",
      function(e) {
        console.log(e.code);
        if (e.code === "Enter") {
          if (oldItem.is_dir[0] !== true) {
             ws.send(JSON.stringify(oldItem));
          }
        }
      }
    );
    ',
    .open = "<", .close = ">"
  )
  html_tags$body(
    class = "site",
    html_tags$div(id = "container"),
    html_tags$script(script0, type="text/javascript", charset="utf-8")
  )
}
