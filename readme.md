## guile-web-driver

This is a web-driver, or selenium 2, client.
It's purpose is to automate browsers, specifically for automatic web server testing.
Chrome or Firefox can be used as the automated browsers,
or it can connect to arbitrary server providing webdriver interface.
The client implements most of the webdriver [specification](https://www.w3.org/TR/webdriver2/).

### Requirements

- guile version 2.2
- guile-json library from http://download.savannah.gnu.org/releases/guile-json/guile-json-4.7.3.tar.gz
- Optional chromedriver command and either chrome or chromium browser.
  Some distribution (arch) install chromedriver as part of chromium package, 
  some others (debian) provide a separate package (chromium-driver).
  Required for unit tests.
- Optional [geckodriver](https://github.com/mozilla/geckodriver/) and mozilla firefox browser.
  Required for unit tests.
- Optionally for unit testing, hdt library is required from https://github.com/her01n/hdt

### Licence

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program. If not, see <https://www.gnu.org/licenses/>. 

### Usage

```guile
(use-modules (web driver))
```

### Sessions

Following procedures open and close web driver sessions.
Most procedures takes the web driver session as an optional argument.
Implicit session would be open on first call of such a procedure,
so for most use cases it is not necessary to call *open-web-driver*,
only to call *close-web-driver* without argument when done.

- **open-web-driver [#:browser 'browser] [#:url url] [#:headless #t] [#:capabilities capabilities]**

  Start a new web driver session.

  *browser* argument should be a symbol, one of the following:

  - 'chrome, 'chromium or 'chromedriver
    Launch *chromedriver* command, open a chrome or chromium browser.
    The command should be in *PATH*. This is the default.
  - 'firefox or 'geckodriver
    Launch *geckodriver* command, open a firefox browser.
    The command should be in *PATH*.
  - 'headless-firefox
    Launch *geckodriver* command and open headless firefox.
    Deprecated, use *#:browser firefox #:headless #t*.

  If url argument is given, connect to remote webdriver server at the url,
  and start a new web driver session there.
  *url* should start with "http://".
  *browser* must not be specified.

  With *#:headless* option set to *#t*, opens the browser in headless mode.
  The page and user interface is not visible, but does not require a window system.
  This only works with geckodriver.

  Desired *capabilities* may be requested.
  The capabilities are submitted in "alwaysMatch" property.
  *capabilities* parameter should be an association list or a hash-table.
  To pass an object as a value, it must be specified as a hash-table.
  For example:

  ```
  (open-web-driver
    #:browser 'geckodriver
    #:capabilities
      `(("browserName" . "firefox")
        ("moz:firefoxOptions" . ,(alist->hash-table `(("args" . ("-headless")))))))
  ```

  The caller may use *json* macro from *(json)* package to build the hash tables conveniently.

  The new driver would become the default driver in case there is no default driver open yet.

- **web-driver? object**

  Checks if the object is an instance of web driver, as returned by open-web-driver.

- **close-web-driver [driver]**

  Closes the web driver.
  If the argument is not specified, closes the implicitly open web driver session.
  Does nothing if the argument is not specified and the session was not yet imlicitly started.

- **call-with-web-driver proc**

  Start a web driver session, and call *proc* with the resulting session object.
  This new session would be used as default for procedures taking optional session argument.
  Closes the session after the proc returns or throws an exception.
  Returns the value that the *proc* returned.

### Timeouts

- **set-script-timeout [driver] [milliseconds|#:never]**

  Sets the timeout for executing scripts
  with methods **execute-javascript** and **execute-javascript-async**.
  Special value **#:never** allows the script to run indefinitely.
  Calling without arguments sets the timeout to the default value, 30 seconds.

- **get-script-timeout [driver]**

  Returns the current script timeout in milliseconds, or **#:never**.

- **set-page-load-timeout [driver] [milliseconds]**

  Sets the timeout for page loading, for example with **navigate-to** method.
  Calling without arguments sets the timeout to the default value, 5 minutes.

- **get-page-load-timeout [driver]**

  Returns the current page load timeout in milliseconds.

- **set-implicit-timeout [driver] [milliseconds]**

  Sets the timeout for element location, for example with **element-by-id** method.
  Calling without arguments sets the timeout to the default value, 0 milliseconds.

- **get-implicit-timeout [driver]**

  Returns the current implicit timeout in milliseconds.

### Navigation

- **navigate-to [driver] url**

  Navigates the browser to given url.
  Should be the same as user entering the url in the address bar.
  In python bindings the analogous method is called 'get'.

- **current-url [driver]**

  Returns the current url, as shown in the address bar.

- **back [driver]**

  Navigates to previous page. Does nothing if the browser is already at the start of history list.

- **forward [driver]**

  Navigates to next page in history list. Does nothing if the browser is at the most recent page.

- **refresh [driver]**

  Reloads current page.

- **title [driver]**

  Returns the title of the current page as string.
  Returns empty string if the page did not set a title.

### Windows

Let's define **window** as a browser window, tab or a similar concept, 
capable of independent navigation.
In the specification, the window is also called **top-level browsing context**.
There is always one *current* window, that would receive navigation calls.
One window is created and made current implicitly at the session opening.

- **current-window [driver]**
  
  Returns the current window.

- **close-window [driver]**

  Close the current window.
  The driver may close this session and all subsequent method calls would fail.

> TODO optionally accept window argument

- **all-windows [driver]**

  Returns the list of all windows of this session.

- **open-new-window [driver]**

  Open a new window.
  Return the new window.
  If the browser does not support windows, open a new tab instead.

- **open-new-tab [driver]**

  Open a new browser tab.
  Return the new window.
  If the browser does not support browser tabs, open a new window instead.

- **switch-to window**

  Makes the window current.

### Browsing Context

**Browsing context** is either the window or a **\<frame\>**, **\<iframe\>** element.
There is always one **current browsing context**, that recieves content calls,
for example **element-by-...** methods.
The current window is selected as current browsing context at session start,
after navigation step, switching to a different window or similar.

- **switch-to frame**

  Makes the frame the current browsing context.
  **frame** must be a **\<frame\>** or **\<iframe\>** element.
  The frame must be a direct child of the current browser context.

> TODO allow switching to any frame, not only to the direct child.

- **switch-to [driver] n**

  Makes the **n**-th child frame of the current browsing context the current browsing context.
  **n** is a zero-based integer.

- **switch-to-parent [driver]**

  If the current browsing context is a frame,
  switch to it's parent frame, or to the window if there is no parent frame.
  Does nothing if the current browsing context is a window.

- **switch-to-window [driver]**

  Makes the current window the current browsing context.

### Rectangle Record

We define **\<rect\>** record type to be used for all screen geometry methods.
It contains four fields: *x*, *y*, *width* and *height*.
All values are integers.

- **make-rect x y width height**

  Returns new rectangle.

- **rect? object**

  Checks if object is a **rect**.

- **rect-x rect**
- **rect-y rect**
- **rect-width rect**
- **rect-height rect**

  Gets a field value.

### Resizing and Positioning Windows

- **window-rect [driver]**

  Returns a screen position and dimension of the current window.

- **set-window-position [driver] x y**
- **set-window-size [driver] width height**
- **set-window-rect [driver] rect**

  Sets the screen position and/or dimension of the current window.
  This implicitly restores the window state to normal.
  It may not be possible to honor the new position exactly,
  in this case the window is moved and resized to the nearest possible position and dimension.
  Returns the new actual window position and dimension.

- **minimize [driver]**

  Minimize (iconify) the current window.
  Does nothing if this is not supported by the window manager.

- **maximize [driver]**

  Maximize the current window.
  If this is not supported by the window manager,
  resize the window to the maximum possible size without going full screen.

- **full-screen [driver]**

  Makes the current window full screen.
  If this is not supported by the window manager, maximize the window.

- **restore [driver]**

  Restores the window to normal, not maximized, full screen or minimized.

> TODO all these methods may accept window as an argument

### Finding Elements

- **element-by-css-selector [driver] selector [#:from element]**

  Finds the first element that matches css selector.
  If there is no such element, throws an exception.
  If from element is specified, consider only elements below this element.

- **elements-by-css-selector [driver] selector [#:from element]**

  Finds all the elements that matches css selector.
  Returns empty list in case there is no such element.
  If from element is specified, consider only elements below this element.

- **element-by-id [driver] id [#:from element]**

  Finds the first element with the given id.
  If there is no such element, throws an exception.
  If from element is specified, consider only elements below this element.

- **elements-by-id [driver] id [#:from element]**

  Finds all the element with the given id.
  Returns empty list in case there is no such element.
  If from element is specified, consider only elements below this element.

- **element-by-class-name [driver] class-name [#:from element]**

  Finds the first element of the class.
  If there is no such element, throws an exception.
  If from element is specified, consider only elements below this element.

- **elements-by-class-name [driver] class-name [#:from element]**

  Finds all the element of the class.
  Returns empty list in case there is no such element.
  If from element is specified, consider only elements below this element.

- **element-by-link-text [driver] link-text [#:from element]**

  Finds an *a* element that have the rendered text equal to *link-text*.
  If there is no such element, throws an exception.
  If from element is specified, consider only elements below this element.

- **elements-by-link-text [driver] link-text [#:from element]**

  Finds all *<a>* elements that have the rendered text equal to *link-text*.
  Returns empty list in case there is no such element.
  If from element is specified, consider only elements below this element.

- **element-by-partial-link-text [driver] link-text [#:from element]**

  Finds an *<a>* element where *link-text* is a substring of rendered text.
  If there is no such element, throws an exception.
  If from element is specified, consider only elements below this element.

- **elements-by-partial-link-text [driver] link-text [#:from element]**

  Finds all *a* elements where *link-text* is a substring of rendered text.
  Returns empty list in case there is no such element.
  If from element is specified, consider only elements below this element.

- **element-by-tag-name [driver] tag [#:from element]**

  Finds the first element with the tag.
  If there is no such element, throws an exception.
  If from element is specified, consider only elements below this element.

- **elements-by-tag-name [driver] tag [#:from element]**

  Finds all the elements with the tag.
  Returns empty list in case there is no such element.
  If from element is specified, consider only elements below this element.

- **element-by-xpath [driver] xpath [#:from element]**

  Finds the element matching the XPath.
  If there is no such element, throws an exception.
  If from element is specified, consider only elements below this element.

- **elements-by-xpath [driver] xpath [#:from element]**

  Finds all the the elements matching the XPath.
  Returns empty list in case there is no such element.
  If from element is specified, consider only elements below this element.

- **element-by-label-text [driver] text [#:from element]**

  Finds an **\<input\>** element, that has related **\<label\>** element
  with the specified *text*.
  If there is no such element, throws an exception.
  If from element is specified, consider only elements below this element.

- **element-by-partial-label-text [driver] text [#:from element]**

  Finds an **\<input\>** element, that has related **\<label\>** element
  containing *text*.
  If there is no such element, throws an exception.
  If from element is specified, consider only elements below this element.

- **active-element [driver]**

  Returns the current active element.
  Throws exception if there is no such element.

### Element State

- **selected? element**

  Returns *#t* if the check box or radio box is checked,
  or if **\<select\>** element is selected.
  Throws an exception if the element is not selectable.

- **attribute element name**

  Gets the value of the element's attribute.
  Returns *#f* if the attribute is undefined.

- **property element name**

  Gets the value of element's javascript property.
  Returns *#f* if the property is undefined.

- **css-value element name**

  Returns the computed value from element's style declarations.

- **text element**

  Gets the text content of the element.

- **text [driver]**

  Without an element argument, get the text of the whole page.

- **tag-name element**

  Returns the tag name of the element.

- **rect element**

  Returns position and dimension of the element relative to the document element.

> TODO implement

- **enabled? element**

  Checks if the form control is enabled.

### Element Interaction

- **click element**

  Simulates user clicking the element,
  For example *<a>* element or form control.

- **click [driver] text**

  If *text* is a string, find an element with the text and click it.
  The element may be:

  - An *anchor* with the given link text.

        <a href="https://link.org">text</a>

  - A *button* with the given text content.

        <button>text</button>

  - An *input* of type *button*, *submit* or *reset* with the text *value*.
    
        <input type="button" value="text" />

  - Any *input* that have an associated *label* that contains the text.

        <input type="checkbox" name="alpha" id="alpha" />
        <label for="alpha">text</label>

- **clear element**

  Clears all content of content editable element.
  Resets the state of File Upload form control.

- **send-keys element text**

  Simulates user typing the text with the focus on the element.

  The procedure could be used to choose a file for *input* of type *file*.
  *text* should be an absolute path to the selected file.
  See also *choose-file*.

- **send-keys [driver] label text**

  Find a label with the text content equal *label*.
  Simulates use typing the text into the associated input.

- **choose-file element path**

  Choose a file for *input* of type *file*.
  **path** may be relative or absolute, the file should exist.

### Document

- **page-source [driver]**

  Gets the *html* source of the current browser context (window or frame).

- **execute-javascript [driver] body [arguments ...]**

  Execute javascript in the current browsing context.
  **body** is a string, it may be a single statement or multiple statements separated by ";".
  If a statement returns a value with **return**, this value is returned by this method.
  Element objects are returned as interchangable with objects returned by **element-by-...* methods.
  Other javascript objects are returned as **hash-table**s.
  Otherwise return **#nil**.
  Arguments are passed as a function arguments. 
  They can be accessed through *arguments* Array-like variable.
  This allows passing elemented returned by **element-by-...** methods to javascript.
  It may be practical to pass strings this way to avoid escaping issues.

  Examples:

  ```scheme
  (execute-javascript "return 3 + 4") => 7
  (execute-javascript "return arguments[0] * 2;" 2) => 4
  (execute-javasctipt "arguments[0].innerHTML = 'text'; return 1;" (element-by-id "id"))
  (text (execute-javascript "return document.getElementById('id');")) => "text"
  ```

- **execute-javascript-async [driver] body [arguments ...]**

  Executes javascript and waits for the callback.
  Calllback function is appended to the **arguments** variable.
  This method returns when this function is called.
  The first argument of the function call is the return value.
  This method is still subject to configured timeout.

  Example:

  ```scheme
  (execute-javascript-async 
    "callback = arguments[0];
     window.setTimeout(function() { callback(42); }, 1);") => 42
  ```

### Cookies

- **cookie-name cookie**

  The name of the cookie.

- **cookie-value cookie**

  The cookie value.

- **cookie-path cookie**

  The cookie path. For example "/". Attribute "Path".

- **cookie-domain cookie**

  The domain the cookie is visible to. Attribute "Domain".

- **cookie-secure cookie**

  Whether the cookie is a secure cookie. Attribute "Secure".

- **cookie-http-only cookie**

  Whether the cookie is an HTTP only cookie. Attribute "HttpOnly".

- **cookie-expiry cookie**

  When the cookie expires, specified in seconds since Unix Epoch.
  Calculated from the value of attribute "Max-Age".
  May be *#f* for a session cookie.

- **cookie-same-site cookie**

  Same Site policy value. May be "Lax", "Strict", or *#f*.

- **get-all-cookies [driver]**

  List of cookies associated with the current browsing context (domain and path).

- **get-named-cookie [driver] name**

  Get the cookie with the given name, associated with the current browsing contenxt.
  Throws an exception if there is no such cookie.

- **add-cookie [driver] #:name name #:value value [#:path path] [#:domain domain]**
  **[#:secure secure] [#:http-only http-only] [#:expiry expiry] [#:same-site same-site]**

  Add a cookie.
  The path defaults to "/".
  The domain defaults to current browser domain.
  The expiry defaults to a session cookie.

- **delete-named-cookie [driver] name**

  Delete the named cookie associated with the current browsing context.
  If the cookie does not exist, does nothing.

- **delete-all-cookies [driver]**

  Deletes all cookies associated with the current browsing context.

### Actions

This is a low level interface to generate fine grained input events.
See [Element Interaction](#element-interaction) for higher level interface.

- **key-down key**

  Simulates user pressing a key on a keyboard.
  Key repetition does not apply,
  only one **keydown** event would be fired, even if the key stays pressed for long time.
  **key** is a string representing the key, it may be:

  - Control character associated with the key. For example "\uE003", "\uE009".
  - Single unicode character that results from pressing the key on US keyboard layout.
    For example "a", " ", "[".
  - [KeyboardEvent.code](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/code/code_values) value. 
    For example: "KeyA", "Digit0", "Keypad0", "ControlLeft", "Space", "F4", "ArrowDown". 
    Code is case insensitive, for example "f4" is accepted.

- **key-up key**

  Simulates user releasing a key on a keyboard.
  The key must match key previouly pressed with **key-down**,
  if not the action is silently ignored.

- **mouse-move x y [duration]**

  Simulate user moving mouse pointer to a location.
  **x**, **y** are coordinates relative to the current viewport.
  Simulate the cursor movement for the given duration in milliseconds if given.
  Multiple intermediate events may be fired in this case.

- **mouse-down button**

  Simulates user pressing a mouse button.
  **button** is either integer index of the button (0 for the left button),
  or symbol **#:left**, **#:middle**, **#:right**.
 
- **mouse-up button**

  Simulates user releasing a mouse button.
  **button** should match button previously pressed with **mouse-down**,
  otherwise the action is silently ignored.

- **wait time**

  Warning: Because of a bug, this does not work correctly with chromedriver.

  Wait before performing following actions.
  **time** is given in milliseconds.

  ```scheme
  (perform (key-down "KeyA") (wait 20) (key-up "KeyA")))
  ```

  is roughly equivalent to 

  ```scheme
  (perform (key-down "KeyA"))
  (usleep (* 20 1000))
  (perform (key-up "KeyA"))
  ```

  but potentionaly much more precise.

  Note: In specification, this action is called *pause*.
  We use *wait* because *pause* is a core binding in Guile.

- **release-all**

  Simulates user releasing all currently pressed keys and buttons.

- **perform [driver] action ...**

  Perform the given actions.
  Returns when all the corresponding events were dispatched.

  Examples:

  ```scheme
  (perform 
    (key-down "ShiftRight") (wait 10) (key-down "a") (wait 10) (key-up "a") (wait 10)
    (key-up "ShiftRight")
  (perform 
    (mouse-move 1 1) (key-down "ControlLeft") (mouse-down #:left) 
    (mouse-move 100 100 1000) (release-all))
  ```

