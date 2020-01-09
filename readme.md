## guile-web-driver

This is a web-driver, or selenium 2, client.
It's purpose is to automate browsers, specifically for automatic web server testing.
Right now only chromedriver running chrome or chromium is supported as a server.
The client is still in very incomplete state, but allows for simple testing.

### Requirements

- guile-json library from http://download.savannah.gnu.org/releases/guile-json/guile-json-0.3.1.tar.gz
- chromedriver command and either chrome or chromium browser.
  Some distribution (arch) install chromedriver as part of chromium package, 
  some others (debian) provide a separate package.
- Optionally for unit testing, hdt library is required from https://github.com/her01n/hdt

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

- **open-web-driver**

  Launches chromedriver and starts a web driver session (opens an automated browser window).

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

> TODO

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

### Frames

> TODO

### Rectangle Record

> TODO implement

We define **<rect>** record type to be used for all screen geometry methods.
It contains four fields: *x*, *y*, *width* and *height*.
All values are integers.

- **make-rect x y width height**

  Returns new rectangle.

- **rect-x rect**
- **rect-y rect**
- **rect-width rect**
- **rect-height rect**

  Gets a field value.

### Resizing and Positioning Windows

> TODO implement

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

- **minimize-window [driver]**

  Minimize (iconify) the current window.

- **maximize-window [driver]**

  Maximize the current window.
  If this is not supported by the window manager,
  resize the window to the maximum possible size without going full screen.

- **full-screen-window [driver]**

  Makes the current window full screen.
  If this is not supported by the window manager, maximize the window.

- **minimize-window [driver]**

  Minimize (iconify) the current window.
  Does nothing if this is not supported by the window manager.

- **restore-window [driver]**

  Restores the window to normal, not maximized, full screen or minimized.

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

- **element-by-xpath [driver] xpath [#:from element]**

  Finds all the the elements matching the XPath.
  Returns empty list in case there is no such element.
  If from element is specified, consider only elements below this element.

- **active-element [driver]**

  Returns the current active element.
  Throws exception if there is no such element.

### Element State

- **selected? element**

  Returns *#t* if the check box or radio box is checked,
  or if *<select>* element is selected.
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

- **tag name element**

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

- **clear element**

  Clears all content of content editable element.
  Resets the state of File Upload form control.

- **send-keys element text**

  Simulates user typing the text with the focus on the element.


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

