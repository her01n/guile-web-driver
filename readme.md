## guile-web-driver

This is a web-driver, or selenium 2, client.
It's purpose is to automate browsers, specifically for automatic web server testing.
Right now only chromedriver running chrome or chromium is supported as a server.
The client is still in very incomplete state, but allows for simple testing.

### Requirements

guile-json library from http://download.savannah.gnu.org/releases/guile-json/guile-json-0.3.1.tar.gz
chromedriver command and either chrome or chromium browser.
Some distribution (arch) install chromedriver as part of chromium package, 
some others (debian) provide a separate package.
For unit testing, hdt library is required from github.com/her01n/hdt

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

### Navigation

- **navigate-to [driver] url**
  Navigates the browser to given url.
  Should be the same as user entering the url in the address bar.
  In python bindings the analogous method is called 'get'.

- **current-url [driver]**
  Returns the current url, as shown in the address bar.

### Cookies

- **delete-all-cookies [driver]**
  Deletes all stored cookies for all sites.

### Finding Elements

- **element-by-css-selector [driver] selector**
  Finds the first DOM element that matches css selector.
  If there is no such element, throws an exception.

- **elements-by-css-selector [driver] selector**
  Finds all the DOM elements that matches css selector.
  Returns empty list in case there is no such element.

- **element-by-id [driver] id**
  Finds the first DOM element with the given id.

- **element-by-id [driver] id**
  Finds all the element with the id.

- **element-by-class-name [driver] class-name**
  Finds the first element of the class.

- **elements-by-class-name [driver] class-name**
  Finds all the element of the class.

- **element-by-tag-name [driver] tag**
  Finds the first element with the tag.

- **elements-by-tag-name [driver] tag**
  Finds all the elements with the tag.

### Element Interaction

- **text element**
  Gets the text content of the element.

- **attribute element name**
  Gets the value of the element's attribute.
  TODO what if there is no such attribute

- **click element**
  Simulates user clicking the element.

- **send-keys element text**
  Simulates user typing the text with the focus on the element.


