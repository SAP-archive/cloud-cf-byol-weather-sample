/**
 * =====================================================================================================================
 * Fetch the server status information
 * 
 * Author : Chris Whealy    chris.whealy@sap.com
 * =====================================================================================================================
 **/

var serverTable = []

const url_country_manager_cmd = "/country_manager_cmd"
const url_country_server_cmd  = "/country_server_cmd"
const url_server_status       = "/server_status"

/* =====================================================================================================================
 * XHR requests
 */

// ---------------------------------------------------------------------------------------------------------------------
// Server status command
const fetch_server_info = () => {
  var xhr = new XMLHttpRequest()

  xhr.open("GET", url_server_status, true)

  xhr.onload = evt => {
    if (xhr.readyState === 4) {
      if (xhr.status === 200) {
        show_status(xhr.responseText)
      }
      else {
        show_error(xhr.statusText)
      }
    }
  }

  xhr.onerror = evt => show_error(xhr.statusText)

  xhr.send();
}

// ---------------------------------------------------------------------------------------------------------------------
// Country manager commands
// ---------------------------------------------------------------------------------------------------------------------
const startAllServers     = () => sendCountryManagerCmd("start_all")
const stopAllServers      = () => sendCountryManagerCmd("shutdown_all")
const resetCrashedServers = () => sendCountryManagerCmd("reset_all")

const sortAscending  = col_name => sendCountryManagerCmd("sort_ascending", col_name)
const sortDescending = col_name => sendCountryManagerCmd("sort_descending", col_name)

// ---------------------------------------------------------------------------------------------------------------------
// Toggle country manager debug status
// ---------------------------------------------------------------------------------------------------------------------
const toggleCountryManagerDebug = state       => sendCountryManagerCmd("set_debug", state)
const toggleCountryServerDebug  = (cc, state) => sendCountryServerCmd(cc, (state ? "trace_on" : "trace_off"))

// ---------------------------------------------------------------------------------------------------------------------
// Send a command to the country manager
// ---------------------------------------------------------------------------------------------------------------------
const sendCountryManagerCmd = (cmd, param) => {
  var xhr = new XMLHttpRequest()

  if (isNullOrUndef(param))
    xhr.open("GET", url_country_manager_cmd + "?cmd=" + cmd, true)
  else
    xhr.open("GET", url_country_manager_cmd + "?cmd=" + cmd + "&param=" + param, true)

  xhr.onload = evt => {
    if (xhr.readyState === 4) {
      if (xhr.status === 200) {
        var cmdResponseObj = JSON.parse(xhr.responseText)

        // Was the command successful?
        if (cmdResponseObj.status && cmdResponseObj.status === "error") {
          // Nope, something went wrong
          alert(cmdResponseObj.from_server + " " +
                cmdResponseObj.cmd + " " +
                cmdResponseObj.status + " " +
                cmdResponseObj.reason)
        }
        // We've received a payload that is assumed to be a list of country servers
        else if (isArray(cmdResponseObj.payload))
          document.getElementById("country_servers").innerHTML = htmlGenElement(build_server_table(cmdResponseObj.payload))
      }
      else {
        show_error(xhr.statusText)
      }
    }
  }

  xhr.onerror = evt => show_error(xhr.statusText)

  xhr.send();
}

// ---------------------------------------------------------------------------------------------------------------------
// Send commands to individual country servers
// ---------------------------------------------------------------------------------------------------------------------
const startServer = cc => sendCountryServerCmd(cc,"start")
const stopServer  = cc => sendCountryServerCmd(cc,"shutdown")
const resetServer = cc => sendCountryServerCmd(cc,"reset")

const sendCountryServerCmd = (cc,cmd) => {
  var xhr = new XMLHttpRequest()

  xhr.open("GET", url_country_server_cmd + "?country_code=" + cc + "&cmd=" + cmd, true)

  xhr.onload = evt => {
    if (xhr.readyState === 4) {
      if (xhr.status === 200) {
        var cmdResponseObj = JSON.parse(xhr.responseText)

        if (cmdResponseObj.status && cmdResponseObj.status === "error") {
          alert(cmdResponseObj.from_server + " " +
                cmdResponseObj.cmd + " " +
                cmdResponseObj.status + " " +
                cmdResponseObj.reason)
        }
        else {
          if (cmdResponseObj.cmd === "start"    ||
              cmdResponseObj.cmd === "shutdown" ||
              cmdResponseObj.cmd === "reset") {
            var newRec = cmdResponseObj.payload
            var oldRec = document.getElementById(newRec.name)

            oldRec.innerHTML = build_table_columns(newRec).map(htmlGenElement).join('\n')
          }
        }
      }
      else {
        show_error(xhr.statusText)
      }
    }
  }

  xhr.onerror = evt => show_error(xhr.statusText)

  xhr.send();
}

/* =====================================================================================================================
 * Display results from server
 */
const show_status = responseText => {
  var serverObj    = JSON.parse(responseText)
  var serverStatus = document.getElementById("server_status")

  var cbRow   = build_cm_trace_checkbox_row(serverObj.country_manager_trace)
  var memRow  = build_memory_usage_row(serverObj.erlang_memory_usage)
  var cityRow = build_city_total_row(serverObj.servers.reduce(city_total, 0))

  var info_table = htmlElement("table", null, [cbRow, memRow, cityRow])
  var cmdBtnsDiv = htmlElement("div", null, build_cmd_buttons(serverObj.servers))

  document.getElementById("server_status").innerHTML =
    htmlGenElement(info_table) +
    htmlGenElement(cmdBtnsDiv)

  document.getElementById("country_servers").innerHTML = htmlGenElement(build_server_table(serverObj.servers))
}

const show_error = statusText => alert(statusText)

// ---------------------------------------------------------------------------------------------------------------------
// Create HTML elements for the server report
// ---------------------------------------------------------------------------------------------------------------------
const build_cm_trace_checkbox_row = state => {
  var id = "country_manager_trace"

  var cbParams = [
      htmlParam("name", id)
    , htmlParam("id",   id) 
    , htmlParam("type", "checkbox") 
    , htmlParam("onclick", "toggleCountryManagerDebug(this.checked)") 
  ]

  if (state === "true") cbParams.push(htmlParam("checked"))

  var traceCB    = htmlElement('input', cbParams)
  var traceLabel = htmlElement('label', htmlParam("htmlFor", id), "Country manager debug trace")

  var traceCheckBoxTD = htmlElement("td", TD_ALIGN("left"),  traceCB)
  var traceLabelTD    = htmlElement("td", TD_ALIGN("right"), traceLabel)

  return htmlElement("tr", null, [traceLabelTD, traceCheckBoxTD])
}

const build_trace_checkbox = (cc, state) => {
  var id = "country_server_" + cc + "_trace"

  var cbParams = [
      htmlParam("name", id)
    , htmlParam("id",   id) 
    , htmlParam("type", "checkbox") 
    , htmlParam("onclick", "toggleCountryServerDebug('" + cc + "',this.checked)") 
  ]

  if (state === "true") cbParams.push(htmlParam("checked"))

  return htmlElement('input', cbParams)
}

const build_memory_usage_row = mem =>
  htmlElement("tr",
              null,
              [htmlElement("td", TD_ALIGN("right"), "Erlang runtime memory usage"),
               htmlElement("td", TD_ALIGN("left"), mem)])

const build_city_total_row = cities =>
  htmlElement("tr",
              null,
              [htmlElement("td", TD_ALIGN("right"), "Total number of cities"),
               htmlElement("td", TD_ALIGN("left"), cities)])

const build_cmd_buttons = serverList => {
  var retVal = []
  var startBtn = htmlElement('button',
                             [htmlParam("onclick","startAllServers()")],
                             "Start all servers")
  retVal.push(startBtn)

  var stopBtn  = htmlElement('button',
                             [htmlParam('onclick','stopAllServers()')],
                             "Stop all servers")
  retVal.push(stopBtn)

  if (serverList.reduce((acc, svr) => svr.status === "crashed" || acc, false)) {
    retVal.push(htmlElement('button', [htmlParam('onclick','resetCrashedServers()')], "Reset crashed servers"))
  }

  return retVal
}

const sortable_column = (col_heading, col_name) => {
  var sort_asc  = htmlElement("img",[htmlParam("onclick","sortAscending('" + col_name + "')"),
                                     htmlParam("src","/img/up.png"),
                                     htmlParam("title","Sort Ascending")])
  var sort_desc = htmlElement("img",[htmlParam("onclick","sortDescending('" + col_name + "')"),
                                     htmlParam("src","/img/down.png"),
                                     htmlParam("title","Sort Descending")])

  return htmlElement("div", htmlParam("style","display: inline;"), [col_heading, sort_asc, sort_desc])
}

// ---------------------------------------------------------------------------------------------------------------------
// Create HTML elements for server status table
// ---------------------------------------------------------------------------------------------------------------------
const city_total = (acc, country) => acc += country.city_count

const build_table_by_continent = servers => { }

const build_server_table = servers => {
  var table_hdr  = build_column_headers()
  var table_body = servers.map(build_table_row)

  return htmlElement("table", null, [table_hdr].concat(table_body))
}

const build_table_row = country => {
  return htmlElement("tr",
                     [htmlParam("id","country_server_" + country.country_code.toLowerCase())],
                     build_table_columns(country))
}

const build_table_columns = country => 
  [ htmlElement("td", [TD_ALIGN("center")], build_action_button(country))
  , htmlElement("td", [TD_ALIGN("center")], build_trace_checkbox(country.country_code, country.trace))
  , htmlElement("td", [status_colour(country.status, country.substatus)], get_continent_name(country.continent))
  , htmlElement("td", [status_colour(country.status, country.substatus)], country.country_name)
  , htmlElement("td", [TD_ALIGN("center"), status_colour(country.status, country.substatus)], country.country_code)
  , htmlElement("td", [TD_ALIGN("center"), status_colour(country.status, country.substatus)], country.status)
  , htmlElement("td", [TD_ALIGN("center"), status_colour(country.status, country.substatus)], country.substatus)
  , htmlElement("td", [TD_ALIGN("right")], country.progress)
  , htmlElement("td", [TD_ALIGN("right")], country.city_count)
  , htmlElement("td", [TD_ALIGN("right")], length(country.children))
  , htmlElement("td", [TD_ALIGN("right")], format_as_binary_units(country.mem_usage))
  , htmlElement("td", [TD_ALIGN("right")], country.started_at)
  , htmlElement("td", [TD_ALIGN("right")], ms_to_seconds(country.startup_time))
  ]

// ---------------------------------------------------------------------------------------------------------------------
const build_action_button = (country) => {
  var btn = htmlElement('button', [])

  if (country.status === "crashed") {
    btn.content = "Reset"
    btn.params.push(htmlParam('onclick','resetServer(\'' + country.country_code + '\')'))
  }
  else
    if (country.substatus === "running") {
      btn.content = "Stop"
      btn.params.push(htmlParam('onclick','stopServer(\'' + country.country_code + '\')'))
    }
    else {
      btn.content = "Start"
      btn.params.push(htmlParam('onclick','startServer(\'' + country.country_code + '\')'))
    }

  return btn
}

// ---------------------------------------------------------------------------------------------------------------------
const build_continent_header = ContName => htmlElement('tr', null, htmlElement('th', htmlParam('colspan', '12'), ContName))

// ---------------------------------------------------------------------------------------------------------------------
const build_column_headers = () =>
  htmlElement('tr', null, [ htmlElement('th', [TH_TEXT_COLOUR("white"), BG_BLUE()], 'Action')
                          , htmlElement('th', [TH_TEXT_COLOUR("white"), BG_BLUE()], 'Trace')
                          , htmlElement('th', [TH_TEXT_COLOUR("white"), BG_BLUE()], sortable_column('Continent', 'continent'))
                          , htmlElement('th', [TH_TEXT_COLOUR("white"), BG_BLUE()], sortable_column('Country', 'country_name'))
                          , htmlElement('th', [TH_TEXT_COLOUR("white"), BG_BLUE()], sortable_column('ISO', 'country_code'))
                          , htmlElement('th', [TH_TEXT_COLOUR("white"), BG_BLUE()], 'Status')
                          , htmlElement('th', [TH_TEXT_COLOUR("white"), BG_BLUE()], 'Substatus')
                          , htmlElement('th', [TH_TEXT_COLOUR("white"), BG_BLUE()], 'Progress')
                          , htmlElement('th', [TH_TEXT_COLOUR("white"), BG_BLUE()], sortable_column('City Count', 'city_count'))
                          , htmlElement('th', [TH_TEXT_COLOUR("white"), BG_BLUE()], 'City Servers')
                          , htmlElement('th', [TH_TEXT_COLOUR("white"), BG_BLUE()], sortable_column('Memory Usage', 'mem_usage'))
                          , htmlElement('th', [TH_TEXT_COLOUR("white"), BG_BLUE()], 'Started At')
                          , htmlElement('th', [TH_TEXT_COLOUR("white"), BG_BLUE()], sortable_column('Startup Time', 'startup_time'))
                          ])

// ---------------------------------------------------------------------------------------------------------------------
var status_colour = (status, substatus) =>
  (status === "started")
   ? (substatus === "running")
      ? BG_GREEN()
      : BG_YELLOW()
   : (status === "starting")
      ? BG_YELLOW()
      : (status === "crashed")
         ? BG_RED()
         : BG_LIGHT_GREY()

/* =====================================================================================================================
 * HTML element functions
 */
const emptyElements = ['area',  'base',    'basefont', 'br',   'col',   'frame',   'hr',       'img',
                       'input', 'isindex', 'link',     'meta', 'param', 'command', 'keygen',   'source']
const isEmpty = tagName => emptyElements.indexOf(tagName) >= 0

const htmlElement = (tagName, params, content) => {
  return {
    tagName : tagName,
    params  : params,
    content : content,
    isEmpty : isEmpty(tagName)
  }
}

const htmlParam = (n, v) => { return {name: n, value: v} }

/* =====================================================================================================================
 * HTML generation functions
 */
const htmlGenTagParam  = param  => param.name + (isNullOrUndef(param.value) ? '' : '="' + param.value + '"')
const htmlGenTagParams = params => (isArray(params)) ? (' ' + aggregateParams(params).map(htmlGenTagParam).join(' ')) : ''

const aggregateParams = params => {
  var retVal = []
  
  if (params.length > 0) {
    var paramMap = new Map()
    paramMap.set(params[0].name, params[0].value)

    for (var i=1; i<params.length; i++) {
      var thisVal = paramMap.get(params[i].name)
      paramMap.set(params[i].name, (thisVal === undefined ? "" : thisVal + " ") + params[i].value)
    }

    paramMap.forEach((v, k) => retVal.push(htmlParam(k,v)))
  }

  return retVal
}

/**
 * Generate an HTML element from a tagName, an array of parameter objects and some optional content
 */
const htmlGenElement = el => {
  var html = ""

  // Does the element have anything in it?
  if (!isNullOrUndef(el)) {
    // Is the element a simple string?
    if (typeof el === "string" || typeof el === "number") {
      html = el
    }
    // We assume then that the content is either a single HTML element instance, or an array of HTML element objects
    else {
      // Add the element's start tag
      html += '<' + el.tagName + htmlGenTagParams(el.params) + '>'

      // If we've got just a single object, then wrap it in an array
      if (!isArray(el.content)) {
        el.content = [el.content]
      }

      // Generate the element's content then add an optional closing tag
      html += el.content.map(htmlGenElement).join('\n') + ((el.isEmpty) ? '\n' : '</' + el.tagName + '>\n')
    }
  }

  return html
}

/* =====================================================================================================================
 * Utility functions
 */
const isArray       = obj => !!obj && obj.constructor === Array
const isNullOrUndef = obj => obj === null || obj === undefined || obj === "null" || obj === "undefined"
const length        = obj => isNullOrUndef(obj) ? 0 : obj.length

const BG_RED        = () => htmlParam("style", "background-color: #EE4466;")
const BG_GREEN      = () => htmlParam("style", "background-color: #90EE90;")
const BG_BLUE       = () => htmlParam("style", "background-color: #1478DB;")
const BG_YELLOW     = () => htmlParam("style", "background-color: #FFFF00;")
const BG_LIGHT_GREY = () => htmlParam("style", "background-color: #EEEEEE;")

const TD_ALIGN = dir => htmlParam("style", "text-align: " + dir + ";")

const TH_TEXT_COLOUR = col => htmlParam("style", "color: " + col + ";")

const BR = htmlElement("br")



const KB = 1024
const MB = 1048576
const GB = 1073741824

const format_as_binary_units = n =>
  (isNullOrUndef(n))
  ? "0 bytes"
  : (n < KB)
    ? n + " bytes"
    : (n < MB)
      ? format_as_binary_units_int(n, KB, "Kb")
      : (n < GB)
        ? format_as_binary_units_int(n, MB, "Mb")
        : format_as_binary_units_int(n. GB, "Gb")


const format_as_binary_units_int = (n, unit, unitStr) => {
  var wholeUnits = Math.floor(n / unit)
  var rem = (n - (wholeUnits * unit)) / unit

  return parseFloat(wholeUnits + rem).toFixed(3) + " " + unitStr
}

const ms_to_seconds = ms => isNullOrUndef(ms) ? "" : parseFloat(ms).toFixed(3) + "s"


const get_continent_name = cn =>
  (cn === "AF") ? "Africa"
: (cn === "AN") ? "Antarctica"
: (cn === "AS") ? "Asia"
: (cn === "EU") ? "Europe"
: (cn === "NA") ? "North America"
: (cn === "OC") ? "Oceana"
: (cn === "SA") ? "South America"
: "Unknown: " + cn
