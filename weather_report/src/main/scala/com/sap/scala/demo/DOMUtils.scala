package com.sap.scala.demo

import akka.actor.ActorRef
import org.scalajs.dom.html._
import org.scalajs.dom.raw.HTMLElement
import org.scalajs.dom.{Event, Node, document, html}

import scala.collection.mutable.{ListBuffer, Map}
import scala.scalajs.js


/***********************************************************************************************************************
  * Utilities for accessing and manipulating the DOM
  */
object DOMUtils {
  private val objName = "DOMUtils"

  private val traceActive = true
  private val trace       = Trace.flow(traceActive)(objName)(_: String)(_: scala.Option[Boolean])
  private val traceInfo   = Trace.flowInfo(traceActive)(objName)(_: String)(_: String)

  private val enter = scala.Option(true)
  private val exit  = scala.Option(false)

  /*********************************************************************************************************************
    * Access DOM elements
    */
  def elementById[A <: js.Any](id: String): A = document.getElementById(id).asInstanceOf[A]

  /*********************************************************************************************************************
    * Hide/Reveal DOM elements
    */
  def hideElement[A <: HTMLElement](elName: String): Unit = elementById[A](elName).className = "hidden"
  def showElement[A <: HTMLElement](elName: String): Unit = elementById[A](elName).className = "visible"

  /*********************************************************************************************************************
    * Delete DOM elements
    */
  def deleteChildByName(parentId: String, childId: String): Unit =
    deleteChild(elementById(parentId), elementById(childId))

  def deleteChild(parentId: Node, childId: Node): Unit = {
    traceInfo("deleteChild", s"Deleting ${if (childId != null) childId.nodeName else "null"} from ${parentId.nodeName}")

    if (childId != null)
      parentId.removeChild(childId)
  }

  /*********************************************************************************************************************
    * Create DOM elements
    */
  def createElement[A <: js.Any](tagName: String): A = document.createElement(tagName).asInstanceOf[A]

  def break() = createElement[BR]("br")

  def createParagraph(txt: String): Paragraph = {
    var p = createElement[Paragraph]("p")
    p.textContent = txt
    p
  }

  def createImage(src: String): Image = {
    var img = createElement[Image]("img")
    img.setAttribute("src",src)
    img
  }

  def createInput(id: String): Input = {
    var el = createElement[Input]("input")
    el.id = id
    el
  }

  def createTextInput(inputId: String, placeholder: String): Input = {
    var el = createInput(inputId)
    el.setAttribute("type","text")
    el.setAttribute("placeholder",placeholder)
    el
  }

  def createRadioInput(inputId: String, name: String, checked: Boolean): Input = {
    var el = createInput(inputId)
    el.setAttribute("type","radio")
    el.setAttribute("name",name)

    el.checked = checked

    el
  }

  def createCheckBoxInput(inputId: String, name: String, checked: Boolean): Input = {
    var el = createInput(inputId)
    el.setAttribute("type","checkbox")
    el.setAttribute("name",name)

    el.checked = checked

    el
  }

  def createFieldSet(id: String): FieldSet = {
    var fs = createElement[FieldSet]("fieldset")
    fs.id = id
    fs
  }

  def createLabel(id: String, txt: String): Label = {
    var lbl = createElement[Label]("label")
    lbl.id = id
    lbl.textContent = txt
    lbl
  }

  def createCheckBoxLabelCombo(id: String, name: String, txt: String, checked: Boolean): (Input, Label) =
    (createCheckBoxInput(id, name, checked), createLabel(id, txt))

  def createRadioButtonLabelCombo(id: String, name: String, txt: String, checked: Boolean): (Input, Label) =
    (createRadioInput(id, name, checked), createLabel(id, txt))

  def createDropdownCombo(inputId: String, dlId: String, txt: String): (Input, DataList) = {
    var el1 = createElement[Input]("input")
    var el2 = createElement[DataList]("datalist")

    el1.id = inputId

    el1.setAttribute("type","text")
    el1.setAttribute("list",dlId)
    el1.setAttribute("placeholder",txt)

    el2.setAttribute("id",dlId)

    return (el1, el2)
  }

  def createSelect(id: String): Select = {
    var el = createElement[Select]("select")
    el.id  = id
    el
  }

  def createOptGroup(label: String): OptGroup = {
    var el   = createElement[OptGroup]("optgroup")
    el.label = label
    el
  }

  def createOption(city: City): Option = {
    var opt = createElement[Option]("option")

    // The admin 1 2 and 3 text fields for a city could be null
    opt.text = city.name +
      (if (city.admin3Txt != null) s", ${city.admin3Txt}" else "") +
      (if (city.admin2Txt != null) s", ${city.admin2Txt}" else "") +
      (if (city.admin1Txt != null) s", ${city.admin1Txt}" else "")

    opt.setAttribute("lat", city.lat.toString)
    opt.setAttribute("lng", city.lng.toString)
    opt.setAttribute("featureClass", city.featureClass)
    opt.setAttribute("featureCode", city.featureCode)
    opt.setAttribute("iso2", city.countryCode)
    opt.setAttribute("city", city.name)
    opt.setAttribute("admin1", if (city.admin1Txt == null) "" else city.admin1Txt)
    opt.setAttribute("admin2", if (city.admin2Txt == null) "" else city.admin2Txt)
    opt.setAttribute("tz", city.timezone)

    opt
  }

  def createPlaceholderOption(): Option = {
    var opt = createElement[Option]("option")
    opt.text = StageManager.placeholder
    opt
  }

  def createDiv(id: String): Div = {
    var el = createElement[Div]("div")
    el.id = id
    el
  }

  def createTableRow(): TableRow = createElement[TableRow]("tr")

  def createTableRow(city: City): TableRow = {
    var row = createElement[TableRow]("tr")

    row.appendChild(createTableCell(city.name))
    row.appendChild(createTableCell(city.lat.toString))
    row.appendChild(createTableCell(city.lng.toString))
    row.appendChild(createTableCell(city.featureClass))
    row.appendChild(createTableCell(city.featureCode))
    row.appendChild(createTableCell(city.countryCode))
    row.appendChild(createTableCell(city.timezone))

    row
  }

  def createTableCaption(title: String): TableCaption = {
    var c = createElement[TableCaption]("caption")
    c.textContent = title
    c
  }

  def createTableCell(content: Node): TableDataCell = {
    var el = createElement[TableDataCell]("td")
    el.appendChild(content)
    el
  }

  def createTableCell(text: String): TableDataCell = {
    var el = createElement[TableDataCell]("td")
    el.textContent = text
    el
  }

  def createTableCellWithId(text: String, id: String): TableDataCell = {
    var el = createTableCell(text)
    el.id = id
    el
  }

  // Build a row for the weather table
  def createWeatherTableRow(rowData: Map[String, String]): TableRow =
    rowData.keys.foldLeft(createTableRow()) { (acc: TableRow, key: String) =>
      acc.appendChild(createTableCellWithId(key,"label"))
      acc.appendChild(createTableCell(rowData.get(key).get))
      acc
    }

  /*********************************************************************************************************************
    * Build search input fields
    */
  def buildSearchInputFields(self: ActorRef): Unit = {
    val fnName = "buildSearchInputFields"
    trace(fnName, enter)

    val searchDiv = elementById[Div](MessageBox.searchDiv)

    val cityInput = createTextInput(MessageBox.searchInput, "Enter a city name")
    cityInput.addEventListener("keyup", (msg: Event) => self ! msg)
    searchDiv.appendChild(cityInput)

    val (cb0, lbl0) = createCheckBoxLabelCombo(MessageBox.searchCheckBox, MessageBox.searchCheckBox, "Whole word", true)

    cb0.addEventListener("click", (msg: Event) => self ! msg)

    searchDiv.appendChild(cb0)
    searchDiv.appendChild(lbl0)

    val radioFieldSet = createFieldSet(MessageBox.searchGroup)

    val (rb1, lbl1) = createRadioButtonLabelCombo(MessageBox.searchRadio1, MessageBox.searchGroup, "Starts with", true)
    val (rb2, lbl2) = createRadioButtonLabelCombo(MessageBox.searchRadio2, MessageBox.searchGroup, "Contains",    false)

    rb1.addEventListener("change", (msg: Event) => self ! msg)
    rb2.addEventListener("change", (msg: Event) => self ! msg)

    radioFieldSet.appendChild(rb1)
    radioFieldSet.appendChild(lbl1)

    radioFieldSet.appendChild(rb2)
    radioFieldSet.appendChild(lbl2)

    searchDiv.appendChild(radioFieldSet)

    trace(fnName, exit)
  }



  /*********************************************************************************************************************
    * Build dropdown list of search results
    */
  def buildSearchResults(self: ActorRef, searchResults: js.Dynamic): Unit = {
    val fnName = "buildSearchResults"
    trace(fnName, enter)

    // Get reference to the city HTML elements.
    // Only the city list div is guaranteed to exist
    val searchResultsDiv = elementById[Div](MessageBox.searchResultsDiv)

    // Throw away any existing search results dropdown list
    deleteChildByName(MessageBox.searchResultsDiv, MessageBox.searchResultsSelect)

    // Transform the search results into a sequence of city objects
    var cities: Seq[City] = Seq.empty

    searchResults.map { cityInfo: js.Dynamic => cities :+= new City(cityInfo) }

    traceInfo(fnName, s"Found ${cities.size} cities")

    var searchResultsSelect = createSelect(MessageBox.searchResultsSelect)
    searchResultsSelect.appendChild(createPlaceholderOption())

    var prevCountry: String = null
    var searchResultsOptGrp: OptGroup = null

    // Display cities sorted by name and grouped by country
    cities.sorted.foreach(city => {
      if (searchResultsOptGrp == null) {
        searchResultsOptGrp = createOptGroup(city.country)
        prevCountry = city.country
      }
      else if (city.country != prevCountry) {
        searchResultsSelect.appendChild(searchResultsOptGrp)
        searchResultsOptGrp = createOptGroup(city.country)
        prevCountry = city.country
      }

      searchResultsOptGrp.appendChild(createOption(city))
    })

    // Add last option group to select element
    searchResultsSelect.appendChild(searchResultsOptGrp)
    searchResultsSelect.addEventListener("change", (msg: Event) => self ! msg)

    searchResultsDiv.appendChild(searchResultsSelect)
    trace(fnName, exit)
  }

  /*********************************************************************************************************************
   * Build weather report
   */
  def buildWeatherReport(w: WeatherReportBuilder): Unit = {
    val fnName = "buildWeatherReport"
    trace(fnName, enter)
    traceInfo(fnName, s"Building weather report for " +
      s"${StageManager.thisCity}, " +
      s"${StageManager.thisAdmin2}, " +
      s"${StageManager.thisAdmin1}, " +
      s"${CountryList.getName(StageManager.thisCountryIso)}")

    showElement[Div](MessageBox.weatherReportDiv)

    var weatherDiv = elementById[Div](MessageBox.weatherReportDiv)

    var tab   = createElement[Table]("table")
    var tbody = tab.createTBody()
    var rows  = ListBuffer[Map[String,String]]()

    val captionTxt =
        "Weather for " +
        Utils.textListFirstItem(StageManager.thisCity) +
        Utils.textListItem(StageManager.thisAdmin2) +
        Utils.textListItem(StageManager.thisAdmin1) +
        Utils.textListItem(CountryList.getName(StageManager.thisCountryIso))

    tab.appendChild(createTableCaption(captionTxt))

    // Weather report table must be identifiable in order to delete it
    tab.id = MessageBox.weatherReportTable

    // Temperature is always supplied in Kelvin
    rows += Map("Temperature" -> Utils.kelvinToDegStr(w.main.temp, w.main.temp_min, w.main.temp_max))

    // Atmospheric pressure is supplied either as a single value, or as a
    // sea level/ground level pair
    if (w.main.grnd_level == 0)
      rows += Map("Atmospheric Pressure" -> Utils.formatPressure(w.main.airPressure))
    else {
      rows += Map("Atmospheric Pressure (Ground Level)" -> Utils.formatPressure(w.main.grnd_level))
      rows += Map("Atmospheric Pressure (Sea Level)"    -> Utils.formatPressure(w.main.sea_level))
    }

    rows += Map("Humidity"       -> Utils.formatPercentage(w.main.humidity))
    rows += Map("Visibility"     -> Utils.formatVisibility(w.visibility))
    rows += Map("Wind Speed"     -> Utils.formatVelocity(w.wind.speed))
    rows += Map("Wind Direction" -> Utils.formatHeading(w.wind.heading))
    rows += Map("Cloud Cover"    -> Utils.formatPercentage(w.clouds))

    // Transform each weather value into a table row and add them to the table body
    rows.
      map(createWeatherTableRow).
      map((r: TableRow) => tbody.appendChild(r))

    // Add the icons for (potentially) multiple weather conditions directly to
    // the table body
    w.weatherConditions.map((w: WeatherCond) => {
      var row = createTableRow()

      // Column 1 contains the weather condition's description
      // Column 2 contains the image for that weather condition
      row.appendChild(createTableCellWithId(Utils.formatDescription(w.desc), "label"))
      row.appendChild(createTableCell(createImage(Utils.getOwmImgUrl(w.icon))))

      tbody.appendChild(row)
    })

    tab.appendChild(tbody)
    weatherDiv.appendChild(tab)

    trace(fnName, exit)
  }

  /*********************************************************************************************************************
    * Show "API Key Missing" error message
    */
  def apiKeyMissing(): Unit = {
    trace("apiKeyMissing", None)
    showElement[Div]("missing_api_key")
  }

  /*********************************************************************************************************************
    * Show message for Safari users
    */
  def safariMsg(): Unit = {
    trace("safariMsg", None)
    showElement[Div]("safari_msg")
  }
}

