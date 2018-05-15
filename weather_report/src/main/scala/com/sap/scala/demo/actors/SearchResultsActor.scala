package com.sap.scala.demo.actors

import akka.actor.Actor
import org.scalajs.dom.Event
import org.scalajs.dom.html.Select
import com.sap.scala.demo._
import org.scalajs.dom

import scala.scalajs.js

/***********************************************************************************************************************
  * Actor to handle the input field and associated datalist of cities
  */
class SearchResultsActor extends Actor {
  private val className = "SearchResultsActor"
  private val fnName    = "receive"

  private val traceActive = true
  private val traceMsg    = Trace.flowMsg(traceActive)(className)(fnName)(_: String)
  private val traceInfo   = Trace.flowInfo(traceActive)(className)(fnName)(_: String)

  /*********************************************************************************************************************
    * Wait for messages
    */
  def receive = {
    /*******************************************************************************************************************
      * Client event
      * The user has selected a city
      */
    case evt: Event =>
      traceMsg(s"dom.Event ${evt.`type`} detected from ${evt.target.toLocaleString}")

      // Get a reference to the region datalist and input fields
      val dd_list = DOMUtils.elementById[Select](MessageBox.searchResultsSelect)

      val idx = dd_list.selectedIndex

      var lat = ""
      var lng = ""

      // Has the user selected a city?
      if (idx > -1) {
        var thisOpt = dd_list.options.apply(idx)

        lat = thisOpt.getAttribute("lat")
        lng = thisOpt.getAttribute("lng")

        StageManager.thisCountryIso = thisOpt.getAttribute("iso2")
        StageManager.thisCity       = thisOpt.getAttribute("city")
        StageManager.thisAdmin1     = thisOpt.getAttribute("admin1")
        StageManager.thisAdmin2     = thisOpt.getAttribute("admin2")

        // Yup, so fetch the weather information for the selected location
        traceInfo(s"Fetching weather information for ${thisOpt.text} at ($lat,$lng)")

        StageManager.mapActor           ! MessageBox.RepositionMapMsg(lat, lng)
        StageManager.weatherReportActor ! MessageBox.ClearList
        StageManager.fetchJsonActor     ! MessageBox.FetchJsonMsg(
          StageManager.weatherReportActor,
          Utils.getOwmUrlForLatLng(lat, lng)
        )
      }
      // Nope, so has the city name input field been blanked out?
//      else if (input.textContent.size == 0) {
//        // Yup, so remove the current weather report and reset the city name
//        StageManager.weatherReportActor ! MessageBox.ClearList
//        StageManager.thisCity = ""
//
//        // If this country has no regions (I.E. only a city list), then when the city name is blanked out, reset the
//        // map view to the country view
//        if (StageManager.thisRegion == "")
//          StageManager.mapActor ! MessageBox.ShowCountryMapMsg
//      }

    /*******************************************************************************************************************
      * JSON response handler for incoming list of cities
      */
    case MessageBox.JsonResponseMsg(results) =>
      traceMsg("JsonResponseMsg")

      // Did we get any results?
      if (results.length.asInstanceOf[Int] == 0) {
        // Nope, so clear the city list
        self ! MessageBox.ClearList
      }
      // Have we got a single search result?
      else if (results.length.asInstanceOf[Int] == 1) {
        // Yup, so there's no reason to build a city list.
        self ! MessageBox.ClearList

        // Immediately display the weather information for the only city in this region
        val city = new City(results.asInstanceOf[js.Array[js.Dynamic]].head)

        // Reposition map on the city and request the weather information
        StageManager.thisCountryIso = city.countryCode
        StageManager.thisCity       = city.name
        StageManager.thisAdmin1     = city.admin1Txt
        StageManager.thisAdmin2     = city.admin2Txt

        StageManager.mapActor        ! MessageBox.RepositionMapMsg(city.lat.toString, city.lng.toString)
        StageManager.fetchJsonActor  ! MessageBox.FetchJsonMsg(
          StageManager.weatherReportActor,
          Utils.getOwmUrlForLatLng(city.lat, city.lng)
        )

        traceInfo(s"Found one city: ${StageManager.thisCity}, ${StageManager.thisAdmin2}, ${StageManager.thisAdmin1}")
      }
      else {
        // Multiple search results found
        DOMUtils.buildSearchResults(self, results)
      }

    /*******************************************************************************************************************
      * Throw away the city input field, its associated datalist and the old weather report
      */
    case MessageBox.ClearList =>
      traceMsg("ClearList")

      StageManager.weatherReportActor ! MessageBox.ClearList

      DOMUtils.deleteChildByName(MessageBox.searchResultsDiv, MessageBox.searchResultsSelect)
  }
}
