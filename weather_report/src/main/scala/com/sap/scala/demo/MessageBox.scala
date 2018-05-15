package com.sap.scala.demo

import akka.actor.ActorRef
import scala.scalajs.js

/***********************************************************************************************************************
  * Central reference for all:
  *   o UI element names used by actors
  *   o Actor names
  *   o Messages sent between actors
  */
object MessageBox {
  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // UI elements referenced by the actors
  val searchDiv      = "search_div"
  val searchInput    = "search_input"
  val searchCheckBox = "search_checkbox"
  val searchGroup    = "search_group"
  val searchRadio1   = "search_radio_1"
  val searchRadio2   = "search_radio_2"

  val searchResultsDiv    = "search_results_div"
  val searchResultsTable  = "search_results_table"
  val searchResultsSelect = "search_results_select"

  val mapDiv = "world_map"

  val weatherReportDiv   = "weather_info_div"
  val weatherReportTable = "weather_info_table"

  val itemCountWarning = "item_count_warning"


  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // Actor names
  val actor_search         = "search_actor"
  val actor_search_results = "search_esults_actor"
  val actor_fetch_json     = "fetch_json_actor"
  val actor_map            = "map_actor"
  val actor_weather_report = "weather_report_actor"

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // Messages sent between actors
  case class         ClearList()
  case class      FetchJsonMsg(parentPid: ActorRef, url: String)
  case class        Initialise()
  case class   JsonResponseMsg(jsonData: js.Dynamic)
  case class ShowCountryMapMsg()
  case class  RepositionMapMsg(lat: String, lng: String)
}
