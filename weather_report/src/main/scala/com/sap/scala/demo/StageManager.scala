package com.sap.scala.demo

import akka.actor.ActorRef
import com.felstar.scalajs.leaflet.LMap

/***********************************************************************************************************************
  * Global reference object
  */
object StageManager {
  // Map reference
  var mapRef: LMap = null
  var placeholder = "Select a city"

  // List of all the actors involved in this performance
  var searchActor:        ActorRef = null
  var searchResultsActor: ActorRef = null
  var fetchJsonActor:     ActorRef = null
  var mapActor:           ActorRef = null
  var weatherReportActor: ActorRef = null

  // The currently selected country, region, city and lat/lng
  //var thisCountry:    String = ""
  var thisAdmin1:     String = ""
  var thisAdmin2:     String = ""
  var thisCountryBox: GeoBox = null
  var thisCountryIso: String = ""
  var thisCity:       String = ""
  var thisLat:        String = ""
  var thisLng:        String = ""
}
