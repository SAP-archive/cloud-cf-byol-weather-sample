package com.sap.scala.demo

import com.felstar.scalajs.leaflet._
import org.scalajs.dom

/***********************************************************************************************************************
  * Object to handle all aspects of map manipulation
  */
object Maps {
  private val objName = "Maps"

  private val traceActive = false
  private val trace       = Trace.flow(traceActive)(objName)(_: String)(_: Option[Boolean])
  private val traceInfo   = Trace.flowInfo(traceActive)("DOMUtils")(_: String)(_: String)

  private val enter = Option(true)
  private val exit  = Option(false)

  private val worldZoomLevel: Int = 1
  private val cityZoomLevel:  Int = 14

  private val startPos = L.latLng(0.0,0.0)

  private var lastRect: Rectangle = null

  private def mapBoxHost     = "https://api.tiles.mapbox.com"
  private def mapBoxEndpoint = mapBoxHost + "/v4/{id}/{z}/{x}/{y}.png"

  private var mbQueryParams  = scala.collection.mutable.Map[String,String](
    "access_token" -> "pk.eyJ1IjoiZmFuY2VsbHUiLCJhIjoiY2oxMHRzZm5zMDAyMDMycndyaTZyYnp6NSJ9.AJ3owakJtFAJaaRuYB7Ukw"
  )


  /*********************************************************************************************************************
    * Map "click" event handler
    */
  private val handleMapClick = (evt: LeafletMouseEvent) => {
    // Allow clicks directly on the map
    StageManager.thisLat = evt.latlng.lat.toString
    StageManager.thisLng = (evt.latlng.lng % 180).toString

    traceInfo("worldMap", s"Fetching weather information for location (${StageManager.thisLat},${StageManager.thisLng})")

    // Clear any existing weather report and fetch the weather information for the selected location
    StageManager.weatherReportActor ! MessageBox.ClearList
    StageManager.fetchJsonActor     ! MessageBox.FetchJsonMsg(
      StageManager.weatherReportActor,
      Utils.getOwmUrlForLatLng(StageManager.thisLat, StageManager.thisLng)
    )
  }

  /*********************************************************************************************************************
    * Create a slippy map showing the whole world
    *
    * This function must only be called once
    */
  def worldMap(mapDiv: String): LMap = {
    val fnName = "worldMap"
    trace(fnName, enter)

    // Show map of whole world
    val mapRef = L.map(mapDiv, LMapOptions.zoom(worldZoomLevel).center(startPos))

    val queryStr = (
      for (p <- mbQueryParams.keys)
        yield s"$p=${mbQueryParams.get(p).get}"
      ).mkString("?", "&", "")

    // Add map tile layer
    L.tileLayer(
      mapBoxEndpoint + queryStr,
      TileLayerOptions.
        id("mapbox.streets").
        minZoom(worldZoomLevel).
        maxZoom(19).
        attribution(
          """Map data &copy; <a href="http://openstreetmap.org">OpenStreetMap</a> contributors,
            | Found a map problem? <a href="http://openstreetmap.org/fixthemap">Fix the map</a>,
            | <a href="http://creativecommons.org/licenses/by-sa/2.0/">CC-BY-SA</a>,
            | Imagery © <a href="http://mapbox.com">Mapbox</a>""".stripMargin)
    ).addTo(mapRef)

    // Add click event handler to the map
    mapRef.on(LMapEvent.click.toString, handleMapClick)

    trace(fnName, exit)
    mapRef
  }

  /*********************************************************************************************************************
    * Slide the slippy map to show the selected country
    */
  def slideMapToCountry(): Unit = {
    val fnName = "slideMapToCountry" 
    trace(fnName, enter)

    // Remove any previous country rectangle
    if (lastRect != null)
      StageManager.mapRef.removeLayer(lastRect)

    // Create a rectangle around the selected country
    lastRect = L.rectangle(StageManager.thisCountryBox.bounds, PolylineOptions.fill(false))

    StageManager.mapRef.addLayer(lastRect)
    StageManager.mapRef.fitBounds(StageManager.thisCountryBox.bounds)

    trace(fnName, exit)
  }

  /*********************************************************************************************************************
    * Slide the slippy map to show the selected lat/lng
    */
  def slideMapToLatLng(lat: String, lng: String): Unit = {
    val fnName = "slideMapToLatLng"
    trace(fnName, enter)

    val lat1 = Utils.parseDouble(lat) match { case Some(d) => d; case None => 0.0 }
    val lng1 = Utils.parseDouble(lng) match { case Some(d) => d; case None => 0.0 }

    slideMapToLatLng(lat1, lng1)
    trace(fnName, exit)
  }

  def slideMapToLatLng(lat: Double, lng: Double): Unit = {
    val fnName = "slideMapToLatLng"
    trace(fnName, enter)

    // Lat/Lng of (0.0,0.0) will show up anytime there is a problem translating the string lat/lng value to double
    if (lat == 0.0 && lng == 0.0)
      resetMap
    else
      StageManager.mapRef.flyTo(L.latLng(lat, lng), cityZoomLevel)

    trace(fnName, exit)
  }

  /*********************************************************************************************************************
    * Show the whole world
    */
  def resetMap(): Unit = {
    trace("resetMap", None)

    // Remove any previous rectangle around a country
    if (lastRect != null) {
      StageManager.thisCountryBox = null
      StageManager.mapRef.removeLayer(lastRect)
      lastRect = null
    }

    StageManager.mapRef.setView(startPos, worldZoomLevel)
  }
}
