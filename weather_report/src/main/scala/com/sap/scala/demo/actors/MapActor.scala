package com.sap.scala.demo.actors

import akka.actor.Actor
import com.sap.scala.demo._

/***********************************************************************************************************************
  * Actor to handle creating a slippy map for the selected country
  */
class MapActor extends Actor {
  private val className = "MapActor"
  private val fnName    = "receive"

  private val traceActive = false
  private val traceMsg    = Trace.flowMsg(traceActive)(className)(fnName)(_: String)

  /** *******************************************************************************************************************
    * Wait for messages
    */
  def receive = {
    /** *****************************************************************************************************************
      *  Initialise - create the world map
      */
    case MessageBox.Initialise =>
      traceMsg("Initialise")
      StageManager.mapRef = Maps.worldMap(MessageBox.mapDiv)

    /** *****************************************************************************************************************
      *  Show the selected country
      */
    case MessageBox.ShowCountryMapMsg =>
      traceMsg("ShowCountryMapMsg")
      Maps.slideMapToCountry

    /** *****************************************************************************************************************
      *  Show the selected lat/lng
      */
    case MessageBox.RepositionMapMsg(lat, lng) =>
      traceMsg("RepositionMapMsg")
      Maps.slideMapToLatLng(lat, lng)
  }
}
