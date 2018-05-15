package com.sap.scala.demo

import akka.actor.{ActorSystem, Props}
import com.sap.scala.demo.actors._
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

/***********************************************************************************************************************
  * Top level object
  */
@JSExportTopLevel("Weather2")
object Weather2 {
  private val objName     = "Weather2"
  private val traceActive = true
  private val trace       = Trace.flow(traceActive)(objName)(_: String)(_: scala.Option[Boolean])
  private val traceInfo   = Trace.flowInfo(traceActive)(objName)(_: String)(_: String)

  private val enter = scala.Option(true)
  private val exit  = scala.Option(false)

  private val system = ActorSystem("weather-ui")

  /*********************************************************************************************************************
    * Entry point
    * Invoked from JavaScript in index.html
    */
  @JSExport
  def main(browserName: String, owmApiKey: String): Unit = {
    val fnName = "main"
    trace(fnName, enter)

    // The Open Weather Map API key must be supplied as the 2nd parameter to Weather.main() from the HTML page
    Utils.setOwmApiKey(owmApiKey)

    try   { dramatisPersonae(browserName) }
    catch { case th: Throwable => th.printStackTrace() }

    trace(fnName, exit)
  }

  /*********************************************************************************************************************
    * What actors will make up tonight's show?
    */
  def dramatisPersonae(browserName: String): Unit = {
    val fnName = "dramatisPersonae"

    trace(fnName, enter)
    traceInfo(fnName,s"Running in ${browserName}")

    // Has the user updated the source code with their own OpenWeatherMap API Key?
    if (Utils.owmApiKeyInstalled) {
      traceInfo(fnName, s"API Key test passed: ${Utils.getOwmApiKey}")

      // Yup, so create actors for handling map events, JSON requests and building weather reports
      StageManager.searchActor        = system.actorOf(Props(new SearchActor()),        MessageBox.actor_search)
      StageManager.searchResultsActor = system.actorOf(Props(new SearchResultsActor()), MessageBox.actor_search_results)
      StageManager.mapActor           = system.actorOf(Props(new MapActor()),           MessageBox.actor_map)
      StageManager.fetchJsonActor     = system.actorOf(Props(new FetchJsonActor()),     MessageBox.actor_fetch_json)
      StageManager.weatherReportActor = system.actorOf(Props(new WeatherReportActor()), MessageBox.actor_weather_report)

      // Initialise the actors
      StageManager.searchActor ! MessageBox.Initialise
      StageManager.mapActor    ! MessageBox.Initialise
    }
    else {
      // Nope, the Open Weather Map API key is missing, so this app will remain non-functional
      traceInfo(fnName, s"API Key test failed: ${Utils.getOwmApiKey}")
      DOMUtils.apiKeyMissing
    }

    trace(fnName, exit)
  }
}



