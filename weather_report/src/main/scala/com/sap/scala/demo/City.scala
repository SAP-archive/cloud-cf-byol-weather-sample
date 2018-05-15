package com.sap.scala.demo

import scala.scalajs.js

class City(cityInfo: js.Dynamic) extends Ordered[City] {
  override def toString = name

  def compare(that: City) = {
    if (this.country == that.country)
      if (this.admin1Txt == that.admin1Txt)
        if (this.admin2Txt == that.admin2Txt)
          if (this.admin3Txt == that.admin3Txt)
            if (this.admin4Txt == that.admin4Txt)
              0
            else if (this.admin4Txt > that.admin4Txt)
              1
            else
              -1
          else if (this.admin3Txt > that.admin3Txt)
            1
          else
            -1
        else if (this.admin2Txt > that.admin2Txt)
          1
        else
          -1
      else if (this.admin1Txt > that.admin1Txt)
        1
      else
        -1
    else if (this.country > that.country)
      1
    else
      -1
  }

  val name         = cityInfo.name.asInstanceOf[String].replaceAll("^\"|\"$", "")
  val lat          = cityInfo.lat.asInstanceOf[Double]
  val lng          = cityInfo.lng.asInstanceOf[Double]
  val featureClass = cityInfo.featureClass.asInstanceOf[String]
  val featureCode  = cityInfo.featureCode.asInstanceOf[String]
  val countryCode  = cityInfo.countryCode.asInstanceOf[String]
  val timezone     = cityInfo.timezone.asInstanceOf[String]
  val country      = CountryList.getName(this.countryCode)
  val admin1Txt    = cityInfo.admin1Txt.asInstanceOf[String]
  val admin2Txt    = cityInfo.admin2Txt.asInstanceOf[String]
  val admin3Txt    = cityInfo.admin3Txt.asInstanceOf[String]
  val admin4Txt    = cityInfo.admin4Txt.asInstanceOf[String]
}
