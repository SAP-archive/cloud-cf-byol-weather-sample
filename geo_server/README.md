# Geographic Information Server (acts as the backend server for the weather-report client)

## Cloud Foundry's "Bring Your Own Language" Concept

Java is most certainly one of the most popular development languages at the moment; however, Cloud Foundry offers developers the opportunity to create their own applications using almost ***any*** programming language.

The only factors constraining your choice of language are:

* Has your chosen language been implemented to run on Windows or Linux?
* Does your chosen language have a Cloud Foundry buildpack?

If the answer to the first question is "No", then unfortunately all bets are off and you must choose a different language.

If the answer to the second question is "No", then although you might have to do a little more work, this is certainly not a show-stopper.

## Cloud Foundry Buildpacks

Before developing an application on your local machine, you must first install the set of tools appropriate for compiling your source code and creating an executable application.  A buildpack is nothing more than a set of shell scripts that repeat this process in the Cloud Foundry runtime environment in order to compile and execute your application.


### Buildpacks Built-in To Cloud Foundry

Cloud Foundry provides built-in support for the following languages:

* [Java](https://github.com/cloudfoundry/java-buildpack)
* [Ruby](https://github.com/cloudfoundry/ruby-buildpack)
* [PHP](https://github.com/cloudfoundry/php-buildpack)
* [Go](https://github.com/cloudfoundry/go-buildpack)
* [Python](https://github.com/cloudfoundry/python-buildpack)

and runtime environments:

* [NodeJS (Server-side JavaScript)](https://github.com/cloudfoundry/nodejs-buildpack)
* [Web servers delivering static content (Via an NGINX Web server)](https://github.com/cloudfoundry/staticfile-buildpack)
* [Arbitrary binary Web servers](https://github.com/cloudfoundry/binary-buildpack)
* [.NET Core Applications](https://github.com/cloudfoundry/dotnet-core-buildpack)

### Community-Developed Buildpacks

If however, your favourite language is not listed here, then there is a range of community developed buildpacks from which to choose.  These include buildpacks for languages such as:

* [Clojure](https://github.com/mstine/heroku-buildpack-clojure)
* [Scala](https://github.com/heroku/heroku-buildpack-scala)
* [Haskell](https://github.com/BrianMMcClain/heroku-buildpack-haskell)
* [Erlang](https://github.com/ChrisWhealy/cf-buildpack-erlang)
* [Elixir](https://github.com/HashNuke/heroku-buildpack-elixir)
* [Rust](https://github.com/emk/heroku-buildpack-rust)
* [Swift](https://github.com/cloudfoundry-community/swift-buildpack)

There is even a [Generic Language](https://github.com/oetiker/sourcey-buildpack) buildpack that, using a system of plugins, can compile and build a wide range of languages.

The full list of community-developed buildpacks is listed on the [Cloud Foundry Community Buildpack](https://github.com/cloudfoundry-community/cf-docs-contrib/wiki/Buildpacks) Wiki on GitHub.com.

### Do-It-Yourself Buildpacks

You might however find yourself in the situation that the existing community buildpack for your chosen language has become obsolete or is broken.

This is the situation I encountered when writing in Erlang.  The existing community buildpack had not been updated in about 4 years, and was no longer able build my app using the latest version of Erlang's build tool.

Nonetheless, by following Cloud Foundry's [buildpack documentation](https://docs.cloudfoundry.org/buildpacks/understand-buildpacks.html), I was able adapt the obsolete buildpack quite easily in order to get it back into working order.

My [Erlang buildpack](https://github.com/ChrisWhealy/cf-buildpack-erlang) is now listed on the [Cloud Foundry Community Buildpack page](https://github.com/cloudfoundry-community/cf-docs-contrib/wiki/Buildpacks).


#  geo-server

## Overview

`geo-server` is a Geographic Information Server that provides a lookup service for any town or city worldwide having a population greater than 500.

The server accepts a text string of 3 or more characters as a search criteria and returns all towns and cities with a matching name from any countries in the world for which the corresponding country server has been started.

## Technical Overview

This application is developed in [Erlang](http://www.erlang.org/) and uses the following open source frameworks: [OTP](http://erlang.org/doc/),  [Cowboy](https://ninenines.eu/) and [iBrowse](https://github.com/cmullaparthi/ibrowse)

The geographic and geopolitical data supplied by this server is obtained from [GeoNames.org](http://www.geonames.org).

# Installation

## Clone Git Repository

Clone this Git repository onto your local machine

    $ git clone https://github.wdf.sap.corp/I003638/geo_server.git

Then edit the `manifest.yml` file and change the `route` parameter to point to your own Cloud Foundry server.


##  Deploy to Cloud Foundry

Deploy to Cloud Foundry using the following community build pack for Erlang:

    $ cf push -b https://github.com/ChrisWhealy/cf-buildpack-erlang

***IMPORTANT***  
For performance reasons, all the geographic information supplied by this server is held in memory; therefore, this app requires 2048Mb of memory.  The `geo-server` will probably not fully start if this memory allowance is reduced.

Once all the country servers have fully started, the memory consumption drops to under 700Mb; however, if all the country servers are (re)started simultaneously, nearly the full 2Gb of memory could be required during the startup process.

##  Server Startup

### Country Manager

When this app is deployed to CF and started, the `country_manager` process starts.  This is the supervisor responsible for managing the lifecycle of all country servers.  One country server can be started for each country listed in the GeoNames file [countryInfo.txt](http://download.geonames.org/export/dump/countryInfo.txt).

After deployment to Cloud Foundry, by default, none of the country servers are started.  The country servers must be started manually using the admin interface.

### Admin Interface

In order to start one or more country servers, you must use the `geo-server` admin interface.  This is accessed by adding `/server_info` to `geo-server`'s deployed URL.

![Admin Interface During Startup](./docs/Admin%20Interface.png "Admin Interface During Startup")

Currently, no authentication is required to access this page.

If you do not start any country servers, all search queries will return an empty JSON array!

### Country Server Startup

Once connected to the admin screen, you can either start all the servers at once by clicking on the "Start all servers" button, or individual country servers can be started as required.

#### Startup Processing

The following startup sequence is performed for each country server:

1. When a country server is started for the first time, a [ZIP file](http://download.geonames.org/export/dump/) containing all the geographic and geopolitical information for that country is downloaded from the GeoNames website.

1. Once unzipped, the resulting text file contains a large amount of information - much of which is not relevant for `geo-server`'s requirements.  Therefore, the text file is scanned only for those records having a feature class of "P" (population centres) or "A" (administrative areas).  All other records are ignored.

    * A further restriction is imposed here that feature class "P" records must refer to towns or cities having a population greater than some arbitrary limit (currently set to 500).

    * An internal `FCP` text file is created that contains all the feature class "P" records supplemented with the region information from the relevant feature class "A" records.  This data then becomes the searchable list of towns/cities for that particular country.

    * Each time a country server is started, the existence of the `FCP` text file is checked.  If it exists and is not stale, then the country server will start up using the information in the `FCP` text file rather than downloading the country's ZIP file again.  Starting a country server from its `FCP` text file greatly reduces the start up time from a few minutes down to a few seconds.

1. The eTag value for each country ZIP file is also stored.

    * If the country server is restarted more than 24 hours after the eTag data was downloaded, then the `FCP` text file is considered potentially stale.  The eTag value is now used to check if a new version of the country file exists.  If it does, the new ZIP file is downloaded and a new `FCP` text file is generated.


1. If you start a country server and find that it immediately stops with a substatus of `no_cities`, then this simply means this particular country contains no towns or cities with a population greater than the population limit (currently set to 500)



##  API

In order to perform a search, a client must send an HTTP `GET` request to the `geo-server` hostname with the path `/search`:

`geo-server.cfapps.<server>.hana.ondemand.com/search`

Followed by a query string containing the following three parameters

    search_term :: URL encoded string
    starts_with :: [true | false]
    whole_word  :: [true | false]

For example, to search for all cities containing the string "york" somewhere in the name, the URL would be:

`<hostname>/search?search_term=york&starts_with=false&whole_word=false`

Similarly, to search for all cities starting with the whole word "london", the URL would be:

`<hostname>/search?search_term=london&starts_with=true&whole_word=true`

##  Response

The client will then receive a JSON array containing zero or more instances of a city object.  An example city object is shown below:

    {
      "name": "London",
      "lat": 51.50853,
      "lng": -0.12574,
      "featureClass": "P",
      "featureCode": "PPLC",
      "countryCode": "GB",
      "admin1Txt": "England",
      "admin2Txt": "Greater London",
      "admin3Txt": "null",
      "admin4Txt": "null",
      "timezone": "Europe/London"
    }

###  City Object

Each city object contains the following properties:

| Property | Description |
|---|---|
|  `name` | The name of the city/town |
| `lat` | Latitude |
| `lng` | Longitude |
| `featureClass` | The GeoName feature class (See [http://www.geonames.org/export/codes.html](http://www.geonames.org/export/codes.html) for details) |
| `featureCode` | The GeoName feature code (See [http://www.geonames.org/export/codes.html](http://www.geonames.org/export/codes.html) for details) |
| `admin1Txt` | The name of the top level administrative region to which this town/city belongs |
| `admin2Txt` | The name of the 2nd level administrative region to which this town/city belongs |
| `admin3Txt` | The name of the 3rd level administrative region to which this town/city belongs |
| `admin4Txt` | The name of the 4th level administrative region to which this town/city belongs |
| `timeZone` | The name of the timezone in which this town/city is located |

