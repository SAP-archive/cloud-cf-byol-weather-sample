# cloud-cf-byol-weather-sample

<!-- *********************************************************************** -->
<a name="contents"></a>
## Table of Contents
1. [Overview](#overview)
1. [Description](#description)
1. [Requirements](#requirements)
1. [Download & Installation](#download)
1. [Configuration](#configuration)
1. [Limitations](#limitations)
1. [Known Issues](#issues)
1. [Support and Contributing](#support)
1. [Authors](#authors)
1. [License](#license)


<!-- *********************************************************************** -->
<a name="overview"></a>
## Cloud Foundry's "Bring Your own Language" Concept

Java is most certainly one of the most popular development languages at the moment; however, Cloud Foundry offers developers the opportunity to create their own applications using almost ***any*** programming language.

The only factors constraining your choice of language are:

* Has your chosen language been implemented to run on Windows or Linux?
* Does your chosen language have a Cloud Foundry buildpack?

If the answer to the first question is "No", then unfortunately all bets are off and you must choose a different language.

If the answer to the second question is "No", then although you might have to do a little more work, this is certainly not a show-stopper.

Please read the [Cloud Foundry Buildpack](./docs/cf_buildpacks.md) document for more information about build packs in general and how to create your own.

<a href="#contents">Top</a>

<!-- *********************************************************************** -->
<a name="description"></a>
## Description

The Weather Demo is written in two parts:

* A server-side part that acts as a search engine to supply town/city information
* A client-side part that acts as the user interface for displaying a weather report for any town/city in the world returned from the server

![Solution Diagram](./docs/Weather%20Demo%20Solution%20Diagram.png "Weather Demo Solution Diagram")

### GeoServer

The server-side part of this demo is known as GeoServer.  It has been written in [Erlang](http://www.erlang.org) and deployed to Cloud Foundry using a Cloud Foundry [Community Buildpack](https://github.com/ChrisWhealy/cf-buildpack-erlang)

Detailed documentation for GeoServer can be found [here](./geo_server/README.md).

### Weather Report

The client-side part is written in Scala and compiled to JavaScript using the [Scala.js](https://www.scala-js.org) cross-compiler.  Since the cross-compilation process creates a static JavaScript file, this part of the demo can be deployed to Cloud Foundry using the standard Staticfile buildpack.

Detailed documentation for Weather Report can be found [here](./weather_report/README.md).




<a href="#contents">Top</a>

<!-- *********************************************************************** -->
<a name="requirements"></a>
## Requirements

Since these two parts of the application have been written in different languages and are designed to fulfil different roles, each part has its own specific requirements and deployment instructions.

Please refer to the README documents in the respective `weather_report` and `geo_server` subdirectories to find the relevant deployment instructions.


<a href="#contents">Top</a>

<!-- *********************************************************************** -->
<a name="download"></a>
## Download and Installation

First of all, clone this repository into some suitable directory on your local machine.

    $ git clone https://github.wdf.sap.corp/coolapps/WeatherDemo.git

In your local clone directory, you will now have two subdirectories: `./geo_server` and `./weather_report`.  The former contains the server-side part of the app and the latter, the client-side part.

Since these two parts of the application have been written in different languages and are designed to fulfil different roles, each part has its own specific deployment instructions.  

Please refer to the README documents in the respective `weather_report` and `geo_server` subdirectories to find the relevant installation instructions.



<a href="#contents">Top</a>

<!-- *********************************************************************** -->
<a name="configuration"></a>
## Configuration

Please refer to the README documents in the respective `weather_report` and `geo_server` subdirectories to find the relevant configuration instructions.


<a href="#contents">Top</a>

<!-- *********************************************************************** -->
<a name="limitations"></a>
## Limitations

The server-side app called `geo_server` currently stores all geopolitical data as flat text files.  These files must be downloaded when the application starts, and ***do not*** persist if `geo_server` is stopped.

Therefore, this download process must be repeated each time `geo_server` is started.

<a href="#contents">Top</a>

<!-- *********************************************************************** -->
<a name="issues"></a>
## Known Issues

No known issues so far...


<a href="#contents">Top</a>

<!-- *********************************************************************** -->
<a name="support"></a>
<a name="contributing"></a>
## Support and Contributing

This project is provided "as-is": there is no guarantee that raised issues will be answered or addressed in future releases.


<a href="#contents">Top</a>

<!-- *********************************************************************** -->
<a name="authors"></a>
## Authors
+ **Chris Whealy**  
    <https://github.com/ChrisWhealy>


<a href="#contents">Top</a>

<!-- *********************************************************************** -->
<a name="license"></a>
## License

Copyright (c) 2018 SAP SE or an SAP affiliate company. All rights reserved.
This project is licensed under the Apache Software License, Version 2.0 except as noted otherwise in the [LICENSE](LICENSE) file.

Any Open Source libraries used by these applications are listed in the respective language-specific files:

+ [Scala](./CREDITS-SCALA)
+ [Erlang](./CREDITS-ERLANG)

<a href="#contents">Top</a>
