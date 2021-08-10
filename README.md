# wx

This is a very early-stage emacs mode intended to retrieve and display
weather forecasts and observations from the U.S. National Weather Service.

It uses aeronautical sources, namely METARs and TAFs, and it was
written with a pilot's eye.

I wrote this code a bit at a time during my commute to work, so it is what it is.
If you find it useful, use it. 
Don't send me feature requests - I don't lack ideas, I lack the time to implement them.
Send pull requests instead.

# Usage
Type `M-x wx`.
A new buffer will appear looking like the screenshot below.

![](https://github.com/scarpazza/wx/blob/main/screenshot.png)

In this listing, observations and forecasts are presented from the future to the past: at the bottom of the screen you'll find past weather, and at the top of the screen you'll find the most forward looking forecast.


Observations typically have temperature and humidity, in addition to wind direction, cloud coverage and weather phenomena (such as rain).
Forecasts typically only have wind, weather phenomena and cloud coverage.


Click on most column to sort observations and forecasts by the values in that column.


# Configuration

Type `M-x customize-variable` and then `Weather`.
All customizable variables will appear.

You should only bother changing the airport closest to home.

(to be continued)
