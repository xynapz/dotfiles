#!/bin/bash

# Weather script for Waybar
# Uses wttr.in API - no API key needed
# Change LOCATION to your city or leave empty for auto-detect

LOCATION="london"  # e.g., "London" or leave empty for auto-detect
UNITS="m"    # m=metric, u=imperial

# Fetch weather data
if [ -z "$LOCATION" ]; then
    weather=$(curl -sf "wttr.in/?format=j1&$UNITS")
else
    weather=$(curl -sf "wttr.in/$LOCATION?format=j1&$UNITS")
fi

if [ -z "$weather" ]; then
    echo '{"text":"Weather N/A","tooltip":"Unable to fetch weather data"}'
    exit 0
fi

# Parse JSON using grep and sed (no jq dependency)
temp=$(echo "$weather" | grep -oP '"temp_C":"\K[^"]+' | head -1)
feels=$(echo "$weather" | grep -oP '"FeelsLikeC":"\K[^"]+' | head -1)
condition=$(echo "$weather" | grep -oP '"weatherDesc":\[{"value":"\K[^"]+' | head -1)
humidity=$(echo "$weather" | grep -oP '"humidity":"\K[^"]+' | head -1)
wind=$(echo "$weather" | grep -oP '"windspeedKmph":"\K[^"]+' | head -1)
precip=$(echo "$weather" | grep -oP '"precipMM":"\K[^"]+' | head -1)
uv=$(echo "$weather" | grep -oP '"uvIndex":"\K[^"]+' | head -1)

# Weather icon mapping
case "$condition" in
    *"Clear"*|*"Sunny"*) icon="☀️" ;;
    *"Partly cloudy"*) icon="⛅" ;;
    *"Cloudy"*|*"Overcast"*) icon="☁️" ;;
    *"rain"*|*"drizzle"*) icon="🌧️" ;;
    *"thunder"*) icon="⛈️" ;;
    *"snow"*) icon="❄️" ;;
    *"fog"*|*"mist"*) icon="🌫️" ;;
    *) icon="🌡️" ;;
esac

# Format output - escape special characters for JSON
text="$icon ${temp}°C"

tooltip="<b>${condition}</b>\\nTemperature: ${temp}°C (feels like ${feels}°C)\\nHumidity: ${humidity}%\\nWind: ${wind} km/h\\nPrecipitation: ${precip} mm\\nUV Index: ${uv}"

# Properly format JSON
cat <<EOF
{"text":"$text","tooltip":"$tooltip"}
EOF
