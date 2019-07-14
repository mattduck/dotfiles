#!/usr/bin/env bash
WHITE=ffffd7
YELLOW=fabd2f
GREY=ebdbb2

ICON_LOAD=
ICON_WIFI=
ICON_AUDIO_HIGH=
ICON_AUDIO_LOW=
ICON_AUDIO_EMPTY=
ICON_AUDIO_MUTE=
ICON_BRIGHTNESS=
ICON_BATTERY_FULL=
ICON_BATTERY_THREE_QUARTERS=
ICON_BATTERY_HALF=
ICON_BATTERY_QUARTER=
ICON_BATTERY_EMPTY=

IFS=$'\n'

# text <string> <colour_name>
function text { output+=$(echo -n '{"full_text": "'${1//\"/\\\"}'", "color": "#'${2-$WHITE}'", "separator": false, "separator_block_width": 1}, ') ;}

echo -e '{ "version": 1 }\n['
while :; do

    AUDIO=$(amixer sget Master | grep 'Mono:' | awk -F'[][]' '{ print $2 }' | sed 's/%//')
    if [[ $AUDIO -gt 99 ]]; then ICON_AUDIO=$ICON_AUDIO_HIGH;
    elif [[ $AUDIO -gt 5 ]]; then ICON_AUDIO=$ICON_AUDIO_LOW;
    else ICON_AUDIO=$ICON_AUDIO_EMPTY;
    fi

    BATTERY_PERCENT=$(acpi | grep 'Battery 1' | awk '{print $4}' | sed 's/,//' | sed 's/%//')
    BATTERY_TIME_REMAINING=$(acpi | grep 'Battery 1' | awk '{print $5}' | sed 's/,//' | cut -d ":" -f 1,2)
    if [[ $BATTERY_PERCENT -gt 80 ]]; then ICON_BATTERY=$ICON_BATTERY_FULL;
    elif [[ $BATTERY_PERCENT -gt 60 ]]; then ICON_BATTERY=$ICON_BATTERY_THREE_QUARTERS;
    elif [[ $BATTERY_PERCENT -gt 30 ]]; then ICON_BATTERY=$ICON_BATTERY_HALF;
    elif [[ $BATTERY_PERCENT -gt 10 ]]; then ICON_BATTERY=$ICON_BATTERY_QUARTER;
    else ICON_BATTERY=$ICON_BATTERY_EMPTY;
    fi

    NETWORK_NAME=$(iwgetid --raw)
    WIFI_SIGNAL=''
    BATTERY_STATUS=$(acpi | grep 'Battery 1' | awk '{print $3}' | sed 's/,//')
    BRIGHTNESS=$(xbacklight -get | awk -F . '{print $1}')
    ETHERNET=''
    LOAD=$(cat /proc/loadavg | awk '{ print $1 }')
    DATE=$(date +'%b-%d %H:%M')

    output=''
    text "     $ICON_LOAD/$LOAD" $GREY
    text "     $ICON_WIFI/$NETWORK_NAME"
    text "     $ICON_AUDIO/$AUDIO" $GREY
    text "     $ICON_BRIGHTNESS/$BRIGHTNESS"
    #text "     $ICON_BATTERY/$BATTERY_PERCENT/$BATTERY_TIME_REMAINING/$BATTERY_STATUS" $GREY
    text "     $ICON_BATTERY/$BATTERY_PERCENT/$BATTERY_STATUS" $GREY
    text "     $DATE"
    echo -e "[${output%??}],"
    sleep 3
done
