#!/usr/bin/env bash
WHITE=ffffd7
YELLOW=d79921
# GREEN=98971a
GREEN=b8bb26
GREY1=ebdbb2
GREY2=d5c4a1
GREY3=bdae93
GREY4=a89984
GREY=$GREY4
# RED=cc241d
RED=fb4934
AQUA=689d6a
ORANGE=d65d0e
PURPLE=d3869b

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
ICON_BATTERY_PLUGGED_IN=🔌
ICON_BATTERY_DISCHARGE=🔋
ICON_CLOCK=⌚
ICON_CALENDAR=📅
ICON_FLOPPY_A=💾
ICON_FLOPPY_B=🖫
ICON_PENGUIN=🐧
ICON_PENCIL=✎
ICON_ORG_MODE=$ICON_PENCIL
IFS=$'\n'

# text <string> <colour_name>
function text { output+=$(echo -n '{"full_text": "'${1//\"/\\\"}'", "color": "#'${2-$WHITE}'", "separator": false, "separator_block_width": 10}, ') ;}

echo -e '{ "version": 1, "click_events": true }\n['
while :; do

    AUDIO=$(amixer sget Master | grep 'Mono:' | awk -F'[][]' '{ print $2 }' | sed 's/%//')
    if [[ $AUDIO -gt 99 ]]; then ICON_AUDIO=$ICON_AUDIO_HIGH;
    elif [[ $AUDIO -gt 5 ]]; then ICON_AUDIO=$ICON_AUDIO_LOW;
    else ICON_AUDIO=$ICON_AUDIO_EMPTY;
    fi

    ACPI="$(acpi -b)"
    # The main battery is either Battery 1 or 0 depending on
    # circumstances. Think it's whether it's plugged in or not.
    if [[ "$ACPI" == *"Battery 1"* ]]; then BATTERY_ID="Battery 1"; else BATTERY_ID="Battery 0"; fi
    # BATTERY_TIME_REMAINING=$(echo "$ACPI" | grep '$BATTERY_ID' | awk '{print $5}' | sed 's/,//' | cut -d ":" -f 1,2)
    BATTERY_PERCENT=$(echo "$ACPI" | grep "$BATTERY_ID" | awk -F'[,:%]' '{print $3}' | sed 's/ //g')
    BATTERY_STATUS=$(echo "$ACPI" | grep "$BATTERY_ID" | awk -F'[,:%]'  '{print $2}' | sed 's/ //g')
    if [[ $BATTERY_STATUS == *"Unknown"* ]] && [[ "$(acpi -a)" == *"on-line"* ]] ; then BATTERY_STATUS="Full"; fi
    if [[ $BATTERY_PERCENT -gt 80 ]]; then ICON_BATTERY=$ICON_BATTERY_FULL; BATTERY_COLOR=$GREY;
    elif [[ $BATTERY_PERCENT -gt 60 ]]; then ICON_BATTERY=$ICON_BATTERY_THREE_QUARTERS; BATTERY_COLOR=$GREY;
    elif [[ $BATTERY_PERCENT -gt 30 ]]; then ICON_BATTERY=$ICON_BATTERY_HALF; BATTERY_COLOR=$YELLOW;
    elif [[ $BATTERY_PERCENT -gt 10 ]]; then ICON_BATTERY=$ICON_BATTERY_QUARTER; BATTERY_COLOR=$RED;
    else ICON_BATTERY=$ICON_BATTERY_EMPTY;
    fi
    if [[ $BATTERY_STATUS == "Discharging" ]]; then ICON_BATTERY_STATUS=$ICON_BATTERY_DISCHARGE;
    elif [[ $BATTERY_STATUS == "Full" ]]; then ICON_BATTERY_STATUS=$ICON_BATTERY_PLUGGED_IN;
    elif [[ $BATTERY_STATUS == "Charging" ]]; then ICON_BATTERY_STATUS=$ICON_BATTERY_PLUGGED_IN;
    else ICON_BATTERY_STATUS="?";
    fi

    NETWORK_NAME=$(iwgetid --raw)
    BRIGHTNESS=$(xbacklight -get | awk -F . '{print $1}')
    LOAD=$(cat /proc/loadavg | awk '{ print $1 }')
    DT_DATE=$(date +'%a %d %b')
    DT_TIME=$(date +'%H:%M')

    DUP_S3_COLOR="$RED"
    if test -f /f/duplicity/t450s.s3.last_success; then
        DUP_S3_DATE=$(cat /f/duplicity/t450s.s3.last_success)
        DUP_S3_SECONDS_AGO=$(( ( $(date +%s) - $(date -d "$DUP_S3_DATE" +%s) )))
        DUP_S3_MINUTES_AGO=$(($DUP_S3_SECONDS_AGO / (60)))
        DUP_S3_HOURS_AGO=$(($DUP_S3_SECONDS_AGO / (60 * 60)))
        DUP_S3_DAYS_AGO=$(($DUP_S3_SECONDS_AGO / (24 * 60 * 60)))
        if [ $DUP_S3_DAYS_AGO -gt 1 ]; then DUP_S3_MSG="${DUP_S3_DAYS_AGO}d";
        elif [ $DUP_S3_HOURS_AGO -gt 1 ]; then DUP_S3_MSG="${DUP_S3_HOURS_AGO}h";
        elif [ $DUP_S3_MINUTES_AGO -gt 1 ]; then DUP_S3_MSG="${DUP_S3_MINUTES_AGO}m";
        elif [ $DUP_S3_SECONDS_AGO -gt 1 ]; then DUP_S3_MSG="${DUP_S3_SECONDS_AGO}s";
        else DUP_S3_MSG="?";
        fi
        if [ $DUP_S3_HOURS_AGO -lt 8 ]; then DUP_S3_COLOR="$GREY"; fi
    else DUP_S3_MSG="?";
    fi

    DUP_USB_COLOR="$RED"
    if test -f /f/duplicity/t450s.usb.last_success; then
        DUP_USB_DATE=$(cat /f/duplicity/t450s.usb.last_success)
        DUP_USB_SECONDS_AGO=$(( ( $(date +%s) - $(date -d "$DUP_USB_DATE" +%s) )))
        DUP_USB_MINUTES_AGO=$(($DUP_USB_SECONDS_AGO / (60)))
        DUP_USB_HOURS_AGO=$(($DUP_USB_SECONDS_AGO / (60 * 60)))
        DUP_USB_DAYS_AGO=$(($DUP_USB_SECONDS_AGO / (24 * 60 * 60)))
        if [ $DUP_USB_DAYS_AGO -gt 1 ]; then DUP_USB_MSG="${DUP_USB_DAYS_AGO}d";
        elif [ $DUP_USB_HOURS_AGO -gt 1 ]; then DUP_USB_MSG="${DUP_USB_HOURS_AGO}h";
        elif [ $DUP_USB_MINUTES_AGO -gt 1 ]; then DUP_USB_MSG="${DUP_USB_MINUTES_AGO}m";
        elif [ $DUP_USB_SECONDS_AGO -gt 1 ]; then DUP_USB_MSG="${DUP_USB_SECONDS_AGO}s";
        else DUP_USB_MSG="?";
        fi
        if [ $DUP_USB_DAYS_AGO -lt 1 ]; then DUP_USB_COLOR="$GREY"; fi
    else DUP_USB_MSG="?";
    fi
    PACMAN_DATE=$(,pacman-when)

    EMACS_CLOCK=$(emacsclient -ne '(ignore-errors (md/org-clock-status))' || '-');

    output=''
    # The 1:-1 thing strips the wrapping quotation marks from emacsclient.
    text "  ${ICON_ORG_MODE} ${EMACS_CLOCK:1:-1}" $WHITE
    text "  ${ICON_PENGUIN} $PACMAN_DATE" $GREY
    text "  ${ICON_FLOPPY_A} S3:$DUP_S3_MSG" $DUP_S3_COLOR
    text "  ${ICON_FLOPPY_A} USB:$DUP_USB_MSG" $DUP_USB_COLOR
    text "  ${ICON_LOAD} $LOAD" $GREY
    text "  ${ICON_WIFI} $NETWORK_NAME" $GREY
    text "  ${ICON_AUDIO} $AUDIO" $GREY
    text "  ${ICON_BRIGHTNESS} $BRIGHTNESS" $GREY
    text "  ${ICON_BATTERY_STATUS} ${ICON_BATTERY} ${BATTERY_PERCENT}" $BATTERY_COLOR
    text "  ${ICON_CLOCK} ${DT_DATE}, $DT_TIME" $GREY
    text "  "
    # text "               "
    echo -e "[${output%??}],"
    sleep 2

    # Events would be read like this
    # while read -r line; do
    #     echo '%s\n' "$line" >> /tmp/i3-status
    # done
done
