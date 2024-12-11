#!/bin/bash

# Piper parameters configuration
MODEL="en_US-lessac-medium.onnx" # Text-to-speech model
OUTPUT_FORMAT="raw"             # Output format (e.g. raw, wav)
SAMPLE_RATE=22050               # Sampling rate
AUDIO_FORMAT="S16_LE"           # Audio format for aplay

# User input control
if [ -z "$1" ]; then
    echo "Usage: $0 \"Text to synthesize\""
    exit 1
fi

TEXT="$1"

# Check existence of model and Piper
if [ ! -f "./piper" ]; then
    echo "Error: The Piper executable file is not present in the current directory."
    exit 2
fi

if [ ! -f "$MODEL" ]; then
    echo "Error: Model $MODEL is not present in the current directory."
    exit 3
fi

# Text-to-speech flow
echo "$TEXT" | ./piper --model "$MODEL" --output-$OUTPUT_FORMAT | aplay -r "$SAMPLE_RATE" -f "$AUDIO_FORMAT" -t "$OUTPUT_FORMAT" -
