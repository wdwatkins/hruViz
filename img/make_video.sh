#!/bin/bash

#use ffmpeg to make video from pngs, need to download the executable first
~/Downloads/ffmpeg-1 -framerate 5 -i day_%03d.png $1
