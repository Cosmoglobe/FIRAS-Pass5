#!/bin/bash

save_path="/mn/stornext/d16/www_cmb/aimartin/firas/"
cd $save_path
cd "maps/frequency_maps/"

channels="ll"
modes="ss"
for channel in $channels; do
    for mode in $modes; do
        cd "${channel}_${mode}/galactic/"
        convert -delay 20 -loop 0 *.png "${channel}${mode}.gif"
    done
done