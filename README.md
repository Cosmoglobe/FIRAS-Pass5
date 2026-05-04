This is the directory for development of the low-level re-analysis of FIRAS data. It contains the following:

- `notes`: Information about the FIRAS instrument and data.
- `original_pipeline`: The files used to generate the original FIRAS data products.
- `reference`: Reference datasets (e.g. sampling rate, etc).
- `src`: Source code for the FIRAS re-analysis. To run, use `python -m {DIR}.{MOD}`, where `{DIR}` and `{MOD}` correspond to what you want to run, e.g. use `python -m pipeline.main` to run the pipeline.

Products of the current analysis can be found here.

To run the full analysis, run the following, inside the `src` directory:
```
python -m data.data_prep_v5
python -m pipeline.main
python -m pipeline.plot_map
python -m pipeline.firas_curve
```
where the second to last line plots the produced maps and the last line computes the BB curve.

To make the gif of the maps over frequencies, run `animations.sh` from the visualizations module.