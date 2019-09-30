% Collecting ESAC-Net data
% 13 Oct 17

This was more complicated than getting the EARS-Net data. There's no download
button, so you can only view one year/drug/context combination at a time.

I wrote a pipeline to get this data:

- `abx_keys.tsv`, which I made manually, has all the options for antibiotics
- `scrape.py` loops over years, antibiotics, and contexts and downloads the webpages
- `extract.py` looks inside those webpages and pulls out the data tables
- cleaning scripts check that the data looks how I expect, etc.
