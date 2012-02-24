reval_url = "http://www2.alleghenycounty.us/reval/GeneralInfo.aspx?ParcelID="

baseval_url = "http://www2.county.allegheny.pa.us/RealEstate/GeneralInfo.aspx?ParcelID="

reval_bldg_url = "http://www2.alleghenycounty.us/reval/Building.aspx?ParcelID="

baseval_bldg_url = "http://www2.county.allegheny.pa.us/RealEstate/Building.aspx?ParcelID="

scrape = bin/scrape_properties.py
decode = bin/decode_page.py


default:
	cat Makefile

.PHONY: default

.PHONY: fetch_reval
fetch_reval:
	mkdir -p data/reval
	$(scrape) $(reval_url) data/reval data/mtlebo_properties.csv

.PHONY: fetch_baseval
fetch_baseval:
	mkdir -p data/baseval
	$(scrape) $(baseval_url) data/baseval data/mtlebo_properties.csv

.PHONY: fetch_reval_bldg
fetch_reval_bldg:
	mkdir -p data/reval_bldg
	$(scrape) $(reval_bldg_url) data/reval_bldg data/mtlebo_properties.csv

.PHONY: fetch_baseval_bldg
fetch_baseval_bldg:
	mkdir -p data/baseval_bldg
	$(scrape) $(baseval_bldg_url) data/baseval_bldg data/mtlebo_properties.csv


data/mtlebo_reval.csv: $(decode) data/reval_labels.csv
	$(decode) data/reval_labels.csv data/reval/*.html > $@

data/mtlebo_baseval.csv: $(decode) data/baseval_labels.csv
	$(decode) data/baseval_labels.csv data/baseval/*.html > $@

data/mtlebo_reval_bldg.csv: $(decode) data/reval_bldg_labels.csv
	$(decode) data/reval_bldg_labels.csv data/reval_bldg/*.html > $@

data/mtlebo_baseval_bldg.csv: $(decode) data/baseval_bldg_labels.csv
	$(decode) data/baseval_bldg_labels.csv data/baseval_bldg/*.html > $@
