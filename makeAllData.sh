export searchDir=~/Research/orpheus-results/
export csvFile=_resultsHUGE.csv
export resultFile=all-data.csv

find "$searchDir" -name "$csvFile" -exec tail -1 {} \; | sort > "$resultFile"