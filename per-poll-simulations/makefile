polls := 0045-reid-research \
	0060-colmar-brunton \
	0060-reid-research \
	0082-colmar-brunton \
	0110-reid-research \
	0110-roy-morgan \
	0117-colmar-brunton \
	0138-roy-morgan \
	0167-roy-morgan \
	0187-colmar-brunton \
	0193-reid-research \
	0202-roy-morgan \
	0222-colmar-brunton \
	0230-roy-morgan \
	0257-roy-morgan \
	0293-roy-morgan \
	0309-colmar-brunton \
	0320-roy-morgan \
	0342-roy-morgan \


.PHONY: all-polls latest

latest: 0045-reid-research/Simulated-PV.csv

all-polls: $(addsuffix /Simulated-PV.csv,$(polls))

%/Simulated-PV.csv: common/Election-Simulation-pre-ME-Polls.R %/config.R %/PollsForGam.csv
	cd $* && Rscript ../$^ 2>error.txt


